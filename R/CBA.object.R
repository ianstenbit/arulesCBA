# create a CBA object from a set of rules

### FIXME: we can specify weights/majority etc.

# Constructor
CBA_ruleset <- function(formula, rules, method = "first",
  weights = NULL, default = NULL, description = "Custom rule set"){

  # method
  methods <- c("first", "majority")
  m <- pmatch(method, methods)
  if(is.na(m)) stop("Unknown method")
  method <- methods[m]

  # find class
  formula <- as.formula(formula)
  vars <- .parseformula(formula, as(lhs(rules[1]), "transactions"))
  class <- vars$class_names

  # TODO: filter rules to only use predictors?
  if(all.vars(formula)[2] != ".")
    stop("Formula needs to be of the form class ~ .")

  # only use class rules
  take <- rhs(rules) %in% class
  rules <- rules[take]
  if(any(!take)) warning("Some provided rules are not CARs with the class in the RHS and are ignored. Only ",
    length(rules), " rules used.")

  if(!is.null(weights)) {
    if(is.character(weights))
      weights <- quality(rules)[[weights, exact = FALSE]]
    else {
      weights <- weights[take]
      quality(rules)$ weights <- weights
    }

    if(length(rules) != length(weights))
      stop("length of weights does not match number of rules")
  }

  # FIXME: find default rules if it exists to set default class
  # If not given, use the RHS for the rules with the largest support
  if(!is.null(default)) {
    default <- class[grepl(default, class)]
    if(length(default) != 1) stop("unable to identify default class")
    } else
      default <- names(which.max(sapply(split(quality(rules)$support,
      unlist(as(rhs(rules), "list"))), sum)))

  classifier <- list(
    rules = rules,
    default = default,
    class = class,
    method = method,
    weights = weights,
    description = description)

  class(classifier) <- "CBA"

  return(classifier)
}


# Methods
print.CBA <- function(x, ...)
  writeLines(c(
    "CBA Classifier Object",
    paste("Class:", paste(x$class, collapse = ", ")),
    paste("Default Class:", x$default),
    paste("Number of rules:", length(x$rules)),
    paste("Classification method:", x$method, if(!is.null(x$weights)) "(weighted)" else ""),
    strwrap(paste("Description:", x$description), exdent = 5),
    ""
  ))

rules <- function(x) UseMethod("rules")
rules.CBA <- function(x){
  return(x$rules)
}

predict.CBA <- function(object, newdata, ...){

  method <- object$method
  if(is.null(method)) method <- "majority"

  methods <- c("first", "majority", "weighted", "weightedmean")
  m <- pmatch(method, methods)
  if(is.na(m)) stop("Unknown method")
  method <- methods[m]

  if(!is.null(object$discretization))
    newdata <- discretizeDF(newdata, lapply(object$discretization,
      FUN = function(x) list(method="fixed", breaks=x)))

### MFH: ???
#  if(method == "weightedmean"){
#    newdata <- as.data.frame(newdata)
#    newdata[[object$class]] <- NULL
#  }

  # If new data is not already transactions:
  # Convert new data into transactions and use recode to make sure
  # the new data corresponds to the model data
  newdata <- as(newdata, "transactions")
  newdata <- recode(newdata, match = lhs(object$rules))

  # Matrix of which rules match which transactions (sparse is only better for more
  # than 150000 entries)
  rulesMatchLHS <- is.subset(lhs(object$rules), newdata,
        sparse = (length(newdata) * length(rules(object)) > 150000))
  dimnames(rulesMatchLHS) <- list(NULL, NULL)

  class_levels <- sapply(strsplit(object$class, '='), '[',2)
  classifier.results <- unlist(as(rhs(object$rules), "list"))
  classifier.results <- sapply(strsplit(classifier.results, '='), '[', 2)
  classifier.results <- factor(classifier.results, levels = class_levels)

  # Default class
  default <- strsplit(object$default, '=')[[1]][2]

  # For each transaction, if it is matched by any rule, classify it using
  # the majority, weighted majority or the highest-precidence
  # rule in the classifier


  if(method == "majority" | method == "weighted" | method == "weightedmean") {

    # unweighted
    if(is.null(object$weights)) {
      w <- lapply(1:ncol(rulesMatchLHS), FUN = function(i) which(rulesMatchLHS[,i]))
      output <- sapply(w, FUN = function(x) {
        n <- which.max(table(classifier.results[x]))
        if(length(n)==1) names(n) else NA
      })
      output[sapply(w, length)==0] <- default

    }else{

      if(is.null(object$means)){

        weights <- object$weights

        # use a quality measure
        if(is.character(weights))
          weights <- quality(object$rules)[[weights, exact = FALSE]]

        # replicate single value (same as unweighted)
        if(length(weights) == 1) weights <- rep(weights, length(object$rules))

        # check
        if(length(weights) != length(object$rules))
          stop("length of weights does not match number of rules")

        r <- lapply(1:ncol(rulesMatchLHS), FUN = function(i) which(rulesMatchLHS[,i]))
        l <- lapply(r, FUN = function(x) classifier.results[x])
        w <- lapply(r, FUN = function(x) weights[x])
        w <- sapply(1:length(l), FUN = function(i) sapply(split(w[[i]], l[[i]]), sum))
        output <- rownames(w)[apply(w, MARGIN = 2, which.max)]
        output[sapply(r, length)==0] <- default

      } else {

        r <- lapply(1:ncol(rulesMatchLHS), FUN = function(i) which(rulesMatchLHS[,i]))
        l <- lapply(r, FUN = function(x) classifier.results[x])
        w <- lapply(r, FUN = function(x) object$weights[x])

        output <- lapply(as(1:length(l), 'list'), function(x) sum((object$means[l[[x]]] * w[[x]])) / sum(w[[x]]) )
        output <- unlist(output)

        output[is.nan(output)] <- object$mean
        output <- factor(output, levels = class_levels)

        return(output)

      }
    }

  }else { ### method = first
    w <- apply(rulesMatchLHS, MARGIN = 2, FUN = function(x) which(x)[1])
    output <- classifier.results[w]
    output[is.na(w)] <- default
  }


  # preserve the levels of original data for data.frames
  output <- factor(output, levels = class_levels)

  return(output)

}

