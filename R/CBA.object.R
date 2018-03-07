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
  class <- as.character(formula[[2]])
  if(as.character(formula[[3]]) != ".")
    stop("Formula needs to be of the form class ~ .")

  # only use class rules
  take <- rhs(rules) %pin% class
  rules <- rules[take]
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

  levels <- grep(class, itemLabels(rules), value = TRUE, fixed=TRUE)
  levels <- sapply(strsplit(levels, '='), '[', 2)

  # FIXME: find default rules if it exists to set default class
  if(is.null(default))
    default <- names(which.max(table(unlist(as(rhs(rules), "list")))))

  classifier <- list(
    rules = rules,
    default = default,
    class = class,
    levels = levels,
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
    paste("Class:", x$class, "(labels:", paste(x$levels, collapse = ", "), ")"),
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

  if(!is.null(object$columnlevels)){

    class <- object$formula[[2]]
    cols.to.discretize <- (colnames(newdata) != class & unlist(lapply(newdata, is.numeric)))

    discretize.to.match <- function(newinput, colname){

      lvls <- object$columnlevels[[colname]]

      cuts <- unlist(lapply(strsplit(lvls, ','), function(x) (as.numeric(substr(x[1], 2, nchar(x[1]))))))[2:length(lvls)]
      cuts <- c(-Inf, cuts + .Machine$double.eps, Inf)

      return(discretize(newinput, method = "fixed", categories = cuts, labels = lvls))

    }

    newdata[cols.to.discretize] <- as.data.frame(mapply(discretize.to.match, newdata[cols.to.discretize], colnames(newdata)[cols.to.discretize]))

  }

  if(method == "weightedmean"){
    newdata <- as.data.frame(newdata)
    newdata[[object$class]] <- NULL
  }

  # If new data is not already transactions:
  # Convert new data into transactions and use recode to make sure
  # the new data corresponds to the model data
  newdata <- as(newdata, "transactions")
  newdata <- recode(newdata, match = lhs(object$rules))

  # Matrix of which rules match which transactions
  if(length(newdata) * length(rules(object)) > 150000){
    rulesMatchLHS <- is.subset(lhs(object$rules), newdata, sparse=TRUE)
  } else {
    rulesMatchLHS <- is.subset(lhs(object$rules), newdata)
    dimnames(rulesMatchLHS) <- list(NULL, NULL)
  }

  # FIXME: we might have to check that the RHS has only a single item
  classifier.results <- unlist(as(rhs(object$rules), "list"))
  if(!is.null(object$levels)) {
    classifier.results <- sapply(strsplit(classifier.results, '='), '[', 2)
    classifier.results <- factor(classifier.results, levels = object$levels)
  }

  # Default class
  default <- object$default
  if(!is.null(object$levels)) default <- strsplit(default, '=')[[1]][2]

  # For each transaction, if it is matched by any rule, classify it using
  # the majority, weighted majority or the highest-precidence
  # rule in the classifier


  if(method == "majority" | method == "weighted" | method == "weightedmean") {

    # unweighted
    if(is.null(object$weights)) {
      w <- apply(rulesMatchLHS, MARGIN = 2, FUN = function(x) which(x))
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

        r <- apply(rulesMatchLHS, MARGIN = 2, FUN = function(x) which(x))
        l <- lapply(r, FUN = function(x) classifier.results[x])
        w <- lapply(r, FUN = function(x) weights[x])
        w <- sapply(1:length(l), FUN = function(i) sapply(split(w[[i]], l[[i]]), sum))
        output <- rownames(w)[apply(w, MARGIN = 2, which.max)]
        output[sapply(r, length)==0] <- default

      } else {

        r <- apply(rulesMatchLHS, MARGIN = 2, FUN = function(x) which(x))
        l <- lapply(r, FUN = function(x) classifier.results[x])
        w <- lapply(r, FUN = function(x) object$weights[x])

        output <- lapply(as(1:length(l), 'list'), function(x) sum((object$means[l[[x]]] * w[[x]])) / sum(w[[x]]) )
        output <- unlist(output)

        output[is.nan(output)] <- object$mean

        return(output)

      }
    }

  }else { ### method = first
    w <- apply(rulesMatchLHS, MARGIN = 2, FUN = function(x) which(x)[1])
    output <- classifier.results[w]
    output[is.na(w)] <- default
  }


  # preserve the levels of original data for data.frames
  if(!is.null(object$levels))
    output <- factor(output, levels = object$levels, labels = object$levels)
  else
    output <- factor(output)

  return(output)

}

