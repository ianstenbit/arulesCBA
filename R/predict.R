
predict.CBA <- function(object, newdata, type = c("class", "score"), ...){

  type <- match.arg(type)

  method <- object$method
  if(is.null(method)) method <- "majority"

  methods <- c("first", "majority", "weighted", "logit")
  m <- pmatch(method, methods)
  if(is.na(m)) stop("Unknown method")
  method <- methods[m]

  if(!is.null(object$discretization)) {
    newdata <- discretizeDF(newdata, lapply(object$discretization,
      FUN = function(x) list(method="fixed", breaks=x)))
    newdata <- as(newdata, "transactions")
  } else {
    if(!is(newdata, "transactions"))
    stop("Classifier does not contain discretization information. New data needs to be in the form of transactions. Check ? discretizeDF.")
  }

  # If new data is not already transactions:
  # Convert new data into transactions and use recode to make sure
  # the new data corresponds to the model data
  newdata <- recode(newdata, match = lhs(object$rules))

  # Matrix of which rules match which transactions (sparse is only better for more
  # than 150000 entries)
  rulesMatchLHS <- is.subset(lhs(object$rules), newdata,
    sparse = (length(newdata) * length(rules(object)) > 150000))
  dimnames(rulesMatchLHS) <- list(NULL, NULL)

  # find class label for each rule
  RHSclass <- response(object$formula, object$rules)
  class_levels <- levels(RHSclass)

  #class_levels <- sapply(strsplit(object$class, '='), '[',2)
  ##RHSclass <- unname(unlist(as(rhs(object$rules), "list")))
  #RHSclass <- sapply(strsplit(RHSclass, '='), '[', 2)
  #RHSclass <- factor(RHSclass, levels = class_levels)

  # Default class
  default <- strsplit(object$default, '=')[[1]][2]
  defaultLevel <- which(class_levels == default)

  # classify using first match
  if(method == "first") {
    if(type =="score") stop("prediction type 'score' is not supported for CBA classifiers using classification method 'first' (matching rule).")

    w <- apply(rulesMatchLHS, MARGIN = 2, FUN = function(x) which(x)[1])
    output <- RHSclass[w]
    output[is.na(w)] <- default

    # preserve the levels of original data for data.frames
    #return(factor(output, levels = class_levels))
    return(output)
  }

  # For each transaction, if it is matched by any rule, classify it using
  # the majority, weighted majority

  # weights
  weights <- object$weights
  if(is.character(weights)) weights <- quality(object$rules)[[weights, exact = FALSE]]
  if(is.null(weights)) weights <- rep(1, length(object$rules))
  if(method == "majority") weights <- rep(1, length(object$rules))

  # transform weight vector into a matrix
  if(!is.matrix(weights)) {
    weights <- sapply(1:length(levels(RHSclass)), function(i) {
      w <- weights
      w[as.integer(RHSclass) != i] <- 0
      w
    })
  }

  if(nrow(weights) != length(object$rules) || ncol(weights) != length(levels(RHSclass)))
    stop("number of weights does not match number of rules/classes.")

  if(is.null(object$best_k)) {
    ### score is the sum of the weights of all matching rules

    # biases
    biases <- object$biases

    if(!is.null(biases) && nrow(biases) != length(levels(RHSclass)))
      stop("number of biases does not match number of rules/classes.")

    # sum score and add biases
    scores <- t(crossprod(weights, rulesMatchLHS))
    if(!is.null(biases)) scores <- sweep(scores, 2, biases, '+')
  }else{
    ### score is the average of the top-N matching rules (see CPAR paper by Yin and Han, 2003)

    scores <- t(apply(rulesMatchLHS, MARGIN = 2, FUN = function(m) {
      m_weights <- weights*m
      m_weights <- apply(m_weights, MARGIN = 2, sort, decreasing = TRUE)[1:object$best_k, , drop = FALSE]
      m_weights[m_weights == 0] <- NA
      score <- colMeans(m_weights, na.rm = TRUE)
      score[is.na(score)] <- 0
      score
      }))
  }

  colnames(scores) <- levels(RHSclass)

  if(method == "logit") scores <- exp(scores)/(1+rowSums(exp(scores)))

  if(type =="score") return(scores)

  # make sure default wins for ties
  scores[,defaultLevel] <- scores[,defaultLevel] + .Machine$double.eps

  output <- factor(apply(scores, MARGIN = 1, which.max),
    levels = 1:length(levels(RHSclass)),
    labels = levels(RHSclass))

  return(output)
}

