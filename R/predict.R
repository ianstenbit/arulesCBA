predict.CBA <- function(object, newdata, method = "first", ...){

  methods <- c("first", "majority")
  m <- pmatch(method, methods)
  if(is.na(m)) stop("Unknown method")
  method <- methods[m]

  # If new data is not already transactions:
  # Convert new data into transactions and use recode to make sure
  # the new data corresponds to the model data
  newdata <- as(newdata, "transactions")
  newdata <- recode(newdata, match = lhs(object$rules))

  # Matrix of which rules match which transactions
  # FIXME: sparse is slower, check for large rule sets.
  # rulesMatchLHS <- is.subset(lhs(object$rules), newdata, sparse=TRUE)
  rulesMatchLHS <- is.subset(lhs(object$rules), newdata)
  dimnames(rulesMatchLHS) <- NULL

  # FIXME: we might have to check that the RHS has only a single item
  classifier.results <- unlist(as(rhs(object$rules), "list"))
  if(!is.null(object$levels))
    classifier.results <- sapply(strsplit(classifier.results, '='), '[', 2)

  # Default class
  default <- object$default
  if(!is.null(object$levels)) default <- strsplit(default, '=')[[1]][2]

  # For each transaction, if it is matched by any rule, classify it using the highest-precidence rule in the classifier

  if(method == "majority") {
    w <- apply(rulesMatchLHS, MARGIN = 2, FUN = function(x) which(x))
    output <- sapply(w, FUN = function(x) {
      n <- which.max(table(classifier.results[x]))
      if(length(n)==1) names(n) else NA
    })
    output[sapply(w, length)==0] <- default

  }else{ ### method = first
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
