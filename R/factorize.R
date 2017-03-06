factorize <- function(formula, data, method="cluster", categories=10){

  formula <- as.formula(formula)
  class <- as.character(formula[[2]])
  if(as.character(formula[[3]]) != ".")
    stop("Formula needs to be of the form class ~ .")

  cols.to.discretize <- (colnames(data) != class & unlist(lapply(data, is.numeric)))

  data[cols.to.discretize] <- lapply(data[cols.to.discretize], function(x) discretize(x, method=method, categories=categories))
  cls <- data[[class]]
  data[[class]] <- NULL
  data <- cbind(data, cls)
  colnames(data)[length(data)] <- class

  return(data)

}
