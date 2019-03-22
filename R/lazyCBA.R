lazyCBA <- function(formula, data, support = 0.2, confidence = 0.8, verbose = FALSE,
  parameter = NULL, control = NULL, sort.parameter = NULL, lhs.support = FALSE,
  disc.method = "mdlp"){

  data <- discretizeDF.supervised(formula, data, method=disc.method)

  classifier <- list(
    formula = formula,
    data = as(data, 'itemMatrix'),
    support = support,
    confidence = confidence,
    verbose = verbose,
    parameter = parameter,
    control = control,
    sort.parameter = sort.parameter,
    lhs.support = lhs.support,
    disc.method = disc.method,
    discretization = lapply(data, attr, "discretized:breaks"),
    description = "Lazy Associative Classifier",
    method = "lazy"
  )

  class(classifier) <- "CBA"
  return(classifier)

}
