bCBA <- function(formula, data, support = 0.2, confidence = 0.8, gamma = 0.05, cost = 10.0,
  verbose=FALSE, parameter = NULL, control = NULL, sort.parameter = NULL, lhs.support=FALSE, class.weights=NULL,
	disc.method = "mdlp"){

    return(CBA.internal(formula, data, method="boosted", support = support,
      confidence = confidence, gamma = gamma, cost = cost, verbose = verbose,
      parameter = parameter, control = control, sort.parameter = sort.parameter,
      lhs.support = lhs.support, disc.method=disc.method))

}
