bCBA <- function(formula, data, gamma = 0.05, cost = 10.0,
  parameter = NULL, control = NULL, sort.parameter = NULL, lhs.support=FALSE, class.weights=NULL,
	disc.method = "mdlp", verbose=FALSE, ...){

    return(CBA.internal(formula, data, method="boosted", gamma = gamma, cost = cost, verbose = verbose,
      parameter = parameter, control = control, sort.parameter = sort.parameter,
      lhs.support = lhs.support, disc.method=disc.method, ...))

}
