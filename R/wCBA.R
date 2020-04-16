wCBA <- function(formula, data,
  parameter = NULL, control = NULL, sort.parameter = NULL, lhs.support=FALSE, class.weights=NULL,
	disc.method = "mdlp", verbose=FALSE, ...){

    return(CBA.internal(formula, data, method="weighted", verbose = verbose,
      parameter = parameter, control = control, sort.parameter = sort.parameter,
      lhs.support = lhs.support, disc.method=disc.method, ...))

}
