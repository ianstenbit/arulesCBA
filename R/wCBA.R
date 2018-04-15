wCBA <- function(formula, data, support = 0.2, confidence = 0.8,
  verbose=FALSE, parameter = NULL, control = NULL, sort.parameter = NULL, lhs.support=FALSE, class.weights=NULL,
	disc.method = "mdlp"){

    return(CBA.internal(formula, data, method="weighted", support = support,
      confidence = confidence, verbose = verbose,
      parameter = parameter, control = control, sort.parameter = sort.parameter,
      lhs.support = lhs.support, disc.method=disc.method))

}
