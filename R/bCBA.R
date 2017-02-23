bCBA <- function(formula, data, support = 0.2, confidence = 0.8, gamma = 0.05, cost = 10.0,
  verbose=FALSE, parameter = NULL, control = NULL){

    return(CBA.internal(formula, data, method="weighted", support = support, confidence = confidence,
      gamma = gamma, cost = cost, verbose = verbose, parameter = parameter, control = control))

}
