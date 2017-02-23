CBA <- function(formula, data, support = 0.2, confidence = 0.8, gamma = 0.05, cost = 10.0,
  verbose=FALSE, parameter = NULL, control = NULL){

    return(CBA.internal(formula, data, method="CBA" support = 0.2, confidence = 0.8, gamma = 0.05, cost = 10.0,
      verbose=FALSE, parameter = NULL, control = NULL))

}
