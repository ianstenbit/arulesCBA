CBA <- function(formula, data, support = 0.2, confidence = 0.8,
  verbose=FALSE, parameter = NULL, control = NULL){

    return(CBA.internal(formula, data, method="CBA", support = support, confidence = confidence,
      verbose=FALSE, parameter = NULL, control = NULL))

}
