CBA <- function(rules, data, class, disc.data, method="CBA", support = 0.2, confidence = 0.8, gamma = 0.05, cost = 10.0,
  verbose=FALSE, parameter = NULL, control = NULL, sort.parameter=NULL, lhs.support=TRUE, class.weights=NULL,
  disc.categories = 10, disc.method="cluster", sort.rules=FALSE){

    return(CBA.internal(rules, data, class, disc.data, method="CBA", support = 0.2, confidence = 0.8, gamma = 0.05, cost = 10.0,
      verbose=FALSE, parameter = NULL, control = NULL, sort.parameter=NULL, lhs.support=TRUE, class.weights=NULL,
      disc.categories = 10, disc.method="cluster", sort.rules=FALSE))

}
