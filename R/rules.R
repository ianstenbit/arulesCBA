rules <- function(x) UseMethod("rules")

rules.CBA <- function(x){
  return(x$rules)
}
