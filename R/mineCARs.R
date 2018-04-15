mineCARs <- function(formula, data, parameter = NULL, control = NULL, ...) {

  if(!is(data, "transactions")) stop("data needs to contain an object of class transactions.")
  vars <- .parseformula(formula, data)

  #Generate CARs with APRIORI
  apriori(data, parameter = parameter,
    appearance = list(rhs=vars$class_names, lhs=vars$var_names),
    control=control, ...)
}
