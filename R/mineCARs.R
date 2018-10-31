mineCARs <- function(formula, data, balanceSupport = FALSE,
  parameter = NULL, control = NULL, ...) {

  if(!is(data, "transactions")) stop("data needs to contain an object of class transactions.")
  vars <- .parseformula(formula, data)


  if(!balanceSupport) {
  #Generate CARs with APRIORI
  apriori(data, parameter = parameter,
    appearance = list(rhs=vars$class_names, lhs=vars$var_names),
    control=control, ...)
  }else {

  if(is.null(parameter)) parameter <- new("APparameter")
  else parameter <- as(parameter, "APparameter")

  iSupport <- itemFrequency(data)[vars$class_ids]
  #freq <- itemFrequency(data[,vars$class_ids])

  suppMultiplier <- iSupport/max(iSupport)

  rs <- lapply(names(suppMultiplier), FUN = function(rhs) {

    newParameter <- parameter
    newParameter@support <- newParameter@support * suppMultiplier[[rhs]]

    apriori(data, parameter = newParameter,
      appearance = list(rhs = rhs, lhs=vars$var_names),
      control=control, ...)

  })

  do.call(c, rs)

  }
}
