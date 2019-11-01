mineCARs <- function(formula, data, balanceSupport = FALSE,
  parameter = NULL, control = NULL, ...) {

  if(!is(data, "transactions")) stop("data needs to contain an object of class transactions.")
  vars <- .parseformula(formula, data)

  # add ... to parameters
  moreParameter <- list(...)
  if(is.null(parameter)) parameter <- as(moreParameter, "APparameter")
  else {
    if(length(moreParameter) > 0) stop("Parameters cannot be specified in parameter and ...!")
    parameter <- as(parameter, "APparameter")
  }

  #Generate CARs with APRIORI
  if(!balanceSupport) {
    apriori(data, parameter = parameter,
      appearance = list(rhs=vars$class_names, lhs=vars$var_names),
      control=control)

  }else {
    # balance support (different support for each class)

    iSupport <- itemFrequency(data)[vars$class_ids]
    #freq <- itemFrequency(data[,vars$class_ids])

    suppMultiplier <- iSupport/max(iSupport)

    rs <- lapply(names(suppMultiplier), FUN = function(rhs) {

      newParameter <- parameter
      newParameter@support <- newParameter@support * suppMultiplier[[rhs]]

      apriori(data, parameter = newParameter,
        appearance = list(rhs = rhs, lhs=vars$var_names),
        control=control)
    })

    do.call(c, rs)

  }
}
