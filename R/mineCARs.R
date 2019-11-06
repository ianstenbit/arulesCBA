mineCARs <- function(formula, data, balanceSupport = FALSE,
  parameter = NULL, control = NULL, ...) {

  if(!is(data, "transactions")) stop("data needs to contain an object of class transactions.")
  vars <- .parseformula(formula, data)

  # add ... to parameters
  parameter <- as(parameter , "APparameter")
  moreParameter <- list(...)
  if(length(moreParameter) > 0) {
    replSlots <- slotNames(parameter)[pmatch(names(moreParameter), slotNames(parameter))]
    for(i in 1:length(replSlots)) slot(parameter, replSlots[i]) <- moreParameter[[i]]
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
