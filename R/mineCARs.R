mineCARs <- function(formula, data, balanceSupport = FALSE,
  parameter = NULL, control = NULL, ...) {

  if(!is(data, "transactions")) stop("data needs to contain an object of class transactions.")
  vars <- .parseformula(formula, data)

  control <- as(control, "APcontrol")

  # add ... to parameters
  parameter <- as(parameter , "APparameter")
  moreParameter <- list(...)
  if(length(moreParameter) > 0) {
    replSlots <- slotNames(parameter)[pmatch(names(moreParameter), slotNames(parameter))]
    if(any(is.na(replSlots)))
      stop("Not valid slot names for class APparameter: ",
        paste(names(moreParameter)[is.na(replSlots)], collapse = ", "))

    for(i in 1:length(replSlots)) slot(parameter, replSlots[i]) <- moreParameter[[i]]
  }

  # Generate CARs with APRIORI

  if(is.logical(balanceSupport) &&!balanceSupport) {
    # single support
    cars <- apriori(data, parameter = parameter,
      appearance = list(rhs=vars$class_names, lhs=vars$var_names),
      control=control)
  }else{
    if(is.numeric(balanceSupport)) {
    # specify class support directly
      if(length(balanceSupport) != length(vars$class_ids))
        stop("balanceSupport requires 0ne support value for each class label.")
      support <- balanceSupport
      if(is.null(names(support))) names(support) <- vars$class_names

    }else{ # balanceSupport is TRUE
      # Following roughly: Liu B., Ma Y., Wong C.K. (2000) Improving an Association
      #  Rule Based Classifier.
      classSupport <- itemFrequency(data)[vars$class_ids]
      support <- parameter@support * classSupport/max(classSupport)
    }

    rs <- lapply(names(support), FUN = function(rhs) {
      if(control@verbose) cat("\n*** Mining CARs for class", rhs, "***\n")

      parameter@support <- support[[rhs]]

      apriori(data, parameter = parameter,
        appearance = list(rhs = rhs, lhs=vars$var_names),
        control=control)
    })

    cars <- do.call(c, rs)

  }

  cars
}
