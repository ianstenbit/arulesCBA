# Note: mineCars redefined confidence to .5 and maxlen to 5!

mineCARs <- function(formula, transactions, parameter = NULL, control = NULL, balanceSupport = FALSE, verbose = TRUE, ...) {

  if(!is(transactions, "transactions")) stop("transactions needs to contain an object of class transactions.")
  vars <- .parseformula(formula, transactions)

  control <- as(control, "APcontrol")
  if(!verbose) control@verbose <- FALSE

  dotParameter <-  list(...)
  if(!is.null(parameter) && length(dotParameter)>0) stop("You cannot specify parameters only either using parameter or ...")
  if(is.null(parameter)) parameter <- dotParameter
  if(is.null(parameter) || is.list(parameter)) {
    if(is.null(parameter$conf)) parameter$confidence <- .5
    if(is.null(parameter$maxlen)) parameter$maxlen <- 5L
  }
  parameter <- as(parameter , "APparameter")

  # Generate CARs with APRIORI
  if(is.logical(balanceSupport) &&!balanceSupport) {
    # single support

    ### suppress maxlen warnings!
    suppressWarnings(
      cars <- apriori(transactions, parameter = parameter,
        appearance = list(rhs=vars$class_names, lhs=vars$var_names),
        control=control)
    )
  }else{
    if(is.numeric(balanceSupport)) {
    # specify class support directly
      if(length(balanceSupport) != length(vars$class_ids))
        stop("balanceSupport requires One support value for each class label.")
      support <- balanceSupport
      if(is.null(names(support))) names(support) <- vars$class_names

    }else{ # balanceSupport is TRUE
      # Following roughly: Liu B., Ma Y., Wong C.K. (2000) Improving an Association
      #  Rule Based Classifier.
      classSupport <- itemFrequency(transactions)[vars$class_ids]
      support <- parameter@support * classSupport/max(classSupport)
    }

    rs <- lapply(names(support), FUN = function(rhs) {
      if(control@verbose) cat("\n*** Mining CARs for class", rhs, "***\n")

      parameter@support <- support[[rhs]]

      ### suppress maxlen warnings!
      suppressWarnings(
        apriori(transactions, parameter = parameter,
          appearance = list(rhs = rhs, lhs=vars$var_names),
          control=control)
      )
    })

    cars <- do.call(c, rs)

  }

  cars
}
