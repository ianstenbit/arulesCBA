# formula helper

.parseformula <- function(formula, data) {
  formula <- as.formula(formula)
  vars <- all.vars(formula)

  ### class variables and values
  class <- vars[1]

  if(is(data, "itemMatrix")) {
    # make sure we only have the variable name
    class <- strsplit(class, "=")[[1]]
    if(length(class) > 1) {
      warning("lhs in formula contains a class variable with a value. Using only ",
        sQuote(class[1]), " as the class variable.")
      class <- class[1]
      }

    ii <- itemInfo(data)
    if(is.null(ii$variables) || is.null(ii$levels))
      stop("transaction data does not contain variables and levels in itemInfo.")
    class_ids <- which(ii$variables == class)
    class_names <- as.character(ii$levels[class_ids])
    class_items <- itemLabels(data)[class_ids]

  } else { ### for data.frame
    class_ids <- pmatch(class, colnames(data))
    if(!is.factor(data[[class_ids]]))
      stop("class variable needs to be a factor in the data!")
    class_names <- colnames(data)[class_ids]
    class_items <- NA
  }

  if(any(is.na(class_ids)) || length(class_ids) == 0)
    stop("Cannot identify column ", sQuote(class), " specified as class in the formula.")

  ### predictors
  vars <- vars[-1]

  if(is(data, "itemMatrix")) {
    ii <- itemInfo(data)
    if(length(vars) == 1 && vars == ".") var_ids <- setdiff(seq(ncol(data)), class_ids)
    else var_ids <- which(ii$variables %in% vars)
    var_names <- ii$variables[var_ids]
    var_items <- itemLabels(data)[var_ids]

  } else { ### for data.frame
    if(length(vars) == 1 && vars == ".")
      var_ids <- setdiff(which(sapply(data, is.numeric)), class_ids)
    else {
      var_ids <- pmatch(vars, colnames(data))
      if(any(is.na(var_ids)))
        stop(paste("Cannot identify term", vars[is.na(var_ids)], "in data!"))
    }
    var_names <- colnames(data)[var_ids]
    var_items <- NA
  }

  list(
    formula = formula,
    class_ids = class_ids, class_names = class_names, class_items = class_items,
    var_ids = var_ids, var_names = var_names, var_items = var_items
  )
}
