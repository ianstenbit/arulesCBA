
### formula helper
.parseformula <- function(formula, data) {
  formula <- as.formula(formula)
  vars <- all.vars(formula)

  ### class
  # for transactions, class can match multiple items!
  class <- vars[1]

  if(is(data, "itemMatrix")) {
    class_ids <- which(grepl(paste0("^", class), colnames(data)))
  } else {
    class_ids <- pmatch(class, colnames(data))
  }
  if(any(is.na(class_ids)) || length(class_ids) == 0)
    stop("Cannot identify column specified as class in the formula.")
  class_names <- colnames(data)[class_ids]

  if(!is(data, "itemMatrix") && !is.factor(data[[class_ids]]))
    stop("class variable needs to be a factor!")

  ### predictors
  vars <- vars[-1]
  if(is(data, "itemMatrix")) {
    if(length(vars) == 1 && vars == ".") var_ids <- setdiff(seq(ncol(data)), class_ids)
    else var_ids <- which(grepl(paste0("^", vars, collapse = "|"), colnames(data)))
  } else {
    if(length(vars) == 1 && vars == ".") var_ids <- setdiff(which(sapply(data, is.numeric)), class_ids)
    else var_ids <- pmatch(vars, colnames(data))
  }

  if(any(is.na(var_ids))) stop(paste("Cannot identify term", vars[is.na(var_ids)], "in data! "))
  var_names <- colnames(data)[var_ids]


  list(class_ids = class_ids, class_names = class_names,
    var_ids = var_ids, var_names = var_names,
    formula = formula)
}
