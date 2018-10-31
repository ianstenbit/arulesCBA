classFrequency <- function(formula, x, type = "relative") {
  x <- items(x)
  vars <- .parseformula(formula, x)
  itemFrequency(x[,vars$class_ids], type)
}

