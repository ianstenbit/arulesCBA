classFrequency <- function(formula, x, type = "relative") {
  x <- items(x)
  if(is(x, "itemMatrix")) {
    vars <- .parseformula(formula, x)
    x <- x[,vars$class_ids]
    itemFrequency(x, type)
  } else { stop("Only implemented for transactions!") }
}

majorityClass <- function(formula, transactions) {
  majorityItem <- names(which.max(classFrequency(Species ~ ., transactions)))
  strsplit(majorityItem, "=")[[1]][2]
}

response <- function(formula, x) {
  x <- items(x)
  if(is(x, "itemMatrix")) {
    vars <- .parseformula(formula, x)
    x <- x[,vars$class_ids]
    l <- sapply(strsplit(itemLabels(x), "="), '[', 2)
    factor(unlist(LIST(x, decode = FALSE)), levels = 1:length(l), labels = l)
  } else { stop("Only implemented for transactions!") }
}
