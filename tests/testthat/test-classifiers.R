library("testthat")
library("arulesCBA")
data("iris")

context("Test classifiers")

classifiers <- c(CBA, FOIL, RCAR)
if("RWeka" %in% utils::installed.packages()[,"Package"])
  classifiers <- append(classifiers, c(RIPPER_CBA, PART_CBA, C4.5_CBA))

### use raw data
dat <- iris
f <- Species ~ .
true <- response(f, dat)

for(cl in classifiers) {
  res <- cl(f, dat)
  print(res)

  p <- predict(res, dat)
  tbl <- table(p, true)
  accuracy <- sum(diag(tbl))/ sum(tbl)
  cat("Accuracy:", round(accuracy, 3), "\n\n")
}

### use transactions
dat <- prepareTransactions(f, iris)

for(cl in classifiers) {
  res <- cl(f, dat)
  print(res)

  p <- predict(res, dat)
  tbl <- table(p, true)
  accuracy <- sum(diag(tbl))/ sum(tbl)
  cat("Accuracy:", round(accuracy, 3), "\n\n")
}

### use regular transactions
# NOTE: this does not work with Weka-based classifiers.
classifiers <- c(CBA, FOIL, RCAR)

data(Groceries)
dat <- sample(Groceries, 500)
f <- `bottled beer` ~ .
true <- response(f, dat)

for(cl in classifiers) {
  res <- cl(f, dat)
  print(res)

  p <- predict(res, dat)
  tbl <- table(p, true)
  accuracy <- sum(diag(tbl))/ sum(tbl)
  cat("Accuracy:", round(accuracy, 3), "\n\n")
}

## test transactions with logical variables
#classifiers <- c(CBA, FOIL, RCAR)
# RCAR is too slow
classifiers <- c(CBA, FOIL)
if("RWeka" %in% utils::installed.packages()[,"Package"])
  classifiers <- append(classifiers, c(RIPPER_CBA, PART_CBA, C4.5_CBA))

data(Zoo, package = "mlbench")
Zoo$legs <- Zoo$legs > 0

dat <- Zoo
f <- type ~ .
true <- response(f, dat)

for(cl in classifiers) {
  res <- cl(f, dat)
  print(res)

  p <- predict(res, dat)
  tbl <- table(p, true)
  accuracy <- sum(diag(tbl))/ sum(tbl)
  cat("Accuracy:", round(accuracy, 3), "\n\n")
}


