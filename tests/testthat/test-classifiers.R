library("testthat")
library("arulesCBA")
data("iris")

context("Test classifiers")

classifiers <- c(CBA, FOIL, RCAR)
if("RWeka" %in% utils::installed.packages()[,"Package"])
  classifiers <- append(classifiers, c(RIPPER_CBA, PART_CBA, C4.5_CBA))

### use raw data
dat <- iris
true <- iris$Species

for(cl in classifiers) {
  res <- cl(Species ~ ., dat)
  print(res)

  p <- predict(res, dat)
  tbl <- table(p, true)
  accuracy <- sum(diag(tbl))/ sum(tbl)
  cat("Accuracy:", round(accuracy, 3), "\n")
}

### use transactions
dat <- as(discretizeDF.supervised(Species ~ ., iris), "transactions")
true <- iris$Species

for(cl in classifiers) {
  res <- cl(Species ~ ., dat)
  print(res)

  p <- predict(res, dat)
  tbl <- table(p, true)
  accuracy <- sum(diag(tbl))/ sum(tbl)
  cat("Accuracy:", round(accuracy, 3), "\n\n")
}


