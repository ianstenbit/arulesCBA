library("testthat")
library("arulesCBA")
data("iris")

context("Test classifiers")

classifiers <- c(CBA, FOIL, RIPPER_CBA, PART_CBA, RCAR)

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


