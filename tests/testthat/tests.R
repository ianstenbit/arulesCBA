library("testthat")
library("arulesCBA")

context("arulesCBA Interface")

data(iris)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=9)))
irisDisc$Species <- iris$Species
classifier <- CBA(irisDisc, "Species", supp = 0.05, conf=0.9)
results <- predict(classifier, irisDisc)

expect_equal(length(classifier[[1]]), 12)
expect_equal(classifier[[2]], "Species=versicolor")
expect_equal(results[1], factor("setosa",
  levels = c("setosa", "versicolor", "virginica")))





