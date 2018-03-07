library("testthat")
library("arulesCBA")

context("arulesCBA Interface")

data(iris)
irisDisc$Species <- iris$Species
classifier <- CBA(Species ~ ., irisDisc, supp = 0.05, conf=0.9, lhs.support=FALSE)
results <- predict(classifier, irisDisc)

expect_equal(length(classifier$rules), 1L)
expect_equal(classifier$default, "Species=setosa")
expect_equal(results[1], factor("setosa",
  levels = c("setosa", "versicolor", "virginica")))
