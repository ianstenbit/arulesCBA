library("testthat")
library("arulesCBA")

context("arulesCBA Interface")

data(iris)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x)
  discretize(x, categories=9)))
irisDisc$Species <- iris$Species
classifier <- CBA(Species ~ ., irisDisc, supp = 0.05, conf=0.9, lhs.support=FALSE)
results <- predict(classifier, irisDisc)

expect_equal(length(classifier$rules), 11L)
expect_equal(classifier$default, "Species=versicolor")
expect_equal(results[1], factor("setosa",
  levels = c("setosa", "versicolor", "virginica")))
