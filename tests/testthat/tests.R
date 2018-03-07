library("testthat")
library("arulesCBA")

context("arulesCBA Interface")

data(iris)
classifier <- CBA(Species ~ ., iris, supp = 0.05, conf=0.9, lhs.support=FALSE)
results <- predict(classifier, iris)

expect_equal(length(classifier$rules), 3L)
expect_equal(classifier$default, "Species=setosa")
expect_equal(results[1], factor("setosa",
  levels = c("setosa", "versicolor", "virginica")))
