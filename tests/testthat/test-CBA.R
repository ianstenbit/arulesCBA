library("testthat")
library("arulesCBA")
data("iris")

context("CBA")

cba_classifier <- CBA(Species ~ ., iris, supp = 0.05, conf = 0.9, pruning = "M1")
expect_equal(length(rules(cba_classifier)), 8L)

results <- predict(cba_classifier, iris)
expect_equal(results[1], factor("setosa",
  levels = c("setosa", "versicolor", "virginica")))

results <- predict(cba_classifier, head(iris, n = 5))
expect_equal(length(results), 5L)

context("Prediction methods")

cba_classifier$method <- "majority"
results <- predict(cba_classifier, head(iris, n = 5))
expect_equal(length(results), 5L)

cba_classifier$method <- "weighted"
results <- predict(cba_classifier, head(iris, n = 5))
expect_equal(length(results), 5L)

# FIXME: We need to check what the output of M2 should be
cba_classifier_M2 <- CBA(Species ~ ., iris, supp = 0.05, conf = 0.9, pruning = "M2")
# FIXME: there is a bug in totalError calculation in M2
#expect_equal(length(rules(cba_classifier_M2)), 8L)

