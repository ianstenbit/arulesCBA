library("testthat")
library("arulesCBA")
data("iris")

context("CBA")

cba_classifier <- CBA(Species ~ ., iris, supp = 0.05, conf=0.9, lhs.support=FALSE)
expect_equal(length(rules(cba_classifier)), 8L)
expect_equal(cba_classifier$default, "Species=setosa")

results <- predict(cba_classifier, iris)
expect_equal(results[1], factor("setosa",
  levels = c("setosa", "versicolor", "virginica")))

results <- predict(cba_classifier, head(iris, n = 5))
expect_equal(length(results), 5L)


context("RCAR")
rcar_classifier <- RCAR(Species ~ ., iris, supp = 0.05, conf=0.9, lambda = 0.001)
expect_equal(length(rules(rcar_classifier)), 20L)

results <- predict(rcar_classifier, iris)
expect_equal(results[1], factor("setosa",
  levels = c("setosa", "versicolor", "virginica")))

results <- predict(rcar_classifier, head(iris, n = 5))
expect_equal(length(results), 5L)

results <- predict(rcar_classifier, head(iris, n = 5), type = "score")
expect_equal(dim(results), c(5L, 3L))
# FIXME: somehow they do not add up to exactly 1
#expect_equal(rowSums(results), rep(1, 5))

context("Prediction methods")

cba_classifier$method <- "majority"
results <- predict(cba_classifier, head(iris, n = 5))
expect_equal(length(results), 5L)

cba_classifier$method <- "weighted"
results <- predict(cba_classifier, head(iris, n = 5))
expect_equal(length(results), 5L)

context("Custom Classifier")
rules <- mineCARs(Species ~ ., iris,
  parameter = list(support = 0.01, confidence = 0.8))

classifier <- arulesCBA::CBA_ruleset(Species ~ ., rules, method = "majority")
classifier
