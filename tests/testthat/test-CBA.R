library("testthat")
library("arulesCBA")

context("CBA")

data("iris")
formula <- Species ~ .

trans <- prepareTransactions(formula, iris, disc.method = "mdlp")
rulebase <- mineCARs(
  formula = Species ~ .,
  transactions = trans,
  supp = 0.05,
  conf = 0.9
)
n_rules <- length(rulebase)

cba_no_pruning <- CBA(formula, iris, supp = 0.05, conf = 0.9, pruning = NULL)
expect_equal(length(rules(cba_no_pruning)), n_rules)

cba_classifier <- CBA(formula, iris, supp = 0.05, conf = 0.9, pruning = "M1")
expect_equal(length(rules(cba_classifier)), 8L)

results <- predict(cba_classifier, iris)
expect_equal(
  results[1], factor("setosa", levels = c("setosa", "versicolor", "virginica"))
)

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
cba_classifier_M2 <- CBA(formula, iris, supp = 0.05, conf = 0.9, pruning = "M2")
# FIXME: there is a bug in totalError calculation in M2
#expect_equal(length(rules(cba_classifier_M2)), 8L)

