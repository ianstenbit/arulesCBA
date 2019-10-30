library("testthat")
library("arulesCBA")
data("iris")

context("CBA_ruleset")


# Shuffle and split into training and test set (80/20 split)
iris <- iris[sample(seq(nrow(iris))),]

iris_train <- iris[1:(nrow(iris)*.8), ]
iris_test <- iris[-(1:(nrow(iris)*.8)),]

# Discretization, conversion to transactions and mining CARs
iris_train_disc <- discretizeDF.supervised(Species ~ .,
  data = iris_train, method = "mdlp")
trans_train <- as(iris_train_disc, "transactions")

iris_test_disc <- discretizeDF(iris_test, iris_train_disc)
trans_test <- as(iris_test_disc, "transactions")

# build custom classifier
rules <- mineCARs(Species ~ ., trans_train,
  parameter = list(support = 0.01, confidence = 0.8))

classifier <- arulesCBA::CBA_ruleset(Species ~ ., rules, method = "majority")
classifier

predict(classifier, head(trans_test))


