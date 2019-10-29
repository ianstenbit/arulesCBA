library("testthat")
library("arulesCBA")


context("Discretization")

data("iris")
disc <- discretizeDF.supervised(Species ~ ., iris)
expect_true(all(sapply(disc, is.factor)))

disc <- discretizeDF.supervised(Species ~ ., iris, method = "chi2")
expect_true(all(sapply(disc, is.factor)))

## test with missing values
iris$Species[1:5] <- NA
disc <- discretizeDF.supervised(Species ~ ., iris)
disc <- discretizeDF.supervised(Species ~ ., iris, method = "chi2")

iris[1:5,1] <- NA
disc <- discretizeDF.supervised(Species ~ ., iris)
disc <- discretizeDF.supervised(Species ~ ., iris, method = "chi2")


