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

iris$disc1 <- cut(iris$Sepal.Length, breaks = 2, include.lowest = TRUE, right = TRUE)
iris$disc2 <- cut(iris$Petal.Length, breaks = 2, include.lowest = TRUE, right = TRUE)

# no disc.method
expect_error(discretizeDF.supervised(Species ~ disc1 + disc2, iris, method = "chi2"))
iris_cat <- iris[c("Species", "disc1", "disc2")]
expect_no_error(prepareTransactions(Species ~ disc1 + disc2, data = iris_cat, disc.method = NULL))
