library("arules")
library("arulesCBA")

data(iris)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=9)))
irisDisc$Species <- iris$Species
classifier <- CBA(irisDisc, "Species", supp = 0.05, conf=0.9)
results <- predict.CBA(classifier, irisDisc)


