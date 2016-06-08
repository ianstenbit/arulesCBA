\name{predict}
\alias{predict}
\title{Classification with CBA classifier}
\description{
  Uses a classifier based on association rules to classify a new set of data entries.
}
\usage{
\method{predict}{CBA}(object, classifier, \dots)
}
\arguments{
  \item{object}{An S3 object (a CBA classifier) with a default class and a sorted list of association rules}
  \item{newdata}{A data.frame or arules transaction set containing rows of new entries to be classified}
  \item{\dots}{Additional arguments not used}
}
\details{
  Runs a linear pass through newdata and uses the CBA classifier to assign it a class
}
\value{
  Returns a factor array of the classes of the rows in newdata
}
\author{Ian Johnson}
\examples{
data(iris)
library(arules)
library(caret)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=9)))
irisDisc$Species <- iris$Species
classifier <- CBA(irisDisc, "Species", supp = 0.05, conf=0.9)
results <- predict(classifier, irisDisc)
\dontrun{confusionMatrix(irisDisc$Species, results)}
}