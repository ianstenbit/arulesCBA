\name{predict}
\alias{predict}
\alias{predict.CBA}
\title{Classification with CBA classifier}
\description{
  Uses a classifier based on association rules to classify a new set of data entries.
}
\usage{
\method{predict}{CBA}(object, newdata, \dots)
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
  Returns a vector of class labels, one for rows in newdata.
}
\author{Ian Johnson}
\examples{
# prepare data
data(iris)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=9)))
irisDisc$Species <- iris$Species
irisDisc <- irisDisc[sample(1:nrow(irisDisc)),]


# train classifier on the first 100 examples
classifier <- CBA(irisDisc[1:100,], "Species", supp = 0.05, conf=0.9)

# predict the class for the remaining 50 examples
results <- predict(classifier, irisDisc[101:150,])
table(results, irisDisc$Species[101:150])

\dontrun{
# use caret to get more statistics
library("caret")
confusionMatrix(results, irisDisc$Species[101:150])
}
}
