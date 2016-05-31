\name{print.CBA}
\alias{print.CBA}
\title{Printing a CBA classifier}
\description{
  Prints basic information about a CBA classifier object
}
\usage{
\method{print}{CBA}(x, \dots)
}
\arguments{
  \item{x}{An S3 object (a CBA classifier) with a default class and a sorted list of association rules}
  \item{\dots}{Additional arguments not used}
}
\details{
  Prints number of rules in classifier, default class, and detailed information about the first few rules
}
\author{Ian Johnson}
\examples{
data(iris)
require(arules)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=9)))
irisDisc$Species <- iris$Species
classifier <- CBA(irisDisc, "Species", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
print.CBA(classifier)
}