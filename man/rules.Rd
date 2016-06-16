\name{rules}
\alias{rules}
\title{Get the Association Rules from a CBA classifier}
\description{
  Returns the association rules used in the model from a CBA classifier object.
}
\usage{
rules(classifier)
}
\arguments{
  \item{classifier}{An object of class \code{CBA}.}
  \item{\dots}{Additional arguments not used}
}
\details{
  Returns an arules rule set (object of class \code{rules})
  of all of the association rules used in the classifier.
}
\author{Ian Johnson}
\examples{
data(iris)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=9)))
irisDisc$Species <- iris$Species

classifier <- CBA(irisDisc, "Species", supp = 0.05, conf=0.9)
rules <- rules(classifier)
inspect(rules)
}
