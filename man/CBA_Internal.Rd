\name{CBA.internal}
\alias{CBA.internal}
\alias{cba.internal}
\title{Classification Based on Association Rules}
\description{
  Build a classifier based on association rules mined for an input dataset.
  The CBA algorithm used is a modified version of the algorithm described by
  Liu, et al. 1998.
}
\usage{
  CBA.internal(rules, data, class, disc.data, method="CBA", support = 0.2, confidence = 0.8, gamma = 0.05, cost = 10.0,
    verbose=FALSE, parameter = NULL, control = NULL, sort.parameter=NULL, lhs.support=TRUE, class.weights=NULL,
    disc.categories = 10, disc.method="cluster", sort.rules=FALSE){
}

\arguments{
	\item{disc.categories}{Number of discretization categories for arules discretize function for factorizing numeric input (default: 10)}
}
\details{
  Formats the input data frame and calls a C implementation of the CBA algorithm
  from Liu, et al. 1998 split up into three stages to build a classifier based on a set of association rules.

  Before the CBA algorithm in C is executed, association rules are generated with the Apriori algorithm from the arules package.

  A default class is selected for the classifier. Note that for datasets which do not yield any strong association rules
  it's possible that no rules will be included in the classifier, and only a default class.
}
\value{
  Returns an object of class \code{\link{CBA.object}} representing the trained classifier.
}
\references{
Liu, B. Hsu, W. and Ma, Y (1998). Integrating Classification and Assocoiation Rule Mining. \emph{KDD'98 Proceedings of the Fourth International Conference on Knowledge Discovery and Data Mining,} New York, 27-31 August. AAAI. pp. 80-86.
}

\author{Ian Johnson}
\seealso{
\code{\link{CBA.object}},
\code{\link[arules]{apriori}},
\code{\link[arules]{rules}},
\code{\link[arules]{transactions}}.
}
\examples{
  library(arulesCBA)
}
