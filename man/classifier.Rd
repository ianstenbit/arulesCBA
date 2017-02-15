\name{CBA}
\alias{CBA}
\alias{cba}
\title{Classification Based on Association Rules}
\description{
  Build a classifier based on association rules mined for an input dataset.
  The CBA algorithm used is a modified version of the algorithm described by
  Liu, et al. 1998.
}
\usage{
CBA(formula, data, method="default", support = 0.2,
    confidence = 0.8, gamma = 0.05, cost = 10.0,
    verbose=FALSE, parameter = NULL, control = NULL)
}



\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}. The class is the variable name (part of the item label before \code{=}).}
  \item{data}{A data.frame or an object of class transactions
    (from \pkg{arules}) containing the training data.}
  \item{support, confidence}{Minimum support and confidence for creating association rules.}
  \item{verbose}{Optional logical flag to allow verbose execution, where additional intermediary execution information is printed at runtime.}
  \item{parameter, control}{Optional parameter and control lists for apriori.}
}
\details{
  Formats the input data frame and calls a C implementation of the CBA algorithm
  from Liu, et al. 1998 split up into three stages to build a classifier based on a set of association rules.

  Before the CBA algorithm in C is executed, association rules are generated with the Apriori algorithm from the arules package.

  A default class is selected for the classifier. Note that for datasets which do not yield any strong association rules
  it's possible that no rules will be included in the classifier, and only a default class.
}
\value{
  Returns an object of class \code{CBA} representing the trained classifier
  with fields:
  \item{rules}{the classifier rule base.}
  \item{default}{deault class label.}
  \item{levels}{levels of the class variable.}
}
\author{Ian Johnson}
\seealso{
\code{\link[arules]{apriori}},
\code{\link[arules]{rules}},
\code{\link[arules]{transactions}}.
}
\examples{
data(iris)
irisDisc <- as.data.frame(lapply(iris[1:4],
  function(x) discretize(x, categories=9)))
irisDisc$Species <- iris$Species

classifier <- CBA(Species ~ ., irisDisc, supp = 0.05, conf=0.9)
classifier

# training a classifier directly with transaction data (arules)
trans <- as(irisDisc, "transactions")
trans
classifier <- CBA(Species ~ ., trans, supp = 0.05, conf=0.9)
classifier
}
