\name{bCBA}
\alias{bCBA}
\alias{bcba}
\title{Classification Based on Association Rules}
\description{
  Build a classifier using a transaction boosting classification by association algorithm.
  The algorithm is currently in development, and is not yet formally documented.
}
\usage{
bCBA(formula, data, support = 0.2, confidence = 0.8,
     gamma = 0.05, cost = 10.0, verbose = FALSE, parameter = NULL,
     control = NULL, sort.parameter = NULL, lhs.support = FALSE,
     class.weights = NULL, disc.method = "mdlp")
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}. The class is the variable name (part of the item label before \code{=}).}
  \item{data}{A data.frame containing the training data.}
  \item{support, confidence}{Minimum support and confidence for creating association rules.}
  \item{gamma, cost}{Hyperparameters for the bCBA algorithm.}
  \item{verbose}{Optional logical flag to allow verbose execution, where additional intermediary execution information is printed at runtime.}
  \item{parameter, control}{Optional parameter and control lists for apriori.}
  \item{sort.parameter}{Ordered vector of arules interest measures (as characters) which are used to sort rules in preprocessing.}
  \item{lhs.support}{Logical variable, which, when set to default value of True, indicates that LHS support should be used for rule mining.}
  \item{class.weights}{Weights that should be assigned to the rows of each class (ordered by appearance in levels(classColumn))}
	\item{disc.method}{Discretization method for factorizing numeric input (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more supervised discretization methods.}
}

\details{
  Formats the input data frame and calls a C implementation of a transaction-boosted classification algorithm
  which is currently being developed. This R package provides an interface to the current most stable release

  Before the 'bCBA' algorithm in C is executed, association rules are generated with the Apriori algorithm from the arules package.

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
\code{\link{predict.CBA}},
\code{\link{CBA}},
\code{\link[arules]{apriori}},
\code{\link[arules]{rules}},
\code{\link[arules]{transactions}}.
}
\examples{
data("iris")

classifier <- bCBA(Species ~ ., data = iris, supp = 0.05, conf = 0.9,
		   lhs.support = TRUE)

predict(classifier, head(iris))
}
