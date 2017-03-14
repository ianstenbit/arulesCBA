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
CBA(formula, data, support = 0.2, confidence = 0.8,
    verbose=FALSE, parameter = NULL, control = NULL,
    sort.parameter = NULL, lhs.support = TRUE,
		disc.method = "cluster", disc.categories = 10)
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}. The class is the variable name (part of the item label before \code{=}).}
  \item{data}{A data.frame containing the training data.}
  \item{support, confidence}{Minimum support and confidence for creating association rules.}
  \item{verbose}{Optional logical flag to allow verbose execution, where additional intermediary execution information is printed at runtime.}
  \item{parameter, control}{Optional parameter and control lists for apriori.}
  \item{sort.parameter}{Ordered vector of arules interest measures (as characters) which are used to sort rules in preprocessing.}
  \item{lhs.support}{Logical variable, which, when set to default value of True, indicates that LHS support should be used for rule mining.}
	\item{disc.method}{Discretization method for arules discretize function for factorizing numeric input (default: "cluster")}
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
  Returns an object of class \code{CBA} representing the trained classifier
  with fields:
  \item{rules}{the classifier rule base.}
  \item{default}{default class label.}
  \item{levels}{levels of the class variable.}
}
\references{
Liu, B. Hsu, W. and Ma, Y (1998). Integrating Classification and Assocoiation Rule Mining. \emph{KDD'98 Proceedings of the Fourth International Conference on Knowledge Discovery and Data Mining,} New York, 27-31 August. AAAI. pp. 80-86.
}

\author{Ian Johnson}
\seealso{
\code{\link{predict.CBA}},
\code{\link[arules]{apriori}},
\code{\link[arules]{rules}},
\code{\link[arules]{transactions}}.
}
\examples{
  library(arulesCBA)
  library(discretization)

  data(iris)
  irisDisc <- mdlp(iris)$Disc.data
  irisDisc <- as.data.frame(lapply(irisDisc, as.factor))

  classifier <- CBA(Species ~ ., data = irisDisc, supp = 0.05, conf=0.9, lhs.support=TRUE)

  result <- predict(classifier, irisDisc)
}
