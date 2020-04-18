\name{wCBA}
\alias{wCBA}
\alias{wcba}
\title{Classification Based on Association Rules}
\description{
  Build a classifier using a naive rule-weighting algorithm.
  The algorithm is currently in development, and is not yet formally documented.
}
\usage{
wCBA(formula, data, parameter = NULL, control = NULL,
    sort.parameter = NULL, lhs.support = FALSE, class.weights = NULL,
    disc.method = "mdlp", verbose = FALSE, ...)
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}. The class is the variable name (part of the item label before \code{=}).}
  \item{data}{A data.frame containing the training data.}
  \item{parameter, control}{Optional parameter and control lists for apriori.}
  \item{sort.parameter}{Ordered vector of arules interest measures (as characters) which are used to sort rules in preprocessing.}
  \item{lhs.support}{Logical variable, which, when set to default value of True, indicates that LHS support should be used for rule mining.}
  \item{class.weights}{Weights that should be assigned to the rows of each class (ordered by appearance in levels(classColumn))}
	\item{disc.method}{Discretization method for factorizing numeric input (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more supervised discretization methods.}
  \item{verbose}{Optional logical flag to allow verbose execution, where additional intermediary execution information is printed at runtime.}
  \item{...}{Additional parameters are added to the apriori parameters (e.g., support and confidence).}
}

\details{
  Mines association rules on input data and creates a weighted-vote classifier where a rules weight is the product of its support and confidence.
	Default class is set to the most common class in the training data.
}
\value{
  Returns an object of class \code{CBA} representing the trained classifier
  with fields:
  \item{rules}{the classifier rule base.}
  \item{default}{default class label.}
  \item{levels}{levels of the class variable.}
}
\author{Ian Johnson}
\seealso{
\code{\link{predict.CBA}},
\code{\link{CBA.object}},
\code{\link[arules]{apriori}},
}
\examples{
data("iris")

classifier <- wCBA(Species ~ ., data = iris, supp = 0.05, conf = 0.9)
classifier

predict(classifier, head(iris))
}
