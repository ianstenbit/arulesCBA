\name{CBA_ruleset}
\alias{CBA_ruleset}
\alias{cba_ruleset}
\title{Rules-based Classifier from Association Rules}
\description{
  Use a set of association rules for classification.
}
\usage{
CBA_ruleset(rules, class)
}
\arguments{
  \item{rules}{A set of association rules (from \pkg{arules}).}
  \item{class}{A character vector of size 1 indicating the name of the variable which represents the class variable.}
}
\details{
  This method just takes a user provided set of association rules
  to produce a classifier. The user needs to make sure that the
  rules are predictive and sorted from most to least predictive.
}
\value{
  Returns an object of class \code{CBA} representing the trained classifier
  with fields:
  \item{rules}{the classifier rule base.}
  \item{default}{deault class label.}
  \item{levels}{levels of the class variable.}
}
\author{Michael Hahsler}
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

# create transactions
trans <- as(irisDisc, "transactions")
truth <- irisDisc$Species

# create rule base
rules <- apriori(trans, parameter=list(support=.01, confidence = .8),
  appearance = list(rhs=grep("Species=", itemLabels(trans), value = TRUE), default = "lhs"))

rules <- rules[!is.redundant(rules)]
rules <- sort(rules, by = "conf")

# create classifier
cl <- CBA_ruleset(rules, class = "Species")
cl

p <- predict(cl, trans)
library("caret")
confusionMatrix(p, truth)
}
