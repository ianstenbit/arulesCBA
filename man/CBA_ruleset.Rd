\name{CBA_ruleset}
\alias{CBA_ruleset}
\alias{cba_ruleset}
\title{Rules-based Classifier from Association Rules}
\description{
  Use a set of association rules for classification.
}
\usage{
CBA_ruleset(formula, rules, method = "first", weights = NULL, default = NULL,
  description = "Custom rule set")
}
\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}. The class is the variable name (part of the item label before \code{=}).}
  \item{rules}{A set of association rules (from \pkg{arules}).}
  \item{method}{Classification method \code{"first"} found rule or \code{"majority"}.}
  \item{weights}{Rule weights for method majority. If missing, then equal weights are used}
  \item{default}{Default class of the form \code{variable=level}. If not
    specified then the most frequent RHS in rules is used.},
  \item{description}{Description field used when the classifier is printed. }
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
  \item{class}{class variable.}
  \item{levels}{levels of the class variable.}
  \item{default}{default class label.}
  \item{method}{classification method.}
  \item{weights}{rule weights.}
}
\author{Michael Hahsler}
\seealso{
\code{\link{predict.CBA}},
\code{\link[arules]{apriori}},
\code{\link[arules]{rules}},
\code{\link[arules]{transactions}}.
}
\examples{
library("caret")

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
cl <- CBA_ruleset(Species ~ ., rules)
cl

p <- predict(cl, trans)
confusionMatrix(p, truth)

# use weighted majority
cl <- CBA_ruleset(Species ~ ., rules, method = "majority", weights = "lift")
cl

p <- predict(cl, trans)
confusionMatrix(p, truth)
}
