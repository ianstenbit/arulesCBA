\name{CBA.object}
\alias{CBA.object}
\alias{CBA_ruleset}
\alias{predict.CBA}
\alias{print.CBA}
\alias{rules}
\alias{rules.CBA}
\title{Objects for Classifiers Based on Association Rules}
\description{
  Objects for classifiers based on association rules have class \code{"CBA"}.
  A creator function \code{CBA_ruleset()} and several methods are provided.
}
\usage{
CBA_ruleset(formula, rules, method = "first",
  weights = NULL, default = NULL,
  description = "Custom rule set")

\method{print}{CBA}(x, ...)
\method{rules}{CBA}(x)
\method{predict}{CBA}(object, newdata, \dots)
}
\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}. The class is the variable name (part of the item label before \code{=}).}
  \item{rules}{A set of association rules (from \pkg{arules}).}
  \item{method}{Classification method \code{"first"} found rule or \code{"majority"}.}
  \item{weights}{Rule weights for method majority. Either a quality measure available in \code{rules} or a numeric vector of the same length are \code{rules} can be specified. If missing, then equal weights are used}
  \item{default}{Default class of the form \code{variable=level}. If not
    specified then the most frequent RHS in rules is used.},
  \item{description}{Description field used when the classifier is printed. }
  \item{x, object}{An object of class \code{CBA}.}
  \item{newdata}{A data.frame or transactions containing
  rows of new entries to be classified.}
  \item{\dots}{Additional arguments currently not used.}
}
\details{
  \code{CBA_ruleset} creates a new object of class \code{CBA}
  using the provides rules as the rule base.
  For method \code{"first"}, the user needs to make sure that the
  rules are predictive and sorted from most to least predictive.
}
\value{
  \code{CBA_ruleset()} returns an object of class
  \code{CBA} representing the trained classifier
  with fields:
  \item{rules}{the classifier rule base.}
  \item{class}{class variable.}
  \item{levels}{levels of the class variable.}
  \item{default}{default class label.}
  \item{method}{classification method.}
  \item{weights}{rule weights.}

\code{predict} retuns predicted labels for \code{newdata}.

\code{rules} returns the rule base.
}
\author{Michael Hahsler}
\seealso{
\code{\link{CBA}},
\code{\link[arules]{apriori}},
\code{\link[arules]{rules}},
\code{\link[arules]{transactions}}.
}
\examples{
library("caret")
data("iris")

iris.disc <- discretizeDF.supervised(Species ~., iris)

# create transactions
trans <- as(iris.disc, "transactions")
truth <- iris.disc$Species

# create rule base
rules <- apriori(trans, parameter = list(support = .01, confidence = .8),
  appearance = list(rhs=grep("Species=", itemLabels(trans), value = TRUE),
                    default = "lhs"))

rules <- rules[!is.redundant(rules)]
rules <- sort(rules, by = "conf")

# create classifier
cl <- CBA_ruleset(Species ~ ., rules)
cl

# look at the rule base
rules(cl)

# make predictions
p <- predict(cl, trans)
confusionMatrix(p, truth)

# use weighted majority
cl <- CBA_ruleset(Species ~ ., rules, method = "majority", weights = "lift")
cl

p <- predict(cl, trans)
confusionMatrix(p, truth)
}
