\name{CMAR}
\alias{CMAR}
\alias{pruneCMAR}
\alias{cmar}
\title{Classification Based on Multiple Association Rules (CMAR)}
\description{
  Build a classifier based on association rules using sorting, pruning and classification based on the CMAR algorithm by Li, et al. (2001).
}
\usage{
CMAR(formula, data, support = 0.1, confidence = 0.8, coverage = 4,
    disc.method = "mdlp", balanceSupport = FALSE,
    parameter = NULL, control = NULL, ...)

pruneCMAR(formula, rules, trans, coverage = 4, verbose = FALSE)
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}
  or \code{class ~ predictor1 + predictor2}.}
  \item{data}{A data.frame or a transaction set containing the training data. Data frames are automatically discretized and converted to transactions.}
  \item{support, confidence}{Minimum support and confidence for creating association rules.}
  \item{disc.method}{Discretization method used to discretize continuous variables if data is a data.frame (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more supervised discretization methods.}
  \item{balanceSupport}{balanceSupport parameter passed to \code{\link{mineCARs}} function.}
  \item{coverage}{By how many rules does a transaction need to matched before it is considered covered by the algorithm.}
  \item{parameter, control}{Optional parameter and control lists for apriori.}
  \item{...}{For convenience, additional parameters are used to create the \code{parameter} control list for apriori (e.g., to specify the support and confidence thresholds).}
  \item{rules, trans}{prune a set of rules using a transaction set.}
  \item{verbose}{Show progress?}
}
\details{
  Implementation the CMAR sorting, pruning and prediction strategy
  introduced by Li, et al. (2001).

  Instead of the FP-Tree, Apriori is used to mine candidate CARs.
  The rules are ranked by by confidence, support and size.
  (1) More specific, lower confidence rules are pruned. (2) Only positively
  correlated rules are selected (i.e., rules with a significant Chi-squared statistic of more than 3.84). (3) Database coverage pruning is applied where only rules that cover at least one transaction in the training set are retained.
  Rules are processed in rank order and a transaction is considered covered if it was covered by at least \code{coverage} many rules.

  Weighted classification is used. The used weight is the chi-squared value
  corrected by the max-chi-squared value (see Li et al, 2001).
}
\value{
  Returns an object of class \code{\link{CBA.object}} representing the trained classifier.
}
\references{
Wenmin Li, Jiawei Han and Jian Pei, "CMAR: accurate and efficient classification based on multiple class-association rules," Proceedings 2001 IEEE International Conference on Data Mining, San Jose, CA, USA, 2001, pp. 369-376.
}

\author{Michael Hahsler}
\seealso{
\code{\link{CBA.object}}.
}
\examples{
data("iris")

# learn a classifier using automatic default discretization
classifier <- CMAR(Species ~ ., data = iris, supp = 0.05, conf = 0.9)
classifier

# make predictions for the first few instances of iris
predict(classifier, head(iris))

# inspect the rule base
inspect(rules(classifier))

# learn classifier from transactions
trans <- as(discretizeDF.supervised(Species ~ ., iris), "transactions")
classifier <- CMAR(Species ~ Sepal, data = trans, supp = 0.05, conf = 0.9)
classifier
predict(classifier, head(trans))
}
