\name{CBA}
\alias{CBA}
\alias{pruneCBA_M1}
\alias{pruneCBA_M2}
\alias{cba}
\title{Classification Based on Association Rules Algorithm (CBA)}
\description{
  Build a classifier based on association rules using the ranking, pruning and
  classification strategy of the CBA algorithm by
  Liu, et al. (1998).
}
\usage{
CBA(formula, data, pruning = "M1",
    parameter = NULL, control = NULL, balanceSupport = FALSE,
    disc.method = "mdlp", verbose = FALSE, ...)

pruneCBA_M1(formula, rules, transactions, verbose = FALSE)
pruneCBA_M2(formula, rules, transactions, verbose = FALSE)
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}
  or \code{class ~ predictor1 + predictor2}.}
  \item{data}{A data.frame or a transaction set containing the training data. Data frames are automatically discretized and converted to transactions.}
  \item{pruning}{Pruning strategy used: "M1" or "M2".}
  \item{parameter, control}{Optional parameter and control lists for apriori.}
  \item{balanceSupport}{balanceSupport parameter passed to \code{\link{mineCARs}} function.}
  \item{disc.method}{Discretization method used to discretize continuous variables if data is a data.frame (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more supervised discretization methods.}
  \item{...}{For convenience, additional parameters are used to create the \code{parameter} control list for apriori (e.g., to specify the support and confidence thresholds).}
  \item{rules, transactions}{prune a set of rules using a transaction set.}
  \item{verbose}{Show progress?}
}
\details{
  Implementation the CBA algorithm with the M1 or M2 pruning strategy
  introduced by Liu, et al. (1998).

  Candidate classification association rules (CARs) are mined with the standard APRIORI algorithm.
  Rules are ranked by confidence, support and size. Then either the M1 or M2 algorithm are used
  to perform database coverage pruning and to determin the number of rules to use and the default class.
}
\value{
  Returns an object of class \code{\link{CBA.object}} representing the trained classifier.
}
\references{
Liu, B. Hsu, W. and Ma, Y (1998). Integrating Classification and Association Rule Mining. \emph{KDD'98 Proceedings of the Fourth International Conference on Knowledge Discovery and Data Mining,} New York, 27-31 August. AAAI. pp. 80-86.
\url{https://dl.acm.org/doi/10.5555/3000292.3000305}
}
\author{Ian Johnson and Michael Hahsler}
\seealso{
\code{\link{CBA.object}},
\code{\link{mineCARs}}.
}
\examples{
data("iris")

# 1. Learn a classifier using automatic default discretization
classifier <- CBA(Species ~ ., data = iris, supp = 0.05, conf = 0.9)
classifier

# inspect the rule base
inspect(rules(classifier))

# make predictions
predict(classifier, head(iris))
table(pred = predict(classifier, iris), true = iris$Species)


# 2. Learn classifier from transactions (and use verbose)
iris_trans <- prepareTransactions(Species ~ ., iris, disc.method = "mdlp")
iris_trans
classifier <- CBA(Species ~ ., data = iris_trans, supp = 0.05, conf = 0.9, verbose = TRUE)
classifier

# make predictions. Note: response extracts class information from transactions.
predict(classifier, head(iris_trans))
table(pred = predict(classifier, iris_trans), true = response(Species ~ ., iris_trans))
}
