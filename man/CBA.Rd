\name{CBA}
\alias{CBA}
\alias{pruneCBA_M1}
\alias{pruneCBA_M2}
\alias{cba}
\title{Classification Based on Association Rules Algorithm (CBA)}
\description{
  Build a classifier based on association rules mined for an input dataset.
  The CBA algorithm used is a modified version of the algorithm described by
  Liu, et al. (1998).
}
\usage{
CBA(formula, data, support = 0.1, confidence = 0.8, pruning = "M1",
    disc.method = "mdlp", balanceSupport = FALSE,
    parameter = NULL, control = NULL, ...)

pruneCBA_M1(formula, rules, trans, verbose = FALSE)
pruneCBA_M2(formula, rules, trans)
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}
  or \code{class ~ predictor1 + predictor2}.}
  \item{data}{A data.frame or a transaction set containing the training data. Data frames are automatically discretized and converted to transactions.}
  \item{support, confidence}{Minimum support and confidence for creating association rules.}
  \item{disc.method}{Discretization method used to discretize continuous variables if data is a data.frame (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more supervised discretization methods.}
  \item{balanceSupport}{balanceSupport parameter passed to \code{\link{mineCARs}} function.}
  \item{pruning}{Pruning strategy used: "M1" or "M2".}
  \item{parameter, control}{Optional parameter and control lists for apriori.}
  \item{...}{For convenience, additional parameters are used to create the \code{parameter} control list for apriori (e.g., to specify the support and confidence thresholds).}
  \item{rules, trans}{prune a set of rules using a transaction set.}
  \item{verbose}{Show progress?}
}
\details{
  Implementation the CBA algorithm with the M1 or M2 pruning strategy
  introduced by Liu, et al. (1998).

  The data is converted into a transaction set from package arules.
  Then candidate CARs are created using \code{mineCARs()} and
  \code{pruneCBA_M2()} selects the rules included into the classifier's rule base.
}
\value{
  Returns an object of class \code{\link{CBA.object}} representing the trained classifier.
}
\references{
Liu, B. Hsu, W. and Ma, Y (1998). Integrating Classification and Association Rule Mining. \emph{KDD'98 Proceedings of the Fourth International Conference on Knowledge Discovery and Data Mining,} New York, 27-31 August. AAAI. pp. 80-86.
}

\author{Ian Johnson}
\seealso{
\code{\link{CBA.object}},
\code{\link{discretizeDF.supervised}},
\code{\link{mineCARs}},
\code{\link[arules]{apriori}},
\code{\link[arules]{rules}},
\code{\link[arules]{transactions}}.
}
\examples{
data("iris")

# learn a classifier using automatic default discretization
classifier <- CBA(Species ~ ., data = iris, supp = 0.05, conf = 0.9)
classifier

# make predictions for the first few instances of iris
predict(classifier, head(iris))

# inspect the rule base
inspect(rules(classifier))

# learn classifier from transactions
trans <- as(discretizeDF.supervised(Species ~ ., iris), "transactions")
classifier <- CBA(Species ~ Sepal, data = trans, supp = 0.05, conf = 0.9)
classifier
predict(classifier, head(trans))
}
