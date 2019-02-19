\name{RCAR}
\alias{RCAR}
\alias{rcar}
\title{Regularized Class Association Rules (RCAR)}
\description{
  Build a classifier based on association rules mined for an input dataset and weighted with LASSO regularized logistic regression (Azmi, et al., 2019).
}
\usage{
RCAR(formula, data, support = 0.3, confidence = 0.7,
    verbose=FALSE, maxlen = 6, lambda = 0.001,
    balanceSupport = FALSE, disc.method = "mdlp")
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}
  or \code{class ~ predictor1 + predictor2}.}
  \item{data}{A data.frame containing the training data.}
  \item{support, confidence}{Minimum support and confidence for creating association rules.}
  \item{verbose}{Optional logical flag to allow verbose execution, where additional intermediary execution information is printed at runtime.}
  \item{maxlen}{The maximum length of mined association rules. Parameter given to class association rule mining algorithm.}
  \item{lambda}{The amount of weight given to regularization during the logistic regression learning process.}
	\item{balanceSupport}{balanceSupport parameter passed to \code{\link{mineCARs}} function.}
	\item{disc.method}{Discretization method for factorizing numeric input (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more supervised discretization methods.}
}
\details{
  Uses logistic regression to assign rule weights to class assocation rules. The logistic regression is regularized with LASSO regularization, encouraging sparsity in rule weights.
}
\value{
  Returns an object of class \code{RCAR} representing the trained classifier.
}
\references{
M. Azmi, G.C. Runger, and A. Berrado (2019). Interpretable regularized class association rules algorithm for classification in a categorical data space. \emph{Information Sciences,} Volume 483, May 2019. Pages 313-331.

Original code at \url{https://github.com/azemi/RCAR}.
}

\author{Tyler Giallanza}
\examples{
data("iris")

classifier <- RCAR(Species~., iris)
classifier

# make predictions for the first few instances of iris
predict(classifier, head(iris))

# inspect the rule base
inspect(rules(classifier))
}
