\name{RCAR}
\alias{RCAR}
\alias{rcar}
\title{Regularized Class Association Rules (RCAR)}
\description{
  Build a classifier based on association rules mined for an input dataset and weighted with LASSO regularized logistic regression (Azmi, et al., 2019).
}
\usage{
RCAR(formula, data, support = 0.1, confidence = 0.8, lambda = NULL, alpha = 1,
    balanceSupport = FALSE, disc.method = "mdlp",
    parameter = NULL, control = NULL, ...)
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}
  or \code{class ~ predictor1 + predictor2}.}
  \item{data}{A data.frame containing the training data.}
  \item{support, confidence}{Minimum support and confidence for creating association rules.}
  \item{lambda}{The amount of weight given to regularization during the logistic regression learning process. If not specified (\code{NULL}) then cross-validation with the argest value of lambda such that error is within 1 standard error of the minimum is used (see \code{\link{cv.glmnet}}). }
  \item{alpha}{The elasticnet mixing parameter. \code{alpha = 1} is the lasso penalty (default RCAR), and \code{alpha = 0} the ridge penalty. }
	\item{balanceSupport}{balanceSupport parameter passed to \code{\link{mineCARs}} function.}
	\item{disc.method}{Discretization method for factorizing numeric input (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more supervised discretization methods.}
  \item{parameter, control}{Optional parameter and control lists for apriori.}
  \item{...}{For convenience, additional parameters are used to create the \code{parameter} control list for apriori (e.g., to specify the support and confidence thresholds).}
}
\details{
  Uses logistic regression to assign rule weights to class assocation rules. The logistic regression is regularized with LASSO regularization, encouraging sparsity in rule weights.
}
\value{
  Returns an object of class \code{CBA} representing the trained classifier with the additional fields:

  \item{all_rules}{all rules used to build the classifier, including the rules with a weight of zero.}
  \item{reg_model}{them multinomial logistic regression model as an object of class code{\link{glmnet}}.}

}
\seealso{
\code{\link{glmnet}} and \code{\link{cv.glmnet}}.
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
