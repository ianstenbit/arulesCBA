\name{classFrequency}
\alias{classFrequency}
\title{Class Frequency in Transactions or CAR Sets}
\description{
Check transactions for class imbalance or the proportion of rules for each class label in a set of CARs.
}
\usage{
classFrequency(formula, x, type = "relative")
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted.}
  \item{x}{An object of class \code{\link[arules]{transactions}} or
    \code{\link[arules]{rules}}.}
  \item{type}{\code{"relative"} or \code{"absolute"} to return proportions or absolute counts.}
}
%\details{
%}
\value{
  Returns the item frequency for each class label as a vector.
}
\author{Michael Hahsler}
\seealso{
\code{\link[arules]{itemFrequency}},
\code{\link[arules]{rules}},
\code{\link[arules]{transactions}}.
}
\examples{
data("iris")

iris.disc <- discretizeDF.supervised(Species ~ ., iris)
iris.trans <- as(iris.disc, "transactions")

# Class distribution. The iris dataset is perfectly balanced.
classFrequency(Species ~ ., iris.trans)

# Number of rules for each class
cars <- mineCARs(Species ~ ., iris.trans, parameter = list(support = 0.3))
classFrequency(Species ~ ., cars, type = "absolute")
}
