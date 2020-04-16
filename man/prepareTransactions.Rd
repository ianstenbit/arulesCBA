\name{prepareTransactions}
\alias{prepareTransactions}
\title{Helper to Convert Data into Transactions}
\description{
Converts a data.frame into transactions by applying class-based discretization.
}
\usage{
prepareTransactions(formula, data, disc.method = "mdlp", match = NULL)
}
\arguments{
  \item{formula}{ the formula. }
  \item{data}{ a data.frame with the data.}
  \item{disc.method}{Discretization method used to discretize continuous variables if data is a data.frame
  (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more supervised discretization methods.}
  \item{match}{ typically \code{NULL}. Only used internally if data is a already a set of transactions.}
}
\value{
An object of class \code{\link[arules]{transactions}} from \pkg{arules} with an attribute called \code{"disc_info"} that
contains information on the used discretization for each column.
}
\author{
Michael Hahsler
}
\seealso{
\code{\link[arules]{transactions}}.
}
\examples{
data("iris")

iris_trans <- prepareTransactions(Species ~ ., iris)
iris_trans
}
