\name{classFrequency}
\alias{classFrequency}
\alias{majorityClass}
\alias{response}
\title{Extracting the Response and Class Frequency for Transactions or CAR Sets}
\description{
Converts the class items in transactions/CARs back to a class label. Class frequency can be used to
check transactions for class imbalance or the proportion of rules for each class label in a set of CARs.
}
\usage{
response(formula, x)
classFrequency(formula, x, type = "relative")
majorityClass(formula, transactions)
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted.}
  \item{x, transactions}{An object of class \code{\link[arules]{transactions}} or
    \code{\link[arules]{rules}}.}
  \item{type}{\code{"relative"} or \code{"absolute"} to return proportions or absolute counts.}
}
%\details{
%}
\value{
\code{response} returns the response label as a factor.

\code{classFrequency} returns the item frequency for each class label as a vector.

\code{majorityClass} returns the most frequent class label in the transactions.
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
inspect(head(iris.trans, n = 2))

# convert the class items back to a class label
response(Species ~ ., head(iris.trans, n = 2))

# Class distribution. The iris dataset is perfectly balanced.
classFrequency(Species ~ ., iris.trans)

# Majority Class
# (Note: since all class frequencies for iris are the same, the first one is returned)
majorityClass(Species ~ ., iris.trans)

# Use for CARs
cars <- mineCARs(Species ~ ., iris.trans, parameter = list(support = 0.3))

# Number of rules for each class
classFrequency(Species ~ ., cars, type = "absolute")

# conclusion (item in the RHS) of the rule as a class label
response(Species ~ ., head(iris.trans, n = 2))
}
