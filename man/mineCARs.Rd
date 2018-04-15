\name{mineCARs}
\alias{mineCARs}
\title{Mine Class Association Rules}
\description{
  Class Association Rules (CARs) are association rules that have only items with class values
  in the RHS (Liu, et al., 1998).
}
\usage{
mineCARs(formula, data, parameter = NULL, control = NULL, ...)
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted.}
  \item{data}{An object of class \code{\link[arules]{transactions}} containing the training data.}
  \item{parameter, control}{Optional parameter and control lists for the \code{\link[arules]{apriori}} algorithm.}
  \item{...}{Additional parameters are currently ignored.}
}
%\details{
%}
\value{
  Returns an object of class \code{\link[arules]{rules}}.
}
\references{
Liu, B. Hsu, W. and Ma, Y (1998). Integrating Classification and Assocoiation Rule Mining. \emph{KDD'98 Proceedings of the Fourth International Conference on Knowledge Discovery and Data Mining,} New York, 27-31 August. AAAI. pp. 80-86.
}

\author{Michael Hahsler}
\seealso{
\code{\link[arules]{apriori}},
\code{\link[arules]{rules}},
\code{\link[arules]{transactions}}.
}
\examples{
data("iris")

iris.disc <- discretizeDF.supervised(Species ~ ., iris)
iris.trans <- as(iris.disc, "transactions")

# mine CARs with items for "Species" in the RHS
cars <- mineCARs(Species ~ ., iris.trans, parameter = list(support = 0.3))
inspect(cars)

# restrict the predictors to items starting with "Sepal"
cars <- mineCARs(Species ~ Sepal, iris.trans, parameter = list(support = 0.1))
inspect(cars)
}
