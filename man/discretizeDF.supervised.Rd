\name{discretizeDF.supervised}
\alias{discretizeDF.supervised}
\alias{discretizeDF}
\alias{discretize}

\title{Supervised Methods to Convert Continuous Variables into Categorical Variables}

\description{
This function implements several supervised methods to convert continuous variables into a categorical variables (factor) suitable for association rule mining
and building associative classifiers.
A whole data.frame is discretized (i.e., all numeric columns are discretized).
}

\usage{
discretizeDF.supervised(formula, data, method = "mdlp", dig.lab = 3, ...)
}

\arguments{
  \item{formula}{a formula object to specify the class variable for supervised discretization and the
  predictors to be discretized in the form \code{class ~ .} or \code{class ~ predictor1 + predictor2}.}
  \item{data}{a data.frame containing continuous variables to be discretized}
  \item{method}{ discretization method. Available are:
      \code{"mdlp"}, \code{"caim"}, \code{"cacc"},
      \code{"ameva"}, \code{"chi2"}, \code{"chimerge"},
      \code{"extendedchi2"}, and \code{"modchi2"}.}
  \item{dig.lab}{ integer; number of digits used to create labels.}
  \item{\dots}{ Additional parameters are passed on to the
      implementation of the chosen discretization method.}
}
\details{
\code{discretizeDF.supervised} only implements supervised discretization. See
\code{discretizeDF} in package \pkg{arules} for unsupervised discretization.
}
\value{
\code{discretizeDF} returns a discretized data.frame. Discretized columns have
an attribute \code{"discretized:breaks"} indicating the used breaks
  or and \code{"discretized:method"} giving the used method.
}
%\references{
%}
\seealso{
  Unsupervised discretization from \pkg{arules}:
  \code{\link[arules]{discretize}},
  \code{\link[arules]{discretizeDF}}.

  Details about the available supervised discretization methods from
  \pkg{discretization}:
  \code{\link[discretization]{mdlp}},
  \code{\link[discretization]{caim}},
  \code{\link[discretization]{cacc}},
  \code{\link[discretization]{ameva}},
  \code{\link[discretization]{chi2}},
  \code{\link[discretization]{chiM}},
  \code{\link[discretization]{extendChi2}},
  \code{\link[discretization]{modChi2}}.
}
\examples{
data("iris")
summary(iris)

# supervised discretization using Species
iris.disc <- discretizeDF.supervised(Species ~ ., iris)
summary(iris.disc)

attributes(iris.disc$Sepal.Length)

# discretize the first few instances of iris using the same breaks as iris.disc
discretizeDF(head(iris), methods = iris.disc)

# only discretize predictors Sepal.Length and Petal.Length
iris.disc2 <- discretizeDF.supervised(Species ~ Sepal.Length + Petal.Length, iris)
head(iris.disc2)
}
\author{Michael Hahsler}
\keyword{manip}
