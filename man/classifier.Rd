\name{CBA}
\alias{CBA}
\title{Classification Based on Association Rules}
\description{
  Build a classifier based on association rules mined for an input dataset.
  The CBA algorithm used is a modified version of the algorithm described by
  Liu, et al. 1998.
}
\usage{
CBA(dataset, column, apriori_parameter, verbose=FALSE)
}
\arguments{
  \item{dataset}{A data.frame containing at least two columns, where one column is the class of the data entry for each row.}
  \item{column}{A character vector of size 1 indicating the name of the column which represents the class of the input data entries.}
  \item{apriori_parameter}{An object of class \code{\linkS4class{APparameter}} or named list. This is a control parameter for apriori, the algorithm from arules used to generate association rules. The default behavior is to mine rules with support 0.1, confidence 0.8, and maxlen 10.}
  \item{verbose}{Optional logical flag to allow verbose execution, where additional intermediary execution information is printed at runtime.}

}
\details{
  Formats the input data frame and calls a C implementation of the CBA algorithm
  from Liu, et al. 1998 split up into three stages to build a classifier based on a set of association rules.

  Before the CBA algorithm in C is executed, association rules are generated with Apriori from the arules package.

  A default class is selected for the classifier. Note that for datasets which don't yield any strong association rules 
  it's possible that no rules will be included in the classifier, and only a default class. 
  
}
\value{
  Returns an S3 object with a default class, and an ordered list of association rules used for the classifier
}
\author{Ian Johnson}
\examples{
data(iris)
library(gmodels)
library(arules)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=9)))
irisDisc$Species <- iris$Species
classifier <- CBA(irisDisc, "Species", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
results <- predict.CBA(classifier, irisDisc)
CrossTable(x=irisDisc$Species, y=results, prop.chisq = FALSE)
}