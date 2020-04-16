\name{FOIL}
\alias{FOIL}
\alias{foil}
\title{Use FOIL to learn a rule set for classification}
\description{
  Build a classifier rule base using FOIL (First Order Inductive Learner),
  a greedy algorithm that learns rules to distinguish positive
  from negative examples.
}
\usage{
FOIL(formula, data, max_len = 3, min_gain = .7, best_k = 5,
    disc.method = "mdlp")
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}
  or \code{class ~ predictor1 + predictor2}.}
  \item{data}{A data.frame or a transaction set containing the training data. Data frames are automatically discretized and converted to transactions.}
  \item{max_len}{maximal length of the LHS of the created rules. }
  \item{min_gain}{minimal gain required to expand a rule. }
  \item{best_k}{ use the average expected accuracy (laplace) of the best k rules per class for prediction. }
  \item{disc.method}{Discretization method used to discretize continuous variables if data is a data.frame (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more supervised discretization methods.}
}
\details{
Implements FOIL (Quinlan and Cameron-Jones, 1995) to learn rules and then use them as a classifier following Xiaoxin and Han (2003).

For each class, we find the positive and negative examples and lean the rules using FOIL. Then the rules for all classes are combined and sorted by Laplace accuracy on the training data.

Following Xiaoxin and Han (2003), we classify new examples by
\enumerate{
\item select all the rules whose bodies are satisfied by the example;
\item from the rules select the best k rules per class (highest expected Laplace accuracy);
\item average the expected Laplace accuracy per class and choose the class with the highest average.
}
}
\value{
  Returns an object of class \code{\link{CBA.object}} representing the trained classifier.
}
\references{
Quinlan, J.R., Cameron-Jones, R.M. Induction of logic programs: FOIL and related systems. NGCO 13, 287-312 (1995). \doi{https://doi.org/10.1007/BF03037228}

Yin, Xiaoxin and Jiawei Han. CPAR: Classification based on Predictive Association Rules, SDM, 2003. \doi{10.1137/1.9781611972733.40}
}

\author{Michael Hahsler}
\seealso{
\code{\link{CBA.object}}.
}
\examples{
data("iris")

# learn a classifier using automatic default discretization
classifier <- FOIL(Species ~ ., data = iris)
classifier

# inspect the rule base
inspect(rules(classifier))

# make predictions for the first few instances of iris
predict(classifier, head(iris))
}
