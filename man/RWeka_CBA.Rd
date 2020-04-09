\name{RWeka_CBA}
\alias{RWeka_CBA}
\alias{RIPPER}
\alias{PART}
\alias{PART_CBA}
\title{CBA classifiers based on rule-based classifiers in RWeka}
\description{
  Provides CBA-type classifiers based on RIPPER (Cohen, 1995)
  and PART (Frank and Witten, 1998) using the implementaion
  in Weka via RWeka (Hornik et al, 2009).
}
\usage{
RIPPER(formula, data, control = NULL, disc.method = "mdlp")
PART_CBA(formula, data, control = NULL, disc.method = "mdlp")
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted. Has to be of form \code{class ~ .}
  or \code{class ~ predictor1 + predictor2}.}
  \item{data}{A data.frame or a transaction set containing the training data. Data frames are automatically discretized and converted to transactions.}
  \item{disc.method}{Discretization method used to discretize continuous variables if data is a data.frame (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more supervised discretization methods.}
  \item{control}{ algorithmic control options for R/Weka Rule learners (see Details Section). }
}
\details{
You need to install package \pkg{RWeka} to use these classifiers.

See R/Weka function \code{\link{JRip}} (RIPPER) and
\code{\link{PART}} for algorithm details and how control options can be passed on via \code{control}.
An example is given in the Examples Section below.
}
\value{
  Returns an object of class \code{\link{CBA.object}} representing the trained classifier.
}
\references{
W. W. Cohen (1995). Fast effective rule induction. In A. Prieditis and S. Russell (eds.), Proceedings of the 12th International Conference on Machine Learning, pages 115-123. Morgan Kaufmann. ISBN 1-55860-377-8.

E. Frank and I. H. Witten (1998). Generating accurate rule sets without global optimization. In J. Shavlik (ed.), Machine Learning: Proceedings of the Fifteenth International Conference. Morgan Kaufmann Publishers: San Francisco, CA.

Hornik K, Buchta C, Zeileis A (2009). "Open-Source Machine Learning: R Meets Weka."
\emph{Computational Statistics}, 24(2), 225-232. \doi{10.1007/s00180-008-0119-7}
}

\author{Michael Hahsler}
\seealso{
\code{\link{JRip}} (RIPPER),
\code{\link{PART}},
\code{\link{CBA.object}}.
}
\examples{
# You need to install rJava and RWeka

\dontrun{
data("iris")

# learn a classifier using automatic default discretization
classifier <- RIPPER(Species ~ ., data = iris)
classifier

# inspect the rule base
inspect(rules(classifier))

# make predictions for the first few instances of iris
predict(classifier, head(iris))

### To use algorithmic options (here for PART), you need to load RWeka
library(RWeka)

# control options can be found using the Weka Option Wizard (WOW)
WOW(PART)

# build PART with control option U (Generate unpruned decision list) set to TRUE
classifier <- PART_CBA(Species ~ ., data = iris, control = RWeka::Weka_control(U = TRUE))
classifier
inspect(rules(classifier))
predict(classifier, head(iris))
}
}
