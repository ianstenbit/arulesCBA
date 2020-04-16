\name{LUCS_KDD_CBA}
\alias{FOIL2}
\alias{PRM}
\alias{CPAR}
\title{Interface to the LUCS-KDD Implementations of FOIL, PRM and CPAR
}
\description{
Interface for the LUCS-KDD Software Library Jave implementations of FOIL, PRM, and CPAR.
\bold{Note:} The Java implementations is not part of \pkg{arulesCBA} and not covered by the packages license.
It will be downloaded and compiled separately. It is available free of charge for \bold{non-commerial use.}
}
\usage{
CPAR(formula, data, parameter = "", best_k = 5, disc.method = "mdlp")
PRM(formula, data, parameter = "", best_k = 5, disc.method = "mdlp")
FOIL2(formula, data, parameter = "", best_k = 5, disc.method = "mdlp")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a symbolic description of the model to be fitted. Has to be of form \code{class ~ .}
  or \code{class ~ predictor1 + predictor2}.}
  \item{data}{A data.frame or a transaction set containing the training data. Data frames are automatically discretized and converted to transactions.}
  \item{parameter}{A character string with parameters for the algorithms. }
  \item{best_k}{ use average expected accuracy (laplace) of the best k rules per class for prediction. }
  \item{disc.method}{Discretization method used to discretize continuous variables if data is a data.frame (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more supervised discretization methods.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns an object of class \code{\link{CBA.object}} representing the trained classifier.
}
\references{
Yin, Xiaoxin and Jiawei Han. CPAR: Classification based on Predictive Association Rules, SDM, 2003. \doi{10.1137/1.9781611972733.40}

Frans Coenen et al. The LUCS-KDD Software Library, \url{https://cgi.csc.liv.ac.uk/~frans/KDD/Software/}
}
\examples{
\dontrun{
data("iris")

cl <- CPAR(Species ~ ., iris)
cl

cl <- PRM(Species ~ ., iris)
cl

cl <- FOIL2(Species ~ ., iris)
cl
}
}
