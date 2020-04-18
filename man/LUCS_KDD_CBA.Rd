\name{LUCS_KDD_CBA}
\alias{CMAR}
\alias{PRM}
\alias{CPAR}
\alias{FOIL2}
\alias{install_LUCS_KDD_CMAR}
\alias{install_LUCS_KDD_CPAR}
\title{Interface to the LUCS-KDD Implementations of CMAR, PRM and CPAR
}
\description{
Interface for the LUCS-KDD Software Library Java implementations of CMAR, PRM, and CPAR.
\bold{Note:} The Java implementations is not part of \pkg{arulesCBA} and not covered by the packages license.
It will be downloaded and compiled separately. It is available free of charge for \bold{non-commercial use.}
}
\usage{
CMAR(formula, data, parameter = "", disc.method = "mdlp")
PRM(formula, data, parameter = "", best_k = 5, disc.method = "mdlp")
CPAR(formula, data, parameter = "", best_k = 5, disc.method = "mdlp")
FOIL2(formula, data, parameter = "", best_k = 5, disc.method = "mdlp")

install_LUCS_KDD_CMAR(force = FALSE,
  source = "https://cgi.csc.liv.ac.uk/~frans/KDD/Software/CMAR/cmar.tgz")
install_LUCS_KDD_CPAR(force = FALSE,
  source = "https://cgi.csc.liv.ac.uk/~frans/KDD/Software/FOIL_PRM_CPAR/foilPrmCpar.tgz")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a symbolic description of the model to be fitted. Has to be of form \code{class ~ .}
  or \code{class ~ predictor1 + predictor2}.}
  \item{data}{A data.frame or a transaction set containing the training data. Data frames are automatically discretized and converted to transactions.}
  \item{parameter}{A character string with parameters for the algorithms. }
  \item{best_k}{ use average expected accuracy (laplace) of the best k rules per class for prediction. }
  \item{disc.method}{Discretization method used to discretize continuous variables if data is a data.frame (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more supervised discretization methods.}
\item{force}{logical; force redownload, rebuilding and reinstallation?}
\item{source}{source for the code. A local file can be specified as a URI starting with \code{file://} (see \code{\link[utils]{download.file}}). }
}
\details{
The LUCS-KDD code is not part of the package and has to be downloaded, compiled and installed using
\code{install_LUCS_KDD_CMAR()} and \code{install_LUCS_KDD_CPAR()}. You need a complete
Java JDK installation including the \code{javac} compiler. On some systems (Windows), you may need to set
the \code{JAVA_HOME} environment variable so the system finds the compiler.

FOIL2 is an alternative Java implementation to the native implementation of FOIL already provided in the \pkg{arulesCBA}.
}
\value{
Returns an object of class \code{\link{CBA.object}} representing the trained classifier.
}
\references{
Li W., Han, J. and Pei, J. CMAR: Accurate and Efficient Classification Based on Multiple Class-Association Rules, ICDM, 2001, pp. 369-376.

Yin, Xiaoxin and Jiawei Han. CPAR: Classification based on Predictive Association Rules, SDM, 2003. \doi{10.1137/1.9781611972733.40}

Frans Coenen et al. The LUCS-KDD Software Library, \url{https://cgi.csc.liv.ac.uk/~frans/KDD/Software/}
}
\examples{
\dontrun{
data("iris")

# install and compile CMAR
install_LUCS_KDD_CMAR()

# build a classifier, inspect rules and make predictions
cl <- CMAR(Species ~ ., iris)
cl

inspect(rules(cl))

predict(cl, head(iris))

# install CPAR (also installs PRM and FOIL2)
install_LUCS_KDD_CPAR()

cl <- CPAR(Species ~ ., iris)
cl

cl <- PRM(Species ~ ., iris)
cl

cl <- FOIL2(Species ~ ., iris)
cl
}
}
