\name{mineCARs}
\alias{mineCARs}
\title{Mine Class Association Rules}
\description{
  Class Association Rules (CARs) are association rules that have only items with class values
  in the RHS as introduced for the CBA algorithm by Liu et al., 1998.
}
\usage{
mineCARs(formula, data, balanceSupport = FALSE, parameter = NULL, control = NULL, ...)
}

\arguments{
  \item{formula}{A symbolic description of the model to be fitted.}
  \item{data}{An object of class \code{\link[arules]{transactions}} containing the training data.}
  \item{balanceSupport}{logical; if \code{TRUE}, class imbalance is counteracted by
  using class specific minimum support values. Alternatively, a support value for each class can be specified (see Details section).}
  \item{parameter, control}{Optional parameter and control lists for the \code{\link[arules]{apriori}} algorithm.}
  \item{...}{For convenience, additional parameters are used to create the \code{parameter} control list for apriori (e.g., to specify the support and confidence thresholds).}
}
\details{
Class association rules (CARs) are of the form

\deqn{P \Rightarrow c_i,}{P => c_i,}

where the LHS \eqn{P} is a pattern (i.e., an itemset) and \eqn{c_i} is a single items representing the class label.

\bold{Balancing minimum support.} Using a single minimum support threshold for a highly class imbalanced dataset will lead to the problem, that minority classes will only be presented in very few rules. To address this issue,
\code{balanceSupport = TRUE} can be used to adjust minimum support for each class
dependent on the prevalence of
the class (i.e., the frequency of the \eqn{c_i} in the transactions) similar to the minimum class support
suggested for CBA by  Liu et al (2000) we use

\deqn{minsupp_i = minsupp_t \frac{supp(c_i)}{max(supp(C))},}{minsupp_i = minsupp_t x supp(c_i)/max(supp(C)),}

where \eqn{max(supp(C))} is the support of the majority class. Therefore, the defined minimum support is used for the majority class and then minimum support is scaled down for classes which are less prevalent, giving them a chance to also produce a reasonable amount of rules. In addition, a named numerical vector with a support values for each class can be specified.
}
\value{
  Returns an object of class \code{\link[arules]{rules}}.
}
\references{
Liu, B. Hsu, W. and Ma, Y (1998). Integrating Classification and Association Rule Mining. \emph{KDD'98 Proceedings of the Fourth International Conference on Knowledge Discovery and Data Mining,} New York, 27-31 August. AAAI. pp. 80-86.

Liu B., Ma Y., Wong C.K. (2000) Improving an Association Rule Based Classifier. In: Zighed D.A., Komorowski J., Zytkow J. (eds) \emph{Principles of Data Mining and Knowledge Discovery. PKDD 2000. Lecture Notes in Computer Science}, vol 1910. Springer, Berlin, Heidelberg.
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
cars <- mineCARs(Species ~ ., iris.trans, parameter = list(support = 0.3, confidence = 0.9))
inspect(cars)

# for conveiniance this can also be written without parameter and the list
cars <- mineCARs(Species ~ ., iris.trans, support = 0.3, confidence = 0.9)

# restrict the predictors to items starting with "Sepal"
cars <- mineCARs(Species ~ Sepal, iris.trans, support = 0.1, confidence = 0.9)
inspect(cars)

# using different support for different classes using automatically balancing
cars <- mineCARs(Species ~ ., iris.trans, balanceSupport = TRUE, confidence = 0.9)
inspect(cars)

# specify support for each class
cars <- mineCARs(Species ~ ., iris.trans, balanceSupport = c(
  "Species=setosa" = 0.1,
  "Species=versicolor" = 0.5,
  "Species=virginica" = 0.01), confidence = 0.9)
cars
}

