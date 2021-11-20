#' Regularized Class Association Rules for Multi-class Problems (RCAR+)
#'
#' Build a classifier based on association rules mined for an input dataset and
#' weighted with LASSO regularized logistic regression following RCAR (Azmi, et
#' al., 2019). RCAR+ extends RCAR from a binary classifier to a multi-class
#' classifier and can use support-balanced CARs.
#'
#' RCAR+ extends RCAR from a binary classifier to a multi-class classifier
#' using regularized multinomial logistic regression via \pkg{glmnet}.
#'
#' If lambda is not specified (\code{NULL}) then cross-validation with the
#' largest value of lambda such that error is within 1 standard error of the
#' minimum is used to determine the best value (see \code{\link{cv.glmnet}}).
#'
#' See \code{\link{cv.glmnet}} for performing cross-validation in parallel.
#'
#' @aliases RCAR rcar
#' @param formula A symbolic description of the model to be fitted. Has to be
#' of form \code{class ~ .} or \code{class ~ predictor1 + predictor2}.
#' @param data A data.frame containing the training data.
#' @param lambda The amount of weight given to regularization during the
#' logistic regression learning process. If not specified (\code{NULL}) then
#' cross-validation is used to determine the best value (see Details section).
#' @param alpha The elastic net mixing parameter. \code{alpha = 1} is the lasso
#' penalty (default RCAR), and \code{alpha = 0} the ridge penalty.
#' @param cv.glmnet.args,glmnet.args A list of arguments passed on to
#' \code{\link[glmnet]{cv.glmnet}} and \code{\link[glmnet]{glmnet}},
#' respectively. See Example section.
#' @param parameter,control Optional parameter and control lists for apriori.
#' @param balanceSupport balanceSupport parameter passed to
#' \code{\link{mineCARs}} function.
#' @param disc.method Discretization method for factorizing numeric input
#' (default: \code{"mdlp"}). See \code{\link{discretizeDF.supervised}} for more
#' supervised discretization methods.
#' @param verbose Report progress?
#' @param ... For convenience, additional parameters are used to create the
#' \code{parameter} control list for apriori (e.g., to specify the support and
#' confidence thresholds).
#' @return Returns an object of class \code{CBA} representing the trained
#' classifier with the additional field \code{model} containing a list with the
#' following elements:
#'
#' \item{all_rules}{all rules used to build the classifier, including the rules
#' with a weight of zero.} \item{reg_model}{them multinomial logistic
#' regression model as an object of class \code{\link{glmnet}}.}
#' \item{cv}{contains the results for the cross-validation used determine
#' lambda.}
#' @author Tyler Giallanza and Michael Hahsler
#' @seealso \code{\link{CBA.object}}, \code{\link{mineCARs}},
#' \code{\link{glmnet}} and \code{\link{cv.glmnet}}.
#' @references M. Azmi, G.C. Runger, and A. Berrado (2019). Interpretable
#' regularized class association rules algorithm for classification in a
#' categorical data space. \emph{Information Sciences,} Volume 483, May 2019.
#' Pages 313-331.
#'
#' @examples
#'
#' data("iris")
#'
#' classifier <- RCAR(Species~., iris)
#' classifier
#'
#' # inspect the rule base sorted by the larges class weight
#' inspect(sort(rules(classifier), by = "weight"))
#'
#' # make predictions for the first few instances of iris
#' predict(classifier, head(iris))
#'
#' # inspecting the regression model, plot the regularization path, and
#' # plot the cross-validation results to determine lambda
#' str(classifier$model$reg_model)
#' plot(classifier$model$reg_model)
#' plot(classifier$model$cv)
#'
#' # show progress report and use 5 instead of the default 10 cross-validation folds.
#' classifier <- RCAR(Species~., iris, cv.glmnet.args = list(nfolds = 5), verbose = TRUE)
#'
RCAR <- function(formula, data,
  lambda = NULL, alpha = 1, glmnet.args = NULL, cv.glmnet.args = NULL,
  parameter = NULL, control = NULL, balanceSupport = FALSE,
  disc.method = 'mdlp', verbose = FALSE, ...) {

  trans <- prepareTransactions(formula, data, disc.method)
  formula <- as.formula(formula)
  form <- .parseformula(formula, trans)


  if(verbose) {
    glmnet.args$trace.it <- TRUE
    cv.glmnet.args$trace.it <- TRUE
  }

  # mine and prune CARs
  if(verbose) cat("* Mining CARs...\n")
  cars <- mineCARs(formula, trans,
    parameter = parameter, control = control, balanceSupport = balanceSupport,
    verbose = verbose, ...)

  # create coverage matrix
  if(verbose) cat("* Creating model matrix\n")
  X <- is.superset(trans, lhs(cars))
  y <- response(formula, trans)

  # find lambda using cross-validation or fit the model for a fixed lambda
  cv <- NULL
  if(is.null(lambda)) {
    if(verbose) cat("* Fitting glmnet and determine lambda using cross-validation.\n")
    cv <- do.call(glmnet::cv.glmnet, c(list(x = X, y = y,
      family='multinomial', alpha=alpha), cv.glmnet.args))
    lambda <- cv$lambda.1se
    if(verbose) cat("* Found lambda:", lambda, "\n")
    model <- cv$glmnet.fit
    weights <- sapply(model$beta, FUN = function(x) as.vector(x[,model$lambda == lambda]))
    bias <- model$a0[model$lambda == lambda]
  }else{
    if(verbose) cat("* Fitting glmnet for fixed lambda.\n")
    model <- do.call(glmnet::glmnet, c(list(x = X, y = y, family='multinomial',
      alpha=alpha, lambda=lambda), glmnet.args))
    weights <- sapply(model$beta, as.vector)
    bias <- model$a0
  }

  # weights: The odds multiply by exp(beta) for every 1-unit increase of x
  remove <- apply(weights, MARGIN = 1, FUN = function(x) all(x==0))
  quality(cars)$weight <- apply(weights, MARGIN = 1, max)
  quality(cars)$oddsratio <- exp(quality(cars)$weight)
  rulebase <- cars[!remove]
  weights <- weights[!remove,]

  if(verbose) cat("* CARs left:", length(rulebase), "\n")


  CBA_ruleset(
    formula = formula,
    rules = rulebase,
    weights = weights,
    bias = bias,
    method='logit',
    model = list(
      all_rules = cars,
      reg_model = model,
      cv = cv
    ),
    discretization = attr(trans, "disc_info"),
    description = "RCAR+ based on RCAR (Azmi et al., 2019)"
  )
}
