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

  # find lambda using cross-validation
  cv <- NULL
  if(is.null(lambda)) {
    if(verbose) cat("* Determine lambda using cross-validation: ")
    cv <- do.call(glmnet::cv.glmnet, c(list(x = X, y = y, family='multinomial', alpha=alpha), cv.glmnet.args))
    lambda <- cv$lambda.1se
    if(verbose) cat(lambda, "\n")
  }

  if(verbose) cat("* Fitting glmnet\n")
  model <- do.call(glmnet::glmnet, c(list(x = X, y = y, family='multinomial', alpha=alpha, lambda=lambda), glmnet.args))

  # weights: The odds multiply by exp(beta) for every 1-unit increase of x
  weights <- sapply(model$beta, as.vector)
  remove <- apply(weights, MARGIN = 1, FUN = function(x) all(x==0))
  quality(cars)$weight <- apply(weights, MARGIN = 1, max)
  quality(cars)$oddsratio <- exp(quality(cars)$weight)
  rulebase <- cars[!remove]
  weights <- weights[!remove,]
  biases <- model$a0

  if(verbose) cat("* CARs left:", length(rulebase), "\n")


  structure(list(
    formula = formula,
    discretization = attr(trans, "disc_info"),
    rules = rulebase,
    default = NA,
    weights = weights,
    biases = biases,
    method='logit',
    model = list(
      all_rules = cars,
      reg_model = model,
      cv = cv
    ),
    description = paste("RCAR+ based on RCAR (Azmi et al., 2019)")
  ), class = 'CBA')
}
