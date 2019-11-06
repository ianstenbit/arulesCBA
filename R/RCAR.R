RCAR <- function(formula, data, support = 0.1, confidence = 0.8, lambda = NULL, alpha = 1,
  balanceSupport = FALSE, disc.method = 'mdlp', parameter = NULL, control = NULL, ...) {

  disc_info <- NULL
  if(is(data, "data.frame")){
    data <- discretizeDF.supervised(formula, data, method=disc.method)
    disc_info <- lapply(data, attr, "discretized:breaks")
  }

  if(!is(data, "transactions")) data <- as(data, 'transactions')
  form <- .parseformula(formula, data)

  if(is.null(control)) control <- as(list(verbose = FALSE), "APcontrol")
  else  control <- as(control, "APcontrol")

  if(control@verbose) cat("Mining CARs\n")
  model_rules <- mineCARs(formula, data, balanceSupport,
    parameter = parameter, control = control,
    support = support, confidence = confidence, ...)

  if(control@verbose) cat("Creating model matrix\n")
  X <- is.superset(data,lhs(model_rules))
  y <- factor(as(data[, form$class_ids], "matrix") %*% seq(length(form$class_ids)),
    labels = form$class_names)

  # find lambda using cross-validation
  if(is.null(lambda)) {
    if(control@verbose) cat("Find lambda using cross-validation: ")
    lambda <- cv.glmnet(X, y, family='multinomial', alpha=alpha)$lambda.1se
    if(control@verbose) cat(lambda, "\n")
  }

  if(control@verbose) cat("Fitting glmnet\n")
  model <- glmnet(X, y, family='multinomial', alpha=alpha, lambda=lambda)

  weights <- sapply(model$beta, as.vector)
  remove <- apply(weights, MARGIN = 1, FUN = function(x) all(x==0))

  structure(list(
    rules=model_rules[!remove],
    weights=weights[!remove,],
    biases=model$a0,
    class=form$class_names,
    default=form$class_names[which.max(model$a0)],
    discretization=disc_info,
    description='RCAR algorithm by Azmi et al. 2019',
    method='logit',
    formula = formula,
    all_rules=model_rules,
    reg_model=model
    ),
    class = 'CBA')
}
