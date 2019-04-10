RCAR <- function(formula, data, support = 0.3, confidence = 0.7, verbose = FALSE,
  maxlen = 6, lambda = 0.001, balanceSupport = FALSE, disc.method = 'mdlp') {

  disc_info <- NULL
  if(is(data, "data.frame")){
    data <- discretizeDF.supervised(formula, data, method=disc.method)
    disc_info <- lapply(data, attr, "discretized:breaks")
  }

  trans <- as(data, 'transactions')
  model_rules <- mineCARs(formula, trans, balanceSupport,
    parameter=list(supp=support,conf=confidence,maxlen=maxlen),
    control=list(verbose=verbose))

  X <- is.superset(trans,as(lhs(model_rules),'itemMatrix'))
  y_class <- .parseformula(formula, data)$class_names
  model <- glmnet(X,data[[y_class]],family='multinomial',alpha=1,lambda=lambda)
  #num_nonzero_rules <- sum(unlist(lapply(model$beta, function(x) sum(x>0))))

  biases <- model$a0
  weights <- as.vector(Reduce('+',model$beta))
  whole_rules <- model_rules
  model_rules <- model_rules[weights>0]
  weights <- weights[weights>0]

  class_name <- .parseformula(formula, data)$class_names
  structure(list(whole_rules=whole_rules,rules=model_rules,
    weights=weights,biases=biases,
    class=unlist(lapply(model$classnames,function(x) paste0(class_name,'=',x))),
    default=model$classnames[[1]],
    discretization=disc_info,
    description='RCAR algorithm by Azmi et al. 2019',
    method='weighted',
    formula = formula),
    class = 'CBA')
}
