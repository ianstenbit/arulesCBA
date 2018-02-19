bCBA <- function(formula, data, support = 0.2, confidence = 0.8, gamma = 0.05,
  verbose=FALSE, parameter = NULL, control = NULL, sort.rules = TRUE,
  sort.parameter = NULL, lhs.support=FALSE, class.weights=NULL,
	disc.method = "cluster", disc.categories = 10,
  rule.weight.function = function(a,b,c) a-10.0*b){

    return(CBA.internal(formula, data, method="boosted", support = support,
      confidence = confidence, gamma = gamma, verbose = verbose,
      parameter = parameter, control = control, sort.rules = sort.rules,
      sort.parameter = sort.parameter, lhs.support = lhs.support,
      disc.method=disc.method, disc.categories=disc.categories,
      rule.weight.function = rule.weight.function))

}
