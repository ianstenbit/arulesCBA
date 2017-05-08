# create a CBA from a set of rules

### FIXME: we can specify weights/majority etc.

CBA_ruleset <- function(formula, rules, method = "first",
  weights = NULL, default = NULL, description = "Custom rule set"){

  # method
  methods <- c("first", "majority")
  m <- pmatch(method, methods)
  if(is.na(m)) stop("Unknown method")
  method <- methods[m]

  # find class
  formula <- as.formula(formula)
  class <- as.character(formula[[2]])
  if(as.character(formula[[3]]) != ".")
    stop("Formula needs to be of the form class ~ .")

  # only use class rules
  take <- rhs(rules) %pin% class
  rules <- rules[take]
  if(!is.null(weights)) {
    if(is.character(weights))
      weights <- quality(rules)[[weights, exact = FALSE]]
    else
    weights <- weights[take]

    if(length(rules) != length(weights))
      stop("length of weights does not match number of rules")
  }

  levels <- grep(class, itemLabels(rules), value = TRUE)
  levels <- sapply(strsplit(levels, '='), '[', 2)

  # FIXME: find default rules if it exists to set default class
  if(is.null(default))
    default <- names(which.max(table(unlist(as(rhs(rules), "list")))))

  classifier <- list(
    rules = rules,
    default = default,
    class = class,
    levels = levels,
    method = method,
    weights = weights,
    description = description)

  class(classifier) <- "CBA"

  return(classifier)
}
