# create a CBA from a set of rules

### FIXME: we can specify weights/majority etc.

CBA_ruleset <- function(rules, class){

  # only use class rules
  rules <- subset(rules, rhs %pin% class)
  levels <- grep(class, itemLabels(rules), value = TRUE)
  levels <- sapply(strsplit(levels, '='), '[', 2)

  # FIXME: find default rules if it exists to set default class
  default <- names(which.max(table(unlist(as(rhs(rules), "list")))))

  classifier <- list(
    rules = rules,
    default = default,
    levels = levels)

  class(classifier) <- "CBA"

  return(classifier)
}
