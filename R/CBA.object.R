# create a CBA object from a set of rules

# Constructor
CBA_ruleset <- function(formula, rules, method = "first",
  weights = NULL, default = NULL, description = "Custom rule set"){

  # method
  methods <- c("first", "majority")
  m <- pmatch(method, methods)
  if(is.na(m)) stop("Unknown method")
  method <- methods[m]

  # find class
  formula <- as.formula(formula)
  vars <- .parseformula(formula, as(lhs(rules[1]), "transactions"))
  class <- vars$class_names

  # TODO: filter rules to only use predictors?
  if(all.vars(formula)[2] != ".")
    stop("Formula needs to be of the form class ~ .")

  # only use class rules
  take <- rhs(rules) %in% class
  rules <- rules[take]
  if(any(!take)) warning("Some provided rules are not CARs with the class in the RHS and are ignored. Only ",
    length(rules), " rules used.")

  if(!is.null(weights)) {
    if(is.character(weights))
      weights <- quality(rules)[[weights, exact = FALSE]]
    else {
      weights <- weights[take]
      quality(rules)$ weights <- weights
    }

    if(length(rules) != length(weights))
      stop("length of weights does not match number of rules")
  }

  # FIXME: find default rules if it exists to set default class
  # If not given, use the RHS for the rules with the largest support
  if(!is.null(default)) {
    default <- class[grepl(default, class)]
    if(length(default) != 1) stop("unable to identify default class")
  } else
    default <- names(which.max(sapply(split(quality(rules)$support,
      unlist(as(rhs(rules), "list"))), sum)))

  structure(list(
    formula = formula,
    class = class,
    rules = rules,
    default = default,
    method = method,
    weights = weights,
    description = description),
    class = "CBA")
}


# Methods
print.CBA <- function(x, ...)
  writeLines(c(
    "CBA Classifier Object",
    paste("Class:", paste(x$class, collapse = ", ")),
    paste("Default Class:", x$default),
    paste("Number of rules:", length(x$rules)),
    paste("Classification method:", x$method,
      if(is.character(x$weights)) paste("by", x$weights) else "",
      if(!is.null(x$best_k)) paste("- using best", x$best_k, "rules") else ""
      ),
    strwrap(paste("Description:", x$description), exdent = 5),
    ""
  ))

rules <- function(x) UseMethod("rules")
rules.CBA <- function(x) x$rules
