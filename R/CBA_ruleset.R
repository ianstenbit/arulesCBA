#' Constructor for Objects for Classifiers Based on Association Rules
#'
#' Objects for classifiers based on association rules have class \code{"CBA"}.
#' A creator function \code{CBA_ruleset()} and several methods are provided.
#'
#' \code{CBA_ruleset} creates a new object of class \code{CBA} using the
#' provides rules as the rule base.  For method \code{"first"}, the user needs
#' to make sure that the rules are predictive and sorted from most to least
#' predictive.
#'
#' @aliases CBA.object CBA_ruleset predict.CBA print.CBA rules rules.CBA
#' @param formula A symbolic description of the model to be fitted. Has to be
#' of form \code{class ~ .}. The class is the variable name (part of the item
#' label before \code{=}).
#' @param rules A set of class association rules mined with \code{mineCars} or
#' \code{apriori} (from \pkg{arules}).
#' @param default Default class. If not
#' specified then objects that are not matched by rules are classified as \code{NA}.
#' @param method Classification method \code{"first"} found rule or
#' \code{"majority"}.
#' @param weights Rule weights for method majority. Either a quality measure
#' available in \code{rules} or a numeric vector of the same length are
#' \code{rules} can be specified. If missing, then equal weights are used
#' @param bias Class bias vector.
#' @param model An optional list with model information (e.g., parameters).
#' @param discretization A list with discretization information used by \code{predict} to discretize data supplied as a \code{data.frame}.
#' @param description Description field used when the classifier is printed.
#' @param x,object An object of class \code{CBA}.
#' @param newdata A data.frame or transactions containing rows of new entries
#' to be classified.
#' @param type Predict \code{"class"} labels. Some classifiers can also return
#' \code{"scores"}.
#' @param \dots Additional arguments added as list elements to the CBA object.
#' @return \code{CBA_ruleset()} returns an object of class \code{CBA}
#' representing the trained classifier with fields:
#'   \item{formula}{used formula.}
#'   \item{rules}{the classifier rule base.}
#'   \item{default}{default class label or \code{NA}.}
#'   \item{method}{classification method.}
#'   \item{weights}{rule weights.}
#'   \item{bias}{class bias vector if available.}
#'   \item{model}{list with model description.}
#'   \item{discretization}{discretization information.}
#'   \item{description}{description in human readable form.}
#'
#' \code{predict} returns predicted labels for \code{newdata}.
#'
#' \code{rules} returns the rule base.
#' @author Michael Hahsler
#' @seealso \code{\link{CBA}}, \code{\link{mineCARs}},
#' \code{\link[arules]{apriori}}, \code{\link[arules]{rules}},
#' \code{\link[arules]{transactions}}.
#' @examples
#'
#' data("iris")
#'
#' # discretize and create transactions
#' iris.disc <- discretizeDF.supervised(Species ~., iris)
#' trans <- as(iris.disc, "transactions")
#'
#' # create rule base with CARs
#' cars <- mineCARs(Species ~ ., trans, parameter = list(support = .01, confidence = .8))
#'
#' cars <- cars[!is.redundant(cars)]
#' cars <- sort(cars, by = "conf")
#'
#' # create classifier and use the majority class as the default if no rule matches.
#' cl <- CBA_ruleset(Species ~ ., cars, method = "first",
#'   default = uncoveredMajorityClass(Species ~ ., trans, cars))
#' cl
#'
#' # look at the rule base
#' rules(cl)
#'
#' # make predictions
#' prediction <- predict(cl, trans)
#' table(prediction, response(Species ~ ., trans))
#'
#' # use weighted majority
#' cl <- CBA_ruleset(Species ~ ., cars, method = "majority", weights = "lift",
#'   default = uncoveredMajorityClass(Species ~ ., trans, cars))
#' cl
#'
#' prediction <- predict(cl, trans)
#' table(prediction, response(Species ~ ., trans))
#'
CBA_ruleset <- function(formula, rules, default = NA,
      method = "first", weights = NULL, bias = NULL,
      model = NULL, discretization = NULL, description = "Custom rule set", ...){

  # method (need to match with predict!)
  methods <- c("first", "majority", "weighted", "logit")
  m <- pmatch(method, methods)
  if(is.na(m)) stop("Unknown method")
  method <- methods[m]

  # add weights
  if(!is.null(weights)) {
    if(is.character(weights))
      weights <- quality(rules)[[weights, exact = FALSE]]
    else
      weights <- weights
  }

  formula <- as.formula(formula)
  parsedformula <- .parseformula(formula, rhs(rules))

  # RHS can only contain class items
  take <- rhs(rules) %in% parsedformula$class_items
  if(any(!take)) {
    rules <- rules[take]
    if(is.matrix(weights)) weights <- weights[take, ]
    else weights <- weights[take]

    warning("Some provided rules are not CARs with the class in the RHS and are ignored.",
    " Only ", length(rules), " rules used for the classifier.")
  }

  # check if LHS is in formula!
  lhscnt <- itemFrequency(lhs(rules), type = "absolute")
  if(sum(lhscnt[-parsedformula$var_ids]) != 0)
    warning("LHS of CARs contains items not in the formula!")

  structure(c(list(
    formula = formula,
    rules = rules,
    default = default,
    weights = weights,
    method = method,
    model = model,
    discretization = discretization,
    description = description
  ), list(...)

  ),
  class = "CBA")
}

print.CBA <- function(x, ...)
  writeLines(c(
    "CBA Classifier Object",
    paste("Formula:", deparse(x$formula)),
    paste("Number of rules:", length(x$rules)),
    paste("Default Class:", x$default),
    paste("Classification method:", x$method,
      if(is.character(x$weights)) paste("by", x$weights) else "",
      if(!is.null(x$best_k)) paste("- using best", x$best_k, "rules") else ""
      ),
    strwrap(paste("Description:", x$description), exdent = 5),
    ""
  ))

#' @rdname CBA_ruleset
rules <- function(x) UseMethod("rules")

#' @rdname CBA_ruleset
rules.CBA <- function(x) x$rules
