#' Objects for Classifiers Based on Association Rules
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
#' @param method Classification method \code{"first"} found rule or
#' \code{"majority"}.
#' @param weights Rule weights for method majority. Either a quality measure
#' available in \code{rules} or a numeric vector of the same length are
#' \code{rules} can be specified. If missing, then equal weights are used
#' @param default Default class of the form \code{variable=level}. If not
#' specified then the most frequent RHS in rules is used.
#' @param description Description field used when the classifier is printed.
#' @param x,object An object of class \code{CBA}.
#' @param newdata A data.frame or transactions containing rows of new entries
#' to be classified.
#' @param type Predict \code{"class"} labels. Some classifiers can also return
#' \code{"scores"}.
#' @param \dots Additional arguments currently not used.
#' @return \code{CBA_ruleset()} returns an object of class \code{CBA}
#' representing the trained classifier with fields: \item{formula}{used
#' formula.} \item{discretization}{discretization information.}
#' \item{rules}{the classifier rule base.} \item{default}{default class label
#' ot \code{NA}.} \item{weights}{rule weights.} \item{biases}{class biases.}
#' \item{method}{classification method.} \item{description}{description in
#' human readable form.}
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
#' # create classifier
#' cl <- CBA_ruleset(Species ~ ., cars)
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
#' cl <- CBA_ruleset(Species ~ ., cars, method = "majority", weights = "lift")
#' cl
#'
#' prediction <- predict(cl, trans)
#' table(prediction, response(Species ~ ., trans))
#'
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

#' @rdname CBA_ruleset
rules <- function(x) UseMethod("rules")

#' @rdname CBA_ruleset
rules.CBA <- function(x) x$rules
