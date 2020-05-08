#' Classification Based on Association Rules Algorithm (CBA)
#'
#' Build a classifier based on association rules using the ranking, pruning and
#' classification strategy of the CBA algorithm by Liu, et al. (1998).
#'
#' Implementation the CBA algorithm with the M1 or M2 pruning strategy
#' introduced by Liu, et al. (1998).
#'
#' Candidate classification association rules (CARs) are mined with the
#' APRIORI algorithm but minimum support is only checked for the LHS (rule coverage)
#' and not the whole rule. Rules are ranked by confidence, support and
#' size. Then either the M1 or M2 algorithm are used to perform database
#' coverage pruning and default rule pruning.
#'
#' @aliases CBA pruneCBA_M1 pruneCBA_M2 cba
#' @param formula A symbolic description of the model to be fitted. Has to be
#' of form \code{class ~ .} or \code{class ~ predictor1 + predictor2}.
#' @param data A data.frame or a transaction set containing the training data.
#' Data frames are automatically discretized and converted to transactions.
#' @param pruning Pruning strategy used: "M1" or "M2".
#' @param parameter,control Optional parameter and control lists for apriori.
#' @param balanceSupport balanceSupport parameter passed to
#' \code{\link{mineCARs}} function.
#' @param disc.method Discretization method used to discretize continuous
#' variables if data is a data.frame (default: \code{"mdlp"}). See
#' \code{\link{discretizeDF.supervised}} for more supervised discretization
#' methods.
#' @param ... For convenience, additional parameters are used to create the
#' \code{parameter} control list for apriori (e.g., to specify the support and
#' confidence thresholds).
#' @param rules,transactions prune a set of rules using a transaction set.
#' @param verbose Show progress?
#' @return Returns an object of class \code{\link{CBA.object}} representing the
#' trained classifier.
#' @author Ian Johnson and Michael Hahsler
#' @seealso \code{\link{CBA.object}}, \code{\link{mineCARs}}.
#' @references Liu, B. Hsu, W. and Ma, Y (1998). Integrating Classification and
#' Association Rule Mining. \emph{KDD'98 Proceedings of the Fourth
#' International Conference on Knowledge Discovery and Data Mining,} New York,
#' 27-31 August. AAAI. pp. 80-86.
#' \url{https://dl.acm.org/doi/10.5555/3000292.3000305}
#' @examples
#'
#' data("iris")
#'
#' # 1. Learn a classifier using automatic default discretization
#' classifier <- CBA(Species ~ ., data = iris, supp = 0.05, conf = 0.9)
#' classifier
#'
#' # inspect the rule base
#' inspect(rules(classifier))
#'
#' # make predictions
#' predict(classifier, head(iris))
#' table(pred = predict(classifier, iris), true = iris$Species)
#'
#'
#' # 2. Learn classifier from transactions (and use verbose)
#' iris_trans <- prepareTransactions(Species ~ ., iris, disc.method = "mdlp")
#' iris_trans
#' classifier <- CBA(Species ~ ., data = iris_trans, supp = 0.05, conf = 0.9, verbose = TRUE)
#' classifier
#'
#' # make predictions. Note: response extracts class information from transactions.
#' predict(classifier, head(iris_trans))
#' table(pred = predict(classifier, iris_trans), true = response(Species ~ ., iris_trans))
#'
CBA <- function(formula, data, pruning = "M1",
  parameter = NULL, control = NULL, balanceSupport = FALSE,
  disc.method = "mdlp", verbose = FALSE, ...){

  formula <- as.formula(formula)
  trans <- prepareTransactions(formula, data, disc.method)

  # mine and prune CARs
  if(verbose) cat("\nMining CARs...\n")
  rulebase <- mineCARs(formula, trans,
    parameter = parameter, control = control, balanceSupport = balanceSupport,
    verbose = verbose, ...)

  if(verbose) cat("\nPruning CARs...\n")
  if(pruning == "M1") rulebase <- pruneCBA_M1(formula, rulebase, trans)
  else rulebase <- pruneCBA_M2(formula, rulebase, trans)

  if(verbose) cat("CARs left:", length(rulebase), "\n")

  # assemble classifier
  CBA_ruleset(
    formula = formula,
    rules = rulebase,
    method = "first",
    model = list(
      parameter = c(list(...), list(parameter))
    ),
    discretization = attr(trans, "disc_info"),
    description = paste0("CBA algorithm (Liu et al., 1998)")
  )
}


#' @rdname CBA
pruneCBA_M1 <- function(formula, rules, transactions, verbose = FALSE){

  if(verbose)
    cat(paste("CBA M1 pruning for", length(rules), "rules and", nrow(transactions), "transactions.\n"))

  formula <- as.formula(formula)
  parsedFormula <- .parseformula(formula, transactions)
  class_ids <- parsedFormula$class_ids

  # Pre Step: Get rid of redundant rules since they can never cover transactions
  rules <- rules[!is.redundant(rules)]
  if(verbose)
    cat(paste("Using", length(rules), " non-redundant rules.\n"))

  # Step 1: Sort rules by confidence, support and size.
  quality(rules)$size <- size(rules)
  rules <- sort(rules, by=c("confidence", "support", "size"), decreasing = c(TRUE, TRUE, FALSE))

  # Step 2: Calculate covered cases and errors
  ruleStats <- data.frame(
    coveredTrans = numeric(length(rules)),
    errorRules = numeric(length(rules)),
    errorDefault = numeric(length(rules)),
    defaultClass = numeric(length(rules))
  )

  classes <- t(as(transactions[, class_ids], "ngCMatrix"))
  uncoveredTrans <- as(transactions@data, "dgCMatrix")

  lhss <- as(lhs(rules)@data, "dgCMatrix")
  rhss <- as(rhs(rules)@data, "dgCMatrix")
  rulesPerClassLeft <- rowSums(rhss[class_ids, ])

  for(i in 1:length(rules)) {
    lhs <- lhss[, i, drop = FALSE]
    rhs <- rhss[, i, drop = FALSE]

    rulesPerClassLeft <- rulesPerClassLeft - rhs[class_ids]

    covered <- crossprod(uncoveredTrans, lhs) == sum(lhs) ### this is is.subset
    numCovered <- sum(covered)

    if(verbose)
      cat(paste("Rule", i, "covers", numCovered, "transactions.\n"))

    # no rules are covered
    if(numCovered < 1) next

    coveredTrans <-  uncoveredTrans[, covered[,1], drop = FALSE]
    uncoveredTrans <- uncoveredTrans[, !covered[,1], drop = FALSE]

    numTrue <- sum(crossprod(coveredTrans, rhs) > 0)
    numFalse <- numCovered - numTrue

    defaultClassDist <- rowSums(uncoveredTrans[class_ids, , drop = FALSE])
    defaultClass <- which.max(defaultClassDist)

    numDefaultError <- sum(defaultClassDist[-defaultClass])

    ruleStats$coveredTrans[i] <- numCovered
    ruleStats$errorRule[i] <- numFalse
    ruleStats$errorDefault[i] = numDefaultError
    ruleStats$defaultClass[i] = defaultClass

    if(verbose) {
      cat(paste("\tTrans left\t[", paste(defaultClassDist, collapse = ", ") ,"]\n"))
      cat(paste("\tRules left\t[", paste(rulesPerClassLeft, collapse = ", ") ,"]\n"))
    }

    if(ncol(uncoveredTrans) < 1) break   ### all transactions covered
    if(sum(defaultClassDist * rulesPerClassLeft) < 1) break   ### no more rules left for uncovered classes
  }

  # Step 3: select rule set that minimizes the total error
  # (total error is the error made if we delete all rules after and use a default
  # rule with the majority as the RHS)
  strongRules <- which(ruleStats$coveredTrans>0)
  ruleStats <- ruleStats[strongRules,, drop = FALSE]
  ruleStats$errorTotal <- cumsum(ruleStats$errorRule) + ruleStats$errorDefault

  cutoff <- which.min(ruleStats$errorTotal)

  rulebase <- rules[strongRules[1:cutoff]]
  quality(rulebase)$coveredTransactions <- ruleStats$coveredTrans[1:cutoff]
  quality(rulebase)$totalErrors <- ruleStats$errorTotal[1:cutoff]

  ### add default rule
  defaultClass <- colnames(classes)[ruleStats$defaultClass[cutoff]]
  default_rule <- new("rules",
    lhs = encode(character(), itemLabels(rulebase)),
    rhs = encode(defaultClass, itemLabels(rulebase))
  )

  s <- itemFrequency(transactions[, defaultClass])
  quality(default_rule) <- data.frame(support = s, confidence = s, coverage = 1, lift = 1,
    count = length(transactions), size = 1,
    coveredTransactions = length(transactions) - sum(quality(rulebase)$coveredTransactions),
    totalErrors = min(ruleStats$errorTotal))
  rulebase <- c(rulebase, default_rule)

  info(rulebase)$defaultClass <- defaultClass

  info(rulebase)$pruning <- "CBA_M1"
  return(rulebase)
}


### M1 pruning algorithm for CBA - This version is slow
# pruneCBA_M1 <- function(formula, rules, trans){
#
#   formula <- as.formula(formula)
#   parsedFormula <- .parseformula(formula, trans)
#   class <- parsedFormula$class_names
#   class_ids <- parsedFormula$class_ids
#   vars <- parsedFormula$var_names
#
#   quality(rules)$size <- size(rules)
#
#   # Step 1: Sort rules by confidence, support and size.
#   rules <- sort(rules, by=c("confidence", "support", "size"), decreasing = c(TRUE, TRUE, FALSE))
#
#   # Step 2: Calculate covered cases
#   # All matrices are transactions x rules
#   rulesMatchLHS <- t(is.subset(lhs(rules), trans, sparse = TRUE))
#   rulesMatchRHS <- t(is.subset(rhs(rules), trans, sparse = TRUE))
#   trueMatches <- rulesMatchLHS & rulesMatchRHS
#   #falseMatches <- rulesMatchLHS & as(!rulesMatchRHS, "lgCMatrix") ### ! makes the matrix dense
#   falseMatches <- rulesMatchLHS & !rulesMatchRHS
#
#   #matchedTrans <- logical(length(trans))
#   # for performance we use a sparse matrix with one column
#   matchedTrans <- sparseMatrix(x = FALSE, i={}, j={}, dims = c(length(trans),1))
#
#   ruleStats <- data.frame(
#     coveredTrans = numeric(length(rules)),
#     errorRules = numeric(length(rules)),
#     errorDefault = numeric(length(rules)),
#     defaultClass = numeric(length(rules))
#   )
#
#   classes <- t(as(trans[, class_ids], "ngCMatrix"))
#
#   for(i in 1:length(rules)) {
#     tm <- trueMatches[, i, drop = FALSE]
#     fm <- falseMatches[, i, drop = FALSE]
#
#     ruleStats$coveredTrans[i] <- sum(tm * !matchedTrans)
#
#     # nothing is covered!
#     if(ruleStats$coveredTrans[i] < 1) next
#
#     ruleStats$errorRules[i] <- sum(fm * !matchedTrans)
#
#     # remove covered transactions
#     matchedTrans <- matchedTrans | tm
#
#     defaultClassDist <- colSums(classes[!matchedTrans[,1 ],])
#     ruleStats$defaultClass[i] <- which.max(defaultClassDist)
#     ruleStats$errorDefault[i] <- sum(defaultClassDist[-ruleStats$defaultClass[i]])
#
#   }
#
#   ruleStats$errorRules <- cumsum(ruleStats$errorRules)
#   ruleStats$errorTotal <- ruleStats$errorRules + ruleStats$errorDefault
#
#   strongRules <- which(ruleStats$coveredTrans>0)
#   cutoff <- which.min(ruleStats$errorTotal[strongRules])
#
#   quality(rules)$errorTotal <- ruleStats$errorTotal
#
#   rulebase <- rules[strongRules[1:cutoff]]
#   defaultClass <- colnames(classes)[ruleStats$defaultClass[strongRules[cutoff]]]
#
#   info(rulebase)$defaultClass <- defaultClass
#   info(rulebase)$pruning <- "arulesCBA_M1"
#
#   return(rulebase)
# }

# FIXME: verbose needs to be implemented
#' @rdname CBA
pruneCBA_M2 <- function(formula, rules, transactions, verbose = FALSE){

  if(verbose) warning("verbose not implemented yet for pruneCBA_M2.")
  formula <- as.formula(formula)
  class <- .parseformula(formula, transactions)$class_items

  quality(rules)$size <- size(rules)

  # Step 1: Sort rules by confidence, support and size.
  rules <- sort(rules, by=c("confidence", "support", "size"), decreasing = c(TRUE, TRUE, FALSE))

  # Step 2: Calculate covered cases
  rulesMatchLHS <- is.subset(lhs(rules), transactions, sparse = TRUE)
  rulesMatchRHS <- is.subset(rhs(rules), transactions, sparse = TRUE)
  matches <- rulesMatchLHS & rulesMatchRHS
  #falseMatches <- rulesMatchLHS & as(!rulesMatchRHS, "lgCMatrix") ### ! makes the matrix dense
  falseMatches <- rulesMatchLHS & !rulesMatchRHS

  #matrix of rules and classification factor to identify how many times the rule correctly identifies the class
  casesCovered <- vector('integer', length=length(rules))
  strongRules <- vector('logical', length=length(rules))

  a <- .Call("R_stage1", length(transactions), strongRules, casesCovered,
    matches@i, matches@p, length(matches@i),
    falseMatches@i, falseMatches@p, length(falseMatches@i),
    length(rules), PACKAGE = "arulesCBA")

  replace <- .Call("R_stage2", a, casesCovered,
    matches@i, matches@p, length(matches@i),
    strongRules, length(matches@p), PACKAGE = "arulesCBA")

  #initializing variables for stage 3
  ruleErrors <- 0
  rightHand <- unlist(as(transactions[, class], "list"))
  classDistr <- as.integer(factor(rightHand))

  covered <- vector('logical', length=length(transactions)) ### is all FALSE

  # default class is the majority class in the remaining data)
  defaultClasses <- vector('integer', length=length(rules))
  totalErrors <- vector('integer', length=length(rules))

  ### FIXME: there is a bug in stage 3! The totalErrors count is off!
  .Call("R_stage3", strongRules, casesCovered, covered, defaultClasses, totalErrors, classDistr,
    replace,matches@i, matches@p, length(matches@i),
    falseMatches@i, falseMatches@p, length(falseMatches@i), length(class),  PACKAGE = "arulesCBA")

  quality(rules)$coveredTransactions <- casesCovered
  quality(rules)$totalErrors <- totalErrors

  # Step 3: The first rule at which there is the least number of errors recorded is the cutoff rule.
  rulebase <- rules[strongRules][1:which.min(totalErrors[strongRules])]

  ### add default rule
  defaultClass <- class[defaultClasses[strongRules][[which.min(totalErrors[strongRules])]]]

  default_rule <- new("rules",
    lhs = encode(character(), itemLabels(rulebase)),
    rhs = encode(defaultClass, itemLabels(rulebase))
  )
  s <- itemFrequency(transactions[, defaultClass])
  quality(default_rule) <- data.frame(support = s, confidence = s, lift = 1,
    count = length(transactions), size = 1,
    coveredTransactions = length(transactions) - sum(quality(rulebase)$coveredTransactions),
    totalErrors = min(totalErrors[strongRules]))
  rulebase <- c(rulebase, default_rule)

  info(rulebase)$defaultClass <- defaultClass

  info(rulebase)$pruning <- "CBA_M2"
  return(rulebase)
}

