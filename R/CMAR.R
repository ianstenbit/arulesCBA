CMAR <- function(formula, data, support = 0.1, confidence = 0.8, coverage = 4, disc.method = "mdlp",
  balanceSupport = FALSE, parameter = NULL, control = NULL, ...){

  formula <- as.formula(formula)

  # prepare data
  disc_info <- NULL
  if(is(data, "data.frame")){
    data <- discretizeDF.supervised(formula, data, method = disc.method)
    disc_info <- lapply(data, attr, "discretized:breaks")
  }

  # convert to transactions for rule mining
  trans <- as(data, "transactions")

  if(is.null(control)) control <- as(list(verbose = FALSE), "APcontrol")
  else  control <- as(control, "APcontrol")

  # mine and prune CARs
  rulebase <- mineCARs(formula, trans, balanceSupport = balanceSupport,
    parameter = parameter, control = control,
    support = support, confidence = confidence, ...)

  if(control@verbose) cat("\nPruning rules...\n")
  rulebase <- pruneCMAR(formula, rulebase, trans, coverage = coverage)

  if(control@verbose) cat("Done...\n")

  # assemble classifier
  structure(list(
    rules = rulebase,
    class = .parseformula(formula, trans)$class_name,
    default = info(rulebase)$defaultClass,
    discretization = disc_info,
    formula = formula,
    weights = "weightedChiSquared",
    method = "weighted",
    description = paste0("CMAR algorithm (Li et al., 2001) with support=", support,
      " and confidence=", confidence)
  ),
    class = "CBA"
  )

}


# CMAR pruning
# Note: The paper mentions a difference in confidence threshold, but not how it is used!
pruneCMAR <- function(formula, rules, trans, coverage = 4, crit_threshold = 3.8415, verbose = FALSE){

  if(verbose)
    cat(paste("CMAR pruning for", length(rules), "rules and", nrow(trans), "transactions.\n"))

  formula <- as.formula(formula)
  parsedFormula <- .parseformula(formula, trans)
  class <- parsedFormula$class_names
  class_ids <- parsedFormula$class_ids
  vars <- parsedFormula$var_names

  # Step 1: Sort rules by confidence, support and size.
  quality(rules)$size <- size(rules)
  rules <- sort(rules, by=c("confidence", "support", "size"), decreasing = c(TRUE, TRUE, FALSE))

  # this implements keep only more "general rules"
  rules <- rules[!is.redundant(rules)]

  # Step 2: only select positively correlated rules (Chi2 test)
  # Note: I am not sure if the paper uses 3.8415
  quality(rules)$chiSquared <- interestMeasure(rules, measure = "chiSquared", transactions = trans)
  rules <- rules[quality(rules)$chiSquared >= crit_threshold]

  # Step 3: database coverage (like CBA, but transactions need to be covered delta (coverage) times)
  ruleCoverage <- integer(length(rules))
  transCoverageCnt <- integer(length(trans))

  # convert transactions and rules to sparse matrices
  classes <- t(as(trans[, class_ids], "ngCMatrix"))
  uncoveredTrans <- as(trans@data, "dgCMatrix")

  lhss <- as(lhs(rules)@data, "dgCMatrix")
  rhss <- as(rhs(rules)@data, "dgCMatrix")
  rulesPerClassLeft <- rowSums(rhss[class_ids, ])


  # go through rules to check coverage
  for(i in 1:length(rules)) {
    lhs <- lhss[, i, drop = FALSE]
    rhs <- rhss[, i, drop = FALSE]

    rulesPerClassLeft <- rulesPerClassLeft - rhs[class_ids]

    # find covered transactions
    covered <- crossprod(uncoveredTrans, lhs) == sum(lhs) ### this is is.subset
    numCovered <- sum(covered)

    ruleCoverage[i] <- numCovered

    if(verbose)
      cat(paste("Rule", i, "covers", numCovered, "transactions.\n"))

    # don't take a rule that does not cover any transaction
    if(numCovered < 1) next

    coveredId <- which(covered[,1])
    coveredTrans <-  uncoveredTrans[, coveredId, drop = FALSE]
    transCoverageCnt[coveredId] <-  transCoverageCnt[coveredId] + 1L

    # remove covered transactions
    remove <- transCoverageCnt >= coverage
    if(any(remove)) {
      uncoveredTrans <- uncoveredTrans[, !remove, drop = FALSE]
      transCoverageCnt <- transCoverageCnt[!remove]
    }

    transPerClassLeft <- rowSums(uncoveredTrans[class_ids, , drop = FALSE])

    if(verbose) {
      cat(paste("\tTrans left\t[", paste(transPerClassLeft, collapse = ", ") ,"]\n"))
      cat(paste("\tRules left\t[", paste(rulesPerClassLeft, collapse = ", ") ,"]\n"))
    }

    if(ncol(uncoveredTrans) < 1) break   ### all transactions covered
    if(sum(transPerClassLeft * rulesPerClassLeft) < 1) break   ### no more rules left for uncovered classes
  }

  quality(rules)$coveredTransactions <- ruleCoverage
  rules <- rules[ruleCoverage > 0]

  # add weighted Chi2 to the rules
  supP <- support(lhs(rules), trans, type = "absolute")
  supC <- support(rhs(rules), trans, type = "absolute")
  n <- length(trans)
  e <- 1/(supP*supC) + 1/(supP*(n-supC)) + 1/((n-supP)*supC) + 1/((n-supP)*(n-supC))
  maxChiSquared <- (pmin(supP, supC) - supP*supC/n)^2 * n * e
  chiSquared <- quality(rules)$chiSquared
  quality(rules)$weightedChiSquared <- chiSquared^2/maxChiSquared

  # default class. use majority class if not unclassified transactions were left
  defaultClass <- class[which.max(transPerClassLeft)]
  if(is.na(defaultClass)) defaultClass <- colnames(classes)[which.max(colSums(classes))]

  info(rules)$defaultClass <- defaultClass
  info(rules)$pruning <- "CMAR"

  return(rules)
}
