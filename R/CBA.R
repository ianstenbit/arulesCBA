CBA <- function(formula, data, support = 0.1, confidence = 0.8, pruning = "M1", disc.method = "mdlp",
  balanceSupport = FALSE, parameter = NULL, control = NULL, ...){

  # prepare data
  disc_info <- NULL
  if(is(data, "data.frame")){
    data <- discretizeDF.supervised(formula, data, method = disc.method)
    disc_info <- lapply(data, attr, "discretized:breaks")
  }

  # convert to transactions for rule mining
  trans <- as(data, "transactions")

  # parse formula
  formula <- as.formula(formula)
  vars <- .parseformula(formula, trans)
  class <- vars$class_names
  vars <- vars$var_names

  if(is.null(control)) control <- as(list(verbose = FALSE), "APcontrol")
  else  control <- as(control, "APcontrol")

  # mine and prune CARs
  rulebase <- mineCARs(formula, trans, balanceSupport = balanceSupport,
    parameter = parameter, control = control,
    support = support, confidence = confidence, ...)

  if(control@verbose) cat("\nPruning rules...\n")
  if(pruning == "M1") rulebase <- pruneCBA_M1(formula, rulebase, trans)
  else rulebase <- pruneCBA_M2(formula, rulebase, trans)

  if(control@verbose) cat("Done...\n")

  # assemble classifier
  structure(list(
    rules = rulebase,
    class = class,
    default = info(rulebase)$defaultClass,
    discretization = disc_info,
    formula = formula,
    method = "first",
    description = paste0("CBA algorithm by Liu, et al. 1998 with support=", support,
      " and confidence=", confidence)
  ),
    class = "CBA"
  )

}


### M1 pruning algorithm for CBA
pruneCBA_M1 <- function(formula, rules, trans, verbose = FALSE){

  if(verbose)
    cat(paste("CBA M1 pruning for", length(rules), "rules and", nrow(trans), "transactions.\n"))

  formula <- as.formula(formula)
  parsedFormula <- .parseformula(formula, trans)
  class <- parsedFormula$class_names
  class_ids <- parsedFormula$class_ids
  vars <- parsedFormula$var_names

  # Pre Step: Get rid of redundent rules sice they can never cover transactions
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

  classes <- t(as(trans[, class_ids], "ngCMatrix"))
  uncoveredTrans <- as(trans@data, "dgCMatrix")

  lhss <- as(lhs(rules)@data, "dgCMatrix")
  rhss <- as(rhs(rules)@data, "dgCMatrix")
  rulesPerClassLeft <- rowSums(rhss[class_ids, ])

  for(i in 1:length(rules)) {
    lhs <- lhss[, i, drop = FALSE]
    rhs <- rhss[, i, drop = FALSE]

    covered <- crossprod(uncoveredTrans, lhs) == sum(lhs) ### this is is.subset
    numCovered <- sum(covered)
    if(numCovered > 0) coveredTrans <-  uncoveredTrans[, covered[,1], drop = FALSE]
    uncoveredTrans <- uncoveredTrans[, !covered[,1], drop = FALSE]

    rulesPerClassLeft <- rulesPerClassLeft - rhs[class_ids]

    if(numCovered < 1) next

    if(verbose)
      cat(paste("Rule", i, "covers", numCovered, "transactions.\n"))


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
  strongRules <- which(ruleStats$coveredTrans>0)
  ruleStats <- ruleStats[strongRules,, drop = FALSE]
  ruleStats$errorTotal <- cumsum(ruleStats$errorRule) + ruleStats$errorDefault

  cutoff <- which.min(ruleStats$errorTotal)

  rulebase <- rules[strongRules[1:cutoff]]
  defaultClass <- colnames(classes)[ruleStats$defaultClass[cutoff]]
  quality(rulebase)$coveredTransactions <- ruleStats$coveredTrans[1:cutoff]
  quality(rulebase)$ruleErrors <- ruleStats$errorRule[1:cutoff]

  info(rulebase)$defaultClass <- defaultClass
  info(rulebase)$pruning <- "arulesCBA_M1"

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

### M2 pruning algorithm for CBA
pruneCBA_M2 <- function(formula, rules, trans){

  formula <- as.formula(formula)
  vars <- .parseformula(formula, trans)
  class <- vars$class_names
  vars <- vars$var_names

  quality(rules)$size <- size(rules)

  # Step 1: Sort rules by confidence, support and size.
  rules <- sort(rules, by=c("confidence", "support", "size"), decreasing = c(TRUE, TRUE, FALSE))

  # Step 2: Calculate covered cases
  rulesMatchLHS <- is.subset(lhs(rules), trans, sparse = TRUE)
  rulesMatchRHS <- is.subset(rhs(rules), trans, sparse = TRUE)
  matches <- rulesMatchLHS & rulesMatchRHS
  #falseMatches <- rulesMatchLHS & as(!rulesMatchRHS, "lgCMatrix") ### ! makes the matrix dense
  falseMatches <- rulesMatchLHS & !rulesMatchRHS

  #matrix of rules and classification factor to identify how many times the rule correctly identifies the class
  casesCovered <- vector('integer', length=length(rules))
  strongRules <- vector('logical', length=length(rules))

  a <- .Call("R_stage1", length(trans), strongRules, casesCovered,
    matches@i, matches@p, length(matches@i),
    falseMatches@i, falseMatches@p, length(falseMatches@i),
    length(rules), PACKAGE = "arulesCBA")

  replace <- .Call("R_stage2", a, casesCovered,
    matches@i, matches@p, length(matches@i),
    strongRules, length(matches@p), PACKAGE = "arulesCBA")

  #initializing variables for stage 3
  ruleErrors <- 0
  rightHand <- unlist(as(trans[, class], "list"))
  classDistr <- as.integer(factor(rightHand))

  covered <- vector('logical', length=length(trans)) ### is all FALSE

  # default class is the majority class in the remaining data)
  defaultClasses <- vector('integer', length=length(rules))

  totalErrors <- vector('integer', length=length(rules))

  .Call("R_stage3", strongRules, casesCovered, covered, defaultClasses, totalErrors, classDistr,
    replace,matches@i, matches@p, length(matches@i),
    falseMatches@i, falseMatches@p, length(falseMatches@i), length(class),  PACKAGE = "arulesCBA")

  quality(rules)$casesCovered <- casesCovered
  quality(rules)$totalErrors <- totalErrors

  # Step 3: The first rule at which there is the least number of errors recorded is the cutoff rule.
  rulebase <- rules[strongRules][1:which.min(totalErrors[strongRules])]

  #add a default class to the classifier (the default class from the last rule included in the classifier)
  defaultClass <- class[defaultClasses[strongRules][[which.min(totalErrors[strongRules])]]]
  ### add rule for default class!
  info(rulebase)$defaultClass <- defaultClass
  info(rulebase)$pruning <- "arulesCBA_M2"

  return(rulebase)
}

