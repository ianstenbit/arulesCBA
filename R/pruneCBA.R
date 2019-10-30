pruneCBA <- function(formula, rules, trans){

  formula <- as.formula(formula)
  vars <- .parseformula(formula, trans)
  class <- vars$class_names
  vars <- vars$var_names

  rules <- sort(rules, by=c("confidence", "support"))
  #Vector used to identify rules as being 'strong' rules for the final classifier
  strongRules <- vector('logical', length=length(rules))

  rulesMatchLHS <- is.subset(lhs(rules), trans, sparse = TRUE)
  rulesMatchRHS <- is.subset(rhs(rules), trans, sparse = TRUE)

  #matrix of rules and records which constitute correct and false matches
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
  #covered[1:length(ds.mat)] <- FALSE

  defaultClasses <- vector('integer', length=length(rules))
  totalErrors <- vector('integer', length=length(rules))

  .Call("R_stage3", strongRules, casesCovered, covered, defaultClasses, totalErrors, classDistr,
    replace,matches@i, matches@p, length(matches@i),
    falseMatches@i, falseMatches@p, length(falseMatches@i), length(class),  PACKAGE = "arulesCBA")

  #save the classifier as only the rules up to the point where we have the lowest total error count
  classifier <- rules[strongRules][1:which.min(totalErrors[strongRules])]

  ### to the point where the error does not decrease anymore?


  #add a default class to the classifier (the default class from the last rule included in the classifier)
  ### defaultClass <- class[defaultClasses[strongRules][[which.min(totalErrors[strongRules])]]]
  ### add rule for default class!

  return(classifier)
}
