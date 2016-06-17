CBA <- function(data, class, support = 0.2, confidence = 0.8, verbose=FALSE){

  ####Preparing data####
  lvls <- NULL
  if(is(data, "data.frame")) lvls <- levels(data[[class]])

  ds.mat <- as(data, "transactions")
  info <- itemInfo(ds.mat)
  classNames <- info[info$variables == class,'labels']
  if(is.null(lvls)) lvls <- sapply(strsplit(classNames,"="),'[',2)

  rightHand <- as(ds.mat[, classNames], "list")
  if(!all(sapply(rightHand, length) == 1L)) stop("Problem with items used for class. Examples with multiple/no class label!")
  rightHand <- as.factor(unlist(rightHand))

  #rightHand <- as.factor(classNames)
  #rows <- as.integer(rownames(info[info$variables == class,]))
  #rows <- which(info$variables == class)
  #rightHand <- vector('character', nrow(ds.mat))
  #
  #rightHand[ds.mat@data[rows,]] <- classNames
  #
  #for (i in 1:length(classNames)){
  #  rightHand[ds.mat@data[rows[i],]] <- classNames[i]
  #}
  #rightHand <- factor(rightHand)

  #Generate and sort association rules
  rules <- apriori(ds.mat, parameter = list(minlen = 2, supp = support, conf = confidence), appearance = list(rhs=levels(rightHand), default = "lhs"), control=list(verbose=verbose))
  rules.sorted <- sort(rules, by=c("confidence", "support", "lift"))

  #Prune association rule set to remove redundant rules
  # subset.matrix <- is.subset(rules.sorted, rules.sorted)
  # subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
  # redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
  # rules.sorted <- rules.sorted[!redundant]

  #Vector used to identify rules as being 'strong' rules for the final classifier
  strongRules <- vector('logical', length=length(rules.sorted))

  rulesMatchLHS <- is.subset(lhs(rules.sorted), ds.mat)
  rulesMatchRHS <- is.subset(rhs(rules.sorted), ds.mat)

  #matrix of rules and records which constitute correct and false matches
  matches <- rulesMatchLHS & rulesMatchRHS
  falseMatches <- rulesMatchLHS & !rulesMatchRHS

  #matrix of rules and classification factor to identify how many times the rule correctly identifies the class
  casesCovered <- vector('integer', length=length(rules.sorted))

  strongRules <- vector('logical', length=length(rules.sorted))

  a <- .Call("stage1", length(ds.mat), strongRules, casesCovered, matches, falseMatches, length(rules.sorted), PACKAGE = "arulesCBA")

  replace <- .Call("stage2", a, casesCovered, matches, strongRules,  PACKAGE = "arulesCBA")

  #initializing variables for stage 3
  ruleErrors <- 0
  classDistr <- as.integer(rightHand)

  covered <- vector('logical', length=length(ds.mat))
  covered[1:length(ds.mat)] <- FALSE

  defaultClasses <- vector('integer', length=length(rules.sorted))
  totalErrors <- vector('integer', length=length(rules.sorted))

  .Call("stage3", strongRules, casesCovered, covered, defaultClasses, totalErrors, classDistr, replace, matches, falseMatches, length(levels(rightHand)),  PACKAGE = "arulesCBA")

  #save the classifier as only the rules up to the point where we have the lowest total error count
  classifier <- rules.sorted[strongRules][1:which.min(totalErrors[strongRules])]

  #add a default class to the classifier (the default class from the last rule included in the classifier)
  defaultClass <- levels(rightHand)[defaultClasses[strongRules][[which.min(totalErrors[strongRules])]]]

  classifier <- list(
    rules = classifier,
    default = defaultClass,
    levels = lvls)

  class(classifier) <- "CBA"

  return(classifier)

}
