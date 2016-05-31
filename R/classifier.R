CBA <- function(dataset, column, apriori_parameter, verbose=FALSE){
  
  ####Preparing data####
  #Generate and sort association rules
  rules <- apriori(dataset, parameter = apriori_parameter, appearance = list(rhs=paste(column, levels(dataset[,column]), sep="="), default = "lhs"), control=list(verbose=FALSE))
  rules.sorted <- sort(rules, by=c("confidence", "support", "lift"))
  
  #Prune association rule set to remove redundant rules
  # subset.matrix <- is.subset(rules.sorted, rules.sorted)
  # subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
  # redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
  # rules.sorted <- rules.sorted[!redundant]
  
  #Build transaction matrix from dataset
  ds.mat <- as(dataset, 'transactions')
  
  #Vector used to identify rules as being 'strong' rules for the final classifier 
  strongRules <- vector('logical', length=length(rules.sorted))
  
  #right-hand side of data records
  rightHand <- dataset[,column]
  
  rulesMatchLHS <- is.subset(rules.sorted@lhs, ds.mat)
  rulesMatchRHS <- is.subset(rules.sorted@rhs, ds.mat)
  
  #matrix of rules and records which constitute correct and false matches
  matches <- rulesMatchLHS & rulesMatchRHS
  falseMatches <- rulesMatchLHS & !rulesMatchRHS
  
  #matrix of rules and classification factor to identify how many times the rule correctly identifies the class
  casesCovered <- vector('integer', length=length(rules.sorted))
  
  strongRules <- vector('logical', length=length(rules.sorted))
  
  a <- .Call("stage1", dataset, strongRules, casesCovered, matches, falseMatches, length(rules.sorted), PACKAGE = "CBAssociation")
  
  replace <- .Call("stage2", a, casesCovered, matches, strongRules,  PACKAGE = "CBAssociation")
  
  #initializing variables for stage 3
  ruleErrors <- 0
  classDistr <- as.integer(dataset[,column])
  
  covered <- vector('logical', length=length(ds.mat))
  covered[1:length(ds.mat)] <- FALSE
  
  defaultClasses <- vector('integer', length=length(rules.sorted))
  totalErrors <- vector('integer', length=length(rules.sorted))
  
  .Call("stage3", strongRules, casesCovered, covered, defaultClasses, totalErrors, classDistr, replace, matches, falseMatches, length(levels(dataset[,column])),  PACKAGE = "CBAssociation")
  
  #save the classifier as only the rules up to the point where we have the lowest total error count
  classifier <- rules.sorted[strongRules][1:which.min(totalErrors[strongRules])]
  
  #add a default class to the classifier (the default class from the last rule included in the classifier)
  defaultClass <- levels(dataset[,column])[defaultClasses[strongRules][[which.min(totalErrors[strongRules])]]]
  
  df <- paste(column, defaultClass, sep="=")
  classifier <- list(classifier, df)
  
  class(classifier) <- 
  
  return(classifier)
  
}