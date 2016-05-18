require(gmodels)
require(arules)

CBA.C <- function(dataset, column, apriori_parameter, verbose=FALSE){
  
  ####Preparing data####
  #Generate and sort association rules
  rules <- apriori(dataset, parameter = apriori_parameter, appearance = list(rhs=paste(column, levels(dataset[,column]), sep="="), default = "lhs"), control=list(verbose=FALSE))
  rules.sorted <- sort(rules, by=c("confidence", "support", "lift"))
  
  #Prune association rule set to remove redundant rules
  subset.matrix <- is.subset(rules.sorted, rules.sorted)
  subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
  redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
  rules.sorted <- rules.sorted[!redundant]
  
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
  
  #a is a set of wrongly classified records
  #a <- data.frame(dID=integer(),
  #                crule=integer(),
  #                wrule=integer())
  
  #matrix of rules and classification factor to identify how many times the rule correctly identifies the class
  casesCovered <- vector('integer', length=length(rules.sorted))
  
  strongRules <- vector('logical', length=length(rules.sorted))
  
  setwd("~/Dropbox/Summer 2016/CSE 5390/CBA_Algorithm/C")
  dyn.load("test.so")
  a <- .Call("stage1", dataset, strongRules, casesCovered, matches, falseMatches, length(rules.sorted), grep(column, colnames(dataset)))
  
  replace <- .Call("stage2", a, casesCovered, matches, strongRules)
  
  #initializing variables for stage 3
  ruleErrors <- 0
  classDistr <- as.integer(dataset[,column])
  
  covered <- vector('logical', length=length(ds.mat))
  covered[1:length(ds.mat)] <- FALSE
  
  defaultClasses <- vector('integer', length=length(rules.sorted))
  totalErrors <- vector('integer', length=length(rules.sorted))
  
  
  .Call("stage3", strongRules, casesCovered, covered, defaultClasses, totalErrors, classDistr, replace, matches, falseMatches, length(levels(dataset[,column])))
  
  print(totalErrors)
  print(which.min(totalErrors[strongRules]))
  print(defaultClasses)
  
  #save the classifier as only the rules up to the point where we have the lowest total error count
  classifier <- rules.sorted[strongRules][1:which.min(totalErrors[strongRules])]
  
  #add a default class to the classifier (the default class from the last rule included in the classifier)
  defaultClass <- defaultClasses[strongRules][which.min(totalErrors[strongRules])]
  df <- paste(column, defaultClass, sep="=")
  classifier <- list(classifier, df)
  
  return(classifier)

}

data(iris)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=9)))
irisDisc$Species <- iris$Species
classifier <- CBA.C(irisDisc, "Species", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
#results <- classify(irisDisc, classifier)
#CrossTable(x=irisDisc$Species, y=results, prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
