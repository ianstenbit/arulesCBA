require(gmodels)
require(arules)

CBA.2 <- function(dataset, column, apriori_parameter, verbose=FALSE){
  
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
  classDistr <- dataset[,column]
  
  covered <- vector('logical', length=length(ds.mat))
  covered[1:length(ds.mat)] <- FALSE
  
  replace <- as.data.frame(matrix(replace, ncol=3))
  colnames(replace) <- c("rule", "replaceRule", "dID")
  replace$rule <- as.numeric(replace$rule)
  
  defaultClasses <- vector('character', length=length(rules.sorted))
  totalErrors <- vector('numeric', length=length(rules.sorted))
  
  ####Stage 3####
  
  #for every rule in the sorted list of rules
  for(i in 1:length(rules.sorted)){
    
    #if this rule was marked for use in the classifier
    if(strongRules[i]){ 
      
      #if this rule no longer correctly identifies at least one thing, remove it from the list of rules for classifier
      if(casesCovered[i] == 0){
        strongRules[i] <- FALSE
        next
      }
      
      #get list of possible rules to replace this rule
      rule.replace <- replace[replace$rule == i,]
      
      #if there are possible replacements
      if(nrow(rule.replace) > 0){
        
        # for every possible replacement
        for(j in 1:nrow(rule.replace)){
          
          #if the entry for which this rule has been labeled a possible replacement has already been classified by the classifier, decrement its correct classification
          if(covered[as.integer(rule.replace$dID[j])]){
            casesCovered[i] <- casesCovered[i] - 1
          } 
          #if it hasn't been covered, decrement correct classification counter for replacement rule
          else {
            casesCovered[rule.replace$rule[j]] <- casesCovered[rule.replace$rule[j]]  - 1
          }
        }
      }
      
      #recognize which entried we've now covered with the current rule
      covered <- covered | matches[i,]
      
      #save the remaining distribution of classes of those entries which haven't been classified
      classDistr <- dataset[,column][!covered]
      
      #add all of the false matches of this rule to the rule error count
      ruleErrors <- ruleErrors + sum(falseMatches[i,] == TRUE)
      
      #save the default class and calculate how many errors use of that defualt class will generate
      defaultClass <- names(sort(table(classDistr), decreasing=TRUE)[1])
      defaultErrors <- sum(sort(table(classDistr), decreasing=TRUE)[2:length(table(classDistr))])
      
      #save the total number of errors for the current classifier and the default class at this stage
      totalErrors[i] <- ruleErrors + defaultErrors
      defaultClasses[i] <- defaultClass
    }
  }
  
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
classifier <- CBA.2(irisDisc, "Species", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))