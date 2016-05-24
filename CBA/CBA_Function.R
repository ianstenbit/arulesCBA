require(arules)

CBA <- function(dataset, column, apriori_parameter){
  
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
  
  #Remaining rhs of transaction records, used to calculate default class
  rightHand <- dataset[,column]
  
  #For all of the pruned, sorted rules
  for(i in 1:length(rules.sorted)){
    
    #List of transactions which match this rule
    matches <- is.subset(rules.sorted[i]@lhs, ds.mat)
    
    #Remove matching transactions from remaining dataset
    ds.mat <- ds.mat[!matches,]
    rightHand <- rightHand[!matches]
    
    #If no transactions match this rule, skip it
    if(length(matches) == 0){next}
    
    #Save this rule as being a strong rule if it matched any transaction
    strongRules[i] <- Reduce('|', matches)
    
    #Save the default class after this rule
    defaultClass <- sort(table(rightHand), decreasing=TRUE)[1]
    
  }
  
  #Take the subset of pruned, sorted rules which are considered strong. These are our classifier
  classifier <- rules.sorted[strongRules]
  
  #Add a default class to the classifier
  df <- paste(column, names(defaultClass), sep="=")
  classifier <- list(classifier, df)
  
  return(classifier)
  
}


classify <- function(dataset, classifier){
  
  #Save dataset as transaction matrix
  ds.mat <- as(dataset, "transactions")
  
  #Matrix of which rules match which transactions
  rulesMatchLHS <- is.subset(classifier[[1]]@lhs, ds.mat)
  
  #Build a vector of the right hand sides of the rules
  classifier.mat <- as(classifier[[1]]@rhs, "ngCMatrix")
  classifier.results <- vector('numeric', length=length(classifier.mat[0,]))
  
  #Populate RHS vector from binary transaction matrix
  for(i in 1:length(classifier.mat[1,])){
    result <- match(TRUE,classifier.mat[,i])
    classifier.results[i] <- result
  }
  
  classifier.results <- rownames(classifier.mat)[classifier.results]
  
  #Build output of classifications for input data, populate it with the default class
  output <- vector('character', length = length(ds.mat))
  output[1:length(ds.mat)] <- classifier[[2]]
  
  #For each transaction, if it is matched by any rule, classify it using the highest-precidence rule in the classifier
  for(i in 1:length(ds.mat)){
    if(Reduce("|", rulesMatchLHS[,i])){
      firstMatch <- match(TRUE, rulesMatchLHS[,i])
      result <- classifier.results[firstMatch]
      output[i] <- result
    }
  }
  
  return(output)
  
}

CBA.2 <- function(dataset, column, apriori_parameter, verbose=FALSE){
  
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
  
  #a is a set of wrongly classified records
  a <- data.frame(dID=integer(),
                  class=character(),
                  crule=integer(),
                  wrule=integer(),
                  stringsAsFactors = FALSE)
  
  #matrix of rules and classification factor to identify how many times the rule correctly identifies the class
  classCasesCovered <- matrix(0, nrow=length(rules.sorted), ncol=length(levels(dataset[,column])))
  colnames(classCasesCovered) <- levels(dataset[,column])
  
  ####Stage 1####
  
  #for each record
  for(i in 1:length(ds.mat)){
    #find first correct and incorrect rule match for this record
    crule <- match(TRUE, matches[,i])
    wrule <- match(TRUE, falseMatches[,i])
    
    #formatting
    if(is.na(wrule)){wrule <- -1}
    if(is.na(crule)){crule <- -1}
    
    #identify the correct classification of this class by this rule
    class <- as.character((dataset[i, column]))
    if(crule != -1){
      classCasesCovered[crule, class] <- classCasesCovered[crule, class] + 1
    }
    
    #If this was correctly classified, mark the classifying rule for use in the final classifier
    if(crule > wrule){
      strongRules[crule] <- TRUE
    } 
    #otherwise, record this false classification
    else if (wrule > crule){
      a[nrow(a)+1,] <- c(i, class, crule, wrule)
    }
  }
  
  #list of all possible rule replacements for correct rules which were trumped by false rules
  replace <- data.frame(rule=integer(),
                        replaceRule=integer(),
                        dID=integer(),
                        class=character(),
                        stringsAsFactors = FALSE)
  
  ####Stage 2####
  
  #for every wrongly classified record
  for(i in 1:length(a)){
    
    #if the wrong rule was identified for use in our final classifier
    if(strongRules[as.numeric(a$wrule[i])]){
      #modify class cases covered accordingly
      if(a$crule[i] != -1) {classCasesCovered[as.integer(a$crule[i]),a$class[i]] <- classCasesCovered[as.integer(a$crule[i]), a$class[i]] - 1}
      classCasesCovered[as.integer(a$wrule[i]),a$class[i]] <- classCasesCovered[as.integer(a$wrule[i]), a$class[i]] + 1
    } 
    
    #if the wrong rule was not identified for use
    else {
      #list of all possible replacements for this rule
      wSet <- matches[,as.numeric(a$dID[i])]
      
      #for every other rule
      for(j in 1:length(wSet)){
        
        #make sure we're not comparing rule to itself
        if(wSet[j] & j != as.numeric(a$crule[i])){
          
          #mark this rule for use in final classifier
          strongRules[j] <- TRUE
          class <- as.character(a$class[i])
          
          #populate data frame of possible rule replacement
          replace[nrow(replace)+1,] <- c(as.numeric(a$crule[i]), j, a$dID[i], class)
        }
      }
    }
  }
  
  #initializing variables for stage 3
  ruleErrors <- 0
  classDistr <- dataset[,column]
  
  covered <- vector('logical', length=length(ds.mat))
  covered[1:length(ds.mat)] <- FALSE
  
  replace$rule <- as.numeric(replace$rule)
  
  defaultClasses <- vector('character', length=length(rules.sorted))
  totalErrors <- vector('numeric', length=length(rules.sorted))
  
  ####Stage 3####
  
  #for every rule in the sorted list of rules
  for(i in 1:length(rules.sorted)){
    
    #if this rule was marked for use in the classifier
    if(strongRules[i]){ 
      
      #if this rule no longer correctly identifies at least one thing, remove it from the list of rules for classifier
      if(Reduce('|', classCasesCovered[i,]) == FALSE){
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
            classCasesCovered[i,rule.replace$class[j]] <- classCasesCovered[i,rule.replace$class[j]] - 1
          } 
          #if it hasn't been covered, decrement correct classification counter for replacement rule
          else {
            classCasesCovered[rule.replace$rule[j],rule.replace$class[j]] <- classCasesCovered[rule.replace$rule[j],rule.replace$class[j]] - 1
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
  
  if(verbose){
    print(defaultClass)
    print(inspect(classifier[[1]]))
    print(min(totalErrors[strongRules]))
    print(c(which.min(totalErrors[strongRules]), length(classifier[[1]])))
  }
  
  return(classifier)
}

CBA.C <- function(dataset, column, apriori_parameter, verbose=FALSE){
  
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
  
  dyn.load("~/Dropbox/Summer 2016/CSE 5390/CBA_Algorithm/C/CBA.so")
  a <- .Call("stage1", dataset, strongRules, casesCovered, matches, falseMatches, length(rules.sorted))
  
  replace <- .Call("stage2", a, casesCovered, matches, strongRules)
  
  #initializing variables for stage 3
  ruleErrors <- 0
  classDistr <- as.integer(dataset[,column])
  
  covered <- vector('logical', length=length(ds.mat))
  covered[1:length(ds.mat)] <- FALSE
  
  defaultClasses <- vector('integer', length=length(rules.sorted))
  totalErrors <- vector('integer', length=length(rules.sorted))
  
  .Call("stage3", strongRules, casesCovered, covered, defaultClasses, totalErrors, classDistr, replace, matches, falseMatches, length(levels(dataset[,column])))
  
  #save the classifier as only the rules up to the point where we have the lowest total error count
  classifier <- rules.sorted[strongRules][1:which.min(totalErrors[strongRules])]
    
  print(totalErrors[strongRules])
  
  #add a default class to the classifier (the default class from the last rule included in the classifier)
  defaultClass <- levels(dataset[,column])[defaultClasses[strongRules][[which.min(totalErrors[strongRules])]]]
  
  df <- paste(column, defaultClass, sep="=")
  classifier <- list(classifier, df)
  
  return(classifier)
  
}

