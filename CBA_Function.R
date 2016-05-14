require(arules)
require(gmodels)
setwd("~/Dropbox/Summer 2016/CSE 5390/CBA_Algorithm/")


#Test data set 1: Titanic survival
load("titanic.raw.rdata")
head(titanic.raw)
classifier <- CBA.2(titanic.raw, "Survived", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
results <- classify(titanic.raw, classifier)
CrossTable(x = titanic.raw$Survived, y = results, prop.chisq = FALSE)


#Test data set 2: Benign and Malignant tumors
dataset <- read.csv("wisc_bc_data.csv", stringsAsFactors = TRUE)
dataset$id <- NULL
dsDisc <- as.data.frame(lapply(dataset[2:31], function(x) discretize(x, categories = 5)))
dsDisc$diagnosis <- factor(dataset$diagnosis)
dsDisc <- dsDisc[c(1:10, 31)]
classifier <- CBA.2(dsDisc, "diagnosis", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
results <- classify(dsDisc, classifier)
CrossTable(x = dsDisc$diagnosis, y = results, prop.chisq = FALSE)


#Test data set 3: Flower species classification
data(iris)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=5)))
irisDisc$Species <- iris$Species
classifier <- CBA.2(irisDisc, "Species", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
results <- classify(irisDisc, classifier)
CrossTable(x=irisDisc$Species, y=results, prop.chisq = FALSE)


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

CBA.2 <- function(dataset, column, apriori_parameter){
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
  
  classCasesCovered <- matrix(0, nrow=length(rules.sorted), ncol=length(levels(dataset[,column])))
  colnames(classCasesCovered) <- levels(dataset[,column])
  
  #Stage 1
  for(i in 1:length(ds.mat)){
    crule <- match(TRUE, matches[,i])
    wrule <- match(TRUE, falseMatches[,i])
    if(is.na(wrule)){wrule <- -1}
    if(is.na(crule)){crule <- -1}
    class <- as.character((dataset[i, column]))
    if(crule != -1){
      classCasesCovered[crule, class] <- classCasesCovered[crule, class] + 1
    }
    if(crule > wrule){
      strongRules[crule] <- TRUE
    } else if (wrule > crule){
      a[nrow(a)+1,] <- c(i, class, crule, wrule)
    }
  }
  
  replace <- data.frame(rule=integer(),
                        replaceRule=integer(),
                        dID=integer(),
                        class=character(),
                        stringsAsFactors = FALSE)
  
  #Stage 2
  for(i in 1:length(a)){
    if(strongRules[as.numeric(a$wrule[i])]){
      if(a$crule[i] != -1) {classCasesCovered[as.integer(a$crule[i]),a$class[i]] <- classCasesCovered[as.integer(a$crule[i]), a$class[i]] - 1}
      classCasesCovered[as.integer(a$wrule[i]),a$class[i]] <- classCasesCovered[as.integer(a$wrule[i]), a$class[i]] + 1
    } else {
      wSet <- matches[,as.numeric(a$dID[i])]
      for(j in 1:length(wSet)){
        if(wSet[j] & j != as.numeric(a$crule[i])){
          strongRules[j] <- TRUE
          class <- as.character(a$class[i])
          replace[nrow(replace)+1,] <- c(as.numeric(a$crule[i]), j, a$dID[i], class)
        }
      }
    }
  }
  
  ruleErrors <- 0
  classDistr <- dataset[,column]
  
  covered <- vector('logical', length=length(ds.mat))
  covered[1:length(ds.mat)] <- FALSE
  
  replace$rule <- as.numeric(replace$rule)
  
  defaultClasses <- vector('character', length=length(rules.sorted))
  totalErrors <- vector('numeric', length=length(rules.sorted))
  
  #Stage 3
  for(i in 1:length(rules.sorted)){
    if(strongRules[i]){ 
      if(Reduce('|', classCasesCovered[i,]) == FALSE){
        strongRules[i] <- FALSE
        next
      }
      rule.replace <- replace[replace$rule == i,]
      if(nrow(rule.replace) > 0){
        for(j in 1:ncol(rule.replace)){
          if(covered[as.integer(rule.replace$dID[j])]){
            classCasesCovered[i,rule.replace$class[j]] <- classCasesCovered[i,rule.replace$class[j]] - 1
          } else {
            classCasesCovered[rule.replace$rule[j],rule.replace$class[j]] <- classCasesCovered[rule.replace$rule[j],rule.replace$class[j]] - 1
          }
        }
      }
      covered <- covered | matches[i,]
      classDistr <- dataset[,column][!covered]
      ruleErrors <- ruleErrors + sum(falseMatches[i,] == TRUE)
      defaultClass <- names(sort(table(classDistr), decreasing=TRUE)[1])
      defaultErrors <- sum(sort(table(classDistr), decreasing=TRUE)[2:length(table(classDistr))])
      totalErrors[i] <- ruleErrors + defaultErrors
      defaultClasses[i] <- defaultClass
    }
  }
  
  classifier <- rules.sorted[strongRules][1:which.min(totalErrors[strongRules])]
  defaultClass <- defaultClasses[strongRules][which.min(totalErrors[strongRules])]
  df <- paste(column, defaultClass, sep="=")
  classifier <- list(classifier, df)
  return(classifier)
}
