require(arules)
require(gmodels)
setwd("~/Dropbox/Summer 2016/CSE 5390/CBA_Algorithm/")


#Test data set 1: Titanic survival
load("titanic.raw.rdata")
head(titanic.raw)
classifier <- CBA(titanic.raw, "Survived", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
results <- classify(titanic.raw, classifier)
CrossTable(x = titanic.raw$Survived, y = results, prop.chisq = FALSE)


#Test data set 2: Benign and Malignant tumors
dataset <- read.csv("wisc_bc_data.csv", stringsAsFactors = TRUE)
dataset$id <- NULL
dsDisc <- as.data.frame(lapply(dataset[2:31], function(x) discretize(x, categories = 5)))
dsDisc$diagnosis <- factor(dataset$diagnosis)
dsDisc <- dsDisc[c(1:10, 31)]
classifier <- CBA(dsDisc, "diagnosis", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
results <- classify(dsDisc, classifier)
CrossTable(x = dsDisc$diagnosis, y = results, prop.chisq = FALSE)


#Test data set 3: Flower species classification
data(iris)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=5)))
irisDisc$Species <- iris$Species
classifier <- CBA(irisDisc, "Species", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
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
