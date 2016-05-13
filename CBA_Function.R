require(arules)
require(gmodels)
setwd("~/Dropbox/Summer 2016/CSE 5390/CBA_Algorithm/")

load("titanic.raw.rdata")

dataset <- read.csv("wisc_bc_data.csv", stringsAsFactors = TRUE)
dataset$id <- NULL
dsDisc <- as.data.frame(lapply(dataset[2:31], function(x) discretize(x, categories = 5)))
dsDisc$diagnosis <- factor(dataset$diagnosis)
dsDisc <- dsDisc[c(1:10, 31)]

data(iris)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=5)))
irisDisc$Species <- iris$Species

classifier <- CBA(irisDisc, "Species", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
classifier <- CBA(titanic.raw, "Survived", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
classifier <- CBA(dsDisc, "diagnosis", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.75))

results <- classify(dsDisc, classifier)
CrossTable(x = dsDisc$diagnosis, y = results, prop.chisq = FALSE)
head(classifier)

results <- classify(irisDisc, classifier)
CrossTable(x=irisDisc$Species, y=results, prop.chisq = FALSE)

results <- classify(titanic.raw, classifier)
CrossTable(x = titanic.raw$Survived, y = results, prop.chisq = FALSE)


CBA <- function(dataset, column, apriori_parameter){
  
  rules <- apriori(dataset, parameter = apriori_parameter, appearance = list(rhs=paste(column, levels(dataset[,column]), sep="="), default = "lhs"), control=list(verbose=FALSE))
  rules.sorted <- sort(rules, by=c("confidence", "support", "lift"))

  subset.matrix <- is.subset(rules.sorted, rules.sorted)
  subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
  redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
  rules.sorted <- rules.sorted[!redundant]

  ds.mat <- as(dataset, 'transactions')

  strongRules <- vector('logical', length=length(rules.sorted))

  rightHand <- dataset[,column]

  for(i in 1:length(rules.sorted)){
  
    outcome <-as(rules.sorted[i]@rhs, "ngCMatrix")[10,]
    matches <- is.subset(rules.sorted[i]@lhs, ds.mat)
  
    ds.mat <- ds.mat[!matches,]
    rightHand <- rightHand[!matches]
  
    if(length(matches) == 0){next}
  
    strongRules[i] <- Reduce('|', matches)
    defaultClass <- sort(table(rightHand), decreasing=TRUE)[1]
  
  }

  classifier <- rules.sorted[strongRules]
  #classifier <- as(classifier, "data.frame")
  #classifier$default <- defaultClasses[strongRules]
  df <- paste(column, names(defaultClass), sep="=")
  classifier <- list(classifier, df)
  return(classifier)
  
}

classify <- function(dataset, classifier){
  ds.mat <- as(dataset, "transactions")
  rulesMatchLHS <- is.subset(classifier[[1]]@lhs, ds.mat)
  classifier.mat <- as(classifier[[1]]@rhs, "ngCMatrix")
  
  classifier.results <- vector('numeric', length=length(classifier.mat[0,]))
  
  for(i in 1:length(classifier.mat[1,])){
    result <- match(TRUE,classifier.mat[,i])
    classifier.results[i] <- result
  }
  
  classifier.results <- rownames(classifier.mat)[classifier.results]
  
  output <- vector('character', length = length(ds.mat))
  output[1:length(ds.mat)] <- classifier[[2]]
  
  for(i in 1:length(ds.mat)){
       if(Reduce("|", rulesMatchLHS[,i])){
         firstMatch <- match(TRUE, rulesMatchLHS[,i])
         result <- classifier.results[firstMatch]
         output[i] <- result
       }
  }
  
  return(output)
  
}
