require(arules)
require(gmodels)
setwd("~/Dropbox/Summer 2016/CSE 5390/CBA_Algorithm/")

load("titanic.raw.rdata")
#rules <- apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.6), appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"), control=list(verbose=FALSE))

dataset <- read.csv("wisc_bc_data.csv", stringsAsFactors = TRUE)
dataset$id <- NULL

head(iris)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=5)))
irisDisc$Species <- iris$Species

classifier <- CBA(irisDisc, "Species", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
inspect(classifier)


dsDisc <- as.data.frame(lapply(dataset[2:31], function(x) discretize(x, categories = 5)))
dsDisc$diagnosis <- factor(dataset$diagnosis)
dsDisc <- dsDisc[c(1:10, 31)]


classifier <- CBA(titanic.raw, "Survived", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.75))
classifier <- CBA(dsDisc, "diagnosis")

inspect(classifier)

paste("diagnosis", levels(dsDisc[,"diagnosis"]), sep="=")

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
  return(classifier)
  
}

# ###### Using classifier on set used to build it ######
#
# #titanic.mat <- as(titanic.raw, "transactions")
# ds.mat <- as(dsDisc, "transactions")
#
# #results <- vector('logical', length = length(titanic.mat))
# results <- vector('logical', length = length(ds.mat))
# results[1:length(results)] <- defaultClass
#
# #rulesMatchLHS <- is.subset(classifier@lhs, titanic.mat)
# rulesMatchLHS <- is.subset(classifier@lhs, ds.mat)
#
# #survived <- titanic.raw$Survived
# diagnosis <- dsDisc$diagnosis
#
# classifier.results <- as(classifier@rhs, "ngCMatrix")
# classifier.results <- classifier.results[length(classifier.results[,1]),]
#
# output <- vector('logical', length=length(ds.mat))
#
# #for(i in 1:length(titanic.mat)){
# for(i in 1:length(ds.mat)){
#   if(Reduce("|", rulesMatchLHS[,i])){
#     firstMatch <- match(TRUE, rulesMatchLHS[,i])
#     result <- classifier.results[firstMatch]
#     #results[i] <- result == (titanic.raw$Survived[i] == "Survived=Yes")
#     results[i] <- result == (dsDisc$diagnosis[i] == "M")
#     output[i] <- result
#   }
# }
#
# table(as.logical(results))
#
# CrossTable(x = dsDisc$diagnosis == "M", y = output, prop.chisq = FALSE)

