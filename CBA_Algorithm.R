setwd("~/Dropbox/Summer 2016/CSE 5390/CBA_Algorithm/")

load("titanic.raw.rdata")

require(arules)
rules <- apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.6), appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"), control=list(verbose=FALSE))
rules.sorted <- sort(rules, by=c("confidence", "support", "lift"))

titanic.mat <- as(titanic.raw, "transactions")

strongRules <- vector('logical', length=length(rules.sorted))

#defaultClass <- "None"
rightHand <- titanic.raw$Survived

for(i in 1:length(rules.sorted)){
  
  outcome <-as(rules.sorted[i]@rhs, "ngCMatrix")[10,]
  matches <- is.subset(rules.sorted[i]@lhs, titanic.mat)
  
  titanic.mat <- titanic.mat[!matches, ]
  rightHand <- rightHand[!matches]
  
  if(length(matches) == 0){next}
  
  strongRules[i] <- Reduce('|', matches)
  defaultClass <- sort(table(rightHand), decreasing=TRUE)[1]
  print(table(rightHand), decreasing=TRUE)
  
}

inspect(rules.sorted[strongRules])
classifier <- rules.sorted[strongRules]


###### Using classifier on set used to build it ######

titanic.mat <- as(titanic.raw, "transactions")

results <- vector('factor', length = length(titanic.mat))
results[1:length(results)] <- defaultClass

rulesMatchLHS <- is.subset(classifier@lhs, titanic.mat)

survived <- titanic.raw$Survived
table(survived)

inspect(classifier)
length(classifier)

classifier.results <- as(classifier@rhs, "ngCMatrix")[10,]

for(i in 1:length(titanic.mat)){
  if(Reduce("|", rulesMatchLHS[,i])){
    firstMatch <- match(TRUE, rulesMatchLHS[,i])
    result <- classifier.results[firstMatch]
    results[i] <- result == (titanic.raw$Survived[i] == "Survived=Yes")
  }
}

table(results)

