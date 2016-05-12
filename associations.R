setwd("~/Dropbox/Summer 2016/CSE 5390/")

load("titanic.raw.rdata")
str(titanic.raw)

require(arules)
rules <- apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.6), appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"), control=list(verbose=FALSE))
rules.sorted <- sort(rules, by=c("confidence", "support", "lift"))
inspect(rules.sorted)

#subset.matrix <- is.subset(rules.sorted, rules.sorted)
#subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
#redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
#which(redundant)

#rules.pruned <- rules.sorted[!redundant]

rules.pruned <- rules.sorted
inspect(rules.pruned)

titanic.mat <- as(titanic.raw, "transactions")

rulesMatchLHS <- is.subset(rules.pruned@lhs, titanic.mat)
rulesMatchRHS <- is.subset(rules.pruned@rhs, titanic.mat)

matches <- rulesMatchLHS & rulesMatchRHS
falseMatches <- rulesMatchLHS & !rulesMatchRHS

table(matches)
table(falseMatches)

inspect(rules.pruned)

#for(i in 1:2201){
#  inspect(rules.pruned[falseMatches[,i]])
#}

correct <- apply(matches, 2, function(x) Reduce("|", x))
table(correct)
incorrect <- apply(falseMatches, 2, function(x) Reduce("|", x))
table(incorrect)

both <- correct & incorrect
table(both)

matches[,both]
falseMatches[,both]

#Thus for each record if:
#1)No cRule, do nothing
#2)cRule exits but no wRule, mark cRule as a "Strong" cRule
#3)cRule and wRule exist, and cRule has greater precedence than wRule, mark cRule as a "Strong" cRule.
#4)cRule and wRule exist, and wRule has greater precedence than cRule, create a SetA structure and add to linked list of SetA structures.

strongRules <- vector('logical', length=length(rules.pruned))
rules.correct <- vector('numeric', length = length(rules.pruned))
incorrect <- vector('logical', length = length(titanic.mat))
incorrect[1:length(titanic.mat)] <- FALSE

for(i in 1:length(titanic.mat)){
  match <- Reduce("|", matches[, i])
  if(!match){next}
  rule <- match(TRUE, matches[,i])
  falseMatch <- Reduce("|", falseMatches[,i])
  strong <- match & !falseMatch
  orderedCorrect <- TRUE
  orderedCorrect <- !falseMatch | rule < match(TRUE, falseMatches[,i])
  strong <- strong | orderedCorrect
  strongRules[rule] <- strong | strongRules[rule]
  
  if(strong){
    rules.correct[rule] <- rules.correct[rule] + 1
  }
  
  if(!orderedCorrect){
    incorrect[i] <- TRUE  
  }
}

table(strongRules)
strongRules

table(incorrect)
incorrect.transactions <- titanic.mat[incorrect,]

str(incorrect)

for(i in 1:length(incorrect)){
  
  if(!incorrect[i]){next}
  
  wrule <- match(TRUE, falseMatches[,i])
  
  is.strong.rule <- strongRules[wrule]
  crule.exists <- Reduce('|', matches[,i])
  
  if(is.strong.rule){
    
    if(crule.exists){
      crule <- match(TRUE, matches[,i])
      rules.correct[crule] <- rules.correct[crule] - 1
    }
  
    rules.correct[wrule] <- rules.correct[wrule] + 1
  }
  
  if(crule.exists){
    crule <- match(TRUE, matches[,i])
    strongRules[crule] <- TRUE
  }
  ####Override stuff (TODO)####
}

rules.for.classifier <- strongRules & rules.correct > 0

####Part 3 (TODO)####

inspect(rules.pruned[rules.for.classifier])


results <- vector('logical', length = length(titanic.mat))
results[1:length(results)] <- FALSE
rulesMatchLHS <- is.subset(rules.pruned[rules.for.classifier]@lhs, titanic.mat)


for(i in 1:length(titanic.mat)){
  if(Reduce("|", rulesMatchLHS[,i])){
    firstMatch <- match(TRUE, rulesMatchLHS[,i])
    result <- rules.pruned[rules.for.classifier][firstMatch]@rhs
    result <- as(result, "ngCMatrix")[10]
    results[i] <- result == (titanic.raw$Survived[i] == "Survived=Yes")
  }
}

table(results)

