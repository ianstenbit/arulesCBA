setwd("~/Dropbox/Summer 2016/CSE 5390/CBA_Algorithm/")

load("titanic.raw.rdata")

require(arules)
rules <- apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.6), appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"), control=list(verbose=FALSE))
rules.sorted <- sort(rules, by=c("confidence", "support", "lift"))

# subset.matrix <- is.subset(rules.sorted, rules.sorted)
# subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
# redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
# rules.pruned <- rules.sorted[!redundant]

rules.pruned <- rules.sorted
inspect(rules.pruned)

titanic.mat <- as(titanic.raw, "transactions")

rulesMatchLHS <- is.subset(rules.pruned@lhs, titanic.mat)
rulesMatchRHS <- is.subset(rules.pruned@rhs, titanic.mat)

matches <- rulesMatchLHS & rulesMatchRHS
falseMatches <- rulesMatchLHS & !rulesMatchRHS

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
#incorrect <- vector('logical', length = length(titanic.mat))
#incorrect[1:length(titanic.mat)] <- FALSE

for(i in 1:length(rules.pruned)){
  
  outcome <-as(rules.pruned[i]@rhs, "ngCMatrix")[10,]
  matches <- is.subset(rules.pruned[i]@lhs, titanic.mat)
  
  titanic.mat <- titanic.mat[!matches, ]
  
  if(length(matches) == 0){next}
  
  strongRules[i] <- Reduce('|', matches)
  
}

titanic.mat <- as(titanic.raw, "transactions")



# for(i in 1:length(titanic.mat)){
#   match <- Reduce("|", matches[, i])
#   if(!match){next}
#   rule <- match(TRUE, matches[,i])
#   falseMatch <- Reduce("|", falseMatches[,i])
#   strong <- match & !falseMatch
#   orderedCorrect <- TRUE
#   orderedCorrect <- !falseMatch | rule < match(TRUE, falseMatches[,i])
#   strong <- strong | orderedCorrect
#   strongRules[rule] <- strong | strongRules[rule]
#   
#   if(strong){
#     rules.correct[rule] <- rules.correct[rule] + 1
#   }
#   
#   if(!orderedCorrect){
#     incorrect[i] <- TRUE  
#   }
# }

#table(strongRules)
#strongRules

#table(incorrect)
#incorrect.transactions <- titanic.mat[incorrect,]

#str(incorrect)

# for(i in 1:length(incorrect)){
#   
#   if(!incorrect[i]){next}
#   
#   wrule <- match(TRUE, falseMatches[,i])
#   
#   is.strong.rule <- strongRules[wrule]
#   crule.exists <- Reduce('|', matches[,i])
#   
#   if(is.strong.rule){
#     
#     if(crule.exists){
#       crule <- match(TRUE, matches[,i])
#       rules.correct[crule] <- rules.correct[crule] - 1
#     }
#   
#     rules.correct[wrule] <- rules.correct[wrule] + 1
#   }
#   
#   if(crule.exists){
#     crule <- match(TRUE, matches[,i])
#     strongRules[crule] <- TRUE
#   }
#   
#   if(length(falseMatches[,i][falseMatches[,i] == TRUE]) > 1){
#     print(falseMatches[,i])
#     secondFalseRule <- falseMatches[,i][(falseMatches[,i] == TRUE)[2]]
#     print(secondFalseRule)  
#   }
#   ####Override stuff (TODO)####
# }

#rules.for.classifier <- strongRules & rules.correct > 0

####Part 3 (TODO)####

#inspect(rules.pruned[rules.for.classifier])
#classifier <- rules.pruned[rules.for.classifier]


inspect(rules.pruned[strongRules])
classifier <- rules.pruned[strongRules]

results <- vector('logical', length = length(titanic.mat))
results[1:length(results)] <- FALSE

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

