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
  #ds.mat <- as(dataset, 'transactions')
  ds.mat <- dataset
  
  #right-hand side of data records
  rightHand <- dataset[,column]
  
  setwd("~/Dropbox/Summer 2016/CSE 5390/CBA_Algorithm/C")
  dyn.load("test.so")
  .Call("test", ds.mat, rightHand, rules.sorted, nrow(ds.mat), length(rules))
  
  return(1)

}

data(iris)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=9)))
irisDisc$Species <- iris$Species
classifier <- CBA.2(irisDisc, "Species", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))