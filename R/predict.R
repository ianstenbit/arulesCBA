predict.CBA <- function(object, newdata, ...){
  
  #Save dataset as transaction matrix
  if(!is(newdata, "transactions")){
    ds.mat <- as(newdata, "transactions")
  } else { ds.mat <- newdata }
  
  #Matrix of which rules match which transactions
  rulesMatchLHS <- is.subset(object[[1]]@lhs, ds.mat)
  
  #Build a vector of the right hand sides of the rules
  classifier.mat <- as(object[[1]]@rhs, "ngCMatrix")
  classifier.results <- vector('numeric', length=length(classifier.mat[0,]))
  
  #Populate RHS vector from binary transaction matrix
  for(i in 1:length(classifier.mat[1,])){
    result <- match(TRUE,classifier.mat[,i])
    classifier.results[i] <- result
  }
  
  classifier.results <- rownames(classifier.mat)[classifier.results]
  
  #Build output of classifications for input data, populate it with the default class
  output <- vector('character', length = length(ds.mat))
  output[1:length(ds.mat)] <- object[[2]]
  
  #For each transaction, if it is matched by any rule, classify it using the highest-precidence rule in the classifier
  for(i in 1:length(ds.mat)){
    if(Reduce("|", rulesMatchLHS[,i])){
      firstMatch <- match(TRUE, rulesMatchLHS[,i])
      result <- classifier.results[firstMatch]
      output[i] <- result
    }
  }

  output <- sapply(strsplit(output,"="),'[',2)
  
  return(output)
  
}