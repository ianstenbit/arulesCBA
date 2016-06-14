predict.CBA <- function(object, newdata, ...){
  
  #Save dataset as transaction matrix
  if(!is(newdata, "transactions")){
    ds.mat <- as(newdata, "transactions")
  } else { ds.mat <- newdata }


  if(!all(itemInfo(as(newdata, 'transactions'))$labels %in% itemInfo(as(object[[1]]@lhs, 'transactions'))$labels)){
    stop('The data in classifier does not match the data to classify.')
  }
  
  #Matrix of which rules match which transactions
  rulesMatchLHS <- is.subset(lhs(object[[1]]), ds.mat)
  
  classifier.mat <- as(rhs(object[[1]]), "ngCMatrix")
  classifier.results <- apply(classifier.mat, MARGIN = 2, which)
  classifier.results <- rownames(classifier.mat)[classifier.results]

  #Build output of classifications for input data, populate it with the default class
  output <- vector('character', length = length(ds.mat))
  output[1:length(ds.mat)] <- object[[2]]
  
  #For each transaction, if it is matched by any rule, classify it using the highest-precidence rule in the classifier
  for(i in 1:length(ds.mat)){
    if(Reduce("|", rulesMatchLHS[,i])){
      firstMatch <- which(rulesMatchLHS[,i])[1]
      result <- classifier.results[firstMatch]
      output[i] <- result
    }
  }

  output <- sapply(strsplit(output,"="),'[',2)
  
  return(output)
  
}