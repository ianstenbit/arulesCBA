predict.CBA <- function(object, newdata, ...){

  #Save dataset as transaction matrix
  ds.mat <- as(newdata, "transactions")

  #Use recode to make sure the new data corresponds to the model data
  ds.mat <- recode(ds.mat, match = lhs(object$rules))

  #Matrix of which rules match which transactions
  rulesMatchLHS <- is.subset(lhs(object$rules), ds.mat)

  classifier.mat <- as(rhs(object$rules), "ngCMatrix")
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

  # preserve the levels of original data for data.frames
  if(!is.null(object$levels))
    output <- factor(output, levels = object$levels, labels = object$levels)
  else
    output <- factor(output)

  return(output)

}
