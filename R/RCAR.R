RCAR <- function(formula, data, support = 0.3, confidence = 0.7, verbose = FALSE,
                 maxlen = 6, lambda = 0.001, balanceSupport = FALSE) {
  trans <- as(data,'transactions')
  rules <- mineCARs(formula, trans, balanceSupport, parameter=list(supp=support,conf=confidence,maxlen=maxlen), 
                    control=list(verbose=verbose))
  X <- is.superset(trans,as(rules@lhs,'itemMatrix'))
  y_class <- .parseformula(formula, data)$class_names
  model <- glmnet(X,data[[y_class]],family='multinomial',alpha=1,lambda=lambda)
  num_nonzero_rules <- sum(unlist(lapply(model$beta, function(x) sum(x>0))))
  classifier <- list(rules=rules,model=model,method='RCAR classifier',class=model$classnames,default=model$classnames[[1]],
                     description='RCAR algorithm by Azmi et al. 2019')
  class(classifier) <- c('RCAR','CBA')
  
  return(classifier)
}


predict.RCAR <- function(object, newdata, ...){
  rules.space<-as(object[[1]]@lhs,"itemMatrix")
  D<-as(newdata,"transactions")
  X<-arules::is.superset(D,rules.space)
  dimnames(X) <- list(NULL, paste("rule", c(1:dim(X)[[2]]), sep=""))
  lambda<-object$model$lambda
  predicted.classes<-predict(object$model, newx=X, s=lambda, type="class") 
  factor(predicted.classes)
}