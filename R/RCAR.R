RCAR <- function(formula, data, support = 0.3, confidence = 0.7, verbose = FALSE,
                 maxlen = 6, lambda = 0.001, balanceSupport = FALSE) {
  trans <- as(data,'transactions')
  rules <- mineCARs(formula, trans, balanceSupport, parameter=list(maxlen=maxlen), 
                    control=list(verbose=verbose))
  X <- is.superset(trans,as(rules@lhs,'itemMatrix'))
  y_class <- .parseformula(formula, data)$class_names
  model <- glmnet(X,data[[y_class]],family='multinomial',alpha=1,lambda=lambda)
  classifier <- list(rules=rules,model=model,name='RCAR classifier')
  class(classifier) <- 'RCAR'
  
  return(classifier)
}

print.RCAR <- function(x, ...) {
  num_rules <- lapply(x$model$beta, function(x) sum(x>0))
  writeLines(c(
    "RCAR Classifier Object",
    paste("Number of rules:", sum(unlist(num_rules))),
    ""
  ))
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