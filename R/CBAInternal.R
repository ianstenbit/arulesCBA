CBA.internal <- function(rules, data, class, disc.data, method="CBA", support = 0.2, confidence = 0.8, gamma = 0.05, cost = 10.0,
  verbose=FALSE, parameter = NULL, control = NULL, sort.parameter=NULL, lhs.support=TRUE, class.weights=NULL,
  disc.categories = 10, disc.method="cluster", sort.rules=FALSE){

  if(method == "boosted"){
    description <- paste0("Transaction boosted associative classifier with support=", support,
      " confidence=", confidence, " gamma=", gamma, " cost=", cost)
  } else if(method == "weighted"){
		description <- "Weighted CBA algorithm"
	} else {
    description <- paste0("CBA algorithm by Liu, et al. 1998 with support=", support,
      " and confidence=", confidence)
  }

  #Convert to transactions for rule mining
  ds.mat <- data
  info <- itemInfo(ds.mat)
  classNames <- info[info$variables == class,'labels']

  #Compute the levels of the target variable
  lvls <- sapply(strsplit(classNames,"="),'[',2)

  #Build vector of rhe right hahd (target for classification)
  rightHand <- as(ds.mat[, classNames], "list")
  if(!all(sapply(rightHand, length) == 1L)) stop("Problem with items used for class. Examples with multiple/no class label!")
  rightHand <- as.factor(unlist(rightHand))

  #Assign is.null to default value of 1s if no class weights specified
  if(is.null(class.weights)) class.weights <- as.numeric(rep(1, length(levels(rightHand))))

  #Original CBA algorithm, sans pessisimistic error-rate pruning
  if(method == "CBA"){

    if(sort.rules) {
      if(is.null(sort.parameter)){
        rules.sorted <- sort(rules, by=c("confidence", "support", "lift"))
      } else {
        rules.sorted <- sort(rules, by=sort.parameter)
      }
    } else {
      rules.sorted <- rules
    }

    #Vector used to identify rules as being 'strong' rules for the final classifier
    strongRules <- vector('logical', length=length(rules.sorted))

    rulesMatchLHS <- is.subset(lhs(rules.sorted), ds.mat, sparse = TRUE)
    rulesMatchRHS <- is.subset(rhs(rules.sorted), ds.mat, sparse = TRUE)

    #matrix of rules and records which constitute correct and false matches
    matches <- rulesMatchLHS & rulesMatchRHS
    falseMatches <- rulesMatchLHS & !rulesMatchRHS


    #matrix of rules and classification factor to identify how many times the rule correctly identifies the class
    casesCovered <- vector('integer', length=length(rules.sorted))

    strongRules <- vector('logical', length=length(rules.sorted))

    a <- .Call("R_stage1", length(ds.mat), strongRules, casesCovered, matches@i, matches@p, length(matches@i), falseMatches@i, falseMatches@p, length(falseMatches@i), length(rules.sorted), PACKAGE = "arulesCBA")

    replace <- .Call("R_stage2", a, casesCovered, matches@i, matches@p, length(matches@i), strongRules, length(matches@p), PACKAGE = "arulesCBA")

    #initializing variables for stage 3
    ruleErrors <- 0
    classDistr <- as.integer(rightHand)

    covered <- vector('logical', length=length(ds.mat))
    covered[1:length(ds.mat)] <- FALSE

    defaultClasses <- vector('integer', length=length(rules.sorted))
    totalErrors <- vector('integer', length=length(rules.sorted))

    .Call("R_stage3", strongRules, casesCovered, covered, defaultClasses, totalErrors, classDistr, replace,matches@i, matches@p, length(matches@i), falseMatches@i, falseMatches@p, length(falseMatches@i), length(levels(rightHand)),  PACKAGE = "arulesCBA")

    #save the classifier as only the rules up to the point where we have the lowest total error count
    classifier <- rules.sorted[strongRules][1:which.min(totalErrors[strongRules])]

    #add a default class to the classifier (the default class from the last rule included in the classifier)
    defaultClass <- levels(rightHand)[defaultClasses[strongRules][[which.min(totalErrors[strongRules])]]]

    classifier <- list(
      rules = classifier,
      class = class,
      levels = lvls,
      default = defaultClass,
      description = description,
      method = "first"
    )

  } else if(method == "boosted") {

    if(sort.rules) {
      if(is.null(sort.parameter)){
        rules.sorted <- sort(rules, by=c("lift", "confidence", "support"))
        } else {
          rules.sorted <- sort(rules, by=sort.parameter)
      }
    } else {
      rules.sorted <- rules
    }

    rules.sorted <- rules.sorted[1:min(length(rules.sorted), 50000)]

    rule_weights <- rep(0, length(rules.sorted))

    defaultClass <- .Call("R_weighted", rule_weights, rules.sorted@lhs@data@i, rules.sorted@lhs@data@p, rules.sorted@rhs@data@i, ds.mat@data@i, ds.mat@data@p, ds.mat@data@Dim, gamma, cost, length(levels(rightHand)), class.weights)

    classifier <- list(
      rules = rules.sorted[rule_weights > 0],
      weights = rule_weights[rule_weights > 0],
      class = class,
      levels = lvls,
      default = levels(rightHand)[defaultClass],
      description = description,
      method = "weighted"
    )


  } else if(method == "weighted"){

		rule_weights <- rules@quality$support * rules@quality$confidence
		classifier <- list(
				rules = rules,
				weights = rule_weights,
				class = class,
				levels = lvls,
				default = names(sort(-table(rightHand)))[1],
				description = description,
				method = "weighted"
		)

	} else {
    stop("Method must be one of: 'CBA', 'boosted', 'weighted'.")
  }

  class(classifier) <- "CBA"
  classifier[['columns']] <- as.character(unique(itemInfo(data)$variables))
  classifier[['columnlevels']] <- disc.data
  classifier[['formula']] <- paste(class, " ~ .")

  return(classifier)

}
