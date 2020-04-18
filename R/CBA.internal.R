### CBA internal is only used by bCBA and wCBA now

#CBA <- function(formula, data, support = 0.1, confidence = 0.8, verbose = FALSE,
#  parameter = NULL, control = NULL, sort.parameter = NULL, lhs.support = FALSE,
#  disc.method = "mdlp"){
#
#  return(CBA.internal(formula, data, method="CBA", support = support, confidence = confidence,
#    verbose=verbose, parameter = parameter, control = control,
#    sort.parameter = sort.parameter, lhs.support=lhs.support,
#    disc.method = disc.method))
#
#}


CBA.internal <- function(formula, data, method="boosted", gamma = 0.05, cost = 10.0,
  verbose=FALSE, parameter = NULL, control = NULL, sort.parameter=NULL,
  lhs.support=TRUE, class.weights=NULL, disc.method="mdlp", ...){

  if(method == "boosted"){
    description <- "Transaction boosted associative classifier"
  } else if(method == "weighted"){
    description <- "Weighted CBA algorithm"
  } else {
    description <- "CBA algorithm by Liu, et al. 1998"
  }

  control <- as(control, "APcontrol")
  control@verbose <- verbose

  ### FIXME: maybe minlen = 2 is needed!
  dotParameter <-  list(...)
  if(!is.null(parameter) && length(dotParameter)>0) stop("You cannot specify parameters only either using parameter or ...")
  if(is.null(parameter)) parameter <- dotParameter
  if(is.null(parameter) || is.list(parameter)) {
    if(is.null(parameter$conf)) parameter$confidence <- .5
    if(is.null(parameter$maxlen)) parameter$maxlen <- 5L
    if(is.null(parameter$minlen)) parameter$minlen <- 2L
  }
  parameter <- as(parameter , "APparameter")

  disc_info <- NULL

  ####Preparing data####
  if(is(data, "data.frame")){
    #Re-order data to put the class column on the right side, and discretize
    data <- discretizeDF.supervised(formula, data, method=disc.method)
    disc_info <- lapply(data, attr, "discretized:breaks")
  }

  #Convert to transactions for rule mining
  ds.mat <- as(data, "transactions")
  info <- itemInfo(ds.mat)

  #Build vector of rhe right hahd (target for classification)

  formula <- as.formula(formula)
  vars <- .parseformula(formula, ds.mat)
  class <- vars$class_names
  vars <- vars$var_names

  rightHand <- as(ds.mat[, class], "list")
  if(!all(sapply(rightHand, length) == 1L)) stop("Problem with items used for class. Examples with multiple/no class label!")
  rightHand <- as.factor(unlist(rightHand))

  #Assign is.null to default value of 1s if no class weights specified
  if(is.null(class.weights)) class.weights <- rep(1, length(class))
  else if(length(class.weights) != length(class)) stop("Incorrect number of class weights.")

  #LHS rule mining (currently in need of optimization)
  if(lhs.support){

    parameter@minlen <- 1L
    parameter@target <- "frequent itemsets"
    pot_lhs <- apriori(ds.mat, control = control, parameter = parameter,
      appearance = list(items = vars))

    n <- length(pot_lhs)
    lhs_sup <- quality(pot_lhs)$support

    pot_lhs <- items(pot_lhs)
    pot_lhs <- do.call("c", replicate(length(class), pot_lhs))

    lhs_sup <- rep(lhs_sup, each = length(class))

    ### RHS
    pot_rhs <- encode(as.list(rep(class, each = n)),
      itemLabels = itemLabels(ds.mat))


    ### Assemble rules and add quality
    rules <- new("rules", lhs = pot_lhs, rhs = pot_rhs)
    quality(rules) <- cbind(lhs_support = lhs_sup,
      interestMeasure(rules, measure = c("support", "confidence", "lift"), transactions = ds.mat))

  } else {
    #Generate association rules with apriori
    rules <- apriori(ds.mat, parameter = parameter, control = control,
      appearance = list(rhs=class, lhs=vars))
  }

  ### MFH: Maybe this should be a warning and the classifier just always returns the majority class?
  if(length(rules) < 1) stop("No rules found! Reduce support or confidence.")

  #Original CBA algorithm, sans pessisimistic error-rate pruning
  if(method == "CBA"){

    if(is.null(sort.parameter)){
      #      rules.sorted <- sort(rules, by=c("confidence", "support", "lift"))
      ### MFH: CBA does not sort by lift
      rules.sorted <- sort(rules, by=c("confidence", "support"))
    } else {
      rules.sorted <- sort(rules, by=sort.parameter)
    }

    #Vector used to identify rules as being 'strong' rules for the final classifier
    strongRules <- vector('logical', length=length(rules.sorted))

    rulesMatchLHS <- is.subset(lhs(rules.sorted), ds.mat, sparse = TRUE)
    rulesMatchRHS <- is.subset(rhs(rules.sorted), ds.mat, sparse = TRUE)

    #matrix of rules and records which constitute correct and false matches
    matches <- rulesMatchLHS & rulesMatchRHS
    falseMatches <- rulesMatchLHS & as(!rulesMatchRHS, "lgCMatrix") ### ! makes the matrix dense

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

    .Call("R_stage3", strongRules, casesCovered, covered, defaultClasses, totalErrors, classDistr, replace,matches@i, matches@p, length(matches@i), falseMatches@i, falseMatches@p, length(falseMatches@i), length(class),  PACKAGE = "arulesCBA")

    #save the classifier as only the rules up to the point where we have the lowest total error count
    classifier <- rules.sorted[strongRules][1:which.min(totalErrors[strongRules])]

    #add a default class to the classifier (the default class from the last rule included in the classifier)
    defaultClass <- class[defaultClasses[strongRules][[which.min(totalErrors[strongRules])]]]

    classifier <- list(
      formula = formula,
      discretization = disc_info,
      rules = classifier,
      default = defaultClass,
      method = "first",
      description = description
    )

  } else if(method == "boosted") {

    if(is.null(sort.parameter)){
      rules.sorted <- sort(rules, by=c("lift", "confidence", "support"))
    } else {
      rules.sorted <- sort(rules, by=sort.parameter)
    }

    rules.sorted <- rules.sorted[1:min(length(rules.sorted), 50000)]

    rule_weights <- rep(0, length(rules.sorted))

    defaultClass <- .Call("R_weighted", rule_weights, rules.sorted@lhs@data@i, rules.sorted@lhs@data@p, rules.sorted@rhs@data@i, ds.mat@data@i, ds.mat@data@p, ds.mat@data@Dim, gamma, cost, length(class), class.weights)

    classifier <- list(
      formula = formula,
      discretization = disc_info,
      rules = rules.sorted[rule_weights > 0],
      default = class[defaultClass],
      weights = rule_weights[rule_weights > 0],
      method = "weighted",
      description = description
    )


  } else if(method == "weighted"){

    rule_weights <- quality(rules)[["support"]] * quality(rules)[["confidence"]]
    classifier <- list(
      formula = formula,
      discretization = disc_info,
      rules = rules,
      default = names(which.max(rightHand)),
      weights = rule_weights,
      method = "weighted",
      description = description
    )

  } else {
    stop("Method must be one of: 'CBA', 'boosted', 'weighted'.")
  }

  class(classifier) <- "CBA"
  return(classifier)

}
