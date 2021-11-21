#' CBA classifiers based on rule-based classifiers in RWeka
#'
#' Provides CBA-type classifiers based on RIPPER (Cohen, 1995), C4.5 (Quinlan,
#' 1993) and PART (Frank and Witten, 1998) using the implementation in Weka via
#' RWeka (Hornik et al, 2009).
#'
#' You need to install package \pkg{RWeka} to use these classifiers.
#'
#' See R/Weka functions \code{\link[RWeka]{JRip}} (RIPPER),
#' \code{\link[RWeka]{J48}} (C4.5 rules) \code{\link[RWeka]{PART}} for
#' algorithm details and how control options can be passed on via
#' \code{control}. An example is given in the Examples Section below.
#'
#' Memory for \pkg{RWeka} can be increased using the R options (e.g.,
#' \code{options(java.parameters = "-Xmx1024m")}) before \pkg{RWeka} or
#' \pkg{rJava} is loaded or any RWeka-based classigier in this package is used.
#'
#' @name RWeka_CBA
#' @param formula A symbolic description of the model to be fitted. Has to be
#' of form \code{class ~ .} or \code{class ~ predictor1 + predictor2}.
#' @param data A data.frame or a transaction set containing the training data.
#' Data frames are automatically discretized and converted to transactions.
#' @param disc.method Discretization method used to discretize continuous
#' variables if data is a data.frame (default: \code{"mdlp"}). See
#' \code{\link{discretizeDF.supervised}} for more supervised discretization
#' methods.
#' @param control algorithmic control options for R/Weka Rule learners (see
#' Details Section).
#' @return Returns an object of class \code{\link{CBA.object}} representing the
#' trained classifier.
#' @author Michael Hahsler
#' @seealso \code{\link[RWeka]{JRip}} (RIPPER), \code{\link[RWeka]{PART}},
#' \code{\link{CBA.object}}.
#' @references
#' W. W. Cohen (1995). Fast effective rule induction. In A.
#' Prieditis and S. Russell (eds.), Proceedings of the 12th International
#' Conference on Machine Learning, pages 115-123. Morgan Kaufmann. ISBN
#' 1-55860-377-8.
#'
#' E. Frank and I. H. Witten (1998). Generating accurate rule sets without
#' global optimization. In J. Shavlik (ed.), Machine Learning: Proceedings of
#' the Fifteenth International Conference. Morgan Kaufmann Publishers: San
#' Francisco, CA.
#'
#' R. Quinlan (1993). C4.5: Programs for Machine Learning. Morgan Kaufmann
#' Publishers, San Mateo, CA.
#'
#' Hornik K, Buchta C, Zeileis A (2009). "Open-Source Machine Learning: R Meets
#' Weka." \emph{Computational Statistics}, 24(2), 225-232.
#' \doi{10.1007/s00180-008-0119-7}
#' @examples
#'
#' # You need to install rJava and RWeka
#'
#' \dontrun{
#' data("iris")
#'
#' # learn a classifier using automatic default discretization
#' classifier <- RIPPER_CBA(Species ~ ., data = iris)
#' classifier
#'
#' # inspect the rule base
#' inspect(rules(classifier))
#'
#' # make predictions for the first few instances of iris
#' predict(classifier, head(iris))
#'
#' table(predict(classifier, iris), iris$Species)
#'
#' # C4.5
#' classifier <- C4.5_CBA(Species ~ ., iris)
#' inspect(rules(classifier))
#'
#' # To use algorithmic options (here for PART), you need to load RWeka
#' library(RWeka)
#'
#' # control options can be found using the Weka Option Wizard (WOW)
#' WOW(PART)
#'
#' # build PART with control option U (Generate unpruned decision list) set to TRUE
#' classifier <- PART_CBA(Species ~ ., data = iris, control = RWeka::Weka_control(U = TRUE))
#' classifier
#' inspect(rules(classifier))
#' predict(classifier, head(iris))
#' }
#'
NULL

.rules_RWeka <- function(formula, data, what = RWeka::JRip, control = NULL,
  disc.method = "mdlp") {

  if(!.installed("RWeka")) stop("Package 'RWeka' needs to be  installed!")

  if(is.null(control)) control <- RWeka::Weka_control()
  formula <- as.formula(formula)

  # convert to transactions
  trans <- prepareTransactions(formula, data, disc.method = disc.method)

  if(any(is.na(itemInfo(trans)$levels)))
    stop("Weka-based classifiers can only be applied to transactions that have levels for all items in itemInfo.")

  # convert it back since Weka likes it this way
  data <- transactions2DF(trans, itemLabels = FALSE)

  # call classifier
  classifier <- what(formula, data = data, control = control)

  # convert rules
  rules <- rJava::.jcall(classifier$classifier, "S", "toString")
  ilabels <- itemLabels(trans)

  if(substr(rules[1], 1, 4) == "JRIP") { ### RIPPER
    rule_sep <- '\\s+=>\\s+'
  }else{ ### PART
    rules <- gsub(' AND\\n', ' ', x = rules, ) # replace AND used in PART
    rule_sep <- ':\\s+'
    #part does not use Value instead of Variable=Value for RHS!
    ilabels <- gsub(paste0(formula[[2]], '='), '', ilabels)
  }
  rules <- gsub('\\s+=\\s+', '=', rules) # deal with difference in logical and factor

  rules <- strsplit(rules, '\\n+')[[1]]
  rules <- rules[-c(1,2, length(rules))]
  rules <- strsplit(rules, rule_sep)

  lhs <- sapply(rules, '[', 1)
  rhs <- sapply(rules, '[', 2)

  n <- length(rules)
  m <- nitems(trans)

  ### FIXME: This could be done sparse (ngTMatrix?)
  mat <- matrix(FALSE, nrow = n, ncol = m, dimnames = list(rows = NULL, cols = ilabels))
  for(i in 1:length(ilabels)) mat[grep(ilabels[i], lhs, fixed = TRUE), i] <- TRUE
  lhs <- as(mat, "itemMatrix")
  itemInfo(lhs) <- itemInfo(trans)

  mat <- matrix(FALSE, nrow = n, ncol = m, dimnames = list(rows = NULL, cols = ilabels))
  for(i in 1:length(ilabels)) mat[grep(ilabels[i], rhs, fixed = TRUE), i] <- TRUE
  rhs <- as(mat, "itemMatrix")
  itemInfo(rhs) <- itemInfo(trans)

  rules <- new("rules", lhs = lhs, rhs = rhs)

  quality(rules) <- interestMeasure(rules, measure = c("support", "confidence"),
    transactions = trans)

  # assemble classifier
  CBA_ruleset(
    formula = formula,
    rules = rules,
    default = uncoveredMajorityClass(formula, trans, rules),
    method = "first",
    discretization = attr(trans, "disc_info"),
    description = paste("RWeka classifier", attr(what, "meta")$name)
  )
}

#' @rdname RWeka_CBA
RIPPER_CBA <- function(formula, data, control = NULL, disc.method = "mdlp")
  .rules_RWeka(formula, data, RWeka::JRip, control, disc.method)

#' @rdname RWeka_CBA
PART_CBA <- function(formula, data, control = NULL, disc.method = "mdlp")
  .rules_RWeka(formula, data, RWeka::PART, control, disc.method)

### C4.5 are rules extracted from a tree
.tree_RWeka <- function(formula, data, what = RWeka::J48, control = NULL,
  disc.method = "mdlp") {

  if(!.installed("RWeka")) stop("Package 'RWeka' needs to be  installed!")

  if(is.null(control)) control <- RWeka::Weka_control()
  formula <- as.formula(formula)

  # convert to transactions
  trans <- prepareTransactions(formula, data, disc.method = disc.method)

  # convert it back since Weka likes it his way
  data <- transactions2DF(trans, itemLabels = TRUE)

  # call classifier
  classifier <- what(formula, data = data, control = control)

  # convert rules
  tree <- rJava::.jcall(classifier$classifier, "S", "toString")

  # isolate rules
  tree <- strsplit(tree, "\n")[[1]]
  space <- which(tree == "")
  tree <- tree[(space[1]+1L):(space[2]-1L)]

  # find RHS
  leafs <- grep(": .*\\)$", tree)
  rhs <- sub(".*: (.*) \\(.*", "\\1", tree[leafs])

  # construct LHS
  lvl <- nchar(gsub("[^\\|]", "", tree))+1L
  lhs_item <- gsub("\\|\\s+", "", tree)
  lhs_item <- sub(": (.*) \\(.*", "", lhs_item)
  lhs_item <- sub("^\\S+ = ", "", lhs_item)
  lhs <- lapply(leafs, FUN = function(l) {
    lvl1 <- lvl[1:l]
    lvl1[lvl1>lvl[l]] <- 1
    lhs_item[c(which(diff(lvl1)>0), l)]
    })

  rhs <- encode(lapply(rhs, c), itemLabels = itemLabels(trans))
  lhs <- encode(lhs, itemLabels = itemLabels(trans))
  itemInfo(rhs) <- itemInfo(trans)
  itemInfo(lhs) <- itemInfo(trans)

  rules <- new("rules", lhs = lhs, rhs = rhs)
  quality(rules) <- interestMeasure(rules, measure = c("support", "confidence"),
    transactions = trans)

  # assemble classifier
  CBA_ruleset(
    formula = formula,
    rules = rules,
    default = uncoveredMajorityClass(formula, trans, rules),
    method = "first",
    discretization = attr(trans, "disc_info"),
    description = paste("RWeka classifier", attr(what, "meta")$name)
  )
}

#' @rdname RWeka_CBA
C4.5_CBA <- function(formula, data, control = NULL, disc.method = "mdlp")
  .tree_RWeka(formula, data, RWeka::J48, control, disc.method)
