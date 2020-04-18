# Interface to rWeka

# M5Rules is only for binary classifier
# OneR does not make too much sense with pre-discretized data

#reassamble a data.frame from transactions
# This combines variables and renames variables and values to numbers
.trans2DF <- function(trans) {
  ii <- itemInfo(trans)
  if(is.null(ii$variables)) stop("Transaction itemInfo does not contain variable information.")

  vars <- unique(ii$variables)

  df <- lapply(as.character(vars), FUN = function(v) {
    #cat(v, "\n")
    t_v <- trans[,ii$variables == v]
    #factor(apply(as(t_v, "ngCMatrix"), MARGIN = 2, which), labels = (colnames(t_v)))
    r <- as(t_v, "ngCMatrix")
    r2 <- colSums(r * seq(nrow(r)))
    r2[r2 < 1 | r2 >nrow(r)] <- NA
    factor(r2, labels = (colnames(t_v)))
    })

  df <- as.data.frame(df)
  colnames(df) <- vars
  df
}



.rules_RWeka <- function(formula, data, what = RWeka::JRip, control = NULL,
  disc.method = "mdlp") {

  if(!.installed("RWeka")) stop("Package 'RWeka' needs to be  installed!")

  if(is.null(control)) control <- RWeka::Weka_control()
  formula <- as.formula(formula)

  # convert to transactions
  trans <- prepareTransactions(formula, data, disc.method = disc.method)

  # convert it back since weka likes it his way
  data <- .trans2DF(trans)

  # call classifier
  classifier <- what(formula, data = data, control = control)

  # convert rules
  rules <- rJava::.jcall(classifier$classifier, "S", "toString")
  if(substr(rules[1], 1, 4) == "JRIP") { ### RIPPER
    rule_sep <- '\\s+=>\\s+'
  }else{ ### PART
    rules <- gsub(' AND\\n', ' ', x = rules, ) # replace AND used in PART
    rule_sep <- ':\\s+'
  }

  rules <- strsplit(rules, '\\n+')[[1]]
  rules <- rules[-c(1,2, length(rules))]
  rules <- strsplit(rules, rule_sep)

  lhs <- sapply(rules, '[', 1)
  rhs <- sapply(rules, '[', 2)

  ilabels <- itemLabels(trans)
  n <- length(rules)
  m <- nitems(trans)

  ### FIXME: This could be done sparse (ngTMatrix?)
  mat <- matrix(FALSE, nrow = n, ncol = m, dimnames = list(rows = NULL, cols = ilabels))
  for(i in 1:length(ilabels)) mat[grep(ilabels[i], lhs, fixed = TRUE), i] <- TRUE
  lhs <- as(mat, "itemMatrix")

  mat <- matrix(FALSE, nrow = n, ncol = m, dimnames = list(rows = NULL, cols = ilabels))
  for(i in 1:length(ilabels)) mat[grep(ilabels[i], rhs, fixed = TRUE), i] <- TRUE
  rhs <- as(mat, "itemMatrix")

  rules <- new("rules", lhs = lhs, rhs = rhs)
  quality(rules) <- interestMeasure(rules, measure = c("support", "confidence"),
    transactions = trans)

  # assemble classifier
  structure(list(
    rules = rules,
    default = NA,
    discretization = attr(trans, "disc_info"),
    formula = formula,
    method = "first",
    description = paste("RWeka classifier", attr(what, "meta")$name)
  ),
    class = "CBA"
  )
}

RIPPER_CBA <- function(formula, data, control = NULL, disc.method = "mdlp")
  .rules_RWeka(formula, data, RWeka::JRip, control, disc.method)

PART_CBA <- function(formula, data, control = NULL, disc.method = "mdlp")
  .rules_RWeka(formula, data, RWeka::PART, control, disc.method)

### C4.5 are rules extracte from a tree -> does not need a defaut class and first is the only match
.tree_RWeka <- function(formula, data, what = RWeka::J48, control = NULL,
  disc.method = "mdlp") {

  if(!.installed("RWeka")) stop("Package 'RWeka' needs to be  installed!")

  if(is.null(control)) control <- RWeka::Weka_control()
  formula <- as.formula(formula)

  # convert to transactions
  trans <- prepareTransactions(formula, data, disc.method = disc.method)

  # convert it back since weka likes it his way
  data <- .trans2DF(trans)

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

  rules <- new("rules", lhs = lhs, rhs = rhs)
  quality(rules) <- interestMeasure(rules, measure = c("support", "confidence"),
    transactions = trans)

  # assemble classifier
  structure(list(
    rules = rules,
    default = NA,
    discretization = attr(trans, "disc_info"),
    formula = formula,
    method = "first",
    description = paste("RWeka classifier", attr(what, "meta")$name)
  ),
    class = "CBA"
  )
}

C4.5_CBA <- function(formula, data, control = NULL, disc.method = "mdlp")
  .tree_RWeka(formula, data, RWeka::J48, control, disc.method)
