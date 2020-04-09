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

  # prepare data
  disc_info <- NULL
  if(is(data, "data.frame")){
    data <- discretizeDF.supervised(formula, data, method = disc.method)
    disc_info <- lapply(data, attr, "discretized:breaks")
  }

  # convert to transactions
  trans <- as(data, "transactions")


  # convert it back
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
    class = .parseformula(formula, trans)$class_names,
    default = LIST(rhs(tail(rules, 1)))[[1]], ### last rule is the default rules
    #default = names(max(table(data))), ### FIXME: majority class
    discretization = disc_info,
    formula = formula,
    method = "first",
    description = paste("RWeka classifier", attr(what, "meta")$name)
  ),
    class = "CBA"
  )
}

RIPPER <- function(formula, data, control = NULL, disc.method = "mdlp")
  .rules_RWeka(formula, data, RWeka::JRip, control, disc.method)

PART_CBA <- function(formula, data, control = NULL, disc.method = "mdlp")
  .rules_RWeka(formula, data, RWeka::PART, control, disc.method)
