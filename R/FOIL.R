#' Use FOIL to learn a rule set for classification
#'
#' Build a classifier rule base using FOIL (First Order Inductive Learner), a
#' greedy algorithm that learns rules to distinguish positive from negative
#' examples.
#'
#' Implements FOIL (Quinlan and Cameron-Jones, 1995) to learn rules and then
#' use them as a classifier following Xiaoxin and Han (2003).
#'
#' For each class, we find the positive and negative examples and learn the
#' rules using FOIL. Then the rules for all classes are combined and sorted by
#' Laplace accuracy on the training data.
#'
#' Following Xiaoxin and Han (2003), we classify new examples by \enumerate{
#' \item select all the rules whose bodies are satisfied by the example; \item
#' from the rules select the best k rules per class (highest expected Laplace
#' accuracy); \item average the expected Laplace accuracy per class and choose
#' the class with the highest average. }
#'
#' @aliases FOIL foil
#' @param formula A symbolic description of the model to be fitted. Has to be
#' of form \code{class ~ .} or \code{class ~ predictor1 + predictor2}.
#' @param data A data.frame or a transaction set containing the training data.
#' Data frames are automatically discretized and converted to transactions.
#' @param max_len maximal length of the LHS of the created rules.
#' @param min_gain minimal gain required to expand a rule.
#' @param best_k use the average expected accuracy (laplace) of the best k
#' rules per class for prediction.
#' @param disc.method Discretization method used to discretize continuous
#' variables if data is a data.frame (default: \code{"mdlp"}). See
#' \code{\link{discretizeDF.supervised}} for more supervised discretization
#' methods.
#' @return Returns an object of class \code{\link{CBA.object}} representing the
#' trained classifier.
#' @author Michael Hahsler
#' @seealso \code{\link{CBA.object}}.
#' @references Quinlan, J.R., Cameron-Jones, R.M. Induction of logic programs:
#' FOIL and related systems. NGCO 13, 287-312 (1995).
#' \doi{10.1007/BF03037228}
#'
#' Yin, Xiaoxin and Jiawei Han. CPAR: Classification based on Predictive
#' Association Rules, SDM, 2003.
#' \doi{10.1137/1.9781611972733.40}
#' @examples
#'
#' data("iris")
#'
#' # learn a classifier using automatic default discretization
#' classifier <- FOIL(Species ~ ., data = iris)
#' classifier
#'
#' # inspect the rule base
#' inspect(rules(classifier))
#'
#' # make predictions for the first few instances of iris
#' predict(classifier, head(iris))
#'
FOIL <- function(formula, data, max_len = 3, min_gain = .7, best_k = 5, disc.method = "mdlp"){

  formula <- as.formula(formula)

  trans <- prepareTransactions(formula, data, disc.method = disc.method)

  parsedFormula <- .parseformula(formula, trans)
  class_ids <- parsedFormula$class_ids

  # Do FOIL for each class label and join the resulting rules. (see CPAR)

  # convert transactions to pattern and class matrices
  # (items are columns in a column oriented sparse format)
  trans_mat <- t(as(trans, "ngCMatrix"))
  dimnames(trans_mat) <- list(NULL, NULL)
  m <- ncol(trans_mat) # number of items
  n <- nrow(trans_mat) # number of transactions

  rules <- list()

  # positive and negative examples for class
  for(cid in class_ids) {
    # find transactions for the class
    p <- trans_mat[, cid, drop = FALSE]
    p <- p@i+1L ### this is a hack since Matrix does not support sparse subsetting
                ### and we are guaranteed to have  only one column in p
    pos <- trans_mat[p ,, drop = FALSE]
    #neg <- trans_mat[!p,, drop = FALSE]
    neg <- trans_mat[-p,, drop = FALSE]

    patterns <- matrix(NA, nrow = 0, ncol = m)

    while(nrow(pos) > 0) {

      # new lhs pattern of rule cannot have class labels in it
      pat <- logical(m)
      pat[class_ids] <- NA

      pos2 <- pos
      neg2 <- neg

      while(nrow(neg2) > 0 && sum(pat, na.rm = TRUE) < max_len) {
        # calculate gain for adding an item p to r
        # gain(p) = |P*| (log(|P*|/(|P*|+|N*|))-log(|P|/(|P|+|N|)))
        to_check <- which(!pat)

        # calculate gain for all possible added items
        n_pos <- nrow(pos2)
        n_neg <- nrow(neg2)
        n_pos_covered <- colSums(pos2[,to_check, drop = FALSE])
        n_neg_covered <- colSums(neg2[,to_check, drop = FALSE])
        # could use log2!
        gain <- n_pos_covered * (log(n_pos_covered/(n_pos_covered + n_neg_covered)) - log(n_pos/(n_pos + n_neg)))

        if(all(gain < min_gain, na.rm = TRUE)) break

        take_item <- to_check[which.max(gain)]
        pat[take_item] <- TRUE

        # remove examples not covered by the rule so far
        pos2 <- pos2[pos2[,take_item],, drop = FALSE]
        neg2 <- neg2[neg2[,take_item],, drop = FALSE]
      }


      # no more patterns to find
      if(sum(pat, na.rm = TRUE) < 1) break

      # add rule
      ### FIXME: make rules sparse
      patterns <- rbind(patterns, pat)

      # remove positive examples covered by the rule
      pat[is.na(pat)] <- FALSE
      pos <- pos[!rowSums(pos[,pat, drop = FALSE])==sum(pat),, drop = FALSE]
    }

    # convert rules to rule object
    patterns[is.na(patterns)] <- FALSE
    lhs <- new("itemMatrix", data = t(as(patterns, "ngCMatrix")), itemInfo = itemInfo(trans))

    patterns[] <- FALSE
    patterns[, cid] <- TRUE
    rhs <- new("itemMatrix", data = t(as(patterns, "ngCMatrix")), itemInfo = itemInfo(trans))

    classrules <- new("rules", lhs = lhs, rhs = rhs)

    rules[[length(rules)+1L]] <- classrules
  }

  rules <- do.call(c, rules)

  quality(rules) <- interestMeasure(rules, trans, measure = c("support","confidence","lift", "laplace"),
      k = length(class_ids))

  rules <- sort(rules, by = "laplace")

  # assemble classifier
  CBA_ruleset(
    formula = formula,
    rules = rules,
    default = majorityClass(formula, trans), ### FIXME
    method = "weighted",
    weights = "laplace",
    best_k = best_k,
    discretization = attr(trans, "disc_info"),
    description = paste0("FOIL-based classifier (Yin and Han, 2003)")
  )
}
