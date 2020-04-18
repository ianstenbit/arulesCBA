# FOIL algorithm for ARs following Yin & Han (CPAR)

FOIL <- function(formula, data, max_len = 3, min_gain = .7, best_k = 5, disc.method = "mdlp"){

  formula <- as.formula(formula)

  trans <- prepareTransactions(formula, data, disc.method = disc.method)

  parsedFormula <- .parseformula(formula, trans)
  class <- parsedFormula$class_names
  class_ids <- parsedFormula$class_ids
  vars <- parsedFormula$var_names

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
  structure(list(
    rules = rules,
    default = majorityClass(formula, trans), ### FIXME
    discretization = attr(trans, "disc_info"),
    formula = formula,
    method = "weighted",
    weights = "laplace",
    best_k = best_k,
    description = paste0("FOIL-based classifier (Yin and Han, 2003)")
  ),
    class = "CBA"
  )

}
