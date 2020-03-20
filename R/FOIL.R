# FOIL algorithm for ARs following Yin & Han (CPAR)

FOIL <- function(formula, data, max_len = 4, min_gain = .7, disc.method = "mdlp"){

  formula <- as.formula(formula)

  # prepare data
  disc_info <- NULL
  if(is(data, "data.frame")){
    data <- discretizeDF.supervised(formula, data, method = disc.method)
    disc_info <- lapply(data, attr, "discretized:breaks")
  }

  # convert to transactions for rule mining
  trans <- as(data, "transactions")

  parsedFormula <- .parseformula(formula, trans)
  class <- parsedFormula$class_names
  class_ids <- parsedFormula$class_ids
  vars <- parsedFormula$var_names


  # Do FOIL for each class label and join the resulting rules. (see CPAR)

  # convert transactions to pattern and class matrices
  # (items are columns in a column oriented sparse format)
  trans_mat <- t(as(trans, "ngCMatrix"))
  m <- ncol(trans_mat) # number of items

  rules <- list()

  # positive and negative examples for class i
  for(cid in class_ids) {
    p <- trans_mat[, cid]
    pos <- trans_mat[p ,, drop = FALSE]
    neg <- trans_mat[!p,, drop = FALSE]

    patterns <- matrix(NA, nrow = 0, ncol = m)

    while(nrow(pos) > 0) {

      # new lhs pattern of rule cannot have class labels in it
      pat <- logical(m)
      pat[class_ids] <- NA

      pos2 <- pos
      neg2 <- neg

      while(sum(pat, na.rm = TRUE) < max_len) {
        # calculate gain for adding an item p to r
        # gain(p) = |P*| (log(|P*|/(|P*|+|N*|))-log(|P|/(|P|+|N|)))
        to_check <- which(!pat)

        # calculate gain for all possible added items
        n_pos <- nrow(pos2)
        n_neg <- nrow(neg2)
        n_pos_covered <- colSums(pos2[,to_check, drop = FALSE])
        n_neg_covered <- colSums(neg2[,to_check, drop = FALSE])
        gain <- n_pos_covered * (log2(n_pos_covered/(n_pos_covered + n_neg_covered)) - log2(n_pos/(n_pos + n_neg)))

        if(all(gain < min_gain, na.rm = TRUE)) break
        take_item <- to_check[which.max(gain)]
        pat[take_item] <- TRUE

        # remove examples not covered by the rule so far
        pos2 <- pos2[pos2[,take_item],, drop = FALSE]
        neg2 <- neg2[neg2[,take_item],, drop = FALSE]
      }


      # no more patterns to find
      if(sum(pat, na.rm = TRUE) < 1) break

      # add rule and remove positive examples covered by the rule
      ### FIXME: make rules sparse
      patterns <- rbind(patterns, pat)
      pat[is.na(pat)] <- FALSE
      pos <- pos[!rowSums(pos[,pat, drop = FALSE])==sum(pat),, drop = FALSE]
    }

    # convert rules to rule object
    patterns[is.na(patterns)] <- FALSE
    lhs <- new("itemMatrix", data = t(as(patterns, "ngCMatrix")), itemInfo = itemInfo(trans))

    patterns[] <- FALSE
    patterns[, cid] <- TRUE
    rhs <- new("itemMatrix", data = t(as(patterns, "ngCMatrix")), itemInfo = itemInfo(trans))

    rules[[length(rules)+1L]] <- new("rules", lhs = lhs, rhs = rhs)
  }

  rules <- do.call(c, rules)
  quality(rules) <- interestMeasure(rules, measure = c("support", "confidence", "laplace"), transactions = trans)

  rules <- sort(rules, by = "laplace")

  # assemble classifier
  structure(list(
    rules = rules,
    class = .parseformula(formula, trans)$class_name,
    default = class[which.max(itemFrequency(trans)[class_ids])], ### FIXME
    discretization = disc_info,
    formula = formula,
    method = "first",
    description = paste0("FOIL-based classifier (sorted by Laplace Accuracy)")
  ),
    class = "CBA"
  )

}
