
### convert a data.frame into transactions with an attribute "disc_info"
prepareTransactions <- function(formula, data, disc.method = "mdlp", match = NULL) {

  ### Note: transactions might need recoding!
  if(is(data, "transactions")) {
    if(!is.null(match)) return(recode(data, match = items(match[1])))
    else return(data)
  }

  # disc.method is a character string with the method
  if(!is.list(disc.method)) {

    disc_info <- NULL
    data <- discretizeDF.supervised(formula, data, method = disc.method)
    disc_info <- lapply(data, attr, "discretized:breaks")

    data <- as(data, "transactions")
    attr(data, "disc_info") <- disc_info

    return(data)
  }

  ### disc is a list with discretization info
    data <- discretizeDF(data, lapply(disc.method,
      FUN = function(x) list(method = "fixed", breaks = x)))
    data <- as(data, "transactions")

  return(data)
}


