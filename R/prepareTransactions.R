
### convert a data.frame into transactions with an attribute "disc_info"
prepareTransactions <- function(formula, data, disc.method = "mdlp") {
  if(is(data, "transactions")) return(data)

  disc_info <- NULL
  data <- discretizeDF.supervised(formula, data, method = disc.method)
  disc_info <- lapply(data, attr, "discretized:breaks")

  trans <- as(data, "transactions")
  attr(trans, "disc_info") <- disc_info
  trans
}
