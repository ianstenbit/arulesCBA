#' Convert Data as a  Data.Frames to Transactions
#'
#' Convert data in a data.frame into transactions by applying class-based
#' discretization.
#'
#' @param formula the formula.
#' @param data a data.frame with the data.
#' @param disc.method Discretization method used to discretize continuous
#' variables if data is a data.frame (default: \code{"mdlp"}). See
#' \code{\link{discretizeDF.supervised}} for more supervised discretization
#' methods.
#' @param match typically \code{NULL}. Only used internally if data is a
#' already a set of transactions.
#' @return An object of class \code{\link[arules]{transactions}} from
#' \pkg{arules} with an attribute called \code{"disc_info"} that contains
#' information on the used discretization for each column.
#' @author Michael Hahsler
#' @seealso \code{\link[arules]{transactions}}, \code{\link{transactions2DF}}.
#' @examples
#'
#' data("iris")
#'
#' iris_trans <- prepareTransactions(Species ~ ., iris)
#' iris_trans
#'
prepareTransactions <- function(formula, data, disc.method = "mdlp", match = NULL) {


  if(is(data, "transactions")) {
    ### add negative items to handle regular transaction data without variable info
    if(is.null(itemInfo(data)$variables))
      data <- addComplement(data, all.vars(formula)[1])

    ### Note: transactions might need recoding!
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
