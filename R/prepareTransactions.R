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


#' Convert Transactions to a Data.Frame
#'
#' Convert transactions back into data.frames by combining the
#' items for the same variable into a single column.
#'
#' @param transactions an object of class \code{transactions}.
#' @return Returns a data.frame.
#' @author Michael Hahsler
#' @seealso \code{\link[arules]{transactions}}.
#' @examples
#'
#' data("iris")
#'
#' iris_trans <- prepareTransactions(Species ~ ., iris)
#' iris_trans
#'
#' iris_df <- transactions2DF(iris_trans)
#' head(iris_df)
transactions2DF <- function(transactions) {
  ii <- itemInfo(transactions)
  if(is.null(ii$variables)) stop("Transaction itemInfo does not contain variable information.")

  vars <- unique(ii$variables)

  df <- lapply(as.character(vars), FUN = function(v) {
    #cat(v, "\n")
    t_v <- transactions[,ii$variables == v]
    #factor(apply(as(t_v, "ngCMatrix"), MARGIN = 2, which), labels = (colnames(t_v)))
    r <- as(t_v, "ngCMatrix")
    r2 <- colSums(r * seq(nrow(r)))
    r2[r2 < 1 | r2 >nrow(r)] <- NA
    factor(r2, labels = as.character(ii$levels[ii$variables == v]))
    })

  df <- as.data.frame(df)
  colnames(df) <- vars
  df
}

