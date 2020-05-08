#' Convert Transactions to a Data.Frame
#'
#' Convert transactions back into data.frames by combining the
#' items for the same variable into a single column.
#'
#' @param transactions an object of class \code{transactions}.
#' @param itemLabels logical; use the complete item labels (variable=level) as the
#'    levels in the data.frame? By default, only the levels are used.
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
#'
#' iris_df2 <- transactions2DF(iris_trans, itemLabels = TRUE)
#' head(iris_df2)
transactions2DF <- function(transactions, itemLabels = FALSE) {

  variables <- itemInfo(transactions)$variables
  if(!itemLabels) labels <- itemInfo(transactions)$levels
  else labels <- itemInfo(transactions)$labels

  # regular transactions without variables and levels
  if(is.null(variables) || is.null(labels)) return(as.data.frame(as(transactions, "matrix")))

  as.data.frame(sapply(as.character(unique(variables)), FUN = function(v) {
    #cat(v, "\n")
    r <- as(transactions[, variables == v], "ngCMatrix")
    r2 <- colSums(r * seq(nrow(r)))
    r2[r2 < 1 | r2 > nrow(r)] <- NA
    factor(r2, labels = as.character(labels[variables == v]))
    }, simplify = FALSE))
}

