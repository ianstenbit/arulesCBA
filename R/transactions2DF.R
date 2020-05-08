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
#' iris_trans <- prepareTransactions(Species ~ ., iris)
#' iris_trans
#'
#' # standard conversion
#' iris_df <- transactions2DF(iris_trans)
#' head(iris_df)
#'
#' # use item labels in the data.frame
#' iris_df2 <- transactions2DF(iris_trans, itemLabels = TRUE)
#' head(iris_df2)
#'
#' # Conversion of transactions without variables in itemInfo
#' data("Groceries")
#' head(transactions2DF(Groceries), 2)
#'
#' # Conversion of transactions prepared for classification
#' g2 <- prepareTransactions(`shopping bags` ~ ., Groceries)
#' head(transactions2DF(g2), 2)
transactions2DF <- function(transactions, itemLabels = FALSE) {

  variables <- itemInfo(transactions)$variables
  labels <- levels <- itemInfo(transactions)$levels
  if(itemLabels) labels <- itemInfo(transactions)$labels
  labels[is.na(labels)] <- TRUE


  # regular transactions without variables and levels
  if(is.null(variables) || is.null(labels) || is.null(levels))
    return(as.data.frame(as(transactions, "matrix")))

  vars <- as.character(unique(variables))
  df <- as.data.frame(lapply(vars, FUN = function(v) {
    #cat(v, "\n")
    cols <- variables == v
    r <- as(transactions[, cols], "ngCMatrix")

    if(length(which(cols)) == 1 && is.na(levels[cols])) binary_item <- TRUE
    else binary_item <- FALSE

    if(binary_item) r2 <- rep(0L, nrow(transactions)) # 0L will be FALSE
    else r2 <- rep(NA_integer_, nrow(transactions))

    for(i in 1:nrow(r)) r2[r[i,]] <- i

    factor(r2, labels = if(binary_item) c(TRUE, FALSE) else labels[cols])
    }))
  colnames(df) <- vars # this preserves spaces in item labels
  df
}

