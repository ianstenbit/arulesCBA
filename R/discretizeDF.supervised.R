#' Supervised Methods to Convert Continuous Variables into Categorical
#' Variables
#' 
#' This function implements several supervised methods to convert continuous
#' variables into a categorical variables (factor) suitable for association
#' rule mining and building associative classifiers. A whole data.frame is
#' discretized (i.e., all numeric columns are discretized).
#' 
#' \code{discretizeDF.supervised} only implements supervised discretization.
#' See \code{discretizeDF} in package \pkg{arules} for unsupervised
#' discretization.
#' 
#' @aliases discretizeDF.supervised discretizeDF discretize
#' @param formula a formula object to specify the class variable for supervised
#' discretization and the predictors to be discretized in the form \code{class
#' ~ .} or \code{class ~ predictor1 + predictor2}.
#' @param data a data.frame containing continuous variables to be discretized
#' @param method discretization method. Available are: \code{"mdlp"},
#' \code{"caim"}, \code{"cacc"}, \code{"ameva"}, \code{"chi2"},
#' \code{"chimerge"}, \code{"extendedchi2"}, and \code{"modchi2"}.
#' @param dig.lab integer; number of digits used to create labels.
#' @param \dots Additional parameters are passed on to the implementation of
#' the chosen discretization method.
#' @return \code{discretizeDF} returns a discretized data.frame. Discretized
#' columns have an attribute \code{"discretized:breaks"} indicating the used
#' breaks or and \code{"discretized:method"} giving the used method.
#' @author Michael Hahsler
#' @seealso Unsupervised discretization from \pkg{arules}:
#' \code{\link[arules]{discretize}}, \code{\link[arules]{discretizeDF}}.
#' 
#' Details about the available supervised discretization methods from
#' \pkg{discretization}: \code{\link[discretization]{mdlp}},
#' \code{\link[discretization]{caim}}, \code{\link[discretization]{cacc}},
#' \code{\link[discretization]{ameva}}, \code{\link[discretization]{chi2}},
#' \code{\link[discretization]{chiM}},
#' \code{\link[discretization]{extendChi2}},
#' \code{\link[discretization]{modChi2}}.
#' @keywords manip
#' @examples
#' 
#' data("iris")
#' summary(iris)
#' 
#' # supervised discretization using Species
#' iris.disc <- discretizeDF.supervised(Species ~ ., iris)
#' summary(iris.disc)
#' 
#' attributes(iris.disc$Sepal.Length)
#' 
#' # discretize the first few instances of iris using the same breaks as iris.disc
#' discretizeDF(head(iris), methods = iris.disc)
#' 
#' # only discretize predictors Sepal.Length and Petal.Length
#' iris.disc2 <- discretizeDF.supervised(Species ~ Sepal.Length + Petal.Length, iris)
#' head(iris.disc2)
#' 
discretizeDF.supervised <- function(formula, data, method = "mdlp",
  dig.lab = 3, ...) {

  if(!is(data, "data.frame")) stop("data needs to be a data.frame")

  methods = c("mdlp", "caim", "cacc", "ameva", "chi2", "chimerge", "extendedchi2", "modchi2")
  method <- methods[pmatch(tolower(method), methods)]
  if(is.na(method)) stop(paste("Unknown method! Available methods are", paste(methods, collapse = ", ")))

  vars <- .parseformula(formula, data)
  cl_id <- vars$class_ids
  var_ids <- vars$var_ids

  if(any(!sapply(data[var_ids], is.numeric))) stop("Cannot discretize non-numeric column: ",
    colnames(data)[var_ids[!sapply(data[var_ids], is.numeric)]])

  if(method == "mdlp") {
    cps <- structure(vector("list", ncol(data)), names = colnames(data))
    for(i in var_ids) {

      # cutPoints does not handle missing values!
      missing <- is.na(data[[i]])
      cPts <- try(discretization::cutPoints(data[[i]][!missing], data[[cl_id]][!missing]), silent = TRUE)
      if(is(cPts, "try-error")) stop("Problem with discretizing column ", i,
        " (maybe not enough non-missing values?)")

      cps[[i]] <- list(
        breaks = c(-Inf, cPts, Inf),
        method = "fixed")
    }

  } else {
    ### other methods require only numeric columns and
    ### the class to be the last column.
    data_num_id <- var_ids
    data_num <- data[,c(data_num_id, cl_id)]

    res <- switch(method,
      caim         = discretization::disc.Topdown(data_num, method = 1),
      cacc         = discretization::disc.Topdown(data_num, method = 2),
      ameva        = discretization::disc.Topdown(data_num, method = 3),
      chi2         = discretization::chi2(data_num, ...),
      chimerge     = discretization::chiM(data_num, ...),
      extendedchi2 = discretization::extendChi2(data_num, ...),
      modchi2      = discretization::modChi2(data_num, ...)
    )

    ### FIXME: no need for +/-Inf for caim, cacc and ameva?
    cps <- structure(vector("list", ncol(data)), names = colnames(data))
    for(i in 1:length(data_num_id)) {
      cps[[data_num_id[i]]] <- list(
        breaks = c(-Inf, res$cutp[[i]], Inf),
        method = "fixed")
    }

  }

  data <- discretizeDF(data, methods = cps, default = list(method = "none"))

  ### fix method attribute
  for(i in var_ids) attr(data[[i]], "discretized:method") <- method

  data
}
