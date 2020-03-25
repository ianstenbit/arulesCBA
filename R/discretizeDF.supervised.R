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
