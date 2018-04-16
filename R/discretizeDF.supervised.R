### MFH: This code is a fixed copy from arules. Remove after next release of arules (> 1.6-1)
### prevent discretization if method is missing or NA

discretizeDF <- function(df, methods = NULL, default = NULL) {

  ### methods is a data.frame to get the discretization info from
  if(is.data.frame(methods)) return(.rediscretizeDF(methods, df))

  for(i in colnames(df)) {
    if(!is.numeric(df[[i]])) next
    args <- if(is.null(methods[[i]])) default else methods[[i]]

    ### skip columns with method na
    if(!is.null(args) && (is.null(args$method) || args$method == "none")) next

    df[[i]] <- do.call("discretize", c(list(x = df[[i]]), args))
  }

  df
}

.rediscretizeDF <- function(data, newdata) {

  if(!all(colnames(data) == colnames(newdata))) stop("columns in data and newdata do not conform!")

  cps <- lapply(data, FUN = function(x) {
    breaks <- attr(x, "discretized:breaks")
    if(is.null(breaks)) NULL
    else list(breaks = breaks, method = "fixed", labels = levels(x))
  })

  discretizeDF(newdata, methods = cps, default = list(method = "none"))
}

### end


discretizeDF.supervised <- function(formula, data, method = "mdlp",
  dig.lab = 3, ...) {

  if(!is(data, "data.frame")) stop("data needs to be a data.frame")

  methods = c("mdlp", "caim", "cacc", "ameva", "chi2", "chimerge", "extendedchi2", "modchi2")
  method <- methods[pmatch(tolower(method), methods)]
  if(is.na(method)) stop("Unknown method!")

  vars <- .parseformula(formula, data)
  cl_id <- vars$class_ids
  var_ids <- vars$var_ids

  if(any(!sapply(data[var_ids], is.numeric))) stop("Cannot discretize non-numeric column: ",
    colnames(data)[var_ids[!sapply(data[var_ids], is.numeric)]])

  if(method == "mdlp") {
    cps <- structure(vector("list", ncol(data)), names = colnames(data))
    for(i in var_ids) {
      cps[[i]] <- list(
        breaks = c(-Inf, cutPoints(data[[i]], data[[cl_id]]), Inf),
        method = "fixed")
    }

  } else {
    ### other methods require only numeric columns and
    ### the class to be the last column.
    data_num_id <- var_ids
    data_num <- data[,c(data_num_id, cl_id)]

    res <- switch(method,
      caim         = disc.Topdown(data_num, method = 1),
      cacc         = disc.Topdown(data_num, method = 2),
      ameva        = disc.Topdown(data_num, method = 3),
      chi2         = chi2(data_num, ...),
      chimerge     = chiM(data_num, ...),
      extendedchi2 = extendChi2(data_num, ...),
      modchi2      = modChi2(data_num, ...)
    )

    ### FIXME: no need for +/-Inf for caim, cacc and ameva?
    cps <- structure(vector("list", ncol(data)), names = colnames(data))
    for(i in 1:length(data_num_id)) {
      cps[[data_num_id[i]]] <- list(
        breaks = c(-Inf, res$cutp[[i]], Inf),
        method = "fixed")
    }

  }

  ### TODO: Fix method attribute
  ### TODO: discretizeDF uses default for NULL! How can we prevent discretization?
  discretizeDF(data, methods = cps, default = list(method = "none"))

}
