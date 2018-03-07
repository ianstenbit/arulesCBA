### discretize using a formula interface
discretizeDF.supervized <- function(formula, data, method = "mldp",
  dig.lab = 3, ...) {

  methods = c("mldp", "caim", "ccac", "ameva", "chi2", "chimerge",
    "extendedchi2", "modchi2")

  method <- methods[pmatch(tolower(method), methods)]
  if(is.na(method)) stop("Unknown method!")

  ### FIXME: parse RHS of formula and only discretize the mentioned columns
  formula <- as.formula(formula)
  class <- as.character(formula[[2]])
  if(as.character(formula[[3]]) != ".")
    stop("Formula needs to be of the form class ~ .")
  cl_id <- pmatch(class, colnames(data))

  print(class)
  print(colnames(data))
  if(is.na(cl_id)) stop("Cannot identify column specified as class in the formula.")

  if(!is.factor(data[[cl_id]])) stop("class variable needs to be a factor!")

  if(method == "mldp") {
    for(i in 1:ncol(data)) {
      if(!is.numeric(data[[i]])) next

      cp <- c(-Inf, cutPoints(data[[i]], data[[cl_id]]), Inf)
      data[[i]] <- structure(
        cut(data[[i]], breaks = cp,
          include.lowest = TRUE, dig.lab = dig.lab),
        "discretized:breaks" = cp
      )
    }

  } else {
    ### other methods require only numeric columns and
    ### the class to be the last column.
    data_num_id <- which(sapply(data, is.numeric))
    data_num <- data[,c(data_num_id, cl_id)]

    res <- switch(method,
      caim         = disc.Topdown(data_num, method = 1),
      ccac         = disc.Topdown(data_num, method = 2),
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

    data <- discretizeDF(data, methods = cps)
  }

  data
}
