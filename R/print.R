print.CBA <- function(x, ...){

  cat("CBA Classifier Object\n")
  cat("Classes:", x$class, "(levels:", x$levels, ")\n")
  cat("Default Class:", x$default, "\n")
  cat("Number of rules:", length(x$rules), "\n")
  cat("Classification method:", x$method)
  if(!is.null(x$weights)) cat(" (weighted)")
  cat("\n")

}
