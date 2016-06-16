print.CBA <- function(x, ...){

  cat("CBA Object\n")
  cat("Number of rules:", length(x$rules), "\n")
  cat("Class labels:", x$levels, "\n")
  cat("Default class:", x$default, "\n")

}
