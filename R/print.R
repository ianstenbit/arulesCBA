print.CBA <- function(x, ...)
  writeLines(c(
    "CBA Classifier Object",
    paste("Class:", x$class, "(labels:", paste(x$levels, collapse = ", "), ")"),
    paste("Default Class:", x$default),
    paste("Number of rules:", length(x$rules)),
    paste("Classification method:", x$method, if(!is.null(x$weights)) "(weighted)" else ""),
    strwrap(paste("Description:", x$description), exdent = 5),
    ""
  ))

