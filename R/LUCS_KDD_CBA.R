# LUCS-KDD uses item ids and the highest item ids are the class labels.
.write_trans_LUCS_KDD <- function(formula, trans, file = "data.num") {

  # FIXME: make sure that the class is the last attribute!!! For now er just check!
  parsedFormula <- .parseformula(formula, trans)
  if(!all(parsedFormula$class_ids > nitems(trans)-length(parsedFormula$class_ids)))
    stop("Class items need to be the last items. Recoding not implemented yet!")

  l <- LIST(trans, decode = FALSE)
  l <- sapply(l, paste, collapse = ' ')
  writeLines(l, con = file)
}

.parse_rules_LUCS_KDD <- function(ret, trans) {
  k <- grep("Num.*classes.*=", ret, fixed = FALSE, value = TRUE)
  k <- as.numeric(sub('.*= (\\d+)', '\\1', k))
  r <- grep("\\}\\s+->\\s+\\{", ret, fixed = FALSE, value = TRUE)
  #laplace <- as.numeric(sapply(r, FUN = function(r) gsub('.*\\s(\\S+)%', '\\1', r)))
  r <- strsplit(r, "->")
  r <- lapply(r, FUN = function(r) gsub('.*\\{(.*)\\}.*', '\\1', r))

  lhs <- lapply(r, FUN = function(rs) as.integer(strsplit(rs[1], " ")[[1]]))
  lhs <- encode(lhs, itemLabels = itemLabels(trans))

  rhs <- lapply(r, FUN = function(rs) as.integer(strsplit(rs[2], " ")[[1]]))
  rhs <- encode(rhs, itemLabels = itemLabels(trans))

  rules <- new("rules", lhs = lhs, rhs = rhs)
  #quality(rules) <- data.frame(laplace_FOIL = laplace)
  quality(rules) <- interestMeasure(rules, trans,
    measure = c("support", "confidence", "lift", "laplace"), k = k)
  rules
}

# from http://stackoverflow.com/a/34031214/470769
.Sys.which2 <- function(cmd) {
  stopifnot(length(cmd) == 1)
  if (.Platform$OS.type == "windows") {
    suppressWarnings({
      pathname <- shell(sprintf("where %s 2> NUL", cmd), intern=TRUE)[1]
    })
    if (!is.na(pathname)) return(dQuote(stats::setNames(pathname, cmd)))
  }

  found <- Sys.which(cmd)

  if(found == "") stop(cmd, " not found! Make sure it is install correcly.")
  found
}

.java <- function() .Sys.which2("java")
.javac <- function() .Sys.which2("javac")

.install_LUCS_KDD_FPC <- function() {
  directory <- file.path(system.file(package = "arulesCBA"), "LUCS_KDD")
  src <- file.path(system.file("LUCS_KDD_java", package = "arulesCBA"))
  path <- file.path(directory, "FOIL_PRM_CPAR")

  if(file.exists(path)) return(path)

  dir.create(directory)

  message("You are about to download and compile the LUCS-KDD Software Library implementations of the FOIL, PRM and CPAR algorithms.",
    "\nThis requires a working installation of the Java JDK including java and javac.",
    "\nNote: The algorithms are only free to use for **non-commercial purpose**!",
    "\nFor details see: https://cgi.csc.liv.ac.uk/~frans/KDD/Software/",
    "\n")

  # check for java/javac (stops if not found)
  .java()
  .javac()

  if(!is.null(options("LUCS_KDD_FPC_FILE")))
    FPCfile <- paste0("file://", normalizePath(options()$LUCS_KDD_FPC_FILE))
  else FPCfile <- "https://cgi.csc.liv.ac.uk/~frans/KDD/Software/FOIL_PRM_CPAR/foilPrmCpar.tgz"


  cat("Installing from:", FPCfile, "\n")
  utils::download.file(FPCfile,
    destfile = file.path(directory, "foilPrmCpar.tgz"))

  utils::untar(file.path(directory, "foilPrmCpar.tgz"), exdir = file.path(directory))

  file.copy(file.path(src, "runCPAR.java"), path)
  file.copy(file.path(src, "runFOIL.java"), path)
  file.copy(file.path(src, "runPRM.java"), path)

  cat("Compiling.\n")
  exe <- paste(.javac(), "-cp", path, file.path(path, "runCPAR.java"))
  ret <- system(exe, intern = TRUE)
  exe <- paste(.javac(), "-cp", path, file.path(path, "runFOIL.java"))
  ret <- system(exe, intern = TRUE)
  exe <- paste(.javac(), "-cp", path, file.path(path, "runPRM.java"))
  ret <- system(exe, intern = TRUE)

  return(path)
}

### FIXME: add CMAR
.LUCS_KDD <- function(formula, trans, method = c("FOIL", "PRM", "CPAR"), parameter = "") {

  path <- .install_LUCS_KDD_FPC()

  parsedFormula <- .parseformula(formula, trans)
  classParameter <- paste0("-N", length(parsedFormula$class_ids))

  # write transactions
  filename <- tempfile(fileext = ".num")
  .write_trans_LUCS_KDD(formula, trans, filename)

  # execute java
  #if(method == "CMAR") path <- normalizePath("/home/hahsler/baR/arulesCBA/Work/LUCS-KDD/CMAR/")
  #else path <- normalizePath("/home/hahsler/baR/arulesCBA/Work/LUCS-KDD/FOIL_PRM_CPAR/")

  exe <- paste(.java(), "-cp", path, paste0("run", method),
    classParameter, paste0("-F", filename), parameter)

  ret <- system(exe, intern = TRUE)
  rules <- .parse_rules_LUCS_KDD(ret, trans)

  rules
}

### Note: We use the most prevalent class if no rules match!

FOIL2 <- function(formula, data, parameter = "", best_k = 5, disc.method = "mdlp") {
  formula <- as.formula(formula)
  trans <- prepareTransactions(formula, data, disc.method = disc.method)
  parsed_formula <- .parseformula(formula, trans)

  rules <- .LUCS_KDD(formula, trans, method = "FOIL", parameter = parameter)

  structure(list(
    rules = rules,
    class = parsed_formula$class_name,
    default = parsed_formula$class_name[which.max(itemFrequency(trans)[parsed_formula$class_ids])],
    discretization = attr(trans, "disc_info"),
    formula = formula,
    method = "weighted",
    weights = "laplace",
    best_k = best_k,
    description = paste0("FOIL-based classifier (Yin and Han, 2003 - LUCS-KDD implementation).")
  ),
    class = "CBA"
  )
}

CPAR <- function(formula, data, parameter = "", best_k = 5, disc.method = "mdlp") {
  formula <- as.formula(formula)
  trans <- prepareTransactions(formula, data, disc.method = disc.method)
  parsed_formula <- .parseformula(formula, trans)
  rules <- .LUCS_KDD(formula, trans, method = "CPAR", parameter = parameter)

  structure(list(
    rules = rules,
    class = parsed_formula$class_name,
    default = parsed_formula$class_name[which.max(itemFrequency(trans)[parsed_formula$class_ids])],
    discretization = attr(trans, "disc_info"),
    formula = formula,
    method = "weighted",
    weights = "laplace",
    best_k = best_k,
    description = paste0("CPAR (Yin and Han, 2003 - LUCS-KDD implementation).")
  ),
    class = "CBA"
  )
}

PRM <- function(formula, data, parameter = "", best_k = 5, disc.method = "mdlp") {
  formula <- as.formula(formula)
  trans <- prepareTransactions(formula, data, disc.method = disc.method)
  parsed_formula <- .parseformula(formula, trans)
  rules <- .LUCS_KDD(formula, trans, method = "PRM", parameter = parameter)

  structure(list(
    rules = rules,
    class = parsed_formula$class_name,
    default = parsed_formula$class_name[which.max(itemFrequency(trans)[parsed_formula$class_ids])],
    discretization = attr(trans, "disc_info"),
    formula = formula,
    method = "weighted",
    weights = "laplace",
    best_k = best_k,
    description = paste0("PRM (Yin and Han, 2003 - LUCS-KDD implementation).")
  ),
    class = "CBA"
  )
}
