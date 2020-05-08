#' Interface to the LUCS-KDD Implementations of CMAR, PRM and CPAR
#'
#' Interface for the LUCS-KDD Software Library Java implementations of CMAR
#' (Li, Han and Pei, 2001), PRM, and CPAR (Yin and Han, 2003). \bold{Note:} The
#' Java implementations is not part of \pkg{arulesCBA} and not covered by the
#' packages license. It will be downloaded and compiled separately. It is
#' available free of charge for \bold{non-commercial use.}
#'
#' \bold{Installation:} The LUCS-KDD code is not part of the package and has to
#' be downloaded, compiled and installed using \code{install_LUCS_KDD_CMAR()}
#' and \code{install_LUCS_KDD_CPAR()}. You need a complete Java JDK
#' installation including the \code{javac} compiler. On some systems (Windows),
#' you may need to set the \code{JAVA_HOME} environment variable so the system
#' finds the compiler.
#'
#' \bold{Memory:} The memory for Java can be increased via R options. For
#' example: \code{options(java.parameters = "-Xmx1024m")}
#'
#' \bold{Note:} The implementation does not expose the min. gain parameter for
#' CPAR, PRM and FOIL2. It is fixed at 0.7 (the value used by Yin and Han,
#' 2001). FOIL2 is an alternative Java implementation to the native
#' implementation of FOIL already provided in the \pkg{arulesCBA}.
#' \code{\link{FOIL}} exposes min. gain.
#'
#' @name LUCS_KDD_CBA
#' @param formula a symbolic description of the model to be fitted. Has to be
#' of form \code{class ~ .} or \code{class ~ predictor1 + predictor2}.
#' @param data A data.frame or a transaction set containing the training data.
#' Data frames are automatically discretized and converted to transactions.
#' @param support,confidence minimum support and minimum confidence thresholds
#' for CMAR (range [0, 1]).
#' @param best_k use average expected accuracy (laplace) of the best k rules
#' per class for prediction.
#' @param disc.method Discretization method used to discretize continuous
#' variables if data is a data.frame (default: \code{"mdlp"}). See
#' \code{\link{discretizeDF.supervised}} for more supervised discretization
#' methods.
#' @param verbose Show verbose output?
#' @param force logical; force redownload, rebuilding and reinstallation?
#' @param source source for the code. A local file can be specified as a URI
#' starting with \code{file://} (see \code{\link[utils]{download.file}}).
#' @return Returns an object of class \code{\link{CBA.object}} representing the
#' trained classifier.
#' @references Li W., Han, J. and Pei, J. CMAR: Accurate and Efficient
#' Classification Based on Multiple Class-Association Rules, ICDM, 2001, pp.
#' 369-376.
#'
#' Yin, Xiaoxin and Jiawei Han. CPAR: Classification based on Predictive
#' Association Rules, SDM, 2003.
#' \doi{10.1137/1.9781611972733.40}
#'
#' Frans Coenen et al. The LUCS-KDD Software Library,
#' \url{https://cgi.csc.liv.ac.uk/~frans/KDD/Software/}
#' @examples
#'
#' \dontrun{
#' data("iris")
#'
#' # install and compile CMAR
#' install_LUCS_KDD_CMAR()
#'
#' # build a classifier, inspect rules and make predictions
#' cl <- CMAR(Species ~ ., iris, support = .2, confidence = .8, verbose = TRUE)
#' cl
#'
#' inspect(rules(cl))
#'
#' predict(cl, head(iris))
#'
#' # install CPAR (also installs PRM and FOIL2)
#' install_LUCS_KDD_CPAR()
#'
#' cl <- CPAR(Species ~ ., iris)
#' cl
#'
#' cl <- PRM(Species ~ ., iris)
#' cl
#'
#' cl <- FOIL2(Species ~ ., iris)
#' cl
#' }
#'
NULL


# Find java and javac from http://stackoverflow.com/a/34031214/470769
.Sys.which2 <- function(cmd) {
  stopifnot(length(cmd) == 1)
  if (.Platform$OS.type == "windows") {
    suppressWarnings({
      pathname <- shell(sprintf("where %s 2> NUL", cmd), intern=TRUE)[1]
    })
    if (!is.na(pathname)) return(dQuote(stats::setNames(pathname, cmd)))
  }

  found <- Sys.which(cmd)

  if(found == "") stop(cmd, " not found! Make sure it is installed correcly.")
  found
}

.java <- function() .Sys.which2("java")
.javac <- function() .Sys.which2("javac")

## Install and compile
.getLUCS_KDD <- function(what, stop = TRUE) {

  dir <- what
  if(what == "CPAR") dir <- "FOIL_PRM_CPAR"
  path <- file.path(system.file(package = "arulesCBA"), "LUCS_KDD", dir)
  attr(path, "exists") <- file.exists(path)

  if(!attr(path, "exists") && stop)
    stop("You need to install ", what, ". See ? ", what," for instructions.")

  path
}

### Write and read LUCS-KDD format files
# LUCS-KDD uses item ids and the highest item ids are the class labels.
.write_trans_LUCS_KDD <- function(formula, trans, file = "data.num") {

  # make sure that the class ids have the highest id (i.e., are the last items)
  parsedFormula <- .parseformula(formula, trans)
  if(!all(parsedFormula$class_ids > nitems(trans)-length(parsedFormula$class_ids)))
    trans <- trans[,c(parsedFormula$var_ids, parsedFormula$class_ids)]

  l <- LIST(trans, decode = FALSE)
  l <- sapply(l, paste, collapse = ' ')
  writeLines(l, con = file)
}

.parse_rules_LUCS_KDD <- function(ret, formula, trans) {
  k <- grep("Num.*classes.*=", ret, fixed = FALSE, value = TRUE)
  k <- as.numeric(sub('.*= (\\d+)', '\\1', k))
  r <- grep("\\}\\s+->\\s+\\{", ret, fixed = FALSE, value = TRUE)
  # we calulate laplace below
  #laplace <- as.numeric(sapply(r, FUN = function(r) gsub('.*\\s(\\S+)%', '\\1', r)))
  r <- strsplit(r, "->")
  r <- lapply(r, FUN = function(r) gsub('.*\\{(.*)\\}.*', '\\1', r))
  lhs <- lapply(r, FUN = function(rs) as.integer(strsplit(rs[1], " ")[[1]]))
  rhs <- lapply(r, FUN = function(rs) as.integer(strsplit(rs[2], " ")[[1]]))

  # fix item order if class items were not the last
  parsedFormula <- .parseformula(formula, trans)
  if(!all(parsedFormula$class_ids > nitems(trans)-length(parsedFormula$class_ids))) {
    itemOrder <- c(parsedFormula$var_ids, parsedFormula$class_ids)
    lhs <- lapply(lhs, FUN = function(i) itemOrder[i])
    rhs <- lapply(rhs, FUN = function(i) itemOrder[i])
  }

  lhs <- encode(lhs, itemLabels = itemLabels(trans))
  rhs <- encode(rhs, itemLabels = itemLabels(trans))
  itemInfo(lhs) <- itemInfo(trans)
  itemInfo(rhs) <- itemInfo(trans)

  rules <- new("rules", lhs = lhs, rhs = rhs)

  #quality(rules) <- data.frame(laplace_FOIL = laplace)
  quality(rules) <- interestMeasure(rules, trans,
    measure = c("support", "confidence", "lift", "laplace"), k = k)

  rules
}

### Run the algorithms
.LUCS_KDD <- function(formula, trans, method = c("FOIL", "PRM", "CPAR", "CMAR"), parameter = "", verbose = FALSE) {
  method <- match.arg(method)

  if(verbose) cat(paste("LUCS-KDD:", method, "\n"))

  if(method == "CMAR") path <- .getLUCS_KDD("CMAR")
  else path <- .getLUCS_KDD("CPAR")

  parsedFormula <- .parseformula(formula, trans)
  classParameter <- paste0("-N", length(parsedFormula$class_ids))

  # write transactions
  filename <- tempfile(fileext = ".num")
  .write_trans_LUCS_KDD(formula, trans, filename)

  exe <- paste(.java(), options()$java.parameters[1], "-cp", path, paste0("run", method),
    classParameter, paste0("-F", filename), parameter)
  if(verbose) cat(paste("Call:", exe, "\n\n"))

  ret <- system(exe, intern = TRUE)
  if(!is.null(attr(ret, "status")) && attr(ret, "status") != 0) stop("Error in call: ", exe, "\n",ret)
  if(verbose) print(ret)

  rules <- .parse_rules_LUCS_KDD(ret, formula, trans)
  if(verbose) cat(paste("\nRules used:", length(rules), "\n"))

  rules
}

### NOTE: MIN_GAIN parameter is not exposed by LUCS-KDD CPAR implementation. It is set to 0.7
### NOTE: We use the most prevalent class if no rules match!
#' @rdname LUCS_KDD_CBA
FOIL2 <- function(formula, data, best_k = 5, disc.method = "mdlp", verbose = FALSE) {
  formula <- as.formula(formula)
  trans <- prepareTransactions(formula, data, disc.method = disc.method)
  parsed_formula <- .parseformula(formula, trans)

  rules <- .LUCS_KDD(formula, trans, method = "FOIL", parameter = "", verbose = verbose)

  CBA_ruleset(
    formula = formula,
    rules = rules,
    default = majorityClass(formula, trans),
    method = "weighted",
    weights = "laplace",
    best_k = best_k,
    discretization = attr(trans, "disc_info"),
    description = "FOIL-based classifier (Yin and Han, 2003 - LUCS-KDD implementation)."
  )
}

#' @rdname LUCS_KDD_CBA
CPAR <- function(formula, data, best_k = 5, disc.method = "mdlp", verbose = FALSE) {
  formula <- as.formula(formula)
  trans <- prepareTransactions(formula, data, disc.method = disc.method)
  parsed_formula <- .parseformula(formula, trans)
  rules <- .LUCS_KDD(formula, trans, method = "CPAR", parameter = "", verbose = verbose)

  structure(list(
    formula = formula,
    class = parsed_formula$class_name,
    rules = rules,
    default = majorityClass(formula, trans),
    discretization = attr(trans, "disc_info"),
    method = "weighted",
    weights = "laplace",
    best_k = best_k,
    description = paste0("CPAR (Yin and Han, 2003 - LUCS-KDD implementation).")
  ),
    class = "CBA"
  )
}

#' @rdname LUCS_KDD_CBA
PRM <- function(formula, data, best_k = 5, disc.method = "mdlp", verbose = FALSE) {
  formula <- as.formula(formula)
  trans <- prepareTransactions(formula, data, disc.method = disc.method)
  parsed_formula <- .parseformula(formula, trans)
  rules <- .LUCS_KDD(formula, trans, method = "PRM", parameter = "", verbose = verbose)

  structure(list(
    formula = formula,
    discretization = attr(trans, "disc_info"),
    rules = rules,
    default = majorityClass(formula, trans),
    method = "weighted",
    weights = "laplace",
    best_k = best_k,
    description = paste0("PRM (Yin and Han, 2003 - LUCS-KDD implementation).")
  ),
    class = "CBA"
  )
}

#' @rdname LUCS_KDD_CBA
CMAR <- function(formula, data, support = 0.1, confidence = 0.5, disc.method = "mdlp", verbose = FALSE) {
  formula <- as.formula(formula)
  trans <- prepareTransactions(formula, data, disc.method = disc.method)
  parsed_formula <- .parseformula(formula, trans)
  rules <- .LUCS_KDD(formula, trans, method = "CMAR",
    parameter = paste0("-S",floor(support*100)," -C", floor(confidence*100)),
    verbose = verbose)

  # add weighted Chi2 to the rules
  quality(rules)$chiSquared <- interestMeasure(rules, "chiSquared", transactions = trans)
  supP <- support(lhs(rules), trans, type = "absolute")
  supC <- support(rhs(rules), trans, type = "absolute")
  n <- length(trans)
  e <- 1/(supP*supC) + 1/(supP*(n-supC)) + 1/((n-supP)*supC) + 1/((n-supP)*(n-supC))
  maxChiSquared <- (pmin(supP, supC) - supP*supC/n)^2 * n * e
  quality(rules)$weightedChiSquared <- quality(rules)$chiSquared^2/maxChiSquared

  structure(list(
    formula = formula,
    discretization = attr(trans, "disc_info"),
    parameter = list(support = support, confidence = confidence),
    rules = rules,
    default = majorityClass(formula, trans),
    weights = "weightedChiSquared",
    method = "weighted",
    description = paste0("CMAR (Li, Han and Pei, 2001 - LUCS-KDD implementation).")
  ),
    class = "CBA"
  )
}


#' @rdname LUCS_KDD_CBA
install_LUCS_KDD_CPAR <- function(force = FALSE, source = "https://cgi.csc.liv.ac.uk/~frans/KDD/Software/FOIL_PRM_CPAR/foilPrmCpar.tgz") {

  path <- .getLUCS_KDD("CPAR", stop = FALSE)
  if(attr(path, "exists") && !force) {
    cat(paste0("LUCS-KDD CPAR is already installed.\nLocation: ", path, "\n"))
    return(invisible())
  }

  directory <- file.path(system.file(package = "arulesCBA"), "LUCS_KDD")
  src <- file.path(system.file("LUCS_KDD_java", package = "arulesCBA"))

  dir.create(directory, showWarnings = FALSE)

  message("You are about to download and compile the LUCS-KDD Software Library implementations of the CPAR algorithms.",
    "\nThis requires a working installation of the Java JDK including java and javac.",
    "\nNote: The algorithms are only free to use for **non-commercial purpose**!",
    "\nFor details see: https://cgi.csc.liv.ac.uk/~frans/KDD/Software/",
    "\n")

  # check for java/javac (stops if not found)
  .java()
  .javac()

  if(!is.null(options()$LUCS_KDD_CPAR_FILE))
    source <- paste0("file://", normalizePath(options()$LUCS_KDD_CPAR_FILE))

  cat("Installing from:", source, "\n")
  utils::download.file(source, destfile = file.path(directory, "foilPrmCpar.tgz"))
  utils::untar(file.path(directory, "foilPrmCpar.tgz"), exdir = file.path(directory))

  cat("to:", path, "\n")
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
}

#' @rdname LUCS_KDD_CBA
install_LUCS_KDD_CMAR <- function(force = FALSE, source = "https://cgi.csc.liv.ac.uk/~frans/KDD/Software/CMAR/cmar.tgz") {

  path <- .getLUCS_KDD("CMAR", stop = FALSE)
  if(attr(path, "exists") && !force) {
    cat(paste0("LUCS-KDD CMAR is already installed.\nLocation: ", path, "\n"))
    return(invisible())
  }

  directory <- file.path(system.file(package = "arulesCBA"), "LUCS_KDD")
  src <- file.path(system.file("LUCS_KDD_java", package = "arulesCBA"))

  dir.create(directory, showWarnings = FALSE)

  message("You are about to download and compile the LUCS-KDD Software Library implementations of the CMAR algorithms.",
    "\nThis requires a working installation of the Java JDK including java and javac.",
    "\nNote: The algorithms are only free to use for **non-commercial purpose**!",
    "\nFor details see: https://cgi.csc.liv.ac.uk/~frans/KDD/Software/",
    "\n")

  # check for java/javac (stops if not found)
  .java()
  .javac()

  if(!is.null(options()$LUCS_KDD_CMAR_FILE))
    source <- paste0("file://", normalizePath(options()$LUCS_KDD_CMAR_FILE))

  cat("Installing from:", source, "\n")
  utils::download.file(source,
    destfile = file.path(directory, "cmar.tgz"))

  cat("to:", path, "\n")
  #utils::untar(file.path(directory, "cmar.tgz"), exdir = file.path(directory))
  utils::untar(file.path(directory, "cmar.tgz"), exdir = file.path(path))

  file.copy(file.path(src, "runCMAR.java"), path)

  cat("Compiling.\n")
  exe <- paste(.javac(), "-cp", path, file.path(path, "runCMAR.java"))
  ret <- system(exe, intern = TRUE)
}

