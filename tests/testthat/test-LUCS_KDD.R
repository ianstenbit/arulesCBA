library("testthat")
library("arulesCBA")

### Test these only if we have them.

context("Test LUCS-KDD classifiers")
data("iris")

# install on my systems since I have local copies
if(!is.null(options()$LUCS_KDD_CMAR_FILE)) install_LUCS_KDD_CMAR()
if(!is.null(options()$LUCS_KDD_CPAR_FILE)) install_LUCS_KDD_CPAR()

classifiers <- c()
#if(attr(arulesCBA:::.getLUCS_KDD("CMAR"), "exists")) classifiers <- append(classifiers, CMAR)
if(file.exists(file.path(system.file(package = "arulesCBA"), "LUCS_KDD", "CMAR")))
  classifiers <- append(classifiers, CMAR)
#if(attr(arulesCBA:::.getLUCS_KDD("CPAR"), "exists")) classifiers <- append(classifiers, c(CPAR, PRM, FOIL2))
if(file.exists(file.path(system.file(package = "arulesCBA"), "LUCS_KDD", "FOIL_PRM_CPAR")))
  classifiers <- append(classifiers, c(CPAR, PRM, FOIL2))


if(length(classifiers) > 0) {

  ### use raw data
  dat <- iris
  f <- Species ~ .
  true <- response(f, dat)

  for(cl in classifiers) {
    res <- cl(f, dat)
    print(res)

    p <- predict(res, dat)
    tbl <- table(p, true)
    accuracy <- sum(diag(tbl))/ sum(tbl)
    cat("Accuracy:", round(accuracy, 3), "\n")
  }

  ### use transactions
  dat <- prepareTransactions(f, iris)
  true <- response(f, dat)

  for(cl in classifiers) {
    res <- cl(Species ~ ., dat)
    print(res)

    p <- predict(res, dat)
    tbl <- table(p, true)
    accuracy <- sum(diag(tbl))/ sum(tbl)
    cat("Accuracy:", round(accuracy, 3), "\n\n")
  }

  data(Groceries)
  dat <- sample(Groceries, 500)
  f <- `bottled beer` ~ .
  true <- response(f, dat)

  for(cl in classifiers) {
    res <- cl(f, dat)
    print(res)

    p <- predict(res, dat)
    tbl <- table(p, true)
    accuracy <- sum(diag(tbl))/ sum(tbl)
    cat("Accuracy:", round(accuracy, 3), "\n\n")
  }
}

