#include <R.h>
#include <Rdefines.h>
SEXP test_func(SEXP input) {
  SEXP p1;
  PROTECT(p1 = NEW_INTEGER(1));
  INTEGER(p1)[0] = INTEGER(input)[0] + 1;
  UNPROTECT(1);
  return p1;
}