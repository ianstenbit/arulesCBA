#include <R.h>
#include <Rdefines.h>
SEXP test_func(SEXP input) {
  SEXP p1;
  PROTECT(p1 = NEW_INTEGER(1));
  INTEGER(p1)[0] = INTEGER(input)[0] + 1;
  UNPROTECT(1);
  return p1;
}

SEXP test(SEXP ds, SEXP rightHand, SEXP rules, SEXP numRows, SEXP numRules){
	printf("Test Function\n");
	int nRows, nRules, nCols = 0;

	nRows = INTEGER(numRows)[0];
	nCols = Rf_length(ds);
	nRules = INTEGER(numRules)[0];
	printf("Rows: %u, Columns: %u, Rules: %u\n", nRows, nCols, nRules);

	return ds;
}
