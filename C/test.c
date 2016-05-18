#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#define _ind(x,y,cols) (x) + (y) * (cols)

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

int firstMatch(int* matrix, int entry_row, int numRules, int numEntries){
	for(int i = 0; i < numRules; i++)
		if(matrix[_ind(i, entry_row, numRules)] == 1)
			return i;

	return -1;
}

int* getMatches(int* matrix, int entry_row, int numRules){

	int* r = malloc(numRules * sizeof *r);

	for(int i = 0; i < numRules; i++){
		if(matrix[_ind(i, entry_row, numRules)] == 1)
			r[i] = 1;
		else
			r[i] = 0;
	}

	return r;
}

SEXP stage1(SEXP dataset, SEXP strong_rules, SEXP casesCovered, SEXP matches, SEXP falseMatches, SEXP numRules, SEXP classify_column){

	R_len_t i, nrows, ncols, nrules;
	int crule, wrule, classify;
	ncols = length(dataset);
	nrules = INTEGER(numRules)[0];
	nrows = length(getAttrib(dataset, R_RowNamesSymbol));
	classify = INTEGER(classify_column)[0];

	int* matchMatrix = INTEGER(matches);
	int* falseMatchMatrix = INTEGER(falseMatches);

	int* a_vector = malloc(nrows * 3 * sizeof *a_vector);
	int a_size = 0;

	for(i = 0; i < nrows; i++){
		crule = firstMatch(matchMatrix, i, nrules, nrows);
		wrule = firstMatch(falseMatchMatrix, i, nrules, nrows);

		if(crule != -1){
			INTEGER(casesCovered)[crule]++;
		}

    	
    	if(crule > wrule){
    		LOGICAL(strong_rules)[crule] = TRUE;
    	} else if (wrule > crule){
      		a_vector[a_size++] = i;
      		a_vector[a_size++] = crule;
      		a_vector[a_size++] = wrule;
    	}
		
	}

	SEXP a = allocVector(INTSXP, a_size);
	for(i = 0; i < a_size; i++){
		INTEGER(a)[i] = a_vector[i];
	}

	return a;
}

SEXP stage2(SEXP a, SEXP casesCovered, SEXP matches, SEXP strong_rules){

	int a_length = length(a);
	int entry, crule, wrule;

	int numRules = length(strong_rules);

	int* a_arr = INTEGER(a);
	int* strong_rules_arr = LOGICAL(strong_rules);
	int* cases_covered_arr = INTEGER(casesCovered);
	int* matches_matrix = INTEGER(matches);

	int* replace = malloc(3*a_length*numRules * sizeof *replace);
	int replaceSize = 0;

	for(int i = 0; i < a_length; i+=3){

		entry = a_arr[i];
		crule = a_arr[i+1];
		wrule = a_arr[i+2];

		if(strong_rules_arr[wrule]){
			if(crule != -1) cases_covered_arr[crule]--;
			cases_covered_arr[wrule]++;
		} else {

			int* wSet = getMatches(matches_matrix, entry, numRules);

			for(int j = 0; j < numRules; j++){
				if(wSet[j] == 0 || j == crule) continue;

				strong_rules_arr[j] = TRUE;

				replace[replaceSize++] = crule;
      			replace[replaceSize++] = j;
      			replace[replaceSize++] = entry;

			}

		}
	}

	SEXP rep = allocVector(INTSXP, replaceSize);
	for(int i = 0; i < replaceSize; i++){
		INTEGER(rep)[i] = replace[i];
	}

	return rep;

}

SEXP stage3(SEXP strong_rules, SEXP casesCovered){

}

