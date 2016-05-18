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

int countRecordMatches(int* matrix, int rule_column, int numRules, int numRecords){

	int count = 0;

	for(int i = 0; i < numRecords; i++){
		if(matrix[_ind(rule_column, i, numRules)] == 1)
			count++;
	}

	return count;

}

int* getRecordMatches(int* matrix, int rule_column, int numRules, int numRecords){

	int* r = malloc(numRecords * sizeof *r);

	for(int i = 0; i < numRecords; i++){
		if(matrix[_ind(rule_column, i, numRules)] == 1)
			r[i] = 1;
		else
			r[i] = 0;
	}

	return r;

}

int* getReplacements(int* replace, int rule, int numRules, int rLen){
	int* repl = malloc((numRules-1) * 2 * sizeof *repl);
	int repl_size = 0;

	for(int i = 0; i < numRules - 1; i++) repl[i] = -1;

	for(int i = 0; i < rLen; i+=3){
		if(replace[i] == rule){
			repl[repl_size++] = replace[i+1];
			repl[repl_size++] = replace[i+2];
		}
	}

	return repl;
}

int getMajorityClass(int* classes, int* covered, int classLevels, int numEntries){

	int* counts = malloc(classLevels * sizeof *counts);
	memset(counts, 0, sizeof(int)*classLevels);

	for(int i = 0; i < numEntries; i++)
		if(!covered[i])
			counts[classes[i]-1]++;

	int max_index = 0;

	for(int i = 0; i < classLevels; i++){
		if(counts[i] > counts[max_index]){
			max_index = i;
		}
	}

	return max_index+1;
}

int getDefaultErrors(int* classes, int* covered, int numEntries, int defaultClass){
	int count = 0;

	for(int i = 0; i < numEntries; i++)
		if(!covered[i] && classes[i] != defaultClass)
			count++;

	return count;
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

	free(a_vector);

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

			free(wSet);

		}
	}

	SEXP rep = allocVector(INTSXP, replaceSize);
	for(int i = 0; i < replaceSize; i++){
		INTEGER(rep)[i] = replace[i];
	}

	free(replace);

	return rep;

}

SEXP stage3(SEXP strong_rules, SEXP casesCovered, SEXP covered, SEXP defaultClasses, SEXP totalErrors, SEXP classDistr, SEXP replace, SEXP matches, SEXP falseMatches, SEXP classLevels){

	int nRows = length(covered);
	int numRules = length(strong_rules);
	int replace_len = length(replace);
	int numClasses = INTEGER(classLevels)[0];

	int* strong_rules_arr = LOGICAL(strong_rules);
	int* cases_covered_arr = INTEGER(casesCovered);
	int* replace_arr = INTEGER(replace);
	int* covered_arr = LOGICAL(covered);
	int* matches_matrix = INTEGER(matches);
	int* classes = INTEGER(classDistr);
	int* defaultClasses_arr = INTEGER(defaultClasses);
	int* false_matches_matrix = INTEGER(falseMatches);
	int* total_errors_arr = INTEGER(totalErrors);

	int* replace_list = 0;
	int* rule_covered = 0;

	int ruleErrors = 0;
	int defaultErrors = 0;

	for(int i = 0; i < numRules; i++){

		if(strong_rules_arr[i] == 0) continue;

		if(cases_covered_arr[i] == 0){
			strong_rules_arr[i] = FALSE;
			continue;
		}

		replace_list = getReplacements(replace_arr, i, numRules, replace_len);
			
		int repl_index = 0;

		while(replace_list[repl_index] != -1){

			if(covered_arr[replace_list[repl_index+1]])
				cases_covered_arr[i]--;
			else
				cases_covered_arr[replace_list[repl_index]]--;

			repl_index+=2;

		}

		free(replace_list);

		rule_covered = getRecordMatches(matches_matrix, i, numRules, nRows);
		
		for(int j = 0; j < nRows; j++){
			covered_arr[j] |= rule_covered[j];
		}

		free(rule_covered);

		int classNum = getMajorityClass(classes, covered_arr, numClasses, nRows);

		defaultClasses_arr[i] = classNum;

		defaultErrors = getDefaultErrors(classes, covered_arr, nRows, classNum);
		ruleErrors += countRecordMatches(false_matches_matrix, i, numRules, nRows);

		total_errors_arr[i] = defaultErrors + ruleErrors;

	}

	return R_NilValue;

}

