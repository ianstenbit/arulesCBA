/*
A C implementation of stages I, II, and III of the CBA algorithm described by Liu, et al 1998
Author: Ian Johnson
*/

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#define _ind(x,y,cols) (x) + (y) * (cols) /*Macro for R matrix access in linear c-array*/

/*
Finds the first 1 in the binary matrix 'matrix' in row 'entry_row'
Note that matrix is a linear c-array, accessed like a matrix using the matrix-to-array indexing macro
@param matrix: a binary matrix representing which rules match which entries in the dataset
@param entry_row: the row of the current entry for which we're searching for the first rule
@param numRules: the number of rules in the matrix (the 'width' of the matrix)
@param numEntries: the number of entries in the dataset (the 'height' of the matrix)
@return: the index of the first rule which matches the entry specified, or -1 if no rule is found
*/
int firstMatch(int* matrix, int entry_row, int numRules, int numEntries){

	/*Iterate sideways through one row of the matrix*/
	for(int i = 0; i < numRules; i++)
		if(matrix[_ind(i, entry_row, numRules)] == 1)
			return i;

	/*Default return if no match found*/
	return -1;
}

/*
Finds the all 1s in the binary matrix 'matrix' in row 'entry_row'
Note that matrix is a linear c-array, accessed like a matrix using the matrix-to-array indexing macro
@param matrix: a binary matrix representing which rules match which entries in the dataset
@param entry_row: the row of the current entry for which we're searching for the first rule
@param numRules: the number of rules in the matrix (the 'width' of the matrix)
@return: a pointer to an int c-array containing ones and zeroes.
A 1 at index i indicates that rule i matches this entry.
Note: this pointer MUST NOT be freed by the calling function. It points to a location inside 'matrix'
*/
int* getMatches(int* matrix, int entry_row, int numRules){

	/*return a pointer to the row of the matrix for this entry*/
	return &(matrix[_ind(0, entry_row, numRules)]);

}

/*
Counds the number of records/entries which a given rule matches
Note that matrix is a linear c-array, accessed like a matrix using the matrix-to-array indexing macro
@param matrix: a binary matrix representing which rules match which entries in the dataset
@param rule_column: the column index in 'matrix' which refers to the rule whose matches are being counted
@param numRules: the number of rules in the matrix (the 'width' of the matrix)
@param numRecords: the number of records/entries in the matrix (the 'height' of the matrix)
*/
int countRecordMatches(int* matrix, int rule_column, int numRules, int numRecords){

	int count = 0;

	/*Iterate vertically through a column of the matrix, counting the 1s*/
	for(int i = 0; i < numRecords; i++){
		if(matrix[_ind(rule_column, i, numRules)] == 1)
			count++;
	}

	return count;

}

/*
Finds the all 1s in the binary matrix 'matrix' in column 'rule_column'
Note that matrix is a linear c-array, accessed like a matrix using the matrix-to-array indexing macro
@param matrix: a binary matrix representing which rules match which entries in the dataset
@param rule_column: the column of the current rule for which we're searching for entry matches
@return: a pointer to an int c-array containing ones and zeroes.
A 1 at index i indicates that entry i matches this rule.
Note: this pointer MUST NOT be freed by the calling function. It points to a location inside 'matrix'
Note: this pointer MUST only be accessed at indeces i where i%numRules == 0.
This is because it points to a column inside a matrix saved as a c-array
*/
int* getRecordMatches(int* matrix, int rule_column){

	/*return a pointer to the first element in rule_column in matrix*/
	return &(matrix[rule_column]);

}

/*
Builds a c-array of replacement rule data. This data was originally stored in a data.frame in R,
but it has been linearized for ease of use in C.
@param replace: a linear array of the replace structure from R.
Note:
	- Indeces where i%3 = 0 represent a crule
	- Indeces where i%3 = 1 represent a wrule to replace the crule
	- Indeces where i%3 = 2 represent the entry for which this replacement may occur
@param rule: the index of the crule whose replacements are being found
@param numRules: the number of rules in the data set
@param rLen: the length of the replace array
@return: a list of replacements for rule 'rule'
Note: 
	- What's returned by this method bust be freed by the calling function
	- Indeces where i%2 = 0 represent a wrule
	- Indeces where i%2 = 1 represent an entry index for which this wrule might replace the given crule
*/
int* getReplacements(int* replace, int rule, int numRules, int rLen){

	/*Allocate an int array of all the possible replacements*/
	int* repl = malloc((numRules-1) * 2 * sizeof *repl);
	int repl_size = 0;

	/*Fill the array with -1s*/
	for(int i = 0; i < numRules - 1; i++) repl[i] = -1;

	/*For each replacement, copy over the info if the crule matches the rule for which we're searching*/
	for(int i = 0; i < rLen; i+=3){
		if(replace[i] == rule){
			repl[repl_size++] = replace[i+1];
			repl[repl_size++] = replace[i+2];
		}
	}

	/*Return the new replacement array*/
	return repl;
}

/*
Get the majority class of the remaining unclassified records
Note that the class is stored numerically, as opposed to by factor-string as is done in R
@param classes: the classes of all of the training records
@param covered: a binary array representing wether or not each record has been classified
@param classLevels: the number of different classes
@param numEntries: the number of entries in the training dataset
@return: an integer in [0...classLevels) representing the majority class of the unclassified records in the data set
*/
int getMajorityClass(int* classes, int* covered, int classLevels, int numEntries){

	/*Allocate an array to count the instances of each class*/
	int* counts = malloc(classLevels * sizeof *counts);
	memset(counts, 0, sizeof(int)*classLevels);

	/*Populate the array by parsing through the classes array*/
	for(int i = 0; i < numEntries; i++)
		if(!covered[i])
			counts[classes[i]-1]++;

	/*Calculate the max*/
	int max_index = 0;

	for(int i = 0; i < classLevels; i++){
		if(counts[i] > counts[max_index]){
			max_index = i;
		}
	}

	/*Free the temp array*/
	free(counts);

	/*Return the majority class*/
	return max_index+1;
}

/*
Counts the number of default errors caused by a rule with a given default class
@param classes: the classes of all of the training records
@param covered: a binary array representing wether or not each record has been classified
@param numEntries: the number of entries in the training dataset
@param defaultClass: the integer representation of the default class of the rule whose default errors are being counted
@return the number of entries which are not currently covered and whose class does not match the default class
*/
int getDefaultErrors(int* classes, int* covered, int numEntries, int defaultClass){

	int count = 0;

	/*Iterate through the entries and count the unclassified false matches*/
	for(int i = 0; i < numEntries; i++)
		if(!covered[i] && classes[i] != defaultClass)
			count++;

	return count;
}

/*
Stage 1 of the CBA algorithm as described by Liu, et al. 1998
This stage populates and returns a list A of all falsely classified records, along with the crules and wrules associated with that record
It also constructs a vector of "strong rules" which will be used in the final classifier
All parameters are S Expressions from R, whose purpose is explained within the function declaration
*/
SEXP stage1(SEXP dataset, SEXP strong_rules, SEXP casesCovered, SEXP matches, SEXP falseMatches, SEXP numRules){

	/*Wrapper class for integers*/
	R_len_t i, nrows, nrules;

	/*Integers for use inside the for loop*/
	int crule, wrule, classify;

	/*nrules and nrows are the number of rules and the number of entries in the training data set, respectivelly*/
	nrules = INTEGER(numRules)[0];
	nrows = length(getAttrib(dataset, R_RowNamesSymbol));

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

		rule_covered = getRecordMatches(matches_matrix, i);
		
		for(int j = 0; j < nRows; j++){
			covered_arr[j] |= rule_covered[j*numRules];
		}

		int classNum = getMajorityClass(classes, covered_arr, numClasses, nRows);

		defaultClasses_arr[i] = classNum;

		defaultErrors = getDefaultErrors(classes, covered_arr, nRows, classNum);
		ruleErrors += countRecordMatches(false_matches_matrix, i, numRules, nRows);

		total_errors_arr[i] = defaultErrors + ruleErrors;

	}

	return R_NilValue;

}

