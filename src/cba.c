/*
Utility functions for sparse-matrix computations used in CBA algorithm and weighted ARC algorithm.
Author: Ian Johnson
*/

#include <stdlib.h>
#include <string.h>

/*
Finds the first 1 in the binary matrix 'matrix' in row 'entry_row'
Note that matrix is a linear c-array, accessed like a matrix using the matrix-to-array indexing macro
@param matrix: a binary matrix representing which rules match which entries in the dataset
@param entry_row: the row of the current entry for which we're searching for the first rule
@param numRules: the number of rules in the matrix (the 'width' of the matrix)
@param numEntries: the number of entries in the dataset (the 'height' of the matrix)
@return: the index of the first rule which matches the entry specified, or -1 if no rule is found
*/
int firstMatch(int* matrix_rows, int* matrix_p, int entry_row, int numRules, int numEntries, int numMatches){

	int start_loc = matrix_p[entry_row];

	int end_loc;
	if(entry_row == numEntries - 1)
		end_loc = numMatches;
	else
		end_loc = matrix_p[entry_row+1];

	for(int i = start_loc; i < end_loc; i++)
		return matrix_rows[i];

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
void getMatches(int* matches, int* matrix_i, int* matrix_p, int entry_row, int numMatches, int numRows){

	int numFoundMatches = 0;

	int start_loc = matrix_p[entry_row];
	int end_loc = numMatches;
	if(entry_row != numRows - 1) end_loc = matrix_p[entry_row+1];

	while(start_loc < end_loc){
		matches[numFoundMatches++] = matrix_i[start_loc];
		start_loc++;
	}

	matches[numFoundMatches] = -1;

}

/*
Counds the number of records/entries which a given rule matches
Note that matrix is a linear c-array, accessed like a matrix using the matrix-to-array indexing macro
@param matrix: a binary matrix representing which rules match which entries in the dataset
@param rule_column: the column index in 'matrix' which refers to the rule whose matches are being counted
@param numRules: the number of rules in the matrix (the 'width' of the matrix)
@param numRecords: the number of records/entries in the matrix (the 'height' of the matrix)
*/
int countRecordMatches(int* matrix_i, int* matrix_p, int numMatches, int* covered, int rule_column, int numRules, int numRecords){

	int sum = 0;

	for(int i = 0; i < numMatches; i++)
		if(matrix_i[i] == rule_column)
			sum++;

	return sum;

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
void getRecordMatches(int* matches, int* matrix_rows, int* matrix_p, int numMatches, int numRows, int rule_column){

	int numFoundMatches = 0;
	int column = 0;

	for(int i = 0; i < numMatches; i++){

		if(matrix_rows[i] == rule_column){
			while(column < numRows && matrix_p[column] < i) column++;
			matches[numFoundMatches++] = column-1;
		}
	}

	matches[numFoundMatches] = -1;

	/*return a pointer to the first element in rule_column in matrix*/
	//return &(matrix[rule_column]);

}

void resize(int** arr, int* len){

	int initialSize = *len;

	int* newArr = malloc(*len*2*sizeof(int));
	memcpy(newArr, *arr, *len*sizeof(int));

	*len = (*len) * 2;

	free(*arr);
	*arr = newArr;

	for(int i = initialSize; i < initialSize * 2; i++) (*arr)[i] = -1;

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
void getReplacements(int** repl_to_populate, int* replace, int rule, int numRules, int rLen, int* max_repl_size){

	/*Allocate an int array of all the possible replacements*/

	int* repl = *(repl_to_populate);

	int repl_size = 0;

	/*Fill the array with -1s*/
	for(int i = 0; i < numRules*2; i++) repl[i] = -1;

	/*For each replacement, copy over the info if the crule matches the rule for which we're searching*/
	for(int i = 0; i < rLen; i+=3){
		if(replace[i] == rule){

			if(repl_size >= *max_repl_size - 1){
				resize(repl_to_populate, max_repl_size);
				repl = *(repl_to_populate);
			}

			repl[repl_size++] = replace[i+1];
			repl[repl_size++] = replace[i+2];

		}
	}

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
A C implementation of stages I, II, and III of the CBA algorithm described by Liu, et al 1998
Author: Ian Johnson

PLEASE NOTE: This is currently in development (as of 2/2/2017) to switch to sparse matrix representation.
Some documentation and variable nomenclature is deprecated. Sorry for the inconvenience.
*/

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#define _ind(x,y,cols) (x) + (y) * (cols) /*Macro for R matrix access in linear c-array*/


/*
Stage 1 of the CBA algorithm as described by Liu, et al. 1998
This stage populates and returns a list A of all falsely classified records, along with the crules and wrules associated with that record
It also constructs a vector of "strong rules" which will be used in the final classifier
All parameters, and the returned object, are S Expressions from R, whose purpose is explained within the function declaration
*/
SEXP stage1(SEXP data_rows, SEXP strong_rules, SEXP casesCovered, SEXP matches_i, SEXP matches_p, SEXP num_matches, SEXP falseMatches_i, SEXP falseMatches_p, SEXP num_falseMatches, SEXP numRules){

	/*Wrapper class for integers*/
	R_len_t i, nrows, nrules, nMatches, nFalseMatches;

	/*Integers for use inside the for loop*/
	int crule, wrule;

	/*nrules and nrows are the number of rules and the number of entries in the training data set, respectivelly*/
	nrules = INTEGER(numRules)[0];
	nrows = INTEGER(data_rows)[0];

	nMatches = INTEGER(num_matches)[0];
	nFalseMatches = INTEGER(num_falseMatches)[0];

	/*Local pointers to the binary matrices input from R which represent rule/entry matches */
	int* match_rows = INTEGER(matches_i);
	int* match_p    = INTEGER(matches_p);

	int* falseMatch_rows = INTEGER(falseMatches_i);
	int* falseMatch_p    = INTEGER(falseMatches_p);

	/*Allocate an integer array for the set A*/
	int* a_vector = malloc(nrows * 3 * sizeof(int));
	int a_size = 0;

	/*This is a linear pass through the entire data set. It's the most performance critical part of the algorithm for most datasets.
	For some datasets, there are considerably more rules than there are entries. For those sets, this may not be the most expensive function*/
	for(i = 0; i < nrows; i++){

		/*Find the first correct and incorrect rule matches for this entry*/
		crule = firstMatch(match_rows, match_p, i, nrules, nrows, nMatches);
		wrule = firstMatch(falseMatch_rows, falseMatch_p, i, nrules, nrows, nFalseMatches);

		/*If a crule was found, mark that it correctly covers a case*/
		if(crule != -1){
			INTEGER(casesCovered)[crule]++;
		}


		if(wrule == -1 && crule != -1){
			LOGICAL(strong_rules)[crule] = TRUE;
			continue;
		}

    	/*If the crule was higher precedence than the wrule, save it as a strong rule for the classifier*/
    	if(crule < wrule){
    		LOGICAL(strong_rules)[crule] = TRUE;
    	}
    	/*If the wrule has higher precedence, then save this case in A for further processing in step 2*/
    	else if (wrule < crule){
      		a_vector[a_size++] = i;
      		a_vector[a_size++] = crule;
      		a_vector[a_size++] = wrule;
    	}

	}

	/*Copy integer array a_vector into the set a, an R object*/
	SEXP a = allocVector(INTSXP, a_size);
	for(i = 0; i < a_size; i++){
		INTEGER(a)[i] = a_vector[i];
	}

	/*Free memory allocated by this function*/
	free(a_vector);

	/*Return a, the set of wrongly classified records, along with their respective crules and wrules*/
	return a;
}

/*
Stage 2 of the CBA algorithm as described by Liu, et al. 1998
This stage processes the set A of falsely classified records, and builds a set of possible replacement rules
for rules which generated classification errors in stage 1 of the algorithm.
All parameters, and the returned object, are S Expressions from R, whose purpose is explained within the function declaration
*/
SEXP stage2(SEXP a, SEXP casesCovered, SEXP matches_i, SEXP matches_p, SEXP num_matches, SEXP strong_rules, SEXP num_entries){

	/*a_length = 3 * the number of falsely classified records*/
	int a_length = length(a);
	/*initialize integers to store the data about each element in A*/
	int entry, crule, wrule;

	R_len_t numMatches = INTEGER(num_matches)[0];

	/*The number of rules in the rule set. This is not just the strong rules, it is all rules.
	strong_rules is a binary vector indicating which rules are strong*/
	int numRules = length(strong_rules);

	int numEntries = INTEGER(num_entries)[0];

	/*Build pointers to data payloads of S Expressions*/
	int* a_arr = INTEGER(a);
	int* strong_rules_arr = LOGICAL(strong_rules);
	int* cases_covered_arr = INTEGER(casesCovered);


	int* match_rows = INTEGER(matches_i);
	int* match_p    = INTEGER(matches_p);

	/*Allocate a vector for possible rule replacements*/
	int* replace = malloc(3*a_length*numRules * sizeof *replace);
	int replaceSize = 0;

	int* wSet = malloc((numRules+1) * sizeof(int));
	memset(wSet, 0, sizeof(int)*(numRules + 1));


	/*Iterate through the set A. In Liu, et al. this proccess, alongside the entire linear iteration through the
	dataaset in stage 1, constitute passing through the dataset slightly more than once*/
	for(int i = 0; i < a_length; i+=3){

		/*grab the next entry from the set A*/
		entry = a_arr[i];
		crule = a_arr[i+1];
		wrule = a_arr[i+2];

		/*If the wrule idenfitied in this record has been marked for the classifier, modify the cases covered lists*/
		if(strong_rules_arr[wrule]){
			if(crule != -1) cases_covered_arr[crule]--;
			cases_covered_arr[wrule]++;
		} else {

			/*If the wrule hasn't been identified for the classifier, generate a list of possible replacement rules for the out-prioritized crule*/
			getMatches(wSet, match_rows, match_p, entry, numMatches, numEntries);

			/*For every possible replacement rule*/
			for(int k = 0; k < numRules && wSet[k] != -1; k++){

				int j = wSet[k];

				/*Skip rules which aren't in the replacement subset, and don't replace a rule with itself*/
				if(j == crule) continue;

				/*Mark this rule as a strong rule*/
				strong_rules_arr[j] = TRUE;

				/*Make an entry for this rule in the set replace*/
				replace[replaceSize++] = crule;
      	replace[replaceSize++] = j;
      	replace[replaceSize++] = entry;
			}

		}
	}

	/*Copy the replace set into an R object / S Expression*/
	SEXP rep = allocVector(INTSXP, replaceSize);
	for(int i = 0; i < replaceSize; i++){
		INTEGER(rep)[i] = replace[i];
	}

	/*Free the memory allocated by this function*/
	free(replace);
	free(wSet);

	/* Return the set of all identified rule replacements
	Each item has a crule, a possible replacement, and an entry number from the original dataset */
	return rep;

}

/*
Stage 3 of the CBA algorithm as described by Liu, et al. 1998
This stage processes the set 'replace' of possible replacement rules, and builds a final classifier
All parameters are S Expressions from R, whose purpose is explained within the function declaration
*/
SEXP stage3(SEXP strong_rules, SEXP casesCovered, SEXP covered, SEXP defaultClasses, SEXP totalErrors, SEXP classDistr, SEXP replace, SEXP matches_i, SEXP matches_p, SEXP num_matches, SEXP falseMatches_i, SEXP falseMatches_p, SEXP num_falseMatches, SEXP classLevels){

	/*Save the number of entries,
	the number of rules,
	the number of elements in the replace set, (replace_len = 3* number of elements)
	and the number of possible classes for the classifier*/
	int nRows = length(covered);
	int numRules = length(strong_rules);
	int replace_len = length(replace);
	int numClasses = INTEGER(classLevels)[0];

	/*Build pointers to data payloads of S Expressions*/
	int* strong_rules_arr = LOGICAL(strong_rules);
	int* cases_covered_arr = INTEGER(casesCovered);
	int* replace_arr = INTEGER(replace);
	int* covered_arr = LOGICAL(covered);


	int* match_rows = INTEGER(matches_i);
	int* match_p    = INTEGER(matches_p);
	R_len_t numMatches = INTEGER(num_matches)[0];

	int* falseMatch_rows = INTEGER(falseMatches_i);
	int* falseMatch_p    = INTEGER(falseMatches_p);
	R_len_t numFalseMatches = INTEGER(num_falseMatches)[0];

	int* classes = INTEGER(classDistr);
	int* defaultClasses_arr = INTEGER(defaultClasses);
	int* total_errors_arr = INTEGER(totalErrors);

	/*Allocate integers and int pointers for inside the loop*/
	int* rule_covered = malloc((numMatches + 1) * sizeof(int));
	memset(rule_covered, 0, sizeof(int)*(numMatches + 1));

	int len_replace_list = numRules * 2 + 1;
	int* replace_list = malloc(len_replace_list * sizeof(int));

	/*Rule errors are the errors produced by a rule which falsely classifies a record,
	Default errors are the errors produced by using a default class which falsely classifies a record*/
	int ruleErrors = 0;
	int defaultErrors = 0;

	/*Linear pass through all of the rules*/
	for(int i = 0; i < numRules; i++){

		/*If this rule hasn't been marked for use in the classifier, ignore it*/
		if(strong_rules_arr[i] == 0) continue;

		/*If, after processing the set A, this rule no longer correctly classifies it, remove it from the classifier*/
		if(cases_covered_arr[i] == 0){
			strong_rules_arr[i] = FALSE;
			continue;
		}

		/*Save the list of replacement rules for the rule at index i*/
		getReplacements(&replace_list, replace_arr, i, numRules, replace_len, &len_replace_list);

		/*Iterate through the list of possible replacements*/
		int repl_index = 0;
		while(replace_list[repl_index] != -1){

			/*If the replacement rule covers an entry which is already covered, decrement the count of cases covered for the rule at index i*/
			if(covered_arr[replace_list[repl_index+1]])
				cases_covered_arr[i]--;
			/*Otherwise, decrement the count of cases covered for the replacement rule*/
			else
				cases_covered_arr[replace_list[repl_index]]--;

			/*Iterate by two to skip to the next possible replacement*/
			repl_index+=2;

		}

		ruleErrors += countRecordMatches(falseMatch_rows, falseMatch_p, numFalseMatches, covered_arr, i, numRules, nRows);

		/*Get a list of all of the records which this rule covers*/
		getRecordMatches(rule_covered, match_rows, match_p, numMatches, nRows, i);

		/*Mark all of the records covered by this rule as being covered, if they're not already covered*/
		for(int j = 0; j < numMatches && rule_covered[j] != -1; j++){
			 covered_arr[rule_covered[j]] = 1;
		}

		/*Calculate the best default class for the classifier after this rule has been processed*/
		int classNum = getMajorityClass(classes, covered_arr, numClasses, nRows);

		/*Save the default class for the classifier at this stage*/
		defaultClasses_arr[i] = classNum;

		/*Count the rule errors and the default errors*/
		defaultErrors = getDefaultErrors(classes, covered_arr, nRows, classNum);

		/*Get a list of all of the records which this rule covers*/
 		getRecordMatches(rule_covered, falseMatch_rows, falseMatch_p, numFalseMatches, nRows, i);
		/*Mark all of the records covered by this rule as being covered, if they're not already covered*/
  	for(int j = 0; j < numMatches && rule_covered[j] != -1; j++){
 		 	 if(covered_arr[rule_covered[j]] == 1) ruleErrors--;
 	 		 if(classes[rule_covered[j]] == classNum) defaultErrors--;
 		}

		/*Save the number of total errors. In R, the algorithm will prune the classifier to be the subset of the rules in the classifier
		up to the point where the minimum number of errors occur. If the minimum occurs in two places, the smaller of the two possible subsets
		will be chosen*/
		total_errors_arr[i] = defaultErrors + ruleErrors;

	}

	free(rule_covered);
	/*Free the memory allocated for the temp list of possible rule replacements for rule i*/
	free(replace_list);

	/*This step returns nothing, but in order to interface correctly with R, this R NULL object must be returned*/
	return R_NilValue;

}
