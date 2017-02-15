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
void getRecordMatches(int* matches, int* matrix_rows, int* matrix_p, int numMatches, int numRules, int rule_column){

	int numFoundMatches = 0;
	int column = 0;

	for(int i = 0; i < numMatches; i++){

		if(matrix_rows[i] == rule_column){
			while(column < numRules && matrix_p[column] < i) column++;
			matches[numFoundMatches++] = column;
		}
	}

	matches[numFoundMatches] = -1;

	/*return a pointer to the first element in rule_column in matrix*/
	//return &(matrix[rule_column]);

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
void getReplacements(int* repl, int* replace, int rule, int numRules, int rLen){

	/*Allocate an int array of all the possible replacements*/

	int repl_size = 0;

	/*Fill the array with -1s*/
	for(int i = 0; i < numRules*2; i++) repl[i] = -1;

	/*For each replacement, copy over the info if the crule matches the rule for which we're searching*/
	for(int i = 0; i < rLen; i+=3){
		if(replace[i] == rule){
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
