/*
A C implementation of a weighted association-rule based classification algorithm.
Author: Ian Johnson
*/

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


void populateMatches(int* matches_for_rule, int* false_matches_for_rule, int* lhs_i, int* lhs_p, int* rhs_i, int* df_p, int* df_i, int rule_index, int num_rows){

    int rule_start_index = lhs_p[rule_index], rule_end_index = lhs_p[rule_index+1];

    //for(int col_loc = rule_start_index; col_loc < rule_end_index; col_loc++){
    int num_matches = 0, num_false_matches = 0;

    for(int row_num = 0; row_num < num_rows; row_num++){

       int loc = df_p[row_num], end_loc = df_p[row_num+1], curr_col;

       curr_col = rule_start_index;


       while(loc < end_loc){

         if (df_i[loc] == lhs_i[curr_col]) curr_col++;
         if(curr_col == rule_end_index) break;

         loc++;

       }


       if(curr_col == rule_end_index){

          if(df_i[df_p[row_num+1]-1] == rhs_i[rule_index])
              matches_for_rule[num_matches++] = row_num;
          else
              false_matches_for_rule[num_false_matches++] = row_num;

       }

    }

    matches_for_rule[num_matches] = -1;
    false_matches_for_rule[num_false_matches] = -1;

}

/*
C Interface for R function for model building. Parameters are explained in the R implementation (../R/classifier.R)
*/
//rule_weights, rules.sorted@lhs@data@i, rules.sorted@lhs@data@p, rules.sorted@lhs@data@Dim, rules.sorted@rhs@data@i, rules.sorted@rhs@data@p, ds.mat@data@Dim, ds.mat@data@i, ds.mat@data@p, gamma, cost, length(levels(rightHand))
SEXP weighted(SEXP ruleWeights, SEXP rulesLHS_I, SEXP rulesLHS_P, SEXP rulesRHS_I, SEXP DF_I, SEXP DF_P, SEXP DF_Dim, SEXP Gamma, SEXP Cost, SEXP numClasses, SEXP ClassWeights){

  int num_classes = INTEGER(numClasses)[0];
  int num_rules   = length(rulesRHS_I);
  int num_columns = INTEGER(DF_Dim)[0];
  int num_rows    = INTEGER(DF_Dim)[1];

  int* lhs_i = INTEGER(rulesLHS_I);
  int* lhs_p = INTEGER(rulesLHS_P);

  int* rhs_i = INTEGER(rulesRHS_I);

  int* df_p = INTEGER(DF_P);
  int* df_i = INTEGER(DF_I);

  double* rule_weights = REAL(ruleWeights);
  double* row_weights  = malloc(num_rows * sizeof(double));
  double* class_weights = REAL(ClassWeights);



  memset(class_weights, 0, num_classes * sizeof(double));
  for(int row = 0; row < num_rows; row++)
    class_weights[(df_i[df_p[row+1]-1]) -  num_columns + num_classes] += 1;

  //Future idea... cost matrix?
  for(int i = 0; i < num_rows; i++) row_weights[i] = class_weights[df_i[df_p[i+1]-1] -  num_columns + num_classes];

  double gamma = REAL(Gamma)[0];
  double cost  = REAL(Cost)[0];

  //This array will hold the indeces of the rows which match a given rule
  int* matches_for_rule = malloc((num_rows+1)  * sizeof(int));

  //This array will hold the indeces of the rows which falsely match a given rule
  int* false_matches_for_rule = malloc((num_rows+1)  * sizeof(int));

  //Iterate over every rule, in their sorted order from R
  for(int rule_index = 0; rule_index < num_rules; rule_index++){

    //Populate the true and false matches list using util.c/getRecordMatches
    //The first parameter of the function is modified by the function.
    populateMatches(matches_for_rule, false_matches_for_rule, lhs_i, lhs_p, rhs_i, df_p, df_i, rule_index, num_rows);

    //Each rule gets a weight. This is being initialized here.
    double weight = 0; int match_index = 0;

    //Adjust weight of rule based on weights of rows it matches
    //and adjust the weights of the rows it matches
    while(matches_for_rule[match_index] != -1) {
       weight += row_weights[matches_for_rule[match_index]];
       row_weights[matches_for_rule[match_index++]] -= gamma;
			 if(row_weights[matches_for_rule[match_index-1]] < 0) row_weights[matches_for_rule[match_index-1]] = 0;
		}

    //Adjust weight of rule based on weights of rows it falsely matches
    //and adjust the weights of the rows it falsely matches
    match_index = 0;
    while(false_matches_for_rule[match_index] != -1){
       weight -= cost * row_weights[false_matches_for_rule[match_index]];
       row_weights[false_matches_for_rule[match_index++]] += gamma;
    }

    //Assign the rule weight and move on to the next rule
    rule_weights[rule_index] = weight;

  }

  //Compute the overall class weights to pick a default class
  memset(class_weights, 0, num_classes * sizeof(double));

  //Calculate the actual class weights
  for(int row = 0; row < num_rows; row++)
    class_weights[(df_i[df_p[row+1]-1]) -  num_columns + num_classes] += row_weights[row];


  //Find the maximum of the remaining class weights and use that class as the default
  double max = 0; int default_class = -1;
  for(int class = 0; class < num_classes; class++){
    if(class_weights[class] > max){
        max = class_weights[class];
        default_class = class+1; //Offset by 1 for the 1-indexing in R
    }
  }

  //Create S-expression of default class for R
  SEXP def = allocVector(INTSXP, 1);
  INTEGER(def)[0] = default_class;

  //Free temporary class weights array
  free(row_weights);

  //Free rule-row matching arrays
  free(matches_for_rule);
  free(false_matches_for_rule);

  //Return the default class.
  //Note that the class weights are also "returned" by pass-by-reference like functionality
  return def;

}
