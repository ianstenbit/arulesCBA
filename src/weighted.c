/*
A C implementation of a weighted association-rule based classification algorithm.
Author: Ian Johnson
*/

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


/*
C Interface for R function for model building. Parameters are explained in the R implementation (../R/classifier.R)
*/
SEXP weighted(SEXP rowWeights, SEXP ruleWeights, SEXP matchesI, SEXP matchesP, SEXP matchesDim, SEXP falseMatchesI, SEXP falseMatchesP, SEXP Gamma, SEXP Cost, SEXP rightHand, SEXP numClasses){

  int num_classes  = INTEGER(numClasses)[0];
  int num_matches  = length(matchesI);
  int num_fmatches = length(falseMatchesI);

  double* row_weights  = REAL(rowWeights);
  double* rule_weights = REAL(ruleWeights);

  int* match_rules = INTEGER(matchesI);
  int* match_rows  = INTEGER(matchesP);

  int* false_match_rules = INTEGER(falseMatchesI);
  int* false_match_rows  = INTEGER(falseMatchesP);

  int num_rules = INTEGER(matchesDim)[0];
  int num_rows  = INTEGER(matchesDim)[1];

  int* classes = INTEGER(rightHand);

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
    getRecordMatches(matches_for_rule, match_rules, match_rows, num_matches, num_rules, rule_index);
    getRecordMatches(false_matches_for_rule, false_match_rules, false_match_rows, num_fmatches, num_rules, rule_index);

    //Each rule gets a weight. This is being initialized here.
    double weight = 0; int match_index = 0;

    //Adjust weight of rule based on weights of rows it matches
    //and adjust the weights of the rows it matches
    while(matches_for_rule[match_index] != -1) {
       weight += row_weights[matches_for_rule[match_index]];
       row_weights[matches_for_rule[match_index++]] -= gamma;
    }

    //Adjust weight of rule based on weights of rows it falsely matches
    //and adjust the weights of the rows it falsely matches
    match_index = 0;
    while(false_matches_for_rule[match_index] != -1){
       weight -= cost * row_weights[false_matches_for_rule[match_index]];   
       row_weights[matches_for_rule[match_index++]] += gamma;
    }

    //Assign the rule weight and move on to the next rule
    rule_weights[rule_index] = weight;

  }

  //Compute the overall class weights to pick a default class
  double* class_weights = malloc(num_classes * sizeof(double));
  memset(class_weights, 0, num_classes * sizeof(double));

  //Calculate the actual class weights
  for(int row = 0; row < num_rows; row++){
    class_weights[classes[row] - 1] += row_weights[row];
  }

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
  free(class_weights);

  //Free rule-row matching arrays
  free(matches_for_rule);
  free(false_matches_for_rule);

  //Return the default class.
  //Note that the class weights are also "returned" by pass-by-reference like functionality
  return def;

}
