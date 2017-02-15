//#include "cba.c"

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


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

  int* matches_for_rule = malloc((num_rows+1)  * sizeof(int));
  //memset(matches_for_rule, 0, (num_rows+1)  * sizeof(int));

  int* false_matches_for_rule = malloc((num_rows+1)  * sizeof(int));
  //memset(false_matches_for_rule, 0, (num_rows+1)  * sizeof(int));

  for(int rule_index = 0; rule_index < num_rules; rule_index++){

    getRecordMatches(matches_for_rule, match_rules, match_rows, num_matches, num_rules, rule_index);
    getRecordMatches(false_matches_for_rule, false_match_rules, false_match_rows, num_fmatches, num_rules, rule_index);

    double weight = 0; int match_index = 0;

    while(matches_for_rule[match_index] != -1) {
       weight += row_weights[matches_for_rule[match_index++]];
       row_weights[matches_for_rule[match_index]] -= gamma;
    }

    match_index = 0;
    while(false_matches_for_rule[match_index] != -1){
       weight -= cost * row_weights[false_matches_for_rule[match_index++]];
       row_weights[matches_for_rule[match_index]] += gamma;
    }

    rule_weights[rule_index] = weight;

  }


  double* class_weights = malloc(num_classes * sizeof(double));
  memset(class_weights, 0, num_classes * sizeof(double));

  //get default class as class with least total coverage
  for(int row = 0; row < num_rows; row++){
    class_weights[classes[row] - 1] += row_weights[row];
  }

  double max = 0; int default_class = -1;
  for(int class = 0; class < num_classes; class++){
    if(class_weights[class] > max){
        max = class_weights[class];
        default_class = class+1;
    }
  }

  SEXP def = allocVector(INTSXP, 1);
  INTEGER(def)[0] = default_class;

  free(class_weights);

  free(matches_for_rule);
  free(false_matches_for_rule);

  return def;

}
