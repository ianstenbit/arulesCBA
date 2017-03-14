#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP stage1(SEXP data_rows, SEXP strong_rules, SEXP casesCovered, SEXP matches_i, SEXP matches_p, SEXP num_matches, SEXP falseMatches_i, SEXP falseMatches_p, SEXP num_falseMatches, SEXP numRules);
extern SEXP stage2(SEXP a, SEXP casesCovered, SEXP matches_i, SEXP matches_p, SEXP num_matches, SEXP strong_rules, SEXP num_entries);
extern SEXP stage3(SEXP strong_rules, SEXP casesCovered, SEXP covered, SEXP defaultClasses, SEXP totalErrors, SEXP classDistr, SEXP replace, SEXP matches_i, SEXP matches_p, SEXP num_matches, SEXP falseMatches_i, SEXP falseMatches_p, SEXP num_falseMatches, SEXP classLevels);

extern SEXP weighted(SEXP ruleWeights, SEXP rulesLHS_I, SEXP rulesLHS_P, SEXP rulesRHS_I, SEXP DF_I, SEXP DF_P, SEXP DF_Dim, SEXP Gamma, SEXP Cost, SEXP numClasses, SEXP ClassWeights);

void R_init_arulesCBA(DllInfo *dll) {

    const R_CallMethodDef CallEntries[] = {
	     {"R_stage1", (DL_FUNC) stage1, 10},
	     {"R_stage2", (DL_FUNC) stage2, 7},
	     {"R_stage3", (DL_FUNC) stage3, 14},
       {"R_weighted", (DL_FUNC) weighted, 11},
	     {NULL, NULL, 0}
    };

    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    R_RegisterCCallable("arulesCBA", "R_stage1", (DL_FUNC) stage1);
    R_RegisterCCallable("arulesCBA", "R_stage2", (DL_FUNC) stage2);
    R_RegisterCCallable("arulesCBA", "R_stage3", (DL_FUNC) stage3);
    R_RegisterCCallable("arulesCBA", "R_weighted", (DL_FUNC) weighted);
    
}
