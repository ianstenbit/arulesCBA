# arulesCBA 1.1.6.1 (2020-xx-xx)
* CBA now includes a default rule in the rule base.
* added CMAR.
* added FOIL.
* added RIPPER and PART (via RWeka).

# arulesCBA 1.1.6 (2020-01-05)

# arulesCBA 1.1.5 (2019-11-05)
* added RCAR (by Tyler Giallanza).
* The interface for CBA() was updated.
* CBA now complains if no rules are found.
* CBA has now also M1 pruning.
* mineCARs now uses ... to construct the parameters for apriori().

# arulesCBA 1.1.4 (2018-12-04)
* discretizeDF.supervised method mdlp now produces a better error message if it fails.
* cleaned up the predict code to improve speed.
* mineCARs has now a balanced support option.
* convenience function classFrequency added.

# arulesCBA 1.1.3-1 (2018-04-23)

* added new function discretizeDF.supervised for supervised discretization.
* added new convenience function mineCARs to mine class association rules.
* the formula interface now parsed the right hand side to restrict the used predictors. 
