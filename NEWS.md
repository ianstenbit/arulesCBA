# arulesCBA 1.2.0 (2020-04-17)
* Version 1.2.0 has a major interface cleanup. This might require some change in existing code.
* The classifiers now use as the default a min. confidence of .5 and maxlen of 5 (max. rule length). 
* CBA now includes a default rule in the rule base.
* added prepareTransactions to dicretize and convert a data.frame into transactions. 
* added new function response to convert class items to class labels (factors).
* added new function majorityClass.
* added FOIL.
* added RIPPER C4.5, and PART (via RWeka).
* added PRM, CPAR and CMAR (via LUCS-KDD Software Library).
* added datasets Mushroom and Lymphography.

# arulesCBA 1.1.6 (2020-01-05)
* maintenance release.

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
