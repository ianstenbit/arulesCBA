# Classification Based on Association Rules

[![CRAN version](https://www.r-pkg.org/badges/version/arulesCBA)](https://cran.r-project.org/package=arulesCBA)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/arulesCBA)](https://cran.r-project.org/package=arulesCBA)
  [![R-CMD-check](https://github.com/ianjjohnson/arulesCBA/workflows/R-CMD-check/badge.svg)](https://github.com/ianjjohnson/arulesCBA/actions)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/arulesCBA)](https://cran.r-project.org/package=arulesCBA)

The R package [arulesCBA](https://cran.r-project.org/package=arulesCBA) (Hahsler et al, 2020) 
is an extension of the package [arules](https://cran.r-project.org/package=arules) to perform
association rule-based classification. The package provides the infrastructure for class association rules and implements associative classifiers based on the following algorithms:

* __CBA__:    Classification Based on Association Rules (Liu et al, 1998).
* __CMAR__:   Classification based on Multiple Association Rule  (Li, Han and Pei, 2001) via LUCS-KDD Software Library.
* __CPAR__:   Classification based on Predictive Association Rules (Yin and Han, 2003) via LUCS-KDD Software Library.
* __C4.5__:   Rules extracted from a C4.5 decision tree (Quinlan, 1993) via J48 in R/Weka.
* __FOIL__:   First-Order Inductive Learner (Yin and Han, 2003).
* __PART__:   Rules from Partial Decision Trees (Frank and Witten, 1998) via R/Weka.
* __PRM__:    Predictive Rule Mining (Yin and Han, 2003) via LUCS-KDD Software Library.
* __RCAR__:   Regularized Class Association Rules using Logistic Regression (Azmi et al, 2019).
* __RIPPER__: Repeated Incremental Pruning to Produce Error Reduction (Cohen, 1995) via R/Weka.

The package also provides the infrastructure for associative classification (supervised discetization, mining class association rules (CARs)), and implements various association rule-based classification strategies
(first match, majority voting, weighted voting, etc.).

## Installation

__Stable CRAN version:__ install from within R with
```R
install.packages("arulesCBA")
```
__Current development version:__ 
```R 
library("devtools")
install_github("ianjjohnson/arulesCBA")
```

## Usage

```R
library("arulesCBA")
data("iris")
 
# learn a classifier
classifier <- CBA(Species ~ ., data = iris)
classifier

    CBA Classifier Object
    Class: Species=setosa, Species=versicolor, Species=virginica
    Default Class: Species=versicolor
    Number of rules: 6
    Classification method: first  
    Description: CBA algorithm (Liu et al., 1998)

# inspect the rulebase
inspect(rules(classifier), linebreak = TRUE)
     lhs                           rhs                  support conf lift count 
 [1] {Petal.Length=[-Inf,2.45)} => {Species=setosa}        0.33 1.00  3.0    50 
 [2] {Sepal.Length=[6.15, Inf],       
      Petal.Width=[1.75, Inf]}  => {Species=virginica}     0.25 1.00  3.0    37 
 [3] {Sepal.Length=[5.55,6.15),   
      Petal.Length=[2.45,4.75)} => {Species=versicolor}    0.14 1.00  3.0    21 
 [4] {Sepal.Width=[-Inf,2.95),
      Petal.Width=[1.75, Inf]}  => {Species=virginica}     0.11 1.00  3.0    17
 [5] {Petal.Width=[1.75, Inf]}  => {Species=virginica}     0.30 0.98  2.9    45 
 [6] {}                         => {Species=versicolor}    0.33 0.33  1.0   150

# make predictions for the first few instances of iris
predict(classifier, head(iris))

   [1] setosa setosa setosa setosa setosa setosa
   Levels: setosa versicolor virginica
```

## References

* M. Hahsler, I. Johnson, T. Kliegr and J. Kuchar (2019). [Associative Classification in R: arc, arulesCBA, and rCBA](https://journal.r-project.org/archive/2019/RJ-2019-048/). _The R Journal_ 11(2), pp. 254-267.
* M. Azmi, G.C. Runger, and A. Berrado (2019). Interpretable regularized class association rules algorithm for classification in a categorical data space. _Information Sciences,_ Volume 483, May 2019, pp. 313-331.
* W. W. Cohen (1995). Fast effective rule induction. In A. Prieditis and S. Russell (eds.), _Proceedings of the 12th International Conference on Machine Learning,_ pp. 115-123. Morgan Kaufmann. ISBN 1-55860-377-8.
* E. Frank and I. H. Witten (1998). Generating accurate rule sets without global optimization. In J. Shavlik (ed.), _Machine Learning: Proceedings of the Fifteenth International Conference,_ Morgan Kaufmann Publishers: San Francisco, CA.
* W. Li, J. Han and J. Pei (2001). CMAR: accurate and efficient classification based on multiple class-association rules, _Proceedings 2001 IEEE International Conference on Data Mining,_ San Jose, CA, USA, pp. 369-376.
* B. Liu, W. Hsu and Y. Ma (1998). Integrating Classification and Association Rule Mining. _KDD'98 Proceedings of the Fourth International Conference on Knowledge Discovery and Data Mining,_ New York, AAAI, pp. 80-86.
* R. Quinlan (1993). _C4.5: Programs for Machine Learning._ Morgan Kaufmann Publishers, San Mateo, CA.
* X. Yin and J. Han (2003). CPAR: Classification based on Predictive Association Rules, _Proceedings of the 2003 SIAM International Conference on Data Minin,_ pp. 331-235.
