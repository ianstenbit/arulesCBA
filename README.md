R package arulesCBA: Classification Based on Association Rules
================

[![CRAN
version](https://www.r-pkg.org/badges/version/arulesCBA)](https://cran.r-project.org/package=arulesCBA)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/arulesCBA)](https://cran.r-project.org/package=arulesCBA)
[![R-CMD-check](https://github.com/ianjjohnson/arulesCBA/workflows/R-CMD-check/badge.svg)](https://github.com/ianjjohnson/arulesCBA/actions)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/arulesCBA)](https://cran.r-project.org/package=arulesCBA)

The R package [arulesCBA](https://cran.r-project.org/package=arulesCBA)
(Hahsler et al, 2020) is an extension of the package
[arules](https://cran.r-project.org/package=arules) to perform
association rule-based classification. The package provides the
infrastructure for class association rules and implements associative
classifiers based on the following algorithms:

-   **CBA**: Classification Based on Association Rules (Liu et al,
    1998).
-   **CMAR**: Classification based on Multiple Association Rule (Li, Han
    and Pei, 2001) via LUCS-KDD Software Library.
-   **CPAR**: Classification based on Predictive Association Rules (Yin
    and Han, 2003) via LUCS-KDD Software Library.
-   **C4.5**: Rules extracted from a C4.5 decision tree (Quinlan, 1993)
    via J48 in R/Weka.
-   **FOIL**: First-Order Inductive Learner (Yin and Han, 2003).
-   **PART**: Rules from Partial Decision Trees (Frank and Witten, 1998)
    via R/Weka.
-   **PRM**: Predictive Rule Mining (Yin and Han, 2003) via LUCS-KDD
    Software Library.
-   **RCAR**: Regularized Class Association Rules using Logistic
    Regression (Azmi et al, 2019).
-   **RIPPER**: Repeated Incremental Pruning to Produce Error Reduction
    (Cohen, 1995) via R/Weka.

The package also provides the infrastructure for associative
classification (supervised discetization, mining class association rules
(CARs)), and implements various association rule-based classification
strategies (first match, majority voting, weighted voting, etc.).

## Installation

**Stable CRAN version:** install from within R with

``` r
install.packages("arulesCBA")
```

**Current development version:**

``` r
devtools::install_github("ianjjohnson/arulesCBA")
```

## Usage

``` r
library("arulesCBA")
data("iris")
```

Learn a classifier.

``` r
classifier <- CBA(Species ~ ., data = iris)
classifier
```

    ## CBA Classifier Object
    ## Formula: Species ~ .
    ## Number of rules: 6
    ## Default Class: NA
    ## Classification method: first  
    ## Description: CBA algorithm (Liu et al., 1998)

Inspect the rulebase.

``` r
inspect(rules(classifier), linebreak = TRUE)
```

    ##     lhs                            rhs                  support confidence coverage lift count size coveredTransactions totalErrors
    ## [1] {Petal.Length=[-Inf,2.45)}  => {Species=setosa}        0.33       1.00     0.33  3.0    50    2                  50          50
    ## [2] {Sepal.Length=[6.15, Inf],                                                                                                     
    ##      Petal.Width=[1.75, Inf]}   => {Species=virginica}     0.25       1.00     0.25  3.0    37    3                  37          13
    ## [3] {Sepal.Length=[5.55,6.15),                                                                                                     
    ##      Petal.Length=[2.45,4.75)}  => {Species=versicolor}    0.14       1.00     0.14  3.0    21    3                  21          13
    ## [4] {Sepal.Width=[-Inf,2.95),                                                                                                      
    ##      Petal.Width=[1.75, Inf]}   => {Species=virginica}     0.11       1.00     0.11  3.0    17    3                   5           8
    ## [5] {Petal.Width=[1.75, Inf]}   => {Species=virginica}     0.30       0.98     0.31  2.9    45    2                   4           6
    ## [6] {}                          => {Species=versicolor}    0.33       0.33     1.00  1.0   150    1                  33           6

Make predictions for the first few instances of iris.

``` r
predict(classifier, head(iris))
```

    ## [1] setosa setosa setosa setosa setosa setosa
    ## Levels: setosa versicolor virginica

## References

-   M. Hahsler, I. Johnson, T. Kliegr and J. Kuchar (2019). [Associative
    Classification in R: arc, arulesCBA, and
    rCBA](https://journal.r-project.org/archive/2019/RJ-2019-048/). *The
    R Journal* 11(2), pp. 254-267.
-   M. Azmi, G.C. Runger, and A. Berrado (2019). Interpretable
    regularized class association rules algorithm for classification in
    a categorical data space. *Information Sciences,* Volume 483, May
    2019, pp. 313-331.
-   W. W. Cohen (1995). Fast effective rule induction. In A. Prieditis
    and S. Russell (eds.), *Proceedings of the 12th International
    Conference on Machine Learning,* pp. 115-123. Morgan Kaufmann. ISBN
    1-55860-377-8.
-   E. Frank and I. H. Witten (1998). Generating accurate rule sets
    without global optimization. In J. Shavlik (ed.), *Machine Learning:
    Proceedings of the Fifteenth International Conference,* Morgan
    Kaufmann Publishers: San Francisco, CA.
-   W. Li, J. Han and J. Pei (2001). CMAR: accurate and efficient
    classification based on multiple class-association rules,
    *Proceedings 2001 IEEE International Conference on Data Mining,* San
    Jose, CA, USA, pp. 369-376.
-   B. Liu, W. Hsu and Y. Ma (1998). Integrating Classification and
    Association Rule Mining. *KDD’98 Proceedings of the Fourth
    International Conference on Knowledge Discovery and Data Mining,*
    New York, AAAI, pp. 80-86.
-   R. Quinlan (1993). *C4.5: Programs for Machine Learning.* Morgan
    Kaufmann Publishers, San Mateo, CA.
-   X. Yin and J. Han (2003). CPAR: Classification based on Predictive
    Association Rules, *Proceedings of the 2003 SIAM International
    Conference on Data Minin,* pp. 331-235.
