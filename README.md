# Classification Based on Association Rules

[![Travis-CI Build Status](https://api.travis-ci.org/ianjjohnson/arulesCBA.svg?branch=master)](https://travis-ci.org/ianjjohnson/arulesCBA)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/arulesCBA)](https://cran.r-project.org/package=arulesCBA)
[![CRAN version](http://www.r-pkg.org/badges/version/arulesCBA)](https://cran.r-project.org/package=arulesCBA)

This arulesCBA R package (Hahsler et al, 2020) 
is an extension of the package [arules](https://cran.r-project.org/package=arules) to perform
association rule-based classification. The package implements:

* CBA (Liu et al, 1998)

* CMAR (Li et al, 2001)

* RCAR (Azmi et al, 2019)

* bCBA, wCBA (unpublished)

The package also provides the infrastructure including supervized discetization, mining class association rules (CARs), and to implement various association rule-based classification strategies
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
 
# learn a classifier using automatic default discretization
classifier <- CBA(Species ~ ., data = iris, supp = 0.05, conf = 0.9)
classifier

  CBA Classifier Object
  Class: Species=setosa, Species=versicolor, Species=virginica
  Default Class: Species=setosa
  Number of rules: 8
  Classification method: first 
  Description: CBA algorithm by Liu, et al. 1998 with support=0.05 and confidence=0.9

# make predictions for the first few instances of iris
predict(classifier, head(iris))

   [1] setosa setosa setosa setosa setosa setosa
   Levels: setosa versicolor virginica
```

## References

* Michael Hahsler, Ian Johnson, Tomas Kliegr and Jaroslav Kuchar (2020). [Associative Classification in R: arc, arulesCBA, and rCBA](https://journal.r-project.org/archive/2019/RJ-2019-048/). The R Journal, accepted article.
* Liu, B. Hsu, W. and Ma, Y (1998). Integrating Classification and Association Rule Mining. _KDD'98 Proceedings of the Fourth International Conference on Knowledge Discovery and Data Mining,_ New York, 27-31 August. AAAI. pp. 80-86.
* Wenmin Li, Jiawei Han and Jian Pei, "CMAR: accurate and efficient classification based on multiple class-association rules," Proceedings 2001 IEEE International Conference on Data Mining, San Jose, CA, USA, 2001, pp. 369-376.
* M. Azmi, G.C. Runger, and A. Berrado (2019). Interpretable regularized class association rules algorithm for classification in a categorical data space. Information Sciences, Volume 483, May 2019. Pages 313-331.
