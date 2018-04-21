# Classification Based on Association Rules

[![Travis-CI Build Status](https://api.travis-ci.org/ianjjohnson/arulesCBA.svg?branch=master)](https://travis-ci.org/ianjjohnson/arulesCBA)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/arulesCBA)](https://cran.r-project.org/package=arulesCBA)
[![CRAN version](http://www.r-pkg.org/badges/version/arulesCBA)](https://cran.r-project.org/package=arulesCBA)

This R package 
is an extension of the package [arules](https://cran.r-project.org/package=arules) to perform
association rule-based classification. It includes currently two classification algorithms. The first is the CBA algorithm described in Liu, et al. 1998.
The second is a new weighted majority-vote based algorithm called bCBA which is currently being designed and tested. Time-critical sections of the code are implemented in C.

The package also provides support for supervised discretization and mining Class Association Rules (CARs).


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

* Liu, B. Hsu, W. and Ma, Y (1998). Integrating Classification and Association Rule Mining. _KDD'98 Proceedings of the Fourth International Conference on Knowledge Discovery and Data Mining,_ New York, 27-31 August. AAAI. pp. 80-86.
