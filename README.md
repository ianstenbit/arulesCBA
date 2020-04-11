# Classification Based on Association Rules"

[![Travis-CI Build Status](https://api.travis-ci.org/ianjjohnson/arulesCBA.svg?branch=master)](https://travis-ci.org/ianjjohnson/arulesCBA)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/arulesCBA
)](https://cran.r-project.org/package=arulesCBA)
[![CRAN version](https://www.r-pkg.org/badges/version/arulesCBA)](https://cran.r-project.org/package=arulesCBA)

The [arulesCBA](https://cran.r-project.org/package=arulesCBA) R package (Hahsler et al, 2020) 
is an extension of the package [arules](https://cran.r-project.org/package=arules) to perform
association rule-based classification. The package implements:

* CBA (Liu et al, 1998)
* RCAR (Azmi et al, 2019)
* bCBA, wCBA (unpublished)
* FOIL (Quinlan and Cameron-Jones, 1995)
* RIPPER via R/Weka (Cohen, 1995)
* PART via R/Weka (Frank and Witten, 1998)

The package also provides infrastructure including supervised discetization, mining class association rules (CARs), and an implementation of various association rule-based classification strategies
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

* Michael Hahsler, Ian Johnson, Tomas Kliegr and Jaroslav Kuchar (2019). [Associative Classification in R: arc, arulesCBA, and rCBA](https://journal.r-project.org/archive/2019/RJ-2019-048/). _The R Journal_, 11(2) pp. 254-267.
* Liu, B. Hsu, W. and Ma, Y (1998). Integrating Classification and Association Rule Mining. _KDD'98 Proceedings of the Fourth International Conference on Knowledge Discovery and Data Mining,_ New York, 27-31 August. AAAI. pp. 80-86.
* Wenmin Li, Jiawei Han and Jian Pei, "CMAR: accurate and efficient classification based on multiple class-association rules," _Proceedings 2001 IEEE International Conference on Data Mining,_ San Jose, CA, USA, 2001, pp. 369-376.
* M. Azmi, G.C. Runger, and A. Berrado (2019). Interpretable regularized class association rules algorithm for classification in a categorical data space. _Information Sciences,_ Volume 483, May 2019. Pages 313-331.
* Quinlan, J.R., Cameron-Jones, R.M. Induction of logic programs: FOIL and related systems. NGCO 13, 287-312 (1995). doi: 10.1007/BF03037228
* W. W. Cohen (1995). Fast effective rule induction. In A. Prieditis and S. Russell (eds.), _Proceedings of the 12th International Conference on Machine Learning,_ pages 115–123. Morgan Kaufmann. ISBN 1-55860-377-8.
* E. Frank and I. H. Witten (1998). Generating accurate rule sets without global optimization. In J. Shavlik (ed.), _Machine Learning: Proceedings of the Fifteenth International Conference,_ Morgan Kaufmann Publishers: San Francisco, CA.
