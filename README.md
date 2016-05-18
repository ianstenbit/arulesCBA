##CBA Algorithm in R and C

This repository has a version of the CBA algorithm described in Liu, et al, 1998.

###How to use / test

####Setup

After cloning this repository, you have to edit two files to make this work. You also have to re-compile the C code.

First, in Tests/UnitTests.R, set the working directory to the location of the /Tests folder in this repository.

Second, in CBA/CBA_Function.R, search for 'dyn.load'. Replace the path in this function call to the location of the CBA.so file which is in the /C directory of this repository.

Finally, navigate to the /C directory of this repository in a terminal, and run the command `R CMD SHLIB CBA.c` to recompile the C dependencies of the CBA.c function

####Running the program

First, run the CBA/CBA_Function.R file to load the three CBA functions (CBA, CBA.2, and CBA.C) into memory.

Second, run the Tests/UnitTests.R file. This will load 4 data sets and run the classifier on them. The fourth data set runs somewhat slowly, as there is a very large number of association rules for that data set.

After the classifier is run on each data set, a crosstable will be displayed to demonstrate the results of the classification algorithm.

In the UnitTests.R file, all of the function calls to CBA.C can be replaced with calls to CBA.2 or CBA, which are the original R implementations of the algorithm. The results for CBA.2 and CBA should be exactly the same, but the results for CBA may be different, as it is a previous version of the algorithm.

####SpeedComparison.R

The SpeedComparison.R file in the /C directory has a basic little speed test to demonstrate the speed difference between the C and R implementations. 

Currently, the most computationally expensive part of the algorithm is pruning the list of association rules for large rulesets. This will eventually be implemented in C.
