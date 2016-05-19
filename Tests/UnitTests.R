require(gmodels)
setwd("~/Dropbox/Summer 2016/CSE 5390/CBA_Algorithm/Tests/")

#Test data set 1: Titanic survival
load("titanic.raw.rdata")
classifier <- CBA.C(titanic.raw, "Survived", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.6))
results <- classify(titanic.raw, classifier)
CrossTable(x = titanic.raw$Survived, y = results, prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)


#Test data set 2: Benign and Malignant tumors
dataset <- read.csv("wisc_bc_data.csv", stringsAsFactors = TRUE)
dataset$id <- NULL
dsDisc <- as.data.frame(lapply(dataset[2:31], function(x) discretize(x, categories = 6)))
dsDisc$diagnosis <- factor(dataset$diagnosis)
dsDisc <- dsDisc[c(1:10, 31)]
classifier <- CBA.C(dsDisc, "diagnosis", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
results <- classify(dsDisc, classifier)
CrossTable(x = dsDisc$diagnosis, y = results, prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)


#Test data set 3: Flower species classification
data(iris)
irisDisc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=9)))
irisDisc$Species <- iris$Species
classifier <- CBA.C(irisDisc, "Species", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
results <- classify(irisDisc, classifier)
CrossTable(x=irisDisc$Species, y=results, prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)

#Test data set 4: Congressional voting records
congressData <- read.csv("house-votes-84.data", stringsAsFactors=TRUE, header=FALSE)
colnames(congressData)[1] <- "Party"
classifier <- CBA.C(congressData, "Party", list(minlen=2, supp = 0.10, conf=0.99))
results <- classify(congressData, classifier)
CrossTable(x=congressData$Party, y=results, prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
