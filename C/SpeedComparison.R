setwd("~/Dropbox/Summer 2016/CSE 5390/CBA_Algorithm/Tests/")
dataset <- read.csv("wisc_bc_data.csv", stringsAsFactors = TRUE)
dataset$id <- NULL
dsDisc <- as.data.frame(lapply(dataset[2:31], function(x) discretize(x, categories = 6)))
dsDisc$diagnosis <- factor(dataset$diagnosis)
dsDisc <- dsDisc[c(1:10, 31)]



startTime <- Sys.time()
for(i in 1:10){
  classifier <- CBA.C(dsDisc, "diagnosis", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
}
endTime <- Sys.time()
endTime - startTime

startTime <- Sys.time()
for(i in 1:10){
  classifier <- CBA.2(dsDisc, "diagnosis", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.9))
}
endTime <- Sys.time()
endTime - startTime

