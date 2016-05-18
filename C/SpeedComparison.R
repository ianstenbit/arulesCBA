totalTime2 <- 0
for(i in 1:10){
  start <- Sys.time()
  classifier <- CBA.2(dsDisc, "diagnosis", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.7))
  end <- Sys.time()
  totalTime2 <- totalTime2 + end - start
}

totalTimeC <- 0
for(i in 1:10){
  start <- Sys.time()
  classifier <- CBA.C(dsDisc, "diagnosis", apriori_parameter = list(minlen=2, supp = 0.05, conf=0.7))
  end <- Sys.time()
  totalTimeC <- totalTimeC + end - start
}

as.numeric(totalTime2 - totalTimeC) / as.numeric(totalTime2)