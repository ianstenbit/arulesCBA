setwd("~/Dropbox/Summer 2016/CSE 5390/CBA_Algorithm/C")
dyn.load("test.so")
.Call("test_func", as.integer(5))
