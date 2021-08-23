library(lavaan)
out <- lavaan::mplus2lavaan(inpfile="sem.mplus.inp",run=TRUE)
summary(out)