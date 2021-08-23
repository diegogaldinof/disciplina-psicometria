# Latent Variable Modeling Using R - Beaujean - 2014
library(lavaan)
library(semPlot)
library(psych)
n <- 1000
regression.cor.lwr <- c(1,
                        0.20,1,
                        0.24,0.30,1,
                        0.70,0.80,0.30,1)
regression.sd <- c(10, 30, 20, 5)
nomes <- c("X1", "X2", "X3", "Y") 
regression.cor <- lavaan::getCov(regression.cor.lwr, names=nomes)
print(regression.cor)
regression.cov <- lavaan::cor2cov(R=regression.cor,sds=regression.sd)
regression.model <-"
 # path model for Y
  Y ~ a*X1 + b*X2 + c*X3
 # label the residual variance of Y
  Y ~~ z*Y
"
regression.fit <- lavaan::sem(regression.model, 
                              sample.cov=regression.cov, 
                              sample.nobs=n)
lavaan::varTable(regression.fit)
lavaan::summary(regression.fit, fit=TRUE, std=TRUE, rsq=TRUE)
semPlot::semPaths(regression.fit,
                  layout="tree",
                  style="lisrel",
                  what="std",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1.25,
                  nCharNodes=6, 
                  sizeMan=8,
                  curvePivot=TRUE,
                  fade=FALSE)
semPlot::semPaths(regression.fit,
                  layout="tree",
                  style="lisrel",
                  what="path",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1.5,
                  curvePivot=TRUE,
                  nCharNodes=6, 
                  sizeMan=8,
                  fade=FALSE)
psych::setCor(Y ~ X1 + X2 + X3, data=regression.cor, n.obs=n)
