# Latent Variable Modeling Using R - Beaujean - 2014
# Two factor model of WISC-IV data
library(lavaan)
library(semPlot)
library(semTools)
wisc4.cor.lwr <- c(1,
                   0.72,1,
                   0.64,0.63, 1,
                   0.51,0.48,0.37,1,
                   0.37,0.38,0.38,0.38,1)
wisc4.sd <- c(3.01 , 3.03, 2.99, 2.89, 2.98)
nomes <- c("Information", "Similarities", 
           "Word.Reasoning", "Matrix.Reasoning", "Picture.Concepts")
wisc4.cor <- lavaan::getCov(x=wisc4.cor.lwr,  names=nomes)
wisc4.cov <- lavaan::cor2cov(R=wisc4.cor,sds=wisc4.sd)
print(wisc4.cov)
wisc4.model2<-'
 Verbal =~ a*Information + b*Similarities + c*Word.Reasoning 
 Fluid =~ d*Matrix.Reasoning + e*Picture.Concepts
 Verbal ~~ f*Fluid'
wisc4.fit2 <- lavaan::cfa(model=wisc4.model2, 
                         sample.cov=wisc4.cov, 
                         sample.nobs=550,
                         std.lv=TRUE)
lavaan::varTable(wisc4.fit2)
lavaan::summary(wisc4.fit2, fit=TRUE, std=TRUE, rsq=TRUE)
semTools::reliability(wisc4.fit2)
semTools::discriminantValidity(wisc4.fit2)
semPlot::semPaths(wisc4.fit2,
                  layout="tree",
                  style="lisrel",
                  what="path",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1.25,
                  nCharNodes=6, 
                  sizeMan=8,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
semPlot::semPaths(wisc4.fit2,
                  layout="tree",
                  style="lisrel",
                  what="std",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1.25,
                  nCharNodes=6, 
                  sizeMan=8,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)

