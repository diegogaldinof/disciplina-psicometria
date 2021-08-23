# Latent Variable Modeling Using R - Beaujean - 2014
library(lavaan)
library(semPlot)
library(psych)
n <- 18058
privSchool.cor.upr <- c(1, 0.178, 0.230, 0.106, 0.195, 
                           1,     0.327, 0.245, 0.356, 
                                  1,     0.183, 0.721,
                                         1,     0.178, 
                                                1)
nomes <- c("Race", "SES", "CogAbil", "SchTyp", "AcadAch")
privSchool.cor <- lavaan::getCov(x=privSchool.cor.upr,names=nomes,lower=FALSE)
print(privSchool.cor)
full.model <- "
 AcadAch ~ j*SchTyp + g*SES + d*Race + i*CogAbil
 SchTyp ~ f*SES + b*Race + h*CogAbil"
full.fit <- lavaan::sem(full.model, 
                        sample.cov=privSchool.cor, 
                        sample.nobs=n)
lavaan::varTable(full.fit)
lavaan::summary(full.fit, fit=TRUE, std=TRUE, rsq=TRUE)
semPlot::semPaths(full.fit,
                  layout="tree2",
                  style="lisrel",
                  what="path",
                  whatLabels="path",
                  rotation=2,
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1.25,
                  nCharNodes=4, 
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
semPlot::semPaths(full.fit,
                  layout="tree2",
                  style="lisrel",
                  what="std",
                  whatLabels="std",
                  rotation=2,
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1,
                  nCharNodes=4, 
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
psych::mediate(AcadAch ~ SchTyp + SES + Race + CogAbil + (SchTyp), 
               std=TRUE, n.obs=n, data=privSchool.cor)
out <- psych::mediate(AcadAch ~ SchTyp + SES + Race + CogAbil + (SchTyp), 
                      std=TRUE, n.obs=n, data=privSchool.cor)
summary(out, short = FALSE)
