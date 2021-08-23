# Latent Variable Modeling Using R - path - 2014
library(lavaan)
library(semPlot)
library(psych)
n <- 300
path.cov.lwr <- c(648.07, 
                   30.05,  8.64, 
                  140.18, 25.57, 233.21)
nomes <- c("Salario", "Escolaridade", "QI")
path.cov <- lavaan::getCov(path.cov.lwr,names=nomes)
print(path.cov)
path.model <- "
 Salario ~ a*Escolaridade + c*QI
 QI ~ b*Escolaridade  
 bc := b*c
 Total := a + bc
"
path.fit <- lavaan::sem(path.model, 
                    sample.cov=path.cov, 
                    sample.nobs=n)
lavaan::varTable(path.fit)
lavaan::summary(path.fit, fit=TRUE, std=TRUE, rsq=TRUE)
semPlot::semPaths(path.fit,
                  layout="tree",
                  style="lisrel",
                  what="std",
                  whatLabels="std",
                  rotation=2,
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1.25,
                  nCharNodes=6, 
                  sizeMan=8,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
semPlot::semPaths(path.fit,
                  layout="tree",
                  style="lisrel",
                  what="path",
                  whatLabels="path",
                  rotation=2,
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1.25,
                  nCharNodes=6, 
                  sizeMan=8,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
r <- cov2cor(path.cov)
psych::mediate(Salario ~ Escolaridade + (QI), 
               std=TRUE, n.obs=n, data=r)
out <- psych::mediate(Salario ~ Escolaridade + (QI), 
                      std=TRUE, n.obs=n, data=r)
summary(out, short = FALSE)
