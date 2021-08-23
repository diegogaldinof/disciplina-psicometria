# https://stats.idre.ucla.edu/r/seminars/rsem
library(readxl)
library(lavaan)
library(semPlot)
library(psych)
Dados <- readxl::read_excel("Adm2008_Masc.xlsx")
Dados$Estatura <- Dados$Estatura*100
psych::headTail(Dados)
psych::describe(Dados[,2:4])
psych::lowerCor(Dados[,2:4])
psych::pairs.panels(Dados[,2:4],
 smooth = TRUE, scale = TRUE, density=TRUE,ellipses=FALSE,
 digits = 2,method="pearson", pch = 20, lm=TRUE,cor=TRUE,jiggle=TRUE,
 factor=2, hist.col=NULL,show.points=TRUE,rug=FALSE, 
 breaks = "Sturges",cex.cor=2,wt=NULL,
 smoother=FALSE,stars=FALSE,ci=FALSE,alpha=.05)
r <- round(with(Dados,cor(MCT,Estatura)),3)
cat("Correlacao de Pearson = ", r, "\n",sep="")
fitr <- psych::setCor(MCT ~ Estatura, std=FALSE, data=Dados)
print(fitr)
fitr <- psych::setCor(MCT ~ Estatura, std=TRUE, data=Dados)
print(fitr)
fitr <- psych::setCor(MCT ~ Estatura + Idade, std=FALSE, data=Dados)
print(fitr)
fitr <- psych::setCor(MCT ~ Estatura + Idade, std=TRUE, data=Dados)
print(fitr)
#simple linear regression using lm()
fit <- lm(MCT ~ Estatura, data=Dados)
summary(fit)
confint(fit)
plot(Dados$Estatura,Dados$MCT,main=paste0("r = ",r, 
                                          " n = ", nrow(Dados)))
abline(reg=fit,lty=2)
semPlot::semPaths(fit,
                  layout="tree",
                  style="lisrel",
                  what="path",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1,
                  whatLabels="std", 
                  nCharNodes=6, 
                  sizeMan=8,
                  sizeInt=4,
                  fade=FALSE)
semPlot::semPaths(fit,
                  layout="tree",
                  style="lisrel",
                  what="est",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1,
                  nCharNodes=6, 
                  sizeMan=8,
                  sizeInt=4,
                  fade=FALSE)
semPlot::semPaths(fit,
                  layout="tree",
                  style="lisrel",
                  what="std",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1,
                  nCharNodes=6, 
                  sizeMan=8,
                  sizeInt=4,
                  fade=FALSE)
semPaths(fit, intAtSide=TRUE)
#simple linear regression using lavaan 
regressao <- 'MCT ~ 1 + Estatura'
fitlv <- lavaan::sem(regressao, data=Dados)
lavaan::summary(fitlv, fit=TRUE, std=TRUE, rsq=TRUE)
lavaan::fitMeasures(fitlv,output="text")
lavaan::parameterEstimates(fitlv)
lavaan::varTable(fitlv)
semPlot::semPaths(fitlv,
                  layout="tree",
                  style="lisrel",
                  what="est",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1,
                  nCharNodes=6, 
                  sizeMan=8,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  rotation=4,
                  fade=FALSE)
semPlot::semPaths(fitlv,
                  layout="tree",
                  style="lisrel",
                  what="std",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1,
                  nCharNodes=6, 
                  sizeMan=8,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  rotation=4,
                  fade=FALSE)
semPlot::semPaths(fitlv,
                  layout="tree",
                  style="lisrel",
                  what="path",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1.5,
                  curvePivot=TRUE,
                  nCharNodes=6, 
                  sizeMan=8,
                  sizeInt=4,
                  intAtSide=TRUE,
                  rotation=4,
                  fade=FALSE)

