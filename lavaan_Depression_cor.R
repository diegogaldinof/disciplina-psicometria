library(lavaan)
library(semTools)
library(semPlot) 
library(lavaanPlot)
# Entrada de dados por matriz de covariancia: Gana&Broc (2019, p. 119-24)
cor.matriz <- "
1
.70 1
.65 .66 1
.62 .63 .60 1"
cor.matriz.simetrica <- lavaan::getCov(cor.matriz, 
                                       names=c("X1","X2","X3","X4"))
model.especif <- 'Depres =~ X1 + X2 + X3 + X4'
model.estim <- lavaan::cfa(model.especif, std.lv=TRUE,
                           sample.cov=cor.matriz.simetrica, 
                           sample.nobs=300)
lavaan::summary(model.estim, fit=TRUE, std=TRUE, rsq=TRUE)
semTools::reliability(model.estim)
labels <- list(Depres = "Depression")
semPlot::semPaths(model.estim, 
                  layout="tree",
                  style="lisrel",
                  what="std",
                  whatLabel="std",
                  rotation=3,
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1,
                  nCharNodes=10,
                  sizeLat=10,
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
lavaanPlot::lavaanPlot(model=model.estim, 
                       labels=labels, 
                       node_options=list(shape="box", ontname="Helvetica"), 
                       edge_options=list(color="black"), 
                       coefs=TRUE, covs=TRUE, stand=TRUE,
                       # sig=.05,
                       stars=c("latent","covs"))
