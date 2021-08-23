# Adaptado de Latent Variable Modeling With R - Finch & French - 2015
library(lavaan)
library(semPlot)
library(semTools)
library(MplusAutomation)
library(MVN)
data(HolzingerSwineford1939)
# https://cran.r-project.org/web/packages/MVN/vignettes/MVN.pdf
HS39 <- subset(HolzingerSwineford1939,
               select=c(x1,x2,x3,x4,x5,x6,x7,x8,x9,school))
HS39GW <- subset(HolzingerSwineford1939,
                 school=="Grant-White",
                 select=c(x1,x2,x3,x4,x5,x6,x7,x8,x9,sex,ageyr,agemo))
HS39GW$age <- HS39GW$ageyr + HS39GW$agemo/12
result <- MVN::mvn(data=HS39, subset="school", 
                   mvnTest="hz", univariateTest="SW")
print(result$multivariateNormality)
print(result$univariateNormality)
result1 <- MVN::mvn(data=HS39GW[1:9], mvnTest="hz", 
                    multivariateOutlierMethod="adj")
HS39GW.model0 <- "visual  =~ x1 + x2 + x3 
                  textual =~ x4 + x5 + x6
                  speed   =~ x7 + x8 + x9"
HS39GW.fit0 <- lavaan::sem(HS39GW.model0,std.lv=TRUE,data=HS39GW)
lavaan::summary(HS39GW.fit0, fit=TRUE, std=TRUE, rsq=TRUE)
out <- lavaan::lavExport(HS39GW.fit0, target = "Mplus", export=TRUE)
cat(out)
semPlot::semPaths(HS39GW.fit0,
                  layout="tree",
                  style="lisrel",
                  what="path",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=0.8,
                  nCharNodes=6, 
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
semPlot::semPaths(HS39GW.fit0,
                  layout="tree",
                  style="lisrel",
                  what="std",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=0.8,
                  nCharNodes=6, 
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
semTools::reliability(HS39GW.fit0)

# Modelo com efeito direto e moderacao 
# VD: textual, VE: visual, Mediadora: speed
HS39GW.model1 <- 'visual  =~ x1 + x2 + x3 
                  textual =~ x4 + x5 + x6
                  speed   =~ x7 + x8 + x9
                #Direct effect
                  textual ~ a*visual
                #Mediator effect
                  speed ~ b*visual
                  textual ~ c*speed
                #Indirect effect
                  bc := b*c
                #Total effect
                  total := a+(b*c)'
HS39GW.fit1 <- lavaan::sem(HS39GW.model1,std.lv=TRUE,data=HS39GW)
lavaan::summary(HS39GW.fit1, fit=TRUE, std=TRUE, rsq=TRUE)
semPlot::semPaths(HS39GW.fit1,
                  layout="tree",
                  style="lisrel",
                  what="path",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=0.8,
                  nCharNodes=6, 
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
semPlot::semPaths(HS39GW.fit1,
                  layout="tree",
                  style="lisrel",
                  what="std",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=0.8,
                  nCharNodes=6, 
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
semTools::reliability(HS39GW.fit1)

# Modelo apenas com efeitos diretos 
# VD: textual, VE: visual, speed
HS39GW.model2 <- 'visual  =~ x1 + x2 + x3 
                  textual =~ x4 + x5 + x6
                  speed   =~ x7 + x8 + x9
                #Direct effect
                  textual ~ a*visual
                  textual ~ b*speed
                #Total effect
                  total := a+b'
HS39GW.fit2 <- lavaan::sem(HS39GW.model2,std.lv=TRUE,data=HS39GW)
lavaan::summary(HS39GW.fit2, fit=TRUE, std=TRUE, rsq=TRUE)
semPlot::semPaths(HS39GW.fit2,
                  layout="tree",
                  style="lisrel",
                  what="path",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=0.8,
                  nCharNodes=6, 
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
semPlot::semPaths(HS39GW.fit2,
                  layout="tree",
                  style="lisrel",
                  what="std",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=0.8,
                  nCharNodes=6, 
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
semTools::reliability(HS39GW.fit2)
anova(HS39GW.fit1,HS39GW.fit2)

# Modelo apenas com efeito de mediacao: 
# VD: textual, VE: visual, Mediadora: speed
HS39GW.model3 <- 'visual  =~ x1 + x2 + x3 
                  textual =~ x4 + x5 + x6
                  speed   =~ x7 + x8 + x9
                #Mediator effect
                  speed ~ b*visual
                  textual ~ c*speed
                #Indirect or total effect
                  bc := b*c'
HS39GW.fit3 <- lavaan::sem(HS39GW.model3,std.lv=TRUE,data=HS39GW)
lavaan::summary(HS39GW.fit3, fit=TRUE, std=TRUE, rsq=TRUE)
semPlot::semPaths(HS39GW.fit3,
                  layout="tree",
                  style="lisrel",
                  what="path",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1,
                  nCharNodes=6, 
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
semPlot::semPaths(HS39GW.fit3,
                  layout="tree",
                  style="lisrel",
                  what="std",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=0.8,
                  nCharNodes=6, 
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
semTools::reliability(HS39GW.fit3)
anova(HS39GW.fit1,HS39GW.fit3)

# Modelo com efeito direto e moderacao e covariavel: 
# VD: textual, VE: visual, Mediadora: speed, Covariavel: sex, age
HS39GW.model4 <- 'visual  =~ x1 + x2 + x3 
                  textual =~ x4 + x5 + x6
                  speed   =~ x7 + x8 + x9
                  antrop =~ sex + age
                #Direct effect
                  textual ~ a*visual
                  textual ~ e*antrop
                #Mediator effect
                  speed ~ b*visual
                  textual ~ c*speed
                  speed ~ d*antrop
                #Indirect effect
                  bc := b*c
                  dc := d*c
                #Total effect
                  total1 := a+(b*c)
                  total2 := e+(d*c)'
HS39GW.fit4 <- lavaan::sem(HS39GW.model4,std.lv=TRUE,data=HS39GW)
lavaan::summary(HS39GW.fit4, fit=TRUE, std=TRUE, rsq=TRUE)
semPlot::semPaths(HS39GW.fit4,
                  layout="spring",
                  style="lisrel",
                  what="path",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=1,
                  nCharNodes=3, 
                  sizeMan=5,
                  sizeInt=4,
                  sizeLat=6,
                  curvePivot=FALSE,
                  intAtSide=TRUE,
                  fade=FALSE)
semPlot::semPaths(HS39GW.fit4,
                  layout="spring",
                  style="lisrel",
                  what="std",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=0.7,
                  nCharNodes=3, 
                  sizeMan=5,
                  sizeInt=4,
                  sizeLat=6,
                  curvePivot=FALSE,
                  intAtSide=TRUE,
                  fade=FALSE)
semTools::reliability(HS39GW.fit4)
anova(HS39GW.fit1,HS39GW.fit4)

# Multiple Indicators Multiple Causes (MIMIC) Model
# VD: textual, VE: sex, age
HS39GW.model5 <- 'textual =~ x4 + x5 + x6
                  textual ~ sex + age'
HS39GW.fit5 <- lavaan::sem(HS39GW.model5,std.lv=TRUE,data=HS39GW)
lavaan::summary(HS39GW.fit5, fit=TRUE, std=TRUE, rsq=TRUE)
semPlot::semPaths(HS39GW.fit5,
                  layout="tree",
                  style="lisrel",
                  what="path",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=0.8,
                  nCharNodes=6, 
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
semPlot::semPaths(HS39GW.fit5,
                  layout="tree",
                  style="lisrel",
                  what="std",
                  residuals=TRUE,
                  edge.color="black",
                  edge.label.cex=0.8,
                  nCharNodes=6, 
                  sizeMan=6,
                  sizeInt=4,
                  curvePivot=TRUE,
                  intAtSide=TRUE,
                  fade=FALSE)
