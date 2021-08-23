# Latent Variable Modeling Using R - Beaujean - 2014
# Exercise 3.1
library(lavaan)
library(semPlot)
psychSoc.cov.lwr <- c(0.77, 
                      0.38,  0.65, 
                      0.39,  0.39,  0.62, 
                     -0.25, -0.32, -0.27, 6.09)
nomes <- c("Depression1", "Depression2", "Depression3", "SocialActivity")
psychSoc.cov <- lavaan::getCov(psychSoc.cov.lwr, names=nomes)
print(psychSoc.cov)
# marker variable
marker.model <- '
PsychSocLV =~ Depression2 + Depression1 + Depression3 + SocialActivity'
marker.fit <- lavaan::cfa(marker.model, 
                          std.lv=FALSE,
                          sample.cov=psychSoc.cov, 
                          sample.nobs=6053)
lavaan::varTable(marker.fit)
lavaan::summary(marker.fit, fit.measures=TRUE, standardized=TRUE, rsq=TRUE)
# Model Test User Model:
#   Test statistic                                 9.620
#   Degrees of freedom                                 2
#   P-value (Chi-square)                           0.008
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#  PsychSocLV        0.383    0.012   30.696    0.000    1.000    1.000
semPlot::semPaths(marker.fit,
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
semPlot::semPaths(marker.fit,
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
# standardized latent variable
stdLV.fit1 <- lavaan::cfa(marker.model, 
                          std.lv=TRUE, 
                          sample.cov=psychSoc.cov, 
                          sample.nobs=6053)
lavaan::summary(stdLV.fit1, fit.measures=TRUE, standardized=TRUE, rsq=TRUE)
# Model Test User Model:
#   Test statistic                                 9.620
#   Degrees of freedom                                 2
#   P-value (Chi-square)                           0.008
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#  PsychSocLV        1.000                               1.000    1.000
semPlot::semPaths(stdLV.fit1,
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
# effects-coding
ec.model <- '
PsychSocLV =~ NA*Depression2 + a*Depression2 + b*Depression1 + c*Depression3
              + d*SocialActivity
a+b+c+d == 4
'
ec.fit <- lavaan::cfa(ec.model, 
                      std.lv=FALSE,
                      sample.cov=psychSoc.cov, 
                      sample.nobs=6053)
lavaan::summary(ec.fit, fit.measures=TRUE, standardized=TRUE, rsq=TRUE)
# Model Test User Model:
#   Test statistic                                 9.620
#   Degrees of freedom                                 2
#   P-value (Chi-square)                           0.008
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#  PsychSocLV        0.125    0.007   17.914    0.000    1.000    1.000
semPlot::semPaths(ec.fit,
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
