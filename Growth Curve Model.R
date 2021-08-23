# Latent Variable Modeling With R - Finch & French - 2015
# Language assessment 
# 410 elementary school students; fall2003-spring2006
library(lavaan)
library(semPlot)
library(haven)
library(ggplot2)

# Linear Growth Curve Model (LGCM) sem covariaveis
gcm_data <- haven::read_sav("gcm_data.sav")
attach(gcm_data)
lf3_mean<-mean(lf3)
ls4_mean<-mean(ls4)
lf4_mean<-mean(lf4)
ls5_mean<-mean(ls5)
lf5_mean<-mean(lf5)
ls6_mean<-mean(ls6)
gcm_mean<-rbind(lf3_mean, ls4_mean, lf4_mean, ls5_mean, lf5_mean, ls6_mean)
gcm_time<-rbind(1,2,3,4,5,6)
gcm_mean.dataframe<-data.frame(gcm_mean, gcm_time)
plot(gcm_time,gcm_mean, type="l")
lines(lowess(gcm_time,gcm_mean),lty=2)
lang.growth.model1 <- '
i =~ 1*lf3+1*ls4+1*lf4+1*ls5+1*lf5+1*ls6
s =~ 0*lf3+1*ls4+2*lf4+3*ls5+4*lf5+5*ls6
'
lang.growth.model1.fit <- lavaan::growth(lang.growth.model1,
                                         data=gcm_data)
summary(lang.growth.model1.fit,
        fit.measures=TRUE, standardized=TRUE, rsq=TRUE)
semPlot::semPaths(lang.growth.model1.fit,
                  layout="tree2",
                  what="path",
                  edge.color="black",
                  residuals=FALSE)
semPlot::semPaths(lang.growth.model1.fit,
                  layout="tree2",
                  what="std",
                  edge.color="black",
                  residuals=FALSE)

# Quadratic Growth Curve Model (QGCM) sem covariaveis
lang.growth.model2<-'
i =~ 1*lf3+1*ls4+1*lf4+1*ls5+1*lf5+1*ls6
s =~ 0*lf3+1*ls4+2*lf4+3*ls5+4*lf5+5*ls6
q =~ 0*lf3+1*ls4+4*lf4+9*ls5+16*lf5+25*ls6
'
lang.growth.model2.fit<- lavaan::growth(lang.growth.model2, 
                                        data=gcm_data)
summary(lang.growth.model2.fit, 
        fit.measures=TRUE, standardized=TRUE, rsq=TRUE)
semPlot::semPaths(lang.growth.model2.fit,
                  layout="tree2",
                  what="path",
                  edge.color="black",
                  residuals=FALSE)

# Linear Growth Curve Model (LGCM) com covariaveis 
# MIMIC model
lang.growth.model1.mimic<-'
i =~ 1*lf3+1*ls4+1*lf4+1*ls5+1*lf5+1*ls6
s =~ 0*lf3+1*ls4+2*lf4+3*ls5+4*lf5+5*ls6
i ~ sex
s ~ sex
'
lang.growth.model1.mimic.fit <- lavaan::growth(lang.growth.model1.mimic,
                                               data=gcm_data)
summary(lang.growth.model1.mimic.fit, 
        fit.measures=TRUE, standardized=TRUE, rsq=TRUE)
semPlot::semPaths(lang.growth.model1.mimic.fit,
                  layout="tree",
                  what="path",
                  edge.color="black",
                  residuals=FALSE)

# Linear Growth Curve Model (LGCM) com duas variaveis sem covariaveis
# Language and reading test scores
rf3_mean<-mean(rf3)
rs4_mean<-mean(rs4)
rf4_mean<-mean(rf4)
rs5_mean<-mean(rs5)
rf5_mean<-mean(rf5)
rs6_mean<-mean(rs6)
gcm_mean<-rbind(rf3_mean, rs4_mean, rf4_mean, rs5_mean, rf5_mean, rs6_mean)
gcm_time<-rbind(1,2,3,4,5,6)
test<-factor(c(1,1,1,1,1,1,2,2,2,2,2,2))
gcm_mean_read.dataframe<-data.frame(gcm_mean, gcm_time)
gcm_mean_both.dataframe<-rbind(gcm_mean.dataframe, gcm_mean_read.dataframe)
gcm_mean_both.dataframe<-cbind(gcm_mean_both.dataframe,test)
ggplot2::ggplot(gcm_mean_both.dataframe, aes(gcm_time, gcm_mean, linetype=test))+
  geom_line()+geom_point()
lang.growth.model3 <- '
lang_i =~ 1*lf3+1*ls4+1*lf4+1*ls5+1*lf5+1*ls6
lang_s =~ 0*lf3+1*ls4+2*lf4+3*ls5+4*lf5+5*ls6
read_i =~ 1*rf3+1*rs4+1*rf4+1*rs5+1*rf5+1*rs6
read_s =~ 0*rf3+1*rs4+2*rf4+3*rs5+4*rf5+5*rs6
'
lang.growth.model3.fit <- lavaan::growth(lang.growth.model3,
                                              data=gcm_data)
summary(lang.growth.model3.fit, 
        fit.measures=TRUE, standardized=TRUE, rsq=TRUE)
semPlot::semPaths(lang.growth.model3.fit,
                  layout="circle2",
                  what="path",
                  edge.color="black",
                  residuals=FALSE)



