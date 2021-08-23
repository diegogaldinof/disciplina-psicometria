library(readxl)
library(GGally)
library(car)
library(psych)
Dados <- readxl::read_excel("Felicidade.xlsx")
View(Dados)
sink("Correlacao_Felicidade.txt")
pdf("Correlacao_Felicidade.pdf")
psych::describe(Dados)
sunflowerplot(Q8~Q2,data=Dados)
psych::pairs.panels(Dados,
                    smooth = TRUE, scale = TRUE, density=TRUE,
                    ellipses=FALSE,digits = 2,method="pearson", pch = 20, 
                    lm=TRUE,cor=TRUE,jiggle=TRUE,
                    factor=2, hist.col=NULL,show.points=TRUE,rug=FALSE, 
                    breaks = "Sturges",cex.cor=4,wt=NULL,
                    smoother=FALSE,stars=FALSE,ci=FALSE,alpha=.05)
car::scatterplot(Q8~Q2, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
                 jitter=list(x=1, y=1), col="black", data=Dados)
with(Dados, cor.test(Q2, Q8, 
                     method="pearson",
                     exact=TRUE, 
                     alternative="two.sided",
                     na.rm=TRUE))
round(cor(Dados),2)
GGally::ggcorr(Dados,
               nbreaks=6,
               digits=2,
               label=TRUE,
               label_size=4)
dev.off()
sink()
