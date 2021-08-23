
# -------------------------------------------------------------------------

#            Path analysis de regressao linear e multipla

# -------------------------------------------------------------------------


# Slides 84 em diante

library(readxl)
library(lavaan)
library(semPlot)
library(psych)
Dados <- readxl::read_excel("Adm2008_Masc.xlsx")
Dados$Estatura <- Dados$Estatura * 100
psych::headTail(Dados)
psych::describe(Dados[, 2:4])
psych::lowerCor(Dados[, 2:4])
psych::pairs.panels(Dados[, 2:4], smooth = TRUE,
                    scale = TRUE, density = TRUE, ellipses = FALSE,
                    digits = 2, method = "pearson", pch = 20,
                    lm = TRUE, cor = TRUE, jiggle = TRUE,
                    factor = 2, hist.col = NULL, show.points = TRUE,
                    rug = FALSE, breaks = "Sturges", cex.cor = 2,
                    wt = NULL, smoother = FALSE, stars = FALSE,
                    ci = FALSE, alpha = 0.05)
r <- round(with(Dados, cor(MCT, Estatura)), 3)
cat("Correlacao de Pearson = ", r, "\n",
    sep = "")



# Usando funcao setCor do psych -------------------------------------------

# Essa funcao faz regressao e desenha um path diagram
# Posso usar uma matriz de correlacao ou o proprio dataset
# Essa funcao eh quase igual a funcao stats::lm
# A diferenca eh que ela retorna betas padronizados, enquanto
# que no lm, para ter betas padronizados, eh preciso
# padronizar as variaveis antes de fazer o lm



# Regressao linear simples ----

# Nao padronizado
fitr <- psych::setCor(MCT ~ Estatura, std = FALSE,
                      data = Dados) # nao padronizado (std = FALSE)
fitlm <- lm(MCT ~ Estatura, data = Dados)
print(fitr)
summary(fitlm) # mesmo resultado

## Scatterplot da regressao nao padronizada (usando Dados)
plot(Dados$Estatura, Dados$MCT,
     main = paste0("r = ",
                   r, " n = ", nrow(Dados)))
abline(reg = fitlm, lty = 2)

# Padronizado
fitr <- psych::setCor(MCT ~ Estatura, std = TRUE,
                      data = Dados)
dados2 <- as.data.frame(scale(Dados)) # padroniza dados
fitlm <- lm(MCT ~ Estatura, data = dados2)
print(fitr)
summary(fitlm) # mesmo resultado





# Regressao linear multipla ----

# Agora temos dois preditores, Estatura e Idade

# Nao padronizado
fitr <- psych::setCor(MCT ~ Estatura + Idade,
                      std = FALSE, data = Dados)
fitlm <- lm(MCT ~ Estatura + Idade, data = Dados)
print(fitr)
summary(fitlm) # mesmo resultado


# Padronizado
fitr <- psych::setCor(MCT ~ Estatura + Idade,
                      std = TRUE, data = Dados)
fitlm <- lm(MCT ~ Estatura + Idade, data = dados2)
print(fitr)
summary(fitlm) # mesmo resultado



# Usando semPaths para produzir o layout de "path analysis" ---------------

# O setCor ja faz algo parecido, porque ele calcula a regressao
# e ja mostra o path analysis.
# Agora, podermos criar o path analysis para a analise de regressao
# do lm usando o semPaths
# O semPaths pode receber diversos objetos, como lm, lavaan, principal
# factanal, loadings...


# Mostrando path da regressao multipla com lm
## Usar o argumento what = "path"

semPlot::semPaths(fitlm, layout = "tree",
                  style = "lisrel",
                  what = "path", residuals = TRUE, edge.color = "black",
                  edge.label.cex = 1,
                  whatLabels = "std", # mostra as cargas fatorias padronizadas
                  nCharNodes = 6, sizeMan = 8, sizeInt = 4,
                  fade = FALSE)

## Mostrando path, mas as arestas mudam em magnitude
# por causa do argumento what = "est"
semPlot::semPaths(fitlm, layout = "tree",
                  style = "lisrel",
                  what = "est",
                  residuals = TRUE, edge.color = "black",
                  edge.label.cex = 1, nCharNodes = 6, sizeMan = 8,
                  sizeInt = 4, fade = FALSE)


## Mostrando o path e parametros padronizados.
# Como o modelo ja esta com betas padronizados, fica igual o anterior
# mas a seta (que eh 0) do intercepto
semPlot::semPaths(fitlm, layout = "tree", style = "lisrel",
                  what = "std", residuals = TRUE, edge.color = "black",
                  edge.label.cex = 1, nCharNodes = 6, sizeMan = 8,
                  sizeInt = 4, fade = FALSE)

# Colocar o intercepto do lado
semPaths(fitlm, intAtSide = TRUE) 











# Usando lavaan -----------------------------------------------------------



# Exemplo 1 - regressao simples com intercepto 1 ----

regressao <- "MCT ~ 1 + Estatura" # o 1 representa o intercepto? Fiz sem o 1 e o resultado foi igual. Por que colocar o 1?
fitlv <- lavaan::sem(regressao, data = Dados) # funcao sem do lavaan esta performando um modelo de regressao simples. O primeiro argumento da funcao recebe uma formula

lavaan::summary(fitlv, fit = TRUE, std = TRUE,
                rsq = TRUE, ci = TRUE) # summary do objeto

# O que eh importante olhar no summary:
# Estimator (ML)
# number of model parameters (3, porque tem o beta da regressao, o intercepto do MCT e a variancia do MCT (na figura str, a variancia eh a seta 0.58))

# model test user model (0 graus de liberdade e estatistica de teste 0 significa que nao dah para testar o modelo como um todo. Isso signifca que o modelo desenhado nao eh um modelo autenticamente multivaridado porque o modelo nao eh testavel como um todo. Quando usamos a matriz de covariancia, e nao os dados, ai nao tem o teste onimbus. Modelos de regressao nao sao testatveis pelo metodo de equacoes estruutrais. Eu posso testar a regressao com os dados no jeito clasoico, com setCor ou lm)

# parameter estimates (mostra sobre as estimativas dos parametros da regressao. A carga fatorial (beta) esta no Std.all)

# Suboutputs do summary
lavaan::fitMeasures(fitlv, output = "text") # mostra as medidas de teste do modelo e varios indices
lavaan::parameterEstimates(fitlv)
lavaan::standardizedsolution(fitlv) # mesmo resultado que parameterEstimates, mas retorna tudo padronizado
lavaan::varTable(fitlv) # informacoes sobre as variaveis

# Plotando o diagrama

## Nao-padronizado
semPlot::semPaths(fitlv, layout = "tree",
                  style = "lisrel", what = "est", residuals = TRUE,
                  edge.color = "black", edge.label.cex = 1,
                  nCharNodes = 6, sizeMan = 8, sizeInt = 4,
                  curvePivot = TRUE, intAtSide = TRUE,
                  rotation = 4, fade = FALSE)

# Padronizado
semPlot::semPaths(fitlv, layout = "tree",
                  style = "lisrel",
                  what = "std", # padronizado
                  residuals = TRUE,
                  edge.color = "black", edge.label.cex = 1,
                  nCharNodes = 6, sizeMan = 8, sizeInt = 4,
                  curvePivot = TRUE, intAtSide = TRUE,
                  rotation = 4, fade = FALSE)
# O que sao esses dois triangulos? O que significa essas linhas

# sem nenhuma carga fatorial presente
semPlot::semPaths(fitlv, layout = "tree",
                  style = "lisrel", what = "path", residuals = TRUE,
                  edge.color = "black", edge.label.cex = 1.5,
                  curvePivot = TRUE, nCharNodes = 6, sizeMan = 8,
                  sizeInt = 4, intAtSide = TRUE, rotation = 4,
                  fade = FALSE)






# Exemplo 2 - Regressao simples sem intercepto ----

regressao <- "MCT ~ Estatura"
fitlv <- lavaan::sem(regressao, data = Dados) # funcao sem do lavaan esta performando um modelo de regressao simples. O primeiro argumento da funcao recebe uma formula

lavaan::summary(fitlv, fit = TRUE, std = TRUE,
                rsq = TRUE, ci = TRUE) # summary do objeto

# Padronizado
semPlot::semPaths(fitlv, layout = "tree",
                  style = "lisrel",
                  what = "std", # padronizado
                  residuals = TRUE,
                  edge.color = "black", edge.label.cex = 1,
                  nCharNodes = 6, sizeMan = 8, sizeInt = 4,
                  curvePivot = TRUE, intAtSide = TRUE,
                  rotation = 4, fade = FALSE)
# resultado bem parecido com o setCor padronizado. A diferenca
# eh que o lavaan mostra a variancia(?) da variavel resposta








# Exemplo 3 - Regressao multipla

regressao <- "MCT ~ Estatura + Idade"
fitlv <- lavaan::sem(regressao, data = Dados)

lavaan::summary(fitlv, fit = TRUE, std = TRUE,
                rsq = TRUE, ci = TRUE) # summary do objeto

lavaan::standardizedsolution(fitlv)

# Padronizado
semPlot::semPaths(fitlv, layout = "tree",
                  style = "lisrel",
                  what = "std", # padronizado
                  residuals = TRUE,
                  edge.color = "black", edge.label.cex = 1,
                  nCharNodes = 6, sizeMan = 8, sizeInt = 4,
                  curvePivot = TRUE, intAtSide = TRUE,
                  rotation = 4, fade = FALSE)





# Exemplo 4 - regressao multipla com 3 preditores

## Exemplo do Latent Variable Modeling Using R - Beaujean - 2014
n <- 1000

# matriz de correlacao
regression.cor.lwr <- c(1
                        ,0.2, 1,
                        0.24, 0.3, 1,
                        0.7, 0.8, 0.3, 1)

# desvios-padrao das variaveis
regression.sd <- c(10, 30, 20, 5)
nomes <- c("X1", "X2", "X3", "Y")

# criando matriz de correlacao simetrica
regression.cor <- lavaan::getCov(regression.cor.lwr, names = nomes)
regression.cor

# criando matriz de covariancia a partir da matriz de correlacao
## funcao cor2cov, usando os desvios-padrao
regression.cov <- lavaan::cor2cov(R = regression.cor, sds = regression.sd)

# escrevendo modelo de regressao. Aqui temos termos que precisam ser
# estimados. Eh possivel comentar dentro da formula com ####
regression.model <- "
# path model for Y
Y ~ a*X1 + b*X2 + c*X3
# label the residual variance of Y
Y ~~ z*Y
"
regression.fit <- lavaan::sem(regression.model, 
                              sample.cov = regression.cov, # entrada com a matriz de covariancia
                              sample.nobs = n) # precisa passar o n
lavaan::varTable(regression.fit)
lavaan::summary(regression.fit, fit = TRUE, std = TRUE, rsq = TRUE)

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
