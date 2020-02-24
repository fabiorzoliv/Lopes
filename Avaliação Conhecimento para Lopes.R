############################################
## Candidato: Fabio da Roza Olivera      ##
## Data: 16/02/2020                      ##



library(utf8)

library(ggplot2)
library(e1071)


library(corrgram)
library(psych)
library(Hmisc)



library(dplyr)


library(readr)
df_winequality <- read_delim("C:/Dados Lopes/winequality.csv", 
                          ";", escape_double = FALSE, na = "0", 
                          trim_ws = TRUE)


str(df_winequality)
#Temos problemas com a coluna alcohol, Podemos identificar que temos dados que apresentaram problemas na coleta, podemos desprezar estes dados ou aterá-los.

#Vou alterá-lo na fonte, através da colocação do formato correto. Inferindo que o número 923.333.333.333.333 é uma dizima mal formatada como as demais, irei alterar para 9,23 da mesma forma com os demais.

df_winequality <- read_delim("C:/Dados Lopes/winequality_alterada.csv", 
                             ";", escape_double = FALSE, na = "0", 
                             trim_ws = TRUE)

str(df_winequality)

library(knitr)


kable(df_winequality[1:6,], caption = " 5 primeiras linhas da base de dados")

#Vamos verificar se  temos dados repetidos

kable((psych::describe(df_winequality)) , caption = "Medidas Resumo")


df_winequality <- df_winequality[!duplicated(df_winequality), ]
dim(df_winequality)

table(df_winequality$quality)
#Podemos observar que a concetração maior esta nas categorias 5 e 6.

#Vamos analisar as correlações

Modelo_wine <- factor(df_winequality$type, levels=c("White","Red"))


n1 <- as.numeric(as.numeric(Modelo_wine)==1)
n2 <- as.numeric(as.numeric(Modelo_wine)==2)

df_winequality$White <- n1
df_winequality$Red <- n2

df_wine <- data.frame(c(select(df_winequality, 2:15)))

library(corrr) # Biblioteca


wine_cor <- corrr::correlate(df_wine) # Matriz de correlação

corrr::rplot(wine_cor) # Plot da correlação

attach(df_wine)

skewness(quality)
# 0.1473842

skewness(chlorides)
# 5.335227

skewness(free.sulfur.dioxide)
# 1.361951

skewness(residual.sugar)
# 1.705588

skewness(alcohol)
# 0.5453039

skewness(citric.acid)
# NA

skewness(density)
# 12.84366

skewness(fixed.acidity)
# 1.649487

skewness(volatile.acidity)
# 1.503709

skewness(total.sulfur.dioxide)
# 0.06357857

skewness(sulphates)
# 1.808434

skewness(pH)
# 0.3897493

#Analise da simetria identificou que os dados não são simétricos

colnames(df_wine)

#Vamos usar a transformação box cox, a fim de buscar a normalidade dos dados. 

library(caret)

pre_whine <- preProcess(df_wine[,1:14], c("BoxCox", "center", "scale"))
novo_whine <- data.frame(trans = predict(pre_whine, df_wine))

attach(novo_whine)

skewness(novo_whine$trans.quality)
# 0.1473842

skewness(novo_whine$trans.chlorides)
# -0.09537599

skewness(novo_whine$trans.free.sulfur.dioxide)
# -0.09729952

skewness(novo_whine$trans.residual.sugar)
# 0.3255514

skewness(novo_whine$trans.alcohol)
# 0.0591056

skewness(novo_whine$trans.citric.acid)
# NA

skewness(novo_whine$trans.density)
# 7.029143

skewness(novo_whine$trans.fixed.acidity)
# -0.08182574

skewness(novo_whine$trans.volatile.acidity)
# -0.009889683

skewness(novo_whine$trans.total.sulfur.dioxide)
# 0.06332424

skewness(novo_whine$trans.sulphates)
# 0.02114934

skewness(novo_whine$trans.pH)
# -0.008759978

#Vamos remover outliers, o modelo de analise de regressão é sensivel há valores outliers

novo_whine <- novo_whine[!abs(novo_whine$trans.fixed.acidity) > 3,]
novo_whine <- novo_whine[!abs(novo_whine$trans.volatile.acidity) > 3,]
novo_whine <- novo_whine[!abs(novo_whine$trans.citric.acid) > 3,]
novo_whine <- novo_whine[!abs(novo_whine$trans.residual.sugar) > 3,]
novo_whine <- novo_whine[!abs(novo_whine$trans.chlorides) > 3,]
novo_whine <- novo_whine[!abs(novo_whine$trans.density) > 3,]
novo_whine <- novo_whine[!abs(novo_whine$trans.pH) > 3,]
novo_whine <- novo_whine[!abs(novo_whine$trans.sulphates) > 3,]
novo_whine <- novo_whine[!abs(novo_whine$trans.alcohol) > 3,]


wine_cor_v1 <- corrr::correlate(novo_whine) # Matriz de correlação

corrr::rplot(wine_cor_v1) # Plot da correlação


set.seed(74)   

Index <- sample(1:nrow(novo_whine), 0.8*nrow(novo_whine))  
whinetrain <- novo_whine[Index, ]  
whinetest  <- novo_whine[-Index, ]  

modelo_ln_v0 <- lm(trans.quality ~ trans.fixed.acidity +  trans.volatile.acidity  + trans.citric.acid  +
                     trans.residual.sugar  + trans.chlorides +  trans.free.sulfur.dioxide  + trans.total.sulfur.dioxide + 
                     trans.density  + trans.pH   + trans.sulphates  + trans.alcohol   , whinetrain)  
summary(modelo_ln_v0)

library(tidyverse)
library(caret)


car::vif(modelo_ln_v0)



# Após a analise de Multicolinearidade, podemos retirar trans.density, nosso modelo explica apenas aproximadamente 31,12% dos dados, muito baixo.


modelo_ln_v1 <- lm(trans.quality ~ trans.fixed.acidity +  trans.volatile.acidity  + trans.citric.acid  +
                     trans.residual.sugar  + trans.chlorides +  trans.free.sulfur.dioxide  + trans.total.sulfur.dioxide + 
                      trans.pH   + trans.sulphates  + trans.alcohol   , whinetrain)  
summary(modelo_ln_v1)

#Observamos que para a significância de 5% podemos tirar as variáveis, fixed.acidity, citric.acid , pH

modelo_ln_v2 <- lm(trans.quality ~  trans.volatile.acidity  +  trans.residual.sugar  + trans.chlorides + 
                     trans.free.sulfur.dioxide  + trans.total.sulfur.dioxide +  trans.sulphates  + trans.alcohol   , whinetrain)  
summary(modelo_ln_v2)

car::vif(modelo_ln_v2)

# Após a analise de Multicolinearidade, observamos que não temos mais variáveis a serem retiradas, nosso modelo explica apenas aproximadamente 30,95% dos dados, muito baixo.

#Vamos fazer a analise dos residuais para a v2

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

plot(modelo_ln_v2)

# A analise dos residuais não mostra uma normalidade na sua distribuição, vamos tentar uma nova abordagem.

df_winequality_v1 <- read_delim("C:/Dados Lopes/winequality_alterada.csv", 
                             ";", escape_double = FALSE, na = "0", 
                             trim_ws = TRUE)

df_winequality_v1 <- df_winequality_v1[!duplicated(df_winequality_v1), ]


Modelo_wine_v1 <- factor(df_winequality_v1$type, levels=c("White","Red"))


n1 <- as.numeric(as.numeric(Modelo_wine_v1)==1)
n2 <- as.numeric(as.numeric(Modelo_wine_v1)==2)

df_winequality_v1$White <- n1
df_winequality_v1$Red <- n2

df_wine_v1 <- data.frame(c(select(df_winequality_v1, 2:15)))

df_wine_v1$categoria[df_wine_v1$quality <= 5] <- 0
df_wine_v1$categoria[df_wine_v1$quality > 5] <- 1

df_wine_v1$categoria <- as.factor(df_wine_v1$categoria)
#Criada a categoria para fazermos 



library(knitr)
kable(df_wine_v1[1:6,], caption = " 5 primeiras linhas da base de dados")

#Vou fazer uma regressão logistica, da familia binomial e vamos analisar o resultado

library(caTools)
split_mod_log = sample.split(df_wine_v1$categoria, SplitRatio = 0.7)

whine_train_log = subset(df_wine_v1, split_mod_log==TRUE)
whine_test_log = subset(df_wine_v1, split_mod_log==FALSE)


modelo_glm <- glm(categoria ~ . - quality -Red -White -chlorides-fixed.acidity -density - citric.acid -pH,
                  data = whine_train_log, family=binomial(link = "logit"))
summary(modelo_glm)

mstep <- step(modelo_glm)

pred_mod <- ifelse(predict(mstep, type = "response") > 0.5,"Bom", "Ruim")
head(pred_mod)

summary(pred_mod)

matt <- matrix(predict(modelo_glm, type = "response"))

acuracia <- (sum((matt))/length(whine_train_log$categoria))
msg <- paste("Acurácia do modelo", " ", round(acuracia*100,2), "%")
print(msg)


#Pela analise de regressão o modelo melhorou foi para 61,49% de acurácia

#Aplicando o modelo para os dados de teste


modelo_glm <- glm(categoria ~ . - quality -Red -White -chlorides-fixed.acidity -density - citric.acid -pH,
                  data = whine_test_log, family=binomial(link = "logit"))
summary(modelo_glm)


mstep <- step(modelo_glm)

pred_mod <- ifelse(predict(mstep, type = "response") > 0.5,"Bom", "Ruim")
head(pred_mod)

summary(pred_mod)

matt <- matrix(predict(modelo_glm, type = "response"))


acuracia <- (sum((matt))/length(whine_test_log$categoria))
msg <- paste("Acurácia do modelo", " ", round(acuracia*100,2), "%")
print(msg)


#Pela analise de regressão o modelo melhorou foi para 61,47% de acurácia

#Podemos dizer que a regressão linear binomial, para os modelos executados é a melhor opção, porém, um modelo que explique mais casos é bem melhor, ao menos que fique acima de 90%.
