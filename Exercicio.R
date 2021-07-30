# Definir area de trabalho 
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Regressao_logistica")

install.packages("mclust")
library(mclust)
library(dplyr)
library(caret)

?wdbc

# Carregando dataset 
df <- wdbc

View(df)
str(df)

#Percentual dos dados faltantes 
NAs <- round(colSums(is.na(df))*100/nrow(df),2)
NAs[NAs>0]

# Excluir Id
df$ID <- NULL

#funcao para verificar se ainda tem NA no data frame
anyNA(df)

# Criando o modelo
set.seed(1)

df$Diagnosis <- as.factor(df$Diagnosis)
modelo <- train(Diagnosis ~ ., 
                data = df,
                method = "glmnet",
                trControl = trainControl(method = "cv",
                                         number = 5))
modelo

# Ajuste fino

View(modelo$results[1:3])
modelo$bestTune$lambda

seq(0.1,1, length.out = 19)
alpha<- seq(0.1,1, length.out = 19)
lambda <- modelo$bestTune$lambda

ajuste <- expand.grid(alpha = alpha,
                      lambda = lambda)
#Modelo 1 
set.seed(1)
modelo1 <- train(Diagnosis ~ .,
                 data = df, 
                 method = 'glmnet',
                 tuneGrid = ajuste,
                 trControl = trainControl(method = 'cv',
                                          number = 5))

modelo1$bestTune
mean(modelo1$resample$Accuracy)
View(modelo1$results[1:3])


#Modelo 2

alpha <- c(0.25, 0.4)
seq(0.001, 1, length.out = 10)
lambda <- seq(0.001, 1, length.out = 10)

ajuste <- expand.grid(alpha = alpha,
                      lambda = lambda)
set.seed(1)
modelo2 <- train(Diagnosis ~ .,
                 data = df, 
                 method = 'glmnet',
                 tuneGrid = ajuste,
                 trControl = trainControl(method = 'cv',
                                          number = 5))
modelo2$bestTune
mean(modelo2$resample$Accuracy)
View(modelo2$results[1:3])

mean(modelo$resample$Accuracy)
mean(modelo1$resample$Accuracy)
mean(modelo2$resample$Accuracy)
