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

df$ID <- NULL

anyNA(df)

# Criando o modelo
set.seed(1)

modelo <- train(Diagnosis ~ ., 
                data = df,
                method = "glmnet",
                trControl = trainControl(method = "cv",
                                         number = 5))
modelo

# Ajuste fino

View(modelo$results[1:3])
modelo$bestTune
mean(modelo$resample$Accuracy)

modelo1 <- train(Diagnosis ~ ., 
                data = df,
                method = "glmnet",
                tuneLength = 4, 
                trControl = trainControl(method = "cv",
                                         number = 5))

View(modelo1$results[1:3])
modelo1$bestTune
mean(modelo1$resample$Accuracy)
