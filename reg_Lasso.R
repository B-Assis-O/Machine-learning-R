#Lasso
library(glmnet)
library(dplyr)

setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Modelos_reg_linear")

df <- read.csv("kc_house_data.csv") %>%
  select(-id,-date,-sqft_basement,-zipcode,-lat,-long)
df <- as.matrix(df)

#Separando dados de treino e teste
set.seed(1)
filtro <- sample(1:nrow(df), nrow(df)*0.7)

x_treino <- df[filtro,-1]
y_treino <- df[filtro, 1]

x_teste <- df[-filtro, -1]
y_teste <- df[-filtro, 1]


#Criando o modelo de regressão Lasso
#alpha = 1 -> Lasso
modelo <- glmnet(x_treino, y_treino, alpha = 1, lambda = 1)

#Prevendo os preços no dataset de teste
prev <- predict(modelo, x_teste)

#Calculando R2 
SQt = sum((mean(y_teste)- y_teste)**2)
SQres = sum((prev- y_teste)**2)

R2Lasso = (SQt - SQres)/SQt
R2Lasso

