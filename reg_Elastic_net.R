library(glmnet)
library(dplyr)

setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Modelos_reg_linear")

df <- read.csv("kc_house_data.csv") %>%
  select(-id,-date,-sqft_basement,-zipcode,-lat,-long)
df <- as.matrix(df)
View(df)
?glmnet
#Para fazer o modelo elastic net: 0< alpha < 1
#quanto maior mais proximo da Lasso e quanto menor mais proximo da ridge

#Separando dados de treino e teste
set.seed(1)
filtro <- sample(1:nrow(df), nrow(df)*0.7)

x_treino <- df[filtro,-1]
y_treino <- df[filtro, 1]

x_teste <- df[-filtro, -1]
y_teste <- df[-filtro, 1]


#criando o modelo de regressão Elastic net 
# 0 < alpha < 1 -> Elastic net 

modelo <- glmnet(x_treino, y_treino, alpha = 0.5, lambda = 1)

#Prevendo os preços no dataset de teste
prev <- predict(modelo, x_teste)

#Calculando R2 
SQt = sum((mean(y_teste)- y_teste)**2)
SQres = sum((prev- y_teste)**2)

R2Elastic_net = (SQt - SQres)/SQt
R2Elastic_net
