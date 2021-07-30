library(glmnet)
library(dplyr)

setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Modelos_reg_linear")

df <- read.csv("admission_predict.csv") %>%
  select(-Serial.No.)
View(df)

df <- as.matrix(df)

any(is.na(df))

#Separando dados de treino e teste
set.seed(1)
filtro <- sample(1:nrow(df), nrow(df)*0.7)


#Chance of admit eh a coluna 8:
x_treino <- df[filtro,-8]
y_treino <- df[filtro, 8]

x_teste <- df[-filtro, -8]
y_teste <- df[-filtro, 8]

#Função que cria e compara modelos
modelos <- function(a,b,c,d,peso,elastic){
  SQt = sum((mean(y_teste) - y_teste)**2)
  R2 <- data.frame(Linear = NA)
  
  #Linear 
  linear <- glmnet(a,b,lambda = 0)
  PrevLinear <- predict(linear, c)
  R2$Linear <- (SQt - sum((PrevLinear- d)**2))/SQt
  
  #Ridge
  Ridge <- glmnet(a,b,alpha = 0,lambda = peso)
  PrevRidge <- predict(Ridge, c)
  R2$Ridge <- (SQt - sum((PrevRidge- d)**2))/SQt
  
  #Lasso
  Lasso <- glmnet(a,b,alpha = 1,lambda = peso)
  PrevLasso <- predict(Lasso, c)
  R2$Lasso <- (SQt - sum((PrevLasso- d)**2))/SQt
  
  #Elastic
  Elastic <- glmnet(a,b,alpha = elastic,lambda = peso)
  PrevElastic <- predict(Elastic, c)
  R2$Elastic <- (SQt - sum((PrevElastic- d)**2))/SQt
  
  print(R2)
  
}

modelos(x_treino,y_treino,x_teste,y_teste,1,0.5)
