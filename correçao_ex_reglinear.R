#Correcao do exercicio de Regressao linear

setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R")

library(dplyr)

df <- read.csv("kc_house_data.csv") %>%
  select(-id,-date,-sqft_basement,-zipcode,-lat,-long)

#separando dados de treino e  teste  - 70 %
set.seed(1)
filtro <- sample(1:nrow(df), nrow(df)*0.7)
treino <- df[filtro,]
teste <- df[-filtro,]

#criando o modelo de machine learning
modelo <- lm(price ~., treino)

#prevendo os preços no dataset de treino
prev <- predict(modelo, teste)

#calculando R2

SQt = sum((mean(teste$price)- teste$price)**2)
SQres = sum((prev- teste$price)**2)

R2 = (SQt - SQres)/SQt
R2

#detalhamento
summary(modelo)

# y = mx + b

modelo <- lm(price ~ bedrooms+bathrooms+floors, treino)
prev <- predict(modelo, teste)

summary(modelo)

teste[1,c(1,2,3,6)]
    prev[1]

# como tem mais variáveis, teremos mais coeficientes angulares
# y = b + m1*x1 + m2*x2 + m3*x3

modelo$coefficients

