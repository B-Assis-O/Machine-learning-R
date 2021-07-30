setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R")

df <- read.csv("kc_house_data.csv")

View(df)

df$id <- NULL
df$date <- NULL
df$lat <- NULL
df$long <- NULL
df$zipcode <- NULL
df$sqft_basement <- NULL

library(caret)

filtro <- createDataPartition(df$price, p = 0.7, list= FALSE)
set.seed(1)

treino <- df[filtro,]
teste <- df[-filtro,]


modelo <- lm(price ~ ., treino)
modelo
View(modelo)

modelo$residuals
treino$r <- abs(modelo$residuals)

teste$previsao <- predict(modelo, teste)


SQt = sum((mean(teste$price)- teste$price)**2)
SQres = sum((teste$previsao- teste$price)**2)

R2 = (SQt - SQres)/SQt
R2
