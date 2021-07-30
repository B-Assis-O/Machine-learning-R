#Parte 2
#Separando dados de treino e teste
#install.packages("caret")
library(caret)

#http://www.rdocumentation.org/

?createDataPartition

x <- 1:250
y <- rnorm(250, sd= 25) + x

#criando matriz com linhas dos dados de treino - 70%
filtro <- createDataPartition(x, p= 0.7, list=FALSE)
View(filtro)

#dados de treino e teste 
teste <- data.frame(x,y)
#matriz [linha, coluna]
treino <- teste[filtro,]
teste <- teste[-filtro,]

#exibindo a relação dos dados 
plot(treino$x, treino$y, pch = 1, col="blue")
points(teste$x, teste$y, pch= 19, col="black")

#criando o modelo
modelo <- lm(y ~ x, treino)

modelo

#criando o modelo com um código diferente 
#A notação do ponto serve para indicar todas as variáveis do data frame

modelo <- lm(y ~ ., treino)

modelo

View(modelo)

summary(modelo)

modelo$residuals
treino$r <- abs(modelo$residuals)

plot(treino$x, treino$y, pch = 1, col= "blue")
abline(modelo, col= "black", lwd = 2)
points(treino$x[treino$r>50], treino$y[treino$r>50], pch = 19, col = "red")

#reta com dados de treino 

plot(treino$x, treino$y, pch = 1, col = "blue")
abline(modelo, col = "black", lwd = 2 )

#Parte 3

#prevendo valores de y
#previsões dados de treino com a reta 

plot(teste$x, teste$y, pch=19, col="black")
abline(modelo, col = "black", lwd = 2)

?predict

teste$previsao <- predict(modelo, teste)

#previsões dados de treino  com valores 
points(teste$x, teste$previsao, pch = 19, col = "red")


#Calculo do coeficiente de determinação R2
#R2 = (SQt - SQres)/SQt

SQt = sum((mean(teste$y)- teste$y)**2)
SQres = sum((teste$previsao- teste$y)**2)

R2 = (SQt - SQres)/SQt
R2

#reta com valor médio de y
abline(mean(teste$y), 0, col="blue", lwd= 2)

#função rep = repetir valor (tal coisa, comprimento)
points(teste$x, rep(mean(teste$y),length(teste$x)), pch = 19, col= "blue")
