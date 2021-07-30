# Definir area de trabalho 
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Decision_Trees")

#Arvore de decisao

#carregando dataset

df <- iris

#separando dados de treino e teste
set.seed(123)
filtro <- sample(1:nrow(df), nrow(df)*0.7)
treino <- df[filtro,]
teste <- df[-filtro,]

install.packages("rpart")
library(rpart)

#criando o modelo 
modelo <- rpart(Species ~., data = treino)

library(caret)

#resultado do dataset de teste
confusionMatrix(predict(modelo, teste, type = "class"), teste$Species)$overall["Accuracy"]

res <- confusionMatrix(predict(modelo, teste, type = "class"), teste$Species)

res$overall["Accuracy"]

#visualizando a arvore de decisao
install.packages("rpart.plot")
library(rpart.plot)

prp(modelo)

?prp

prp(modelo, extra = 1)


# Ajuste dos parametros
modelo <- rpart(Species ~.,
                data = treino,
                control = rpart.control(minsplit = 2,cp = 0))

#arvore
prp(modelo, extra = 1)

#resultado do dataset de teste
confusionMatrix(predict(modelo, teste, type = "class"), teste$Species)$overall["Accuracy"]

?rpart

# caret
modelo <- train(Species ~.,
                data = treino,
                method = "rpart",
                tuneLength = 10)

modelo

#resultado do dataset de teste
confusionMatrix(predict(modelo, teste), teste$Species)$overall["Accuracy"]
