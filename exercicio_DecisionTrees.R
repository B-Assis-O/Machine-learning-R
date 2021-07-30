# Definir area de trabalho 
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Decision_Trees")

#Arvore de decisao

#carregando dataset
df <- read.csv("column_2C_weka.csv")
View(df)
any(is.na(df))
nrow(df)
str(df)


#separando dados de treino e teste
set.seed(1)
filtro <- sample(1:nrow(df), nrow(df)*0.7)
treino <- df[filtro,]
teste <- df[-filtro,]

summary(df)
library(rpart)
library(rpart.plot)
library(caret)

#Criando o modelo
modelo <- rpart(class ~ ., 
                data = treino)

#resultado do dataset de teste
confusionMatrix(table(predict(modelo, teste, type = "class"), teste$class))$overall["Accuracy"]

prp(modelo, extra = 1)

modelo <- train(class ~.,
                data = treino,
                method = "rpart",
                tuneLength = 10)

modelo

#resultado do dataset de teste
confusionMatrix(table(predict(modelo, teste), teste$class))$overall["Accuracy"]


# buscando melhor cp
set.seed(1)
cp <- train(class ~ .,
            data = df,
            method = "rpart",
            trControl = trainControl(method = "cv", number = 5))

#cp <- train(class ~ .,
#            data = df,
#            method = "rpart",
#            TuneLength = 10,
#            trControl = trainControl(method = "cv", number = 5))
cp
cp$bestTune$cp
cp$resample$Accuracy
mean(cp$resample$Accuracy)

cp <- cp$bestTune$cp
#Criando o modelo
modelo <- rpart(class ~ ., 
                data = treino,
                control = rpart.control(cp = cp))

#resultado do dataset de teste
confusionMatrix(table(predict(modelo, teste, type = "class"), teste$class))$overall["Accuracy"]

#arvore de decisao
prp(modelo, extra = 1)
