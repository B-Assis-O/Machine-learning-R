# Definir area de trabalho 
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/KNN")

# carregando dataset
df <- iris

View(df)

library(caret)

# criando o modelo
set.seed(1)
modelo <- train( Species ~ .,
                 data = df,
                 method = 'knn',
                 tuneLength = 7,
                 trControl = trainControl(method = "cv", number = 5),
                 preProcess = 'scale')

modelo
mean(modelo$results$Accuracy)
modelo$bestTune$k
modelo$results

# criando modelo com normalização de dados center - pela média
modelo2 <- train( Species ~ .,
                 data = df,
                 method = 'knn',
                 tuneLength = 7,
                 trControl = trainControl(method = "cv", number = 5),
                 preProcess = 'center')

modelo2
mean(modelo2$results$Accuracy)
modelo2$bestTune$k
modelo2$results
