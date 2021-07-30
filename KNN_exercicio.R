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
                trControl = trainControl(method = "cv", number = 5),
                preProcess = 'scale')

mean(modelo$results$Accuracy)
modelo$bestTune$k
modelo$results

# Confusion Matrix 
Prev <- predict(modelo, df)
confusionMatrix(Prev, 
                df$Species,
                dnn = c('Previsto', 'Real'))
