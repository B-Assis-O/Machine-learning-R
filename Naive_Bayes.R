# Definir area de trabalho 
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Naive_Bayes")

# Naive bayes

# carregando data set

df <- iris

install.packages("naivebayes")
library(naivebayes)

?naive_bayes
 
library(caret)

#criando modelo
set.seed(1)
modelo_Naive <- train(Species ~ . ,
                      data = df,
                      method = "naive_bayes",
                      trControl = trainControl(method = 'cv',
                                               number = 5))

mean(modelo_Naive$resample$Accuracy)
