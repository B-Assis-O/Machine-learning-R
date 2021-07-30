# Definir area de trabalho 
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/KNN")

#install.packages("mclust")
library(mclust)
library(caret)

# Carregando dataset 
df <- wdbc
df$ID <- NULL
View(df)

# Criando matriz com linhas dos dados de treino - 70%
set.seed(1)
filtro <- createDataPartition(y = df$Diagnosis, p = 0.7, list = FALSE)

# dados de treino e de teste
treino <- df[filtro,]
teste <- df[-filtro,]

# http://topepo.github.io/caret/available-models.html

# criando o modelo
set.seed(1)
modelo <- train(Diagnosis ~ .,
                data = treino,
                method = 'knn',
                preProcess = "scale")

mean(modelo$results$Accuracy)
modelo$bestTune$k
modelo$results

# Confusion Matrix 
Prev <- predict(modelo, teste)
confusionMatrix(Prev, 
                teste$Diagnosis,
                dnn = c('Previsto', 'Real'))




