# Definindo area de trabalho
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Validacao_cruzada_e_ajuste_fino_dos_parametros")

df <- read.csv("admission_predict.csv")
df$Serial.No.<- NULL

library(caret)

# Criando o modelo
?trainControl

set.seed(1)
modelo <- train(Chance.of.Admit~.,
                data = df,
                method = "glmnet",
                trControl = trainControl(method = "cv", number = 5))

View(modelo)

#"cv" eh cross validation
# number = numero de folds, ou seja, o K

# Resultado 
modelo$resample$Rsquared
summary(modelo$resample$Rsquared)
mean(modelo$resample$Rsquared)

modelo$bestTune # Traz a melhor configuração de parametros do modelo, que nesse caso eh o glmnet

modelo$results
