# Definindo area de trabalho
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Validacao_cruzada_e_ajuste_fino_dos_parametros")

df <- read.csv("admission_predict.csv")
df$Serial.No.<- NULL

library(caret)

# Criando o modelo
?trainControl


controle <- trainControl(method = "cv", number = 5)
set.seed(1)
modelo <- train(Chance.of.Admit~.,
                data = df,
                method = "glmnet",
                tuneLength = 10,
                trControl = controle,
                metric = "Rsquared")

# tuneLenght define a quantidade de combinacoes que pode testar 
# metric -> avaliar o modelo com base naqueles resultados

modelo$bestTune
mean(modelo$resample$Rsquared)

View(modelo$results[c(1,2,3,4)])

summary(modelo$resample$Rsquared)

set.seed(1)
modelo1 <- train(Chance.of.Admit~.,
                data = df,
                method = "glmnet",
                tuneGrid = data.frame(alpha = 0.2, lambda = 0.003781034),
                trControl = controle)

# tuneGrid -> define os parametros 
# obs.: pode ser repassado uma sequencia de valores
modelo1$results$Rsquared
mean(modelo1$results$Rsquared)

# Ajuste fino

# - com base no resultado do modelo, criaremos uma sequencia de valores onde obteve-se os melhores resultados

seq(0.001, 0.009,length.out = 20)
seq(0.2, 0.5,length.out = 5)

lambda <- seq(0.001, 0.009,length.out = 20)
alpha <- seq(0.2, 0.5,length.out = 5)

valores <- expand.grid(alpha = alpha,
                       lambda = lambda)
# Gera os valores em combinacao
View(valores)

set.seed(1)
modelo <- train(Chance.of.Admit~.,
                 data = df,
                 method = "glmnet",
                 tuneGrid = valores,
                 trControl = controle,
                 metric = "Rsquared")

modelo$bestTune
modelo1$results$Rsquared
mean(modelo$resample$Rsquared)
summary(modelo$resample$Rsquared)

print(c(
  paste("Resultado do primeiro modelo:", 
        round(modelo1$results$Rsquared*100,2),
        "%"),
  paste("Resultado após ajuste:", 
        round(mean(modelo$resample$Rsquared)*100,2),
        "%")
  ))
