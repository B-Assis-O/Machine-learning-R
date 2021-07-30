# Definir area de trabalho 
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Decision_Trees")

#Arvore de decisao - regressao

df <- read.csv("Admission_Predict.csv")
df$Serial.No. <- NULL

# separando dados de treino e teste - 70% 
set.seed(123)
filtro <- sample(1:nrow(df), nrow(df)*0.7)
treino <- df[filtro,]
teste <- df[-filtro,]

library(rpart)

#criando o modelo 
modelo <- rpart(Chance.of.Admit ~.,
                data = treino)
#arvore
library(rpart.plot)
prp(modelo, extra = 1)

#previsao
prev <- predict(modelo, teste)
summary(as.factor(round(prev, 2)))


#Coeficiente de determinacao R2
SQt = sum((mean(teste$Chance.of.Admit) -  teste$Chance.of.Admit)**2)
SQres = sum((prev - teste$Chance.of.Admit)**2)
R2 = (SQt - SQres)/SQt 
R2


#Ajuste dos parametros 
library(caret)

set.seed(123)
modelo_2 <- train(Chance.of.Admit ~.,
                  data = df,
                  method = "rpart",
                  tuneGrid = data.frame(cp = c(0,0.01, 0.05, 0.1, 0.15, 0.2, 0.5 )),
                  trControl = trainControl(method = "cv", number = 5))

modelo_2

#Criando o modelo 
modelo_3 <- rpart(Chance.of.Admit ~ .,
                  data= treino,
                  control = rpart.control(cp = 0))

#arvore
prp(modelo_3, extra = 1)

#Previsoes
prev_cp <- predict(modelo_3, teste)

summary(as.factor(round(prev_cp, 2)))


#Coeficiente de determinacao R2
SQres_cp = sum((prev_cp - teste$Chance.of.Admit)**2)
R2_cp = (SQt - SQres_cp)/SQt 
R2_cp
R2
