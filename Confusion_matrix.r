# Definir area de trabalho 
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Confusion_Matrix_e_normalizacao")

#install.packages("mclust")
library(mclust)
library(caret)

# Carregando dataset 
df <- wdbc
df$ID <- NULL

# Quantidade de cada classe
table(df$Diagnosis)

# Proporção de cada classe
prop.table(table(df$Diagnosis))

# Criando matriz com linhas dos dados de treino - 70%
set.seed(1)
filtro <- createDataPartition(y = df$Diagnosis, p = 0.7, list = FALSE)

# dados de treino e de teste
treino <- df[filtro,]
teste <- df[-filtro,]

# criando o modelo
set.seed(1)
modelo <- train(Diagnosis ~ .,
                data = treino,
                method = 'glmnet',
                tuneLength = 4,
                trControl = trainControl(method = "cv",
                                         number = 5))

mean(modelo$resample$Accuracy)

Prev <- predict(modelo, teste)

View(data.frame(teste$Diagnosis, Prev))

# Confusion Matrix

#install.packages("gmodels")
library(gmodels)

CrossTable(Prev,
           teste$Diagnosis,
           dnn = c('Previsto', 'Real'),
           prop.chisq= FALSE,
           prop.t = FALSE,
           prop.r = FALSE,
           prop.c = FALSE)

?CrossTable

# caret
?confusionMatrix

confusionMatrix(Prev, 
                teste$Diagnosis,
                dnn = c('Previsto','Real'))


# Curva ROC/AUC - possibilita ver qual o melhor cenário de falsos positivos e verdadeiros negativos 
# para analisar e aplicar no dataset dependendo da situação que queira estudar.

# Probabilidades
PrevProb <- predict(modelo,teste, type= "prob")

View(round(PrevProb, 2))

PrevProb <- PrevProb$B

install.packages("pROC")
library(pROC)

# Sensibilidade = 1 
# TPR = Sensitivity = TP /(TP + FN)

# Especificidade = 1
# FPR = 1 - Specificity = FP / (FP + TN)

# Curva ROC 
ROC <- roc( teste$Diagnosis ~ PrevProb, levels = c("M","B"))

?roc

plot(ROC)

#AUC - Área sobre a curva 
ROC$auc


View(round(data.frame(ROC$sensitivities, 
                      ROC$specificities, 
                      ROC$thresholds), 2))

# Resultado do modelo 
confusionMatrix(Prev, 
                teste$Diagnosis,
                dnn = c('Previsto', 'Real'))

ROC$thresholds[109]

# Ajustando threshold para que só considere benigno quando houver 99% de chance de ser.
PrevT <- PrevProb
PrevT[PrevT>ROC$thresholds[109]] <- "B"
PrevT[PrevT != "B" ] <- 'M'
PrevT <- as.factor(PrevT)

# Resultado do modelo com threshold ajustado

confusionMatrix(PrevT,
                teste$Diagnosis,
                dnn = c('Previsto', 'Real'))
# Porém o modelo pode ser descartado pela sua acurácia, mesmo não tendo nenhum erro nos tumores malignos

# Desta forma, o ideal é procurar no data frame do ROC uma sensitividade que atenda com a maior especificidade possível

ROC$thresholds[75]

PrevTt <- PrevProb
PrevTt[PrevTt>ROC$thresholds[75]] <- "B"
PrevTt[PrevTt != "B" ] <- 'M'
PrevTt <- as.factor(PrevTt)

# Resultado do modelo com threshold ajustado

confusionMatrix(PrevTt,
                teste$Diagnosis,
                dnn = c('Previsto', 'Real'))
