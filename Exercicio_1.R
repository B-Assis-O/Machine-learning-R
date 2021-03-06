#Exerc�cio_1 - pre-processamento

#Definir �rea de trabalho

setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Pre-processamento")

#Importando base de dados

df <- read.csv("2015-building-energy-benchmarking.csv")


#- Exercicio:- Percentual de dados faltantes maior que 1
#            - Alterar NAs na vari�vel ENERGYSTARcore para sua mediana
# Desafio 

  #Os percentuais encontrados estar�o errados
View(df)
str(df)

summary(df$Outlier)
df$Outlier[df$Outlier == ""]<- NA
summary(df$Outlier)

df[df == ""] <- NA


#Resolu��o do exerc�cio

NAs <- round(colSums(is.na(df))*100/nrow(df), 2)
NAs
NAs[NAs>1]


median(df$ENERGYSTARScore, na.rm = TRUE)
df$ENERGYSTARScore[is.na(df$ENERGYSTARScore)] <- median(df$ENERGYSTARScore, na.rm = TRUE)
View(df)
View(df$ENERGYSTARScore)

is.na(df$ENERGYSTARScore)
any(is.na(df$ENERGYSTARScore))


#Desafio 
df <- read.csv("2015-building-energy-benchmarking.csv", na.strings = "")
