# Definir area de trabalho 
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Feature_selection")

#p_value no dataset  iris
# Quanto menor o valor, mais relevante é a variavel

df <- iris

df$Species <- as.numeric(df$Species)
reg <- lm(Species ~ ., df)

#p - value Probabilidade que a variavel tem de não ser relevante

summary(reg)
