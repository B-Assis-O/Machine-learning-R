# Definir area de trabalho 
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Feature_selection")

Xa <- c(12, 15, 16, 5, 4,2)
Xb <- c(2, 11, 8, 3, 14,5)
Xc <- c(30, 6, 90, 20, 5,70)
Y <- c(1, 1, 1, 0, 0,0)

df <- data.frame(Xa,Xb,Xc,Y)

?chisq.test

df

library(dplyr)

df<- df %>%
  group_by(Y) %>%
  summarise(Xa = sum(Xa),
            Xb = sum(Xb),
            Xc = sum(Xc))
df
df$Y <- NULL

chisq.test(df$Xa)
chisq.test(df$Xb)
chisq.test(df$Xc)

?apply

result <- apply(df, 2, chisq.test) #2 por causa de calcular por coluna 
#1 para linha

result$Xa$statistic
result$Xb$statistic
result$Xc$statistic

#verificando qui-quadrado no dataset iris
df <- iris
head(df)
summary(df$Species)

df<- df %>%
  group_by(Species) %>%
  summarise(SL = sum(Sepal.Length),
            SW = sum(Sepal.Width),
            PL = sum(Petal.Length),
            PW = sum(Petal.Width))

df
df$Species <- NULL

apply(df, 2, chisq.test)
