#Feature selection: Exemplo c�lculo do coeficiente de correla��o de Pearson 

install.packages("mlbench")
library(mlbench)

data(PimaIndiansDiabetes)
df <- PimaIndiansDiabetes

cor(df)
str(df)

df$diabetes <- as.numeric(df$diabetes)

cor(df)
View(cor(df))

install.packages("corrplot")
library(corrplot)

corrplot(cor(df), method = "color")
?corrplot