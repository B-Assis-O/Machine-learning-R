# Definir area de trabalho 
setwd("C:/Users/bassi/Documents/Curso_Machine_Learning_R/Feature_selection")

# Como melhorar o processamento de um algoritmo no computador

#Processamento paralelo

install.packages("doParallel")
library(doParallel)

detectCores(logical = FALSE)

nucleos <- makePSOCKcluster(2)

registerDoParallel(nucleos)

#Modelo 

##Para encerrar 
registerDoSEQ()