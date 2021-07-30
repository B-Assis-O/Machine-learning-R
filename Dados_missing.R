#Base de dados

df <- data.frame(Pessoa = seq(1:300),
                  Idade = NA,
                  Peso = NA,
                  Altura = NA,
                  Emprego = NA)
df$Idade <- sample(20:60, 300, replace = TRUE)
df$Peso <- sample(45:120, 300, replace = TRUE)
df$Altura <- sample(145:195, 300, replace = TRUE)
df$Emprego <- sample(0:1, 300, replace = TRUE)
df$Idade[1:4] <- NA
df$Peso[20:102] <- NA
df$Altura[108:250] <- NA
View(df)

#Tratando dados missing 

is.na(df)

?any

any(is.na(df))

df_1 <- na.omit(df)

nrow(df)
nrow(df_1)


#Percentual de dados faltantes
NAs <- round(colSums(is.na(df))*100/nrow(df), 2)
NAs
NAs[NAs>0]

colSums(is.na(df))
nrow(df)

4/300
83/300
143/300

NAs

df$Altura <- NULL

df$Peso[is.na(df$Peso)] <- mean(df$Peso, na.rm = TRUE)
mean(df$Peso)

df_1 <- na.omit(df)

nrow(df)
nrow(df_1)

#Percentual de dados faltantes
NAs <- round(colSums(is.na(df_1))*100/nrow(df_1), 2)
NAs

any(is.na(df_1))