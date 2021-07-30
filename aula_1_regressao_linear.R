#Aula 1 regressão linear:
 #criando uma reta

peso <- c(45,50,60,55,58,56,48)
altura <- c(1.54,1.55,1.65,1.60,1.65,1.63,1.60)

plot(peso,altura)
lm(altura ~ peso)
abline(lm(altura ~ peso))

#gerando dados:

x <- 1:250
x

rnorm(5)   #gerando 5 dados aleatorios 

y <- rnorm(250)

plot(x,y)

y <- rnorm(250) + x

plot(x,y)

rnorm(5, sd = 30)  #gerando 5 dados aleatorios com desvio padrao igual a 30

y <- rnorm(250, sd = 25) + x

plot(x,y)


?rnorm #help para saber a função

hist(rnorm(250, sd=25))

hist(rnorm(250, mean= 1000, sd= 25))

#definindo o número randômico:
set.seed(1)
  
y <- x + rnorm(250, sd=25)

#exibindo a relação
plot(x,y)

#criando o modelo:

?lm

# https://www.rdocumentation.org/

lm(y ~ x)
# y = m*x + b
# Resultado - b - coeficiente linear =  3.3400 -> x=0
# "     "   - m - coeficiente angular = 0.9778 -> inclinação da reta

#inserindo a reta no gráfico
abline(3.3400,0.9778)
#ou
abline(lm(y ~ x))

#gráfico melhorado 
plot(x,y, pch = 19, col = "blue")
abline(lm(y ~ x),col = "red", lwd = 2)
