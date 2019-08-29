#-----------------------------------------------------------------------
# Extensões de modelos de regressão · CE 092
# leg.ufpr.br/ensino/EMR
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                       Prof. Dr. Paulo Justiniano R. Jr
#
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Ago-29 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------


## limpando área de trabalho de outros objetos
## CUIDADO se quer de fato fazer isto!
rm(list=ls())
##
## Lendo dados
##
df <- read.table("http://www.leg.ufpr.br/~paulojus/CE092/df02.txt", head=TRUE, sep="")
#df <- read.table("df02.txt", header=TRUE)
with(df, plot(Y1 ~ x))
nd <- data.frame(x = seq(0, 2, length=201))
##
## 1. Modelo de regressão linear simples
##
fit01 <- lm(Y1 ~ x, data=df)
nd$pred01 <- predict(fit01, new=nd)
with(nd, lines(pred01 ~ x))
##
##
## 2. Modelos de regressão segmentada com 1 ponto de corte (fixado)
## (x-1.7)+
fit02 <- lm(Y1 ~ x + I((x-1.7)*(x>=1.7)), data=df)
nd$pred02 <- predict(fit02, new=nd)
with(nd, lines(pred02 ~ x, col=2))

## 2a. parametrização alternativa do modelo anterior
fit02a <- lm(Y1 ~ I((x-1.7)*(x<1.7)) + I((x-1.7)*(x>=1.7)), data=df)
nd$pred02a <- predict(fit02a, new=nd)
with(nd, lines(pred02a ~ x, col=4, lty=2))

## verificando que os modelos possuem ajustes idênticos 
logLik(fit02)
logLik(fit02a)

## e o que muda é a interpretação dos parâmetros
summary(fit02)
summary(fit02a)

## gráficos dos modelos ajustados até aqui agora com as bandas de predição
with(df, plot(Y1 ~ x))
nd$pred01 <- predict(fit01, new=nd, interval="pred")
with(nd, matlines(x,pred01, col=1, lty=2))


nd$pred02 <- predict(fit02, new=nd, interval="pred")
with(nd, matlines(x, pred02, col=2, lty=2))

nd$pred02a <- predict(fit02a, new=nd, interval="pred")
with(nd, matlines(x,pred02a, col=4, lty=2))
##
## 3. Modelo com dois pontos de corte (fixados)
##    em (x-0.5)+ e (x-1,7)+
fit03 <- lm(Y1 ~ x +  I((x-0.5)*(x>=0.5)) + I((x-1.7)*(x>=1.7)), data=df)
nd$pred03 <- predict(fit03, new=nd, interval="pred")
with(nd, lines(pred03[,1] ~ x, col=3))
with(nd, matlines(x,pred03, col=3, lty=2))

##
## Comparando ajustes
##
logLik(fit01)
logLik(fit02)
logLik(fit03)

anova(fit01, fit02, fit03)

## 
## 4. Definindo vários nós (pontos de corte) ainda fixados
##
k <- seq(0, 1.6, by=0.4)
xk.f <- function(x, c) ifelse(x<c, 0, x-c)
df$XC <- outer(df$x, k, xk.f)
nd$XC <- outer(nd$x, k, xk.f)

fit04 <- lm(Y1 ~ XC, data=df)
nd$pred04 <- predict(fit04, new=nd, interval="pred")

with(df, plot(Y1 ~ x))
with(nd, lines(x,pred04[,1], col=2, lty=2))
with(nd, matlines(x,pred04, col=2, lty=2))

fit04
logLik(fit04)

## Funções base utilizadas ("Basis functions")
with(df, matplot(x, XC, type="l"))

##
## 5. do linear ao  cúbico - regressão por splines
##    modelo com spline cúbico com um ponto de corte (fixado)
with(df, plot(Y1 ~ x))

## 5a. modelo com spline linear (reg. segmentada) com ponto de corte em 1.0
#df$x1.0 <- ifelse(df$x < 1, 0, (df$x-1))
#nd$x1.0 <- ifelse(nd$x < 1, 0, (nd$x-1))
#fit05a <- lm(Y1 ~ x + x1.0, data=df)

fit05a <- lm(Y1 ~ x + I((x-1.0)*(x>=1.0)), data=df)
nd$pred05a <- predict(fit05a, new=nd, interval="pred")
with(nd, lines(pred05a[,1] ~ x, col=2))

## 5b. modelo com spline cúbico com ponto de corte em 1.0
#df$x1.0c <- ifelse(df$x < 1, 0, (df$x-1)^3)
#nd$x1.0c <- ifelse(nd$x < 1, 0, (nd$x-1)^3)
#fit05b <- lm(Y1 ~ x + I(x^2) + I(x^3) + x1.0c, data=df)
#fit05b <- lm(Y1 ~ poly(x, 3) + I(x1.0^3), data=df)

fit05b <- lm(Y1 ~ poly(x, 3) + I(I((x-1.0)^3)*(x>=1.0)), data=df)
nd$pred05b <- predict(fit05b, new=nd, interval="pred")
with(nd, lines(pred05b[,1] ~ x, col=4))

with(df, plot(Y1 ~ x))
with(nd, matlines(x,pred05a, col=2, lty=2))
with(nd, matlines(x,pred05b, col=4, lty=2))

## função base do modelo cúbico
#with(nd, plot(x, x1.0c, type="l"))
with(nd, plot(x, I(I((x-1.0)^3)*(x>=1.0)), type="l"))

##
## 5c. spline cúbico com múltiplos nós
##
k <- seq(0.4, 1.6, by=0.4)
x3k.f <- function(x, c) ifelse(x<c, 0, (x-c)^3)
df$X3C <- outer(df$x, k, x3k.f)
nd$X3C <- outer(nd$x, k, x3k.f)

fit05c <- lm(Y1 ~ X3C, data=df)
nd$pred05c <- predict(fit05c, new=nd, interval="pred")

with(df, plot(Y1 ~ x))
with(nd, lines(x,pred04[,1], col=2, lty=2))
with(nd, lines(pred05c[,1] ~ x, col=5))
with(nd, matlines(x,pred05c, col=5, lty=2))

fit05c
logLik(fit05c)

## Funções base
with(df, matplot(x, X3C, type="l"))


##
## 6. (um) Ponto de corte desconhecido
##

## 6.a Perfis de verossimilhança para splines linear e cúbico
Ks <- seq(0.05, 1.95, by=0.001)

lL.lin <- lL.cub <- numeric(length(Ks))
for(i in 1:length(Ks)){
    df$x.pos <- ifelse(df$x < Ks[i], 0, df$x - Ks[i])
    lL.lin[i] <- logLik(lm(Y1 ~ x + x.pos, data=df))
    lL.cub[i] <- logLik(lm(Y1 ~ poly(x, 3, raw=TRUE) + I(x.pos^3), data=df))
}

plot(Ks, lL.lin, ylim = range(c(lL.lin,lL.cub)))
lines(Ks, lL.cub)

(K.lin <- Ks[which.max(lL.lin)])
(K.cub <- Ks[which.max(lL.cub)])
abline(v=c(K.lin, K.cub))

##
## Outros tópicos:
##  - uso de pacotes para regressões segmentadas como "segmented" e "strucchange"
##  - ajuste estimando ponto de corte como modelo não linear
##  - outros exemplos de dados como comportamento não monotônico e maior número de dados
##  - gráficos de resíduos

set.seed(1029)
sim01 <- data.frame(x = c(1:10, 13:22), y = numeric(20))
sim01$y[1:10] <- 20:11 + rnorm(10, 0, 1.5)
sim01$y[11:20] <- seq(11, 15, len=10) + rnorm(10, 0, 1.5)
with(sim01, plot(x,y))
