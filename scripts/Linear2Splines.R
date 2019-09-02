##
## De cálculo de médias a curvas suaves  
## Uma introdução à regressão por splines
##
## Simulando dados:
## modelo para simulação:
## Y_i = 0.2 x + cos(x+1) + e_i
## e_i \sim N(0, 10^2)
## cos(\cdot) em radianos
## x \sim U[0, 10]

set.seed(201802)
df <- data.frame(x = sort(runif(50, 0, 10)))
df <- transform(df, Ytrue = 0.2 * x + cos(x+1))
df <- transform(df, Y = Ytrue + rnorm(50, m=0, sd=0.5))

ndf <- data.frame(x = seq(0,10, l=2001))
ndf <- transform(ndf, Ytrue = 0.2 * x + cos(x+1))
ndf$x[ndf$x == 3.5] <- NA
ndf$x[ndf$x == 7] <- NA

with(df, plot(Y ~ x))
with(ndf, lines(Ytrue ~ x, lty=3))

##
##
##
(fit0 <- lm(Y ~ 1, data=df))
ndf <- transform(ndf, y0 = predict(fit0, newdata=ndf))
with(ndf, lines(y0 ~ x, col=2))

df <- transform(df, I1 = ifelse(x < 3.5, 0, 1),
                I2 = ifelse(x < 7, 0, 1))
ndf <- transform(ndf, I1 = ifelse(x < 3.5, 0, 1),
                 I2 = ifelse(x < 7, 0, 1))

(fit0.0 <- lm(Y ~ I1 + I2, data=df))
ndf <- transform(ndf, y0.0 = predict(fit0.0, newdata=ndf))
with(ndf, lines(y0.0 ~ x, col=2))

logLik(fit0.0)
logLik(fit0)

##
##
##
with(df, plot(Y ~ x))

(fit1 <- lm(Y ~ x, data=df))
ndf <- transform(ndf, y1 = predict(fit1, newdata=ndf))
with(ndf, lines(y1 ~ x, col=3))

df <- transform(df, x1 = ifelse(x < 3.5, 0, x-3.5),
                x2 = ifelse(x < 7, 0, x-7))
ndf <- transform(ndf, x1 = ifelse(x < 3.5, 0, x-3.5),
                 x2 = ifelse(x < 7, 0, x-7))

(fit1.0 <- lm(Y ~ x + I1 + x1 + I2 + x2, data=df))
ndf <- transform(ndf, y1.0 = predict(fit1.0, newdata=ndf))
with(ndf, lines(y1.0 ~ x, col=3))

(fit1.1 <- lm(Y ~ x + x1 + x2, data=df))
ndf <- transform(ndf, y1.1 = predict(fit1.1, newdata=ndf))
with(ndf, lines(y1.1 ~ x, col=3, lty=3, lwd=2))

logLik(fit1.0)
logLik(fit1.1)
logLik(fit1)

##
## Polinomios de 3o grau
## OBS: pode-se usar poly(..., raw=TRUE)
## ou, o mesmo ajuste com poly(...) ## raw=FALSE é o default
with(df, plot(Y ~ x))
with(ndf, lines(Ytrue ~ x, lty=3))

(fit3 <- lm(Y ~ poly(x, 3), data=df))
ndf <- transform(ndf, y3 = predict(fit3, newdata=ndf))
with(ndf, lines(y3 ~ x, col=1))

(fit3.0 <- lm(Y ~ poly(x, 3) + I1 + poly(x1, 3) + I2 + poly(x2, 3),
            data=df))
ndf <- transform(ndf, y3.0 = predict(fit3.0, newdata=ndf))
with(ndf, lines(y3.0 ~ x, col=2))

(fit3.1 <- lm(Y ~ poly(x, 3) + poly(x1, 3) + poly(x2, 3),
            data=df))
ndf <- transform(ndf, y3.1 = predict(fit3.1, newdata=ndf))
with(ndf, lines(y3.1 ~ x, col=3))

(fit3.2 <- lm(Y ~ poly(x, 3, raw=T) + poly(x1, 3, raw=T)[,-1] + poly(x2, 3, raw=T)[,-1],
            data=df))
ndf <- transform(ndf, y3.2 = predict(fit3.2, newdata=ndf))
with(ndf, lines(y3.2 ~ x, col=4))

(fit3.3 <- lm(Y ~ poly(x, 3) + I(x1^3) + I(x2^3),
            data=df))
ndf <- transform(ndf, y3.3 = predict(fit3.3, newdata=ndf))
with(ndf, lines(y3.3 ~ x, col=5))

logLik(fit3.0)
logLik(fit3.1)
logLik(fit3.2)
logLik(fit3.3)
logLik(fit3)

##
## Polinômio de 4o grau
##
with(df, plot(Y ~ x))
with(ndf, lines(Ytrue ~ x, lty=3))

(fit4 <- lm(Y ~ poly(x, 4), data=df))
ndf <- transform(ndf, y4 = predict(fit4, newdata=ndf))
with(ndf, lines(y4 ~ x, col=1))

##
## Entendendo as restrições de continuidade na função, na 1a e 2a derivadas
##
par(mfcol=c(3,6), mar=c(3,3,.5, .5))

ndf <- transform(ndf, y3.0d = c(diff(y3.0)/diff(x), NA))
ndf <- transform(ndf, y3.0dd = c(diff(y3.0d)/diff(x), NA))
with(df, plot(Y ~ x))
with(ndf, lines(y3.0 ~ x))
with(ndf, plot(y3.0d ~ x, type="l"))
with(ndf, plot(y3.0dd ~ x, type="l"))

ndf <- transform(ndf, y3.1d = c(diff(y3.1)/diff(x), NA))
ndf <- transform(ndf, y3.1dd = c(diff(y3.1d)/diff(x), NA))
with(df, plot(Y ~ x))
with(ndf, lines(y3.1 ~ x))
with(ndf, plot(y3.1d ~ x, type="l"))
with(ndf, plot(y3.1dd ~ x, type="l"))

ndf <- transform(ndf, y3.2d = c(diff(y3.2)/diff(x), NA))
ndf <- transform(ndf, y3.2dd = c(diff(y3.2d)/diff(x), NA))
with(df, plot(Y ~ x))
with(ndf, lines(y3.2 ~ x))
with(ndf, plot(y3.2d ~ x, type="l"))
with(ndf, plot(y3.2dd ~ x, type="l"))

ndf <- transform(ndf, y3.3d = c(diff(y3.3)/diff(x), NA))
ndf <- transform(ndf, y3.3dd = c(diff(y3.3d)/diff(x), NA))
with(df, plot(Y ~ x))
with(ndf, lines(y3.3 ~ x))
with(ndf, plot(y3.3d ~ x, type="l"))
with(ndf, plot(y3.3dd ~ x, type="l"))

ndf <- transform(ndf, y3d = c(diff(y3)/diff(x), NA))
ndf <- transform(ndf, y3dd = c(diff(y3d)/diff(x), NA))
with(df, plot(Y ~ x))
with(ndf, lines(y3 ~ x))
with(ndf, plot(y3d ~ x, type="l"))
with(ndf, plot(y3dd ~ x, type="l"))

ndf <- transform(ndf, y4d = c(diff(y4)/diff(x), NA))
ndf <- transform(ndf, y4dd = c(diff(y4d)/diff(x), NA))
with(df, plot(Y ~ x))
with(ndf, lines(y4 ~ x))
with(ndf, plot(y4d ~ x, type="l"))
with(ndf, plot(y4dd ~ x, type="l"))

par(mfrow=c(1,1))

##
## O pacote splines fornece funções que já montam as funções base
## (com isto é desnecessário criar as variáveis auxiliares)
##
with(df, plot(Y ~ x))
with(ndf, lines(y3.3 ~ x, col=4))

require(splines) ## para usar função bs()
(fit3.3bs <- lm(Y ~ bs(x, knots=c(3.5, 7.0)),
            data=df))
ndf <- transform(ndf, y3.3bs = predict(fit3.3bs, newdata=ndf))
with(ndf, lines(y3.3bs ~ x, col=6))

## melhor definir bordas no intervalo nas quais se vai fazer a predição
(fit3.3bs <- lm(Y ~ bs(x, knots=c(3.5, 7.0), Boundary.knots=c(0,10)),
            data=df))
ndf <- transform(ndf, y3.3bs = predict(fit3.3bs, newdata=ndf))
with(ndf, lines(y3.3bs ~ x, col=7))

##
## fixando graus de liberdade mudam-se os nós
##
with(df, plot(Y ~ x))
with(ndf, lines(y3.3bs ~ x, col=1))

with(ndf, lines(y3.3bs ~ x, col=6))

(fit3.3bs5 <- lm(Y ~ bs(x, df=5, Boundary.knots=c(0,10)),
            data=df))
ndf <- transform(ndf, y3.3bs5 = predict(fit3.3bs5, newdata=ndf))
with(ndf, lines(y3.3bs5 ~ x, col=2))
attr(with(df, bs(x, df=5, Boundary.knots=c(0,10))), "knots")

(fit3.3ns <- lm(Y ~ ns(x, knots=c(3.5, 7.0), Boundary.knots=c(0,10)),
            data=df))
ndf <- transform(ndf, y3.3ns = predict(fit3.3ns, newdata=ndf))
with(ndf, lines(y3.3ns ~ x, col=3))

(fit3.3ns5 <- lm(Y ~ ns(x, df=5, Boundary.knots=c(0,10)),
            data=df))
ndf <- transform(ndf, y3.3ns5 = predict(fit3.3ns5, newdata=ndf))
with(ndf, lines(y3.3ns5 ~ x, col=4))
attr(with(df, ns(x, df=5, Boundary.knots=c(0,10))), "knots")

logLik(fit3.3)
logLik(fit3.3bs)
logLik(fit3.3bs5)
logLik(fit3.3ns)
logLik(fit3.3ns5)

##
## E se fosse apenas 1 nó!??
##
with(df, plot(Y ~ x))
with(ndf, lines(y3.3bs ~ x, col=2))

require(splines) ## para usar função bs()
(fit3.5bs <- lm(Y ~ bs(x, knots=5, , Boundary.knots=c(0,10)), data=df))
ndf <- transform(ndf, y3.5bs = predict(fit3.5bs, newdata=ndf))
with(ndf, lines(y3.5bs ~ x, col=6))

logLik(fit3.3bs)
logLik(fit3.5bs)
logLik(fit4)

##
## Algumas comparações
##
with(df, plot(Y ~ x))
with(ndf, lines(Ytrue ~ x, lty=3))
with(ndf, lines(y3.3bs ~ x, col=2))
with(ndf, lines(y3.5bs ~ x, col=2))
with(ndf, lines(y3.3ns5 ~ x, col=4))
with(ndf, lines(y4 ~ x, col=1))

## intervalos de confiança e predição
with(df, plot(Y ~ x))
ndf <- transform(ndf, y3.3 = predict(fit3.3, newdata=ndf, interval="confidence"))
with(ndf, matlines(x, y3.3, col=2, lty=c(1,2,2)))
ndf <- transform(ndf, y3.3ns5 = predict(fit3.3ns5, newdata=ndf, interval="confidence"))
with(ndf, matlines(x, y3.3ns5, col=4, lty=c(1,2,2)))
ndf <- transform(ndf, y4 = predict(fit4, newdata=ndf, interval="confidence"))
with(ndf, matlines(x, y4, col=1, lty=c(1,2,2)))

with(df, plot(Y ~ x))
ndf <- transform(ndf, y3.3 = predict(fit3.3, newdata=ndf, interval="prediction"))
with(ndf, matlines(x, y3.3, col=2, lty=c(1,2,2)))
ndf <- transform(ndf, y3.3ns5 = predict(fit3.3ns5, newdata=ndf, interval="prediction"))
with(ndf, matlines(x, y3.3ns5, col=4, lty=c(1,2,2)))
ndf <- transform(ndf, y4 = predict(fit4, newdata=ndf, interval="prediction"))
with(ndf, matlines(x, y4, col=1, lty=c(1,2,2)))


## Extrapolações
ndfex <- data.frame(x = seq(-1,11, len=2001))

## As linhas a seguir são desnecessárias. 
## Se usamos bs() ns() não é necessário definir as variáveis auxiliares!!!
##ndfex <- transform(ndfex, I1 = ifelse(x < 3.5, 0, 1),
##                 I2 = ifelse(x < 7, 0, 1))
##ndfex <- transform(ndfex, x1 = ifelse(x < 3.5, 0, x-3.5),
##                 x2 = ifelse(x < 7, 0, x-7))

ndfex <- transform(ndfex, Ytrue = 0.2 * x + cos(x+1))

ndfex <- transform(ndfex, y3.3bs = predict(fit3.3bs, newdata=ndfex))
ndfex <- transform(ndfex, y3.5bs = predict(fit3.5bs, newdata=ndfex))
ndfex <- transform(ndfex, y3.3ns5 = predict(fit3.3ns5, newdata=ndfex))
ndfex <- transform(ndfex, y4 = predict(fit4, newdata=ndfex))
with(df, plot(Y ~ x, xlim=c(-1, 11)))
with(ndfex, lines(Ytrue ~ x, lty=3))
with(ndfex, lines(y3.3bs ~ x, lty=3, col=2))
with(ndfex, lines(y3.5bs ~ x, lty=3, col=3))
with(ndfex, lines(y3.3ns5 ~ x, lty=3, col=4))
with(ndfex, lines(y4 ~ x, lty=3, col=1))


ndfex <- transform(ndfex, y3.3bs = predict(fit3.3bs, newdata=ndfex, interval="confidence"))
ndfex <- transform(ndfex, y3.5bs = predict(fit3.5bs, newdata=ndfex, interval="confidence"))
ndfex <- transform(ndfex, y3.3ns5 = predict(fit3.3ns5, newdata=ndfex, interval="confidence"))
ndfex <- transform(ndfex, y4 = predict(fit4, newdata=ndfex, interval="confidence"))

with(df, plot(Y ~ x, xlim=c(-1, 11)))
with(ndfex, lines(Ytrue ~ x, lty=3))
with(ndfex, matlines(x, y3.3bs, col=2, lty=c(1,2,2)))
with(ndfex, matlines(x, y3.5bs, col=3, lty=c(1,2,2)))
with(ndfex, matlines(x, y3.3ns5, col=4, lty=c(1,2,2)))
with(ndfex, matlines(x, y4, col=1, lty=c(1,2,2)))

logLik(fit3.3bs)
logLik(fit3.5bs)
logLik(fit3.3ns5)
logLik(fit4)

AIC(fit3.3bs)
AIC(fit3.5bs)
AIC(fit3.3ns5)
AIC(fit4)

BIC(fit3.3bs)
BIC(fit3.5bs)
BIC(fit3.3ns5)
BIC(fit4)
