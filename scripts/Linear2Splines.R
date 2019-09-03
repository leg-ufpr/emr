#-----------------------------------------------------------------------
# Extensões de modelos de regressão · CE 092
# web.leg.ufpr.br/ensino/EMR
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                       Prof. Dr. Paulo Justiniano R. Jr
#
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Ago-29 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#
# De cálculo de médias a curvas suaves:
# Uma introdução à regressão por splines.
#
# Simulando dados
# Modelo para simulação:
#   Y_i = 0.2 x + cos(x + 1) + e_i
#   e_i \sim N(0, 10^2)
#   cos(\cdot) em radianos
#   x \sim U[0, 10]

#--------------------------------------------
# Simulação dos dados.

set.seed(201802)
df <- data.frame(x = sort(runif(50, 0, 10)))
df <- transform(df, Ytrue = 0.2 * x + cos(x + 1))
df <- transform(df, Y = Ytrue + rnorm(50, m = 0, sd = 0.5))

ndf <- data.frame(x = seq(0, 10, l = 2001))
ndf <- transform(ndf, Ytrue = 0.2 * x + cos(x + 1))
ndf$x[ndf$x == 3.5] <- NA
ndf$x[ndf$x == 7] <- NA

# Gráfico dos dados simulados.
with(df, plot(Y ~ x))
with(ndf, lines(Ytrue ~ x, lty = 3))

#--------------------------------------------
# Ajuste de modelos de ordem 0.

# Média.
(fit0 <- lm(Y ~ 1, data = df))
ndf <- transform(ndf, y0 = predict(fit0, newdata = ndf))
with(ndf, lines(y0 ~ x, col = 2))

# Criando covariáveis: partições do domínio de x.
df <- transform(df,
                I1 = ifelse(x < 3.5, 0, 1),
                I2 = ifelse(x < 7, 0, 1))
ndf <- transform(ndf,
                 I1 = ifelse(x < 3.5, 0, 1),
                 I2 = ifelse(x < 7, 0, 1))

# Patamares.
(fit0.0 <- lm(Y ~ I1 + I2, data = df))
ndf <- transform(ndf, y0.0 = predict(fit0.0, newdata = ndf))
with(ndf, lines(y0.0 ~ x, col = 3))

logLik(fit0.0)
logLik(fit0)

#--------------------------------------------
# Ajuste de modelos de ordem 1.

with(df, plot(Y ~ x))

# Uma reta.
(fit1 <- lm(Y ~ x, data = df))
ndf <- transform(ndf, y1 = predict(fit1, newdata = ndf))
with(ndf, lines(y1 ~ x, col = 3))

# Criando variáveis.
df <- transform(df, x1 = ifelse(x < 3.5, 0, x - 3.5),
                x2 = ifelse(x < 7, 0, x - 7))
ndf <- transform(ndf, x1 = ifelse(x < 3.5, 0, x - 3.5),
                 x2 = ifelse(x < 7, 0, x - 7))

# 3 retas desconectadas.
(fit1.0 <- lm(Y ~ x + I1 + x1 + I2 + x2, data = df))
ndf <- transform(ndf, y1.0 = predict(fit1.0, newdata = ndf))
with(ndf, lines(y1.0 ~ x, col = 3))

# 3 retas conectadas.
(fit1.1 <- lm(Y ~ x + x1 + x2, data = df))
ndf <- transform(ndf, y1.1 = predict(fit1.1, newdata = ndf))
with(ndf, lines(y1.1 ~ x, col = 3, lty = 3, lwd = 2))

logLik(fit1.0)
logLik(fit1.1)
logLik(fit1)

#--------------------------------------------
# Ajuste de modelos com polinômios de 3º grau.

# OBS: Pode-se usar poly(..., raw = TRUE)
# ou, o mesmo ajuste com poly(...). O default é raw = FALSE.

with(df, plot(Y ~ x))
with(ndf, lines(Ytrue ~ x, lty = 3))

# Uma curva de polinômio cúbico.
(fit3 <- lm(Y ~ poly(x, 3), data = df))
ndf <- transform(ndf, y3 = predict(fit3, newdata = ndf))
with(ndf, lines(y3 ~ x, col = 1))

# 3 curvas de polinômio cúbico desconectadas.
(fit3.0 <- lm(Y ~ poly(x, 3) + I1 +
                  poly(x1, 3) + I2 +
                  poly(x2, 3),
              data = df))
ndf <- transform(ndf, y3.0 = predict(fit3.0, newdata = ndf))
with(ndf, lines(y3.0 ~ x, col = 2))

# 3 curvas de polinômio cúbico conectadas mas não suaves.
(fit3.1 <- lm(Y ~ poly(x, 3) +
                  poly(x1, 3) +
                  poly(x2, 3), data = df))
ndf <- transform(ndf, y3.1 = predict(fit3.1, newdata = ndf))
with(ndf, lines(y3.1 ~ x, col = 3))

# 3 curvas de polinômio cúbico conectadas e suaves.
(fit3.2 <- lm(Y ~ poly(x, 3, raw = TRUE) +
                  poly(x1, 3, raw = TRUE)[, -1] +
                  poly(x2, 3, raw = TRUE)[, -1],
              data = df))
ndf <- transform(ndf, y3.2 = predict(fit3.2, newdata = ndf))
with(ndf, lines(y3.2 ~ x, col = 4))

# Outras 3 curvas de polinômio cúbico conectadas e suaves.
(fit3.3 <- lm(Y ~ poly(x, 3) + I(x1^3) + I(x2^3), data = df))
ndf <- transform(ndf, y3.3 = predict(fit3.3, newdata = ndf))
with(ndf, lines(y3.3 ~ x, col = 5))

# Nomes dos ajustes baseados em polinômio cúbico.
fits3 <- grep(x = ls(), pattern = "fit3", value = TRUE)

# Salva parâmetros gráficos.
oldpar <- par()

# Faz grid de gráficos.
par(mfrow = c(3, 2), mar = c(2, 2.25, 1, 1))
lapply(fits3,
       FUN = function(fit) {
           with(df, plot(Y ~ x))
           with(ndf, lines(Ytrue ~ x, lty = 3))
           matlines(x = ndf$x,
                    y = predict(get(fit),
                                newdata = ndf,
                                interval = "confidence"),
                    col = 2, lty = c(1, 2, 2))
           abline(v = c(3.5, 7), col = 2, lty = 2)
       })
layout(1)

# Retorna parâmetros gráficos.
par(oldpar)

logLik(fit3.0)
logLik(fit3.1)
logLik(fit3.2)
logLik(fit3.3)
logLik(fit3)

#--------------------------------------------
# Ajuste de modelos com polinômios de 4º grau.

with(df, plot(Y ~ x))
with(ndf, lines(Ytrue ~ x, lty = 3))

# Um polinômio de 4 grau.
(fit4 <- lm(Y ~ poly(x, 4), data = df))
ndf <- transform(ndf, y4 = predict(fit4, newdata = ndf))
with(ndf, lines(y4 ~ x, col = 1))

# Salva parâmetros gráficos.
oldpar <- par()

# Entendendo as restrições de continuidade na função, na 1ª e 2ª
# derivadas.
par(mfcol = c(3, 6), mar = c(3, 3, 0.5, 0.5))

ndf <- transform(ndf, y3.0d = c(diff(y3.0)/diff(x), NA))
ndf <- transform(ndf, y3.0dd = c(diff(y3.0d)/diff(x), NA))
with(df, plot(Y ~ x))
with(ndf, lines(y3.0 ~ x))
with(ndf, plot(y3.0d ~ x, type = "l"))
with(ndf, plot(y3.0dd ~ x, type = "l"))

ndf <- transform(ndf, y3.1d = c(diff(y3.1)/diff(x), NA))
ndf <- transform(ndf, y3.1dd = c(diff(y3.1d)/diff(x), NA))
with(df, plot(Y ~ x))
with(ndf, lines(y3.1 ~ x))
with(ndf, plot(y3.1d ~ x, type = "l"))
with(ndf, plot(y3.1dd ~ x, type = "l"))

ndf <- transform(ndf, y3.2d = c(diff(y3.2)/diff(x), NA))
ndf <- transform(ndf, y3.2dd = c(diff(y3.2d)/diff(x), NA))
with(df, plot(Y ~ x))
with(ndf, lines(y3.2 ~ x))
with(ndf, plot(y3.2d ~ x, type = "l"))
with(ndf, plot(y3.2dd ~ x, type = "l"))

ndf <- transform(ndf, y3.3d = c(diff(y3.3)/diff(x), NA))
ndf <- transform(ndf, y3.3dd = c(diff(y3.3d)/diff(x), NA))
with(df, plot(Y ~ x))
with(ndf, lines(y3.3 ~ x))
with(ndf, plot(y3.3d ~ x, type = "l"))
with(ndf, plot(y3.3dd ~ x, type = "l"))

ndf <- transform(ndf, y3d = c(diff(y3)/diff(x), NA))
ndf <- transform(ndf, y3dd = c(diff(y3d)/diff(x), NA))
with(df, plot(Y ~ x))
with(ndf, lines(y3 ~ x))
with(ndf, plot(y3d ~ x, type = "l"))
with(ndf, plot(y3dd ~ x, type = "l"))

ndf <- transform(ndf, y4d = c(diff(y4)/diff(x), NA))
ndf <- transform(ndf, y4dd = c(diff(y4d)/diff(x), NA))
with(df, plot(Y ~ x))
with(ndf, lines(y4 ~ x))
with(ndf, plot(y4d ~ x, type = "l"))
with(ndf, plot(y4dd ~ x, type = "l"))

# Retorna parâmetros gráficos.
layout(1)
par(oldpar)

#-----------------------------------------------------------------------
# O pacote `splines` fornece funções que já montam as funções base. Com
# isto é desnecessário criar as variáveis auxiliares.

rm(list = fits3)

# Para usar função bs(): base splines.
library(splines)

with(df, plot(Y ~ x))
with(ndf, lines(y3.3 ~ x, col = 4))

(fit3.3bs <- lm(Y ~ bs(x, knots = c(3.5, 7)), data = df))
ndf <- transform(ndf, y3.3bs = predict(fit3.3bs, newdata = ndf))
with(ndf, lines(y3.3bs ~ x, col = 6))

# Melhor definir bordas no intervalo nas quais se vai fazer a predição.
(fit3.3bs <- lm(Y ~ bs(x, knots = c(3.5, 7),
                       Boundary.knots = c(0, 10)),
                data = df))
ndf <- transform(ndf, y3.3bs = predict(fit3.3bs, newdata = ndf))
with(ndf, lines(y3.3bs ~ x, col = 7))

# Fixando graus de liberdade mudam-se os nós.
with(df, plot(Y ~ x))
with(ndf, lines(y3.3bs ~ x, col = 1))
with(ndf, lines(y3.3bs ~ x, col = 6))

# Base splines de grau de liberdade 5.
(fit3.3bs5 <- lm(Y ~ bs(x,
                        df = 5,
                        Boundary.knots = c(0, 10)),
                 data = df))
ndf <- transform(ndf, y3.3bs5 = predict(fit3.3bs5, newdata = ndf))
with(ndf, lines(y3.3bs5 ~ x, col = 2))
abline(v = attr(fit3.3bs5$model[[2]], "knots"), lty = 2)
attr(with(df, bs(x, df = 5, Boundary.knots = c(0, 10))), "knots")

(fit3.3ns <- lm(Y ~ ns(x,
                       knots = c(3.5, 7),
                       Boundary.knots = c(0, 10)),
                data = df))
ndf <- transform(ndf, y3.3ns = predict(fit3.3ns, newdata = ndf))
with(ndf, lines(y3.3ns ~ x, col = 3))
abline(v = attr(fit3.3ns$model[[2]], "knots"), lty = 2)

(fit3.3ns5 <- lm(Y ~ ns(x,
                        df = 5,
                        Boundary.knots = c(0, 10)),
                 data = df))
ndf <- transform(ndf, y3.3ns5 = predict(fit3.3ns5, newdata = ndf))
with(ndf, lines(y3.3ns5 ~ x, col = 4))
abline(v = attr(fit3.3ns5$model[[2]], "knots"), lty = 2)
attr(with(df, ns(x, df = 5, Boundary.knots = c(0, 10))), "knots")

logLik(fit3.3)
logLik(fit3.3bs)
logLik(fit3.3bs5)
logLik(fit3.3ns)
logLik(fit3.3ns5)

# Nomes dos ajustes baseados em polinômio cúbico.
fits3 <- grep(x = ls(), pattern = "fit3", value = TRUE)

# Salva parâmetros gráficos.
oldpar <- par()

# Faz grid de gráficos.
par(mfrow = c(2, 2), mar = c(2, 2.25, 1, 1))
lapply(fits3,
       FUN = function(fit) {
           with(df, plot(Y ~ x))
           title(main = fit)
           with(ndf, lines(Ytrue ~ x, lty = 3))
           matlines(x = ndf$x,
                    y = predict(get(fit),
                                newdata = ndf,
                                interval = "confidence"),
                    col = 2,
                    lty = c(1, 2, 2))
           abline(v = attr(get(fit)$model[[2]], "knots"),
                  lty = 2)
       })
layout(1)

# Retorna parâmetros gráficos.
par(oldpar)

#--------------------------------------------
# E se fosse apenas 1 nó!??

with(df, plot(Y ~ x))
with(ndf, lines(y3.3bs ~ x, col = 2))


(fit3.5bs <- lm(Y ~ bs(x,
                       knots = 5, Boundary.knots = c(0, 10)),
                data = df))
ndf <- transform(ndf, y3.5bs = predict(fit3.5bs, newdata = ndf))
with(ndf, lines(y3.5bs ~ x, col = 6))

logLik(fit3.3bs)
logLik(fit3.5bs)
logLik(fit4)


#--------------------------------------------
# Algumas comparações.

with(df, plot(Y ~ x))
with(ndf, lines(Ytrue ~ x, lty = 3))
with(ndf, lines(y3.3bs ~ x, col = 2))
with(ndf, lines(y3.5bs ~ x, col = 2))
with(ndf, lines(y3.3ns5 ~ x, col = 4))
with(ndf, lines(y4 ~ x, col = 1))

# Intervalos de confiança.
with(df, plot(Y ~ x))
ndf <- transform(ndf, y3.3 = predict(fit3.3,
                                     newdata = ndf,
                                     interval = "confidence"))
with(ndf, matlines(x, y3.3, col = 2, lty = c(1, 2, 2)))
ndf <- transform(ndf, y3.3ns5 = predict(fit3.3ns5,
                                        newdata = ndf,
                                        interval = "confidence"))
with(ndf, matlines(x, y3.3ns5, col = 4, lty = c(1, 2, 2)))
ndf <- transform(ndf, y4 = predict(fit4,
                                   newdata = ndf,
                                   interval = "confidence"))
with(ndf, matlines(x, y4, col = 1, lty = c(1, 2, 2)))

# Intervalos de predição para observação futura.
with(df, plot(Y ~ x))
ndf <- transform(ndf, y3.3 = predict(fit3.3,
                                     newdata = ndf,
                                     interval = "prediction"))
with(ndf, matlines(x, y3.3, col = 2, lty = c(1, 2, 2)))
ndf <- transform(ndf, y3.3ns5 = predict(fit3.3ns5,
                                        newdata = ndf,
                                        interval = "prediction"))
with(ndf, matlines(x, y3.3ns5, col = 4, lty = c(1, 2, 2)))
ndf <- transform(ndf, y4 = predict(fit4,
                                   newdata = ndf,
                                   interval = "prediction"))
with(ndf, matlines(x, y4, col = 1, lty = c(1, 2, 2)))

#--------------------------------------------
# Extrapolações.

ndfex <- data.frame(x = seq(-1, 11, len = 2001))

# As linhas a seguir são desnecessárias. Se usamos `bs()` ou `ns()` não
# é necessário definir as variáveis auxiliares!
#
# ndfex <- transform(ndfex,
#                    I1 = ifelse(x < 3.5, 0, 1),
#                    I2 = ifelse(x < 7, 0, 1))
# ndfex <- transform(ndfex,
#                    x1 = ifelse(x < 3.5, 0, x-3.5),
#                    x2 = ifelse(x < 7, 0, x-7))

ndfex <- transform(ndfex, Ytrue = 0.2 * x + cos(x + 1))

ndfex <- transform(ndfex, y3.3bs = predict(fit3.3bs, newdata = ndfex))
ndfex <- transform(ndfex, y3.5bs = predict(fit3.5bs, newdata = ndfex))
ndfex <- transform(ndfex, y3.3ns5 = predict(fit3.3ns5, newdata = ndfex))
ndfex <- transform(ndfex, y4 = predict(fit4, newdata = ndfex))
with(df, plot(Y ~ x, xlim = c(-1, 11)))
with(ndfex, lines(Ytrue ~ x, lty = 3))
with(ndfex, lines(y3.3bs ~ x, lty = 3, col = 2))
with(ndfex, lines(y3.5bs ~ x, lty = 3, col = 3))
with(ndfex, lines(y3.3ns5 ~ x, lty = 3, col = 4))
with(ndfex, lines(y4 ~ x, lty = 3, col = 1))

ndfex <- transform(ndfex,
                   y3.3bs = predict(fit3.3bs,
                                    newdata = ndfex,
                                    interval = "confidence"),
                   y3.5bs = predict(fit3.5bs,
                                    newdata = ndfex,
                                    interval = "confidence"),
                   y3.3ns5 = predict(fit3.3ns5,
                                     newdata = ndfex,
                                     interval = "confidence"),
                   y4 = predict(fit4,
                                newdata = ndfex,
                                interval = "confidence"))

with(df, plot(Y ~ x, xlim = c(-1, 11)))
with(ndfex, lines(Ytrue ~ x, lty = 3))
with(ndfex, matlines(x, y3.3bs, col = 2, lty = c(1, 2, 2)))
with(ndfex, matlines(x, y3.5bs, col = 3, lty = c(1, 2, 2)))
with(ndfex, matlines(x, y3.3ns5, col = 4, lty = c(1, 2, 2)))
with(ndfex, matlines(x, y4, col = 1, lty = c(1, 2, 2)))

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

#-----------------------------------------------------------------------
