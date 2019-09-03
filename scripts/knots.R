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

rm(list = ls())

#-----------------------------------------------------------------------

# Importa os dados.
dat <- read.table("http://www.leg.ufpr.br/~paulojus/CE092/df03.txt",
                  head = TRUE, sep = ",")
nrow(dat)

# Cria grid mais fino para predição de valores.
ndat <- data.frame(x = seq(0, 2, length.out = 201))

# Diagrama de dispersão.
with(dat, plot(y ~ x))

# Ajuste do modelo com ponto de corte em 0.4.
reg0.4 <- lm(y ~ x + I((x - 0.4) * (x >= 0.4)),
             data = dat)

# Predição.
ndat$y0.4 <- predict(reg0.4, newdata = ndat)
with(ndat, lines(y0.4 ~ x))
abline(v = 0.4, col = 1, lty = 2)

# Ajuste do modelo com ponto de corte em 0.5.
reg0.5 <- lm(y ~ x + I((x - 0.5) * (x >= 0.5)),
             data = dat)

# Predição.
ndat$y0.5 <- predict(reg0.5, newdata = ndat)
with(ndat, lines(y0.5 ~ x, col = 2))
abline(v = 0.5, col = 2, lty = 2)

# Ajuste do modelo com ponto de corte em 0.6.
reg0.6 <- lm(y ~ x + I((x - 0.6) * (x >= 0.6)),
             data = dat)

# Predição.
ndat$y0.6 <- predict(reg0.6, newdata = ndat)
with(ndat, lines(y0.6 ~ x, col = 4))
abline(v = 0.6, col = 4, lty = 2)

# Compara os valor de log-verossimilhança dos modelos.
c(logLik(reg0.4), logLik(reg0.5), logLik(reg0.6))

# Função para ajustar modelos segmentados.
regseg.f <- function(k, data, ndata, lines = FALSE, fit = TRUE, ...) {
    reg <- lm(y ~ x + I((x - k) * (x >= k)),
              data = data)
    if (lines) {
        ndata$yK <- predict(reg, newdata = ndata)
        lines(yK ~ x, data = ndata, ...)
        abline(v = k, lty = 2)
    }
    if (fit) {
        return(reg)
    } else {
        return(invisible())
    }
}

dev.off()

# Usa a função.
with(dat, plot(y ~ x))
regseg.f(0.4, data = dat, ndata = ndat, lines = TRUE, lwd = 2)
regseg.f(0.5, data = dat, ndata = ndat, lines = TRUE, lwd = 2, col = 5)
regseg.f(0.6, data = dat, ndata = ndat, lines = TRUE, lwd = 2, col = 4)

# Avaliando um grid de valores para o ponto de quebra.
Ks <- seq(0, 2, by = 0.1)
fitK <- lapply(Ks, FUN = regseg.f, data = dat, ndata = ndat)
names(fitK) <- paste("K", Ks, sep = "-")

# Extrai a log-verossimilhança dos ajustes.
lLs <- sapply(fitK, FUN = logLik)

# Traço da log-verossimilhança.
plot(Ks, lLs, ty = "o")
abline(h = lLs[which.max(lLs)],
       v = Ks[which.max(lLs)],
       lty = 2, col = 2)

# TODO: experimente novamente com um grid mais fino nos Ks.

# Gráfico do melhor ajuste dessa série de ajustes.
with(dat, plot(y ~ x))
im <- which.max(lLs)
Bfit <- fitK[[im]]
ndat$ypred <- predict(Bfit, newdata = ndat)
lines(ypred ~ x, data = ndat, type = "l", col = 4)
abline(v = Ks[im], col = 4, lty = 2)

#-----------------------------------------------------------------------
# Dados anteriores do curso.

# Importa dados.
df <- read.table("http://www.leg.ufpr.br/~paulojus/CE092/df02.txt",
                 head = TRUE, sep = "")
df <- transform(df, y = Y1)
df

# Grid de valores para predição.
ndf <- data.frame(x = seq(0, 2, len = 201))

# Visualização.
with(df, plot(y ~ x))

# Ajuste da série de uma série de modelos.
Ks <- seq(0, 2, by = 0.2)
fitK <- lapply(Ks, FUN = regseg.f, data = df, ndata = ndf)
names(fitK) <- paste("K", Ks, sep = "-")

# Extrai a log-verossimilhança.
lLs <- sapply(fitK, FUN = logLik)

# Perfil da log-verossimilhança.
plot(Ks, lLs, ty = "o")
abline(h = lLs[which.max(lLs)],
       v = Ks[which.max(lLs)],
       lty = 2, col = 2)

# QUESTION: refinar Ks?

# Gráfico do melhor ajuste dessa série de ajustes.
with(df, plot(y ~ x))
im <- which.max(lLs)
Bfit <- fitK[[im]]
ndf$ypred <- predict(Bfit, newdata = ndf)
lines(ypred ~ x, data = ndf, type = "l", col = 4)
abline(v = Ks[im], col = 4, lty = 2)

#-----------------------------------------------------------------------
