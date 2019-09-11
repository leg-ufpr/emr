#-----------------------------------------------------------------------
# Extensões de modelos de regressão · CE 092
# web.leg.ufpr.br/ensino/EMR
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                       Prof. Dr. Paulo Justiniano R. Jr
#
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Set-10 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Definições da sessão.

rm(list = ls())

#-----------------------------------------------------------------------
# Importação dos dados.

sui <- read.table("http://www.leg.ufpr.br/~walmes/data/preabate.txt",
                  header = TRUE, dec = ",")
str(sui)
sui <- subset(sui, trat == 1)

plot(temp ~ hora, data = sui)
with(sui, abline(h = mean(temp)), lty = 3)

#-----------------------------------------------------------------------
# Ajuste de regressão local.

m0 <- loess(temp ~ hora, data = sui, span = 0.75, degree = 1)

class(m0)
methods(class = "loess")

anova(m0)
summary(m0)

r <- residuals(m0)
sum(r^2)

q <- qnorm(0.975)

pred <- with(sui,
             data.frame(hora = seq(min(hora),
                                   max(hora),
                                   length.out = 101)))
fits <- predict(m0, newdata = pred, se = TRUE)

pred <- cbind(pred, as.data.frame(fits[1:2]))
pred <- transform(pred,
                  lwr = fit - q * se.fit,
                  upr = fit + q * se.fit)
str(pred)

plot(temp ~ hora, data = sui)
with(pred,
     matlines(x = hora,
              y = cbind(fit, lwr, upr),
              lty = c(1, 2, 2), col = 1))

#-----------------------------------------------------------------------
# Prever o valor em x = 102.

x <- seq(Nile)
y <- as.numeric(Nile)

x0 <- 33

plot(y ~ x)
abline(v = x0, col = "orange")

span <- 0.40
a <- abs(x - x0)

if (span < 1) {
    q <- as.integer(span * length(x))
    d <- sort(a)[q]
} else {
    q <- length(x)
    d <- max(abs(x - x0)) * sqrt(span)
}

w <- numeric(length(x))
s <- a <= d
# s <- a < d
w[s] <- (1 - (a[s]/d)^3)^3
i <- as.integer(s)
# w <- w/sum(w) # Mas não precisa somar 1 não.

# Proporção de observações na janela.
sum(s)/length(s)

# Função peso.
curve((1 - (abs(x0 - x)/d)^3)^3 * (abs((x0 - x)/d) < 1) + 0,
      from = min(x),
      to = max(x),
      col = "purple",
      lwd = 1.25,
      n = 201)
abline(v = x0, col = "orange")
points(x = x, y = w)

# Função peso sobreposta como adorno ao diagrama de dispersão.
plot(y ~ x, col = (w > 0) + 1)
rug(x, ticksize = 0.01)
abline(v = x0, col = "orange")
md <- par()$usr[3] * 0.975 + 0.025 * par()$usr[4]
sc <- 0.25 * diff(par()$usr[3:4])
curve(md + sc * (1 - (abs(x0 - x)/d)^3)^3 * (abs((x0 - x)/d) < 1) + 0,
      add = TRUE,
      col = "purple",
      lwd = 1.25,
      n = 201)

# Com loess().
m0 <- loess(y ~ x,
            span = span,
            degree = 2,
            surface = "direct",
            normalize = FALSE)
summary(m0)
predict(m0, newdata = list(x = x0), se = TRUE)

# Com lm() ponderado.
m1 <- lm(y ~ poly(x, degree = 2), weights = w)
predict(m1, newdata = list(x = x0), se.fit = TRUE)

# FIXME: tem algum detalhe faltando para que erro padrão seja o
# mesmo. Creio que seja o grau de liberdade. E o valor predito não bate
# em alguns pontos. Ainda está em aberto o algorítmo exato da
# `loess()`. De qualquer forma, a intuição foi passada.

#--------------------------------------------
# ATTENTION: qual o melhor valor para `span`?

# Determinar por valiação cruzada.
set.seed(092)
k <- 7
u <- ceiling(nrow(sui)/k)
i <- sample(rep(1:k, u))[1:nrow(sui)]

# K-folds.
fit <- function(span, data, folds, ...) {
    fit_k <- function(k, span, data, ...) {
        keep <- folds == k
        test <- data[keep, ]
        train <- data[!keep, ]
        m0 <- loess(temp ~ hora, data = train, span = span, ...)
        sum((test$temp - predict(m0, newdata = test))^2)/nrow(test)
    }
    f <- min(folds):max(folds)
    sqr <- sapply(min(folds):max(folds),
                  FUN = fit_k,
                  span = span,
                  data = data,
                  ...)
    data.frame(span = span, fold = f, sqr = sqr)
}

# Teste antes de serializar.
fit(span = 0.75, data = sui, folds = i)
fit(span = 0.75, data = sui, folds = i, degree = 1)

# Serializando.
span_seq <- seq(0.05, 0.75, length.out = 71)
res <- lapply(span_seq,
              FUN = fit,
              data = sui,
              folds = i)
res <- do.call(rbind, res)

# Gráfico com o traço de cada fold.
plot(sqr ~ span, data = res, col = res$fold)

# Pega o valor médio.
res_m <- aggregate(sqr ~ span, data = res, FUN = mean)

# Traço da soma de quadrados média.
plot(sqr ~ span, data = res_m, type = "o")
abline(v = 0.15, col = "orange")

#-----------------------------------------------------------------------
# Ajuste de regressão local com o valor determinado por CV.

m0 <- loess(temp ~ hora, data = sui, span = 0.15)
summary(m0)

r <- residuals(m0)
sum(r^2)

q <- qnorm(0.975)
pred <- with(sui,
             data.frame(hora = seq(min(hora),
                                   max(hora),
                                   length.out = 201)))
fits <- predict(m0, newdata = pred, se = TRUE)

pred <- cbind(pred, as.data.frame(fits[1:2]))
pred <- transform(pred,
                  lwr = fit - q * se.fit,
                  upr = fit + q * se.fit)
str(pred)

plot(temp ~ hora, data = sui)
with(pred,
     matlines(x = hora,
              y = cbind(fit, lwr, upr),
              lty = c(1, 2, 2), col = 1))

r <- residuals(m0)
f <- fitted(m0)

plot(r ~ f)
abline(h = 0, lty = 2)

plot(abs(r) ~ f)

qqnorm(r)
qqline(r, lty = 2)

#-----------------------------------------------------------------------
