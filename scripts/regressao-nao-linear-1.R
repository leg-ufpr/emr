#-----------------------------------------------------------------------
#                                            Prof. Dr. Walmes M. Zeviani
#                                leg.ufpr.br/~walmes · github.com/walmes
#                                        walmes@ufpr.br · @walmeszeviani
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Out-22 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#=======================================================================
# Exemplo 1.

#-----------------------------------------------------------------------
# Carrega pacotes.

library(lattice)
library(latticeExtra)
library(car)
library(alr3)
# library(rootSolve)
library(numDeriv)

# Função que converte objeto `nls` para `lm` para ver resíduos.
source("https://raw.githubusercontent.com/walmes/wzRfun/master/R/as.lm.R")

# Para bandas de confiança.
source("https://raw.githubusercontent.com/walmes/wzRfun/master/R/panel.cbH.R")

#-----------------------------------------------------------------------
# Ajuste de modelo de regressão não linear.

# turk0
str(turk0)

xtabs(~A, data = turk0)

xyplot(Gain ~ A, data = turk0, type = c("p", "smooth"))

#-----------------------------------------------------------------------
# Valores iniciais baseados na interpretação gráfica.
# Modelo: th0 + th1 * x/(th2 + x);

start <- list(th0 = 625, th1 = 800 - 625, th2 = 0.1)

xyplot(Gain ~ A, data = turk0) +
    layer(panel.curve(th0 + th1 * x/(th2 + x), lty = 2),
          data = start)

#-----------------------------------------------------------------------
# Ajuste.

n0 <- nls(Gain ~ th0 + th1 * A/(th2 + A),
          data = turk0,
          start = start)
summary(n0)

# Curva ajustada sobre os dados.
xyplot(Gain ~ A, data = turk0)+
    layer(panel.curve(th0 + th1 * x/(th2 + x), col = 2),
          data = as.list(coef(n0)))

#-----------------------------------------------------------------------
# Intervalos de confiança.

# Baseado na log-verossimilhança.
confint(n0)

# Gráfico dos perfils.
par(mfrow = c(2, 2))
plot(profile(n0))
layout(1)

# Baseado na aproximação quadrática da verossimilhança, conhecido como
# intervalos de Wald ou assintóticos. São simétricos por construção.
confint.default(n0)

#-----------------------------------------------------------------------
# TIP: Colocar bandas de confiança.

# Modelo escrito como função dos parâmetros (theta).
f <- function(theta, xx) {
    with(as.list(theta),
         th0 + th1 * xx/(th2 + xx))
}

# Matriz de derivadas parciais em theta (n x p).
jacobian(func = f, x = coef(n0), xx = c(0, 0.2, 0.4))

# Valores preditos.
pred <- data.frame(A = seq(0, 0.5, l = 20))
pred$fit <- predict(n0, newdata = pred)

# Matriz de derivadas parciais avaliadas nas estimativas.
X_partial <- jacobian(func = f, x = coef(n0), xx = pred$A)
X_partial

# ATTENTION: os passos a seguir são bastante técnicos mas o que se está
# obtendo é o erro padrão do valor predito para então determinar os
# limites do intervalo de confiança.

# Choleski da matriz de covariância das estimativas.
U <- chol(vcov(n0))

# Erro padrão para cada valor predito.
pred$se <- sqrt(apply(X_partial %*% t(U),
                      MARGIN = 1,
                      FUN = function(x) sum(x^2)))
pred$se

# Estatística t para obter a margem de erro.
tval <- qt(p = c(lwr = 0.025, upr = 0.975),
           df = df.residual(n0))
me <- outer(X = pred$se, Y = tval, FUN = "*")
me

# Obtenão dos limites de confiança.
pred <- cbind(pred,
              sweep(x = me,
                    MARGIN = 1,
                    STATS = pred$fit,
                    FUN = "+"))
head(pred)

# Equação do modelo ajustado.
coef(n0)
formula(n0)

# Observados, preditos e a banda de confiança.
xyplot(Gain ~ A, data = turk0) +
    as.layer(xyplot(fit ~ A,
                    data = pred,
                    type = "l",
                    prepanel = prepanel.cbH,
                    cty = "bands",
                    ly = pred$lwr,
                    uy = pred$upr,
                    panel = panel.cbH))

#=======================================================================
# Exemplo 2.

#-----------------------------------------------------------------------
# Consumo de energia (KWH/dia) em função da temperatura (F).

str(segreg)
xyplot(C ~ Temp, data = segreg, type = c("p", "smooth"))

#-----------------------------------------------------------------------
# Ajuste do modelo platô linear.
# f(x) = th0 + th1 * (x - th2) * (x >= th2) + 0 * (x < th2)

# Inspeciona valores iniciais.
start <- list(th0 = 75, th1 = 0.5, th2 = 50)
xyplot(C ~ Temp, data = segreg) +
    layer(panel.curve(th0 + th1 * (x - th2) * (x >= th2) +
                      0 * (x < th2), lty = 2), data = start)

# Ajuste. TIP: o ponto de quebra é desconhecido.
n2 <- nls(C ~ th0 + th1 * (Temp - th2) * (Temp >= th2) +
              0 * (Temp < th2),
          data = segreg,
          start = start)

# Estimativas e medidas de ajuste.
summary(n2)

# Intervalos de confiança.
# confint(n2)
confint.default(n2)

# Observados e preditos.
xyplot(C ~ Temp, data = segreg) +
    layer(panel.curve(th0 + th1 * (x - th2) * (x >= th2) +
                      0 * (x < th2), col = 2),
          data = as.list(coef(n2)))

#-----------------------------------------------------------------------
# Análise dos resíduos.

m2 <- as.lm(n2)

par(mfrow = c(2, 2))
plot(m2)
layout(1)

#-----------------------------------------------------------------------
# Colocar bandas de confiança.

f <- function(theta, xx) {
    with(as.list(theta),
         th0 + th1 * (xx - th2) * (xx >= th2) +
         0 * (xx < th2))
}

pred <- data.frame(Temp = sort(c(seq(10, 80, l = 100),
                                 coef(n2)["th2"] +
                                 c(-0.001, 0, 0.001))))
pred$fit <- predict(n2, newdata = pred)

# Derivadas parciais.
X_partial <- jacobian(func = f, x = coef(n2), xx = pred$Temp)
head(X_partial)
tail(X_partial)

U <- chol(vcov(n2))
pred$se <- sqrt(apply(X_partial %*% t(U),
                      MARGIN = 1,
                      FUN = function(x) sum(x^2)))
tval <- qt(p = c(lwr = 0.025, upr = 0.975),
           df = df.residual(n2))
me <- outer(X = pred$se, Y = tval, FUN = "*")
pred <- cbind(pred, sweep(x = me,
                          MARGIN = 1,
                          STATS = pred$fit,
                          FUN = "+"))
str(pred)

# Equação do modelo ajustado.
coef(n2)
formula(n2)

# Arredonda as estimativas.
theta <- mapply(round,
                x = coef(n2),
                digits = c(2, 4, 2),
                SIMPLIFY = FALSE)
theta

# Equação para inserir no gráfico.
formula(n2)
eq <- substitute(
    expr = c(
        expression(C==th0~", se"~Temp < th2),
        expression(C==th0 + th1 * (Temp - th2)~", se"~Temp >= th2)),
    env = theta)
eval(eq)

# Observados, preditos e a banda de confiança.
xyplot(C ~ Temp, data = segreg) +
    as.layer(xyplot(fit ~ Temp,
                    data = pred,
                    type = "l",
                    prepanel = prepanel.cbH,
                    cty = "bands",
                    ly = pred$lwr,
                    uy = pred$upr,
                    panel = panel.cbH)) +
    layer(panel.key(points = FALSE,
                    text = eval(eq),
                    corner = c(0.05, 0.95)))

#=======================================================================
# Exemplo 3. Curva de secagem do solo.

sec <- read.table("http://www.leg.ufpr.br/~walmes/data/emr11.txt",
                  header = TRUE,
                  sep = "\t",
                  encoding = "latin1")
str(sec)

xyplot(umrel ~ tempo | nome, data = sec)

# Modelo: logístico.
#
#  model <- umrel ~ th1/(1 + exp(-(tempo - th2)/th3))
#
# x: representado por tempo, período da amostra dentro do microondas.
# y: representado por umrel, umidade relativa o conteúdo total de água.
# th1: assíntota superior.
# th2: tempo para evaporar metade do conteúdo total de água.
# th3: proporcional à taxa máxima do processo.

sec_sub <- subset(sec, nome == "LVAd-A")
plot(umrel ~ tempo, data = sec_sub)

n0 <- nls(umrel ~ th1/(1 + exp(-(tempo - th2)/th3)),
          data = sec_sub,
          start = list(th1 = 1,
                       th2 = 15,
                       th3 = 5),
          trace = TRUE)

# Usando funções self start.
n1 <- nls(umrel ~ SSlogis(tempo, th1, th2, th3),
          data = sec_sub,
          trace = TRUE)

# Funções self start disponíveis.
apropos("^SS", ignore.case = FALSE)

# Modelo de Gompertz.
n2 <- nls(umrel ~ SSgompertz(tempo, tha, th2, th3),
          data = sec_sub,
          trace = TRUE)

# Compara os ajustes pela log-verossimilhança.
logLik(n1)
logLik(n2)

# Para verificar o ajuste.
pred <- data.frame(tempo = seq(0, 80, 1))
pred$y1 <- predict(n1, newdata = pred)
pred$y2 <- predict(n2, newdata = pred)

plot(umrel ~ tempo,
     data = sec_sub,
     xlim = c(0, 80),
     ylim = c(0, 1.2))
with(pred, {
    lines(tempo, y1, col = "orange")
    lines(tempo, y2, col = "purple")
})

# Coeficiente de determinação.
dev_null <- deviance(lm(umrel ~ 1, sec_sub))
1 - deviance(n1)/dev_null
1 - deviance(n2)/dev_null

# Documentação do modelo Gompertz.
help(SSgompertz, h = "html")

#-----------------------------------------------------------------------
# Recursos adicionais para modelos não lineares.

library(nlstools)
ls("package:nlstools")

# Gráfico dos resíduos (substitui a `as.lm()`).
res <- nlsResiduals(n2)
plot(res)

# Contornos de soma de quadrados de resíduos.
rss1 <- nlsContourRSS(n1)
plot(rss1, col.pal = topo.colors(100))

x11()
rss2 <- nlsContourRSS(n2)
plot(rss2, col.pal = topo.colors(100))

# Fecha as janelas gráficas abertas.
graphics.off()

# Região de confiança conjunta por aceitação-rejeição.
conf1 <- nlsConfRegions(n1, exp = 2, length = 3000)
plot(conf1)

x11()
conf2 <- nlsConfRegions(n2, exp = 2, length = 3000)
plot(conf2)

# Fecha as janelas gráficas abertas.
graphics.off()

# Usando gráficos em 3D.
library(rgl)

x_col <- (conf1$rss - min(conf1$rss))/(conf1$rss95 - min(conf1$rss))
colorfunc <- colorRamp(RColorBrewer::brewer.pal(7, "Spectral"))
colors <- rgb(colorfunc(x_col), maxColorValue = 255)

plot3d(x = conf1$cr[, 1],
       y = conf1$cr[, 2],
       z = conf1$cr[, 3],
       col = colors,
       type = "s",
       radius = 0.05)
rgl.points(x = conf1$cr[, 1],
           y = conf1$cr[, 2],
           z = min(conf1$cr[, 3]),
           col = colors)
rgl.points(x = conf1$cr[, 1],
           y = min(conf1$cr[, 2]),
           z = conf1$cr[, 3],
           col = colors)
rgl.points(x = min(conf1$cr[, 1]),
           y = conf1$cr[, 2],
           z = conf1$cr[, 3],
           col = colors)

x_col <- (conf2$rss - min(conf2$rss))/(conf2$rss95 - min(conf2$rss))
colorfunc <- colorRamp(RColorBrewer::brewer.pal(7, "Spectral"))
colors <- rgb(colorfunc(x_col), maxColorValue = 255)

plot3d(x = conf2$cr[, 1],
       y = conf2$cr[, 2],
       z = conf2$cr[, 3],
       col = colors,
       type = "s",
       radius = 0.005)
rgl.points(x = conf2$cr[, 1],
           y = conf2$cr[, 2],
           z = min(conf2$cr[, 3]),
           col = colors)
rgl.points(x = conf2$cr[, 1],
           y = min(conf2$cr[, 2]),
           z = conf2$cr[, 3],
           col = colors)
rgl.points(x = min(conf2$cr[, 1]),
           y = conf2$cr[, 2],
           z = conf2$cr[, 3],
           col = colors)

# Bootstrap.
b <- nlsBoot(n1)
summary(b)

# Jackknife.
j <- nlsJack(n1)
summary(j)

# Até aqui, quantas diferentes formas de obter o intervalo de confiança
# foram vistas?
#   * confint(n1)         : Baseado em perfil da verossimilhança.
#   * confint.default(n1) : Assintótico.
#   * summary(b)          : Bootstrap percentílico.
#   * summary(j)          : Jackknife.

library(nls2)

# Cria grid para busca exaustiva (grid search)
my_tries <- expand.grid(th1 = seq(0.9, 1.1, length.out = 6),
                        th2 = seq(7, 20, length.out = 6),
                        th3 = seq(2, 18, length.out = 6),
                        KEEP.OUT.ATTRS = FALSE)
my_tries

# `grid-search` e `brute-force` representam a mesma coisa.
n_try <- nls2(umrel ~ th1/(1 + exp(-(tempo - th2)/th3)),
              data = subset(sec, nome == "LVAd-A"),
              # algorithm = "brute-force",
              algorithm = "grid-search",
              start = my_tries)
n_try

# Usar esses valores como chutes iniciais para a `nls()`.
start <- as.list(coef(n_try))

#=======================================================================
# Exemplo 4. Curva de produção em função da desfolha do algodão.

# TIP: escolha um dos níveis de estágio da planta.

cap <- read.table("http://www.leg.ufpr.br/~walmes/data/algodão.txt",
                  header = TRUE,
                  sep = "\t",
                  encoding = "latin1")

cap$desf <- cap$desf/100
cap <- subset(cap, select = c(estag, desf, pcapu))
cap$estag <- factor(cap$estag, labels = c("vegetativo",
                                          "botão floral",
                                          "florescimento",
                                          "maçã",
                                          "capulho"))
str(cap)

xyplot(pcapu ~ desf | estag,
       data = cap,
       layout = c(5, 1),
       xlab = "Nível de desfolha artificial",
       ylab = "Peso de capulhos")

# Modelo: linear com potência.
#
#   model <- pcapu ~ f0 - delta * desf^exp(curv)
#
# desf  : representado por desf (nível de desfolha artifical).
# pcapu : representado por pcapu (peso de capulhos), produto do algodão.
# f0    : intercepto, valor da função quando x=0 (situação ótima).
# delta : diferença no valor da função para x=0 e x=1 (situação
#         extrema).
# curv  : forma, indica como a função decresce, se th3=0 então função
#         linear.

#=======================================================================
# Exemplo 5. Curva de produção em função do nível de potássio no solo.

# Escolha um dos níveis de água.

soja <- read.table("http://www.leg.ufpr.br/~walmes/data/soja.txt",
                   header = TRUE,
                   sep = "\t",
                   encoding = "latin1",
                   dec = ",")

soja$agua <- factor(soja$agua)
str(soja)

xyplot(rengrao ~ potassio|agua, data=soja)

# Modelo: linear-plato.
#
#   model <- rengrao ~ f0 + tx * potassio * (potassio < brk) +
#       tx * brk * (potassio >= brk)
#
# x: representado por potássio, conteúdo de potássio do solo.
# y: representado por rengrao, redimento de grãos por parcela.
# f0: intercepto, valor da função quando x=0.
# tx: taxa de incremento no rendimento por unidade de x.
# brk: valor acima do qual a função é constante.

#-----------------------------------------------------------------------
