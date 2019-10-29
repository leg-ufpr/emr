#-----------------------------------------------------------------------
#                                            Prof. Dr. Walmes M. Zeviani
#                                leg.ufpr.br/~walmes · github.com/walmes
#                                        walmes@ufpr.br · @walmeszeviani
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Out-24 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Carrega pacotes.

library(lattice)
library(latticeExtra)
library(nlme)
library(numDeriv)

# Função que converte objeto `nls` para `lm` para ver resíduos.
source("https://raw.githubusercontent.com/walmes/wzRfun/master/R/as.lm.R")

# Para bandas de confiança.
source("https://raw.githubusercontent.com/walmes/wzRfun/master/R/panel.cbH.R")

#=======================================================================

ls("package:nlme")

#=======================================================================
# Exemplo 1. Curva de secagem do solo.

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

#-----------------------------------------------------------------------
# Ajuste em lote.

start <- list(th1 = 1, th2 = 15, th3 = 8)
xyplot(umrel ~ tempo | nome, data = sec,
       xlim = c(0, NA), ylim = c(0, NA)) +
    layer(panel.curve(th1/(1 + exp(-(x - th2)/th3))),
          data = start)

# Ajuste em lote do mesmo modelo separado para cada solo.
n0 <- nlsList(model = umrel ~ th1/(1 + exp(-(tempo - th2)/th3)) | nome,
              data = sec,
              start = start)

summary(n0)

# As estimativas pontuais para cada solo.
coef(n0)

# As estimativas intervalares para cada solo (Wald).
intervals(n0)

# Intervalos no gráfico.
plot(intervals(n0), layout = c(1, NA))

# Intervalos baseados na verossimilhança.
confint(n0)

# IMPORTANT: a `inner variable` (tempo) tem que mudar rápido e a `outer
# variable` (nome) devagar para que a predição saia correto.
pred <- with(sec,
             expand.grid(tempo = seq(0, 40, by = 0.5),
                         nome = unique(nome),
                         KEEP.OUT.ATTRS = FALSE))
pred$y <- predict(n0, newdata = pred)
str(pred)

# Sobrepondo observado e ajustado.
xyplot(umrel ~ tempo | nome, data = sec,
       xlim = c(0, NA), ylim = c(0, NA)) +
    as.layer(xyplot(y ~ tempo | nome, data = pred, type = "l"))

#-----------------------------------------------------------------------
# Ajustando com um único modelo ao invés de ajustes separados.
#
# Prós:
#
#   * é um único modelo com tipo de solo sendo um fator qualitativo e
#     tempo um fator quantitativo.
#   * pode-se verificar se existe efeito do fator solo nos parâmetros do
#     modelo e até interação.
#   * estima-se uma variância do erro comum à todos os solos.
#   * pode-se fazer restrições nos parâmetros que correspondem à
#     hipóteses e modelos reduzidos.
#
# Contras:
#
#   * é um modelo mais dificil de ajustar por ter mais parâmetros para
#     estimar.
#   * assume que a variância do erro é igual para todos os solos, mas
#     pode não ser.

# Já que sem tem as estimativas dos ajustes feitos em separado, pode-se
# usá-los como valores iniciais (bastante acurados) para o modelo
# conjunto.

# ATTENTION: lembre da parametrização tipo tratamento do R no qual o
# primeiro nível é referência e os demais são diferenças para com o
# primeiro.
start <- as.matrix(coef(n0))
start

# Ajuste do modelo conjunto.
# NOTE: o `0` na fórmula é para ter estimativas por nível de solo.
n1 <- gnls(umrel ~ th1/(1 + exp(-(tempo - th2)/th3)),
           data = sec,
           params = list(th1 ~ 0 + nome,
                         th2 ~ 0 + nome,
                         th3 ~ 0 + nome),
           start = c(start),
           verbose = TRUE)

# Neste caso as estimativas pontuais foram as mesmas.
cbind(n1 = coef(n1), n0 = unlist(coef(n0)))

# Sobrepondo observado e ajustado.
pred$y1 <- predict(n1, newdata = pred)
xyplot(umrel ~ tempo | nome, data = sec,
       xlim = c(0, NA), ylim = c(0, NA)) +
    as.layer(xyplot(y1 ~ tempo | nome, data = pred,
                    type = "l", col = "orange"))

# Agora ajustar o modelo sem o `0` na fórmula, ou seja, com a restrição
# tipo tratamento na qual se não houver diferença entre solos, k - 1
# parâmetros serão nulos (k = 3 nesse caso pois são 4 solos). Com essa
# restrição linear, pode-se testar hipóteses do efeito de solo nos
# parâmetros.

# as.matrix(coef(n0))
start <- matrix(coef(n1), ncol = 3)
start[1, , drop = FALSE] # Nível de referência.
start[-1, ]              # Demais níveis de solo.

# Valores iniciais: subtrair do nível de referência.
start <- rbind(start[1,, drop = FALSE],
               sweep(start[-1, ],
                     MARGIN = 2,
                     STATS = start[1,, drop = FALSE],
                     FUN = "-"))
start    # Em forma de matriz.
c(start) # Em forma de vetor (esteja atento na ordem dos valores).

n2 <- gnls(umrel ~ th1/(1 + exp(-(tempo - th2)/th3)),
           data = sec,
           params = list(th1 ~ nome, th2 ~ nome, th3 ~ nome),
           # params = th1 + th2 + th3 ~ nome,
           # start = c(1, 0, 0, 0,
           #           9, 0, 0, 0,
           #           7, 0, 0, 0),
           start = c(start),
           verbose = TRUE)

# Apenas confere que os valores iniciais mudaram pouco.
cbind(n2 = coef(n2), start = c(start))

# Sobrepondo observado e ajustado.
pred$y2 <- predict(n2, newdata = pred)
xyplot(umrel ~ tempo | nome, data = sec,
       xlim = c(0, NA), ylim = c(0, NA)) +
    as.layer(xyplot(y2 ~ tempo | nome, data = pred,
                    type = "l", col = "purple"))

# Teste para o efeito de solo nos parâmetros.
anova(n2, type = "marginal")

# Ajuste do modelo reduzido.
n3 <- update(n2,
             params = list(th1 ~ 1, th2 ~ nome, th3 ~ nome),
             start = c(start)[-c(2:4)])

# Teste da razão de verossimilhanças entre modelos encaixados.
anova(n3, n2)

anova(n3, type = "marginal")

#-----------------------------------------------------------------------
# Fazer a banda de confiança para o modelo `n1`.

# 1. Obter a matriz de derivadas parciais por solo.
# 2. Juntar todas as matrizes em uma só.
# 3. Determinar os extremos do IC.

# Modelo escrito como função dos parâmetros (theta).
f <- function(theta, tempo) {
    with(as.list(theta),
         th1/(1 + exp(-(tempo - th2)/th3)))
}

# Sequencia de valores da `inner variable` (tempo).
tempo_seq <- seq(0, 40, by = 0.5)

# Níveis da `outer variable` (solo).
nome_seq <- levels(sec$nome)

# Valores para predição.
grid <- with(sec,
             expand.grid(tempo = tempo_seq,
                         nome = nome_seq))
grid$fit <- predict(n1, newdata = grid)

xyplot(umrel ~ tempo | nome, data = sec,
       xlim = c(0, NA), ylim = c(0, NA)) +
    as.layer(xyplot(fit ~ tempo | nome, data = grid,
                    type = "l", col = "purple"))

# Matriz com os parâmetros de cada em uma mesma linha.
params <- matrix(coef(n1),
                 ncol = 3,
                 dimnames = list(nome_seq,
                                 c("th1", "th2", "th3")))
params

# Lista de matrizes gradiente de derivadas parciais avaliadas nas
# estimativas. Cada solo tem sua matriz de derivadas parciais.
X_partials <- sapply(nome_seq,
                     simplify = FALSE,
                     FUN = function(nome) {
                         X_partial <-
                             jacobian(func = f,
                                      x = params[nome, ],
                                      tempo = tempo_seq)
                         colnames(X_partial) <-
                             sprintf("%s.nome%s",
                                     colnames(params),
                                     nome)
                         return(X_partial)
                     })
str(X_partials)

# Cria matriz bloco diagonal a partir das matrizes na lista e atribui
# nomes.
X <- as.matrix(Matrix::bdiag(X_partials))
colnames(X) <- c(sapply(X_partials, colnames))

# IMPORTANT: Troca colunas de lugar para corresponder com a ordem dos
# parâmetros em `vcov()`.
cbind("ordem atual" = colnames(X),
      "ordem necessária" = colnames(vcov(n1)))
X <- X[, colnames(vcov(n1))]

# ATTENTION: os passos a seguir são bastante técnicos mas o que se está
# obtendo é o erro padrão do valor predito para então determinar os
# limites do intervalo de confiança.

# Choleski da matriz de covariância das estimativas.
U <- chol(vcov(n1))

# Erro padrão para cada valor predito.
grid$se <- sqrt(apply(X %*% t(U),
                      MARGIN = 1,
                      FUN = function(x) sum(x^2)))
# grid$se

# Estatística t para obter a margem de erro.
tval <- qt(p = c(lwr = 0.025, upr = 0.975),
           df = length(residuals(n1)) - length(coef(n1)))

# Obtenão dos limites de confiança.
grid <- cbind(grid,
              sweep(x = outer(X = grid$se, Y = tval, FUN = "*"),
                    MARGIN = 1,
                    STATS = grid$fit,
                    FUN = "+"))
head(grid)

# Observados, preditos e a banda de confiança.
xyplot(umrel ~ tempo | nome, data = sec,
       xlim = c(0, NA), ylim = c(0, NA)) +
    as.layer(xyplot(fit ~ tempo | nome,
                    data = grid,
                    type = "l",
                    prepanel = prepanel.cbH,
                    cty = "bands",
                    ly = grid$lwr,
                    uy = grid$upr,
                    panel = panel.cbH)) +
    layer(panel.abline(h = 1, lty = 2, col = "gray"))

#=======================================================================
# Exemplo 2. Curva de produção em função do nível de potássio no solo.

soja <- read.table("http://www.leg.ufpr.br/~walmes/data/soja.txt",
                   header = TRUE,
                   sep = "\t",
                   encoding = "latin1",
                   dec = ",")

soja$agua <- factor(soja$agua)
str(soja)

xyplot(rengrao ~ potassio | agua, data = soja)


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

source("https://raw.githubusercontent.com/walmes/wzRfun/master/R/rp.nls.R")
library(rpanel)

model <- rengrao ~ f0 + tx * potassio * (potassio < brk) +
    tx * brk * (potassio >= brk)

start <- list(f0 = c(0, 50),
              tx = c(0, 1),
              brk = c(0, 150))

fits <- rp.nls(model = model,
               data = soja,
               start = start,
               subset = "agua")

sapply(fits, FUN = coef)

# TODO: Faça o ajuste usando a `gnls()` para considerar o efeito de
# potássio e água no mesmo modelo. Obtenha as estimativas dos
# parâmetros, faça o teste de Wald para avaliar o efeito dos níveis de
# água nos parâmetros e obtenha as curvas preditas com intervalo de
# confiança para a média.

#-----------------------------------------------------------------------
# Exemplo 2. Curva de produção em função da desfolha do algodão.

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

model <- pcapu ~ f0 - delta * desf^exp(curv)

start <- list(f0 = c(20, 40),
              delta = c(0, 100),
              curv = c(-15, 15))

fits <- rp.nls(model = model,
               data = cap,
               start = start,
               subset = "estag")

sapply(fits, FUN = coef)
lapply(fits, FUN = confint.default)

# TODO: Faça o ajuste usando a `gnls()` para considerar o efeito de
# desfolha e estágio no mesmo modelo. Obtenha as estimativas dos
# parâmetros, faça o teste de Wald para avaliar o efeito dos níveis de
# estágio nos parâmetros e obtenha as curvas preditas com intervalo de
# confiança para a média.

#-----------------------------------------------------------------------
