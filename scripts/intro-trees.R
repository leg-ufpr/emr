#-----------------------------------------------------------------------
# Extensões de modelos de regressão · CE 092
# web.leg.ufpr.br/ensino/EMR
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                       Prof. Dr. Paulo Justiniano R. Jr
# Parte dos comandos copiada/adaptada de Faraway 
# ttps://people.bath.ac.uk/jjf23/ELM/
#
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Ago-29 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------
#
#rm(list=ls())
## Salva parâmetros gráficos.
oldpar <- par(no.readonly=TRUE)
#rm(list=ls())
##
## Selecionando pacote e base de dados para ser utilizada como exemplo
##
data(ozone, package="faraway")
head(ozone)

## Visualização inicial dos dados
plot(ozone)
car::scatterplotMatrix(ozone)
summary(ozone)

## Inicialmente vamos utilizar apenas uma covariável para ilustrar as ideias principais
with(ozone, plot(O3 ~ temp))
## Montando um data-frame apenas com os dados a serem utilizados
oztemp <- ozone[, c("O3", "temp")]
oztemp <- with(oztemp, oztemp[order(temp),])

## Ajustes de médias por seggmentos para alguns pontos de corte
aov(O3 ~ 1, oztemp)
aov(O3 ~ cut(temp, c(20, 50, 100)), oztemp)
aov(O3 ~ cut(temp, c(20, 60, 100)), oztemp)
aov(O3 ~ cut(temp, c(20, 70, 100)), oztemp)

## dentre as opções acima 70 parece ser o melhor. Vamos ver como extrair informações
## relevantes da aov() em especial a soma de quadrados
m <- aov(O3 ~ cut(temp, c(20, 70, 100)), oztemp)
names(m)
anova(m)
names(anova(m))
anova(m)[[2]]
anova(m)[["Sum Sq"]][2]

## Agora passamos a escolher o melhor ponto de corte para todo o conjunto de dados

## Vamos inicialmente definir uma função que retorna a soma de quadrados para um dado corte

## código "amador", não otimizado!
rssC <- function(pc, y, x)
    anova(aov(O3 ~ cut(temp, c(min(x), pc, max(x)), include=TRUE), oztemp))[["Sum Sq"]][2]

## testando para os cortes anteriores
with(oztemp, rssC(50, O3, temp))
with(oztemp, rssC(60, O3, temp))
with(oztemp, rssC(70, O3, temp))

## idem anterior de uma só vez
foo <- c(50,60,70)
with(oztemp, sapply(foo, rssC, y=O3, x=temp))

## E agora vamos tomar como o conjunto de possíveis pontos de corte
## os pontos médios dos intervalos entre os valores das covariáveis
(aptemp <- unique(oztemp$temp))
(cortes <- aptemp[-length(aptemp)]+diff(aptemp)/2)

## calculando e visualizando para ver qual ponto de corte mais reduz a soma de quadrados de resíduos
rss1 <- with(oztemp, sapply(cortes, rssC, y=O3, x=temp))

plot(cortes, rss1, type="b")
(c1 <- cortes[which.min(rss1)])
(rss1 <- rss1[which.min(rss1)])

## visualizando o modelo com o 1o corte
with(oztemp, plot(O3 ~ temp))
#oztemp <- transform(oztemp, I1 = ifelse(temp < c1, 0, 1))
#(fit1 <- lm(O3 ~ I1, data=oztemp))
(fit1 <- lm(O3 ~ ifelse(temp < c1, 0, 1), data=oztemp))
noztemp <- with(oztemp, data.frame(temp=seq(min(temp), max(temp), l=501)))
noztemp <- transform(noztemp, y1 = predict(fit1, newdata=noztemp))

with(noztemp, lines(y1 ~ temp, col=2))


## Partições em segundo nível

## para 1a partição
oztemp1.1 <- subset(oztemp, temp < c1)
dim(oztemp1.1)

(aptemp <- unique(oztemp1.1$temp))
(cortes1.1 <- aptemp[-length(aptemp)]+diff(aptemp)/2)
rss1.1 <- with(oztemp1.1, sapply(cortes1.1, rssC, y=O3, x=temp))

plot(cortes1.1, rss1.1, type="b")
(c1.1 <- cortes1.1[which.min(rss1.1)])
(rss1.1 <- rss1.1[which.min(rss1.1)])

(fit1.1 <- lm(O3 ~ ifelse(temp < c1.1, 0, 1), data=oztemp1.1))
noztemp1.1 <- with(oztemp1.1, data.frame(temp=seq(min(temp), max(temp), l=501)))
noztemp1.1 <- transform(noztemp1.1, y1 = predict(fit1.1, newdata=noztemp1.1))

## para 2a partição
oztemp1.2 <- subset(oztemp, temp >= c1)
dim(oztemp1.2)

(aptemp <- unique(oztemp1.2$temp))
(cortes1.2 <- aptemp[-length(aptemp)]+diff(aptemp)/2)
rss1.2 <- with(oztemp1.2, sapply(cortes1.2, rssC, y=O3, x=temp))

plot(cortes1.2, rss1.2, type="b")
(c1.2 <- cortes1.2[which.min(rss1.2)])
(rss1.2 <- rss1.2[which.min(rss1.2)])

(fit1.2 <- lm(O3 ~ ifelse(temp < c1.2, 0, 1), data=oztemp1.2))
noztemp1.2 <- with(oztemp1.2, data.frame(temp=seq(min(temp), max(temp), l=501)))
noztemp1.2 <- transform(noztemp1.2, y1 = predict(fit1.2, newdata=noztemp1.2))


## Modelo com partições de dois níveis (em ambos lados)
with(oztemp, plot(O3 ~ temp))
with(noztemp, lines(y1 ~ temp, col=2))
with(noztemp1.1, lines(y1 ~ temp, col=4))
with(noztemp1.2, lines(y1 ~ temp, col=4))

## o mecanismo acima pode ser continuado para subpartições 
## etc etc

## Agora iremos utilizar uma implementação do pacote rpart
##
require(rpart)
require(rpart.plot)

## obtendo a árvore com opções "default" do pacote
fit <- rpart(O3 ~ temp, data=oztemp)
## visualizações alternativas do ajuste
plot(fit)
text(fit)
plot(fit, uniform=TRUE)
text(fit)
## ... e por coincidência foi ajustada uma árvore apenas em dóis níveis!

## Saída que descreve a árvore e partições
## verificar valores em relação aos obtidos anteriormente!!!
fit
## uma saída mais detalhada:
summary(fit)

## o pacotye rpart.plot fornece diversas visualizações adicionais/alternativas 
rpart.plot(fit, main = 'Árvore de regressão')
## explorar as opções da função/pacote!!!

## visualizando predições
with(oztemp, plot(O3 ~ temp))
grid <- seq(20,100, length=501)
lines(grid, predict(fit,
                    newdata = data.frame(temp = grid)),
      col = 'black', lwd = 2)
## tal como obtivemos anteriormente!!


## Vamos agora forçar mais divisoes
fit <- rpart(O3 ~ temp, control = rpart.control(cp = 0.001), data=oztemp)
plot(fit)
fit

##
printcp(fit)
plotcp(fit)

## uma pode "arbitrária"
fit1 <- prune.rpart(fit,0.005)
plot(fit1)

## Pode-se neste ponto discutir/adotar diferentes critérios de ajuste

## Notar que os resultados de validação cruzada envolvem sorteios aleatórios
## e portanto não se repetem a menos que seja "setada" a semente

## Intuitivamente a função de custo/;complexidade é uma expécia de análogo
## a critérios como AIC e semilates
## ... enquanto que o parâmetro \alpha é análogo a paãmostros que definem
## um grau de suavização me métodos suavizadores


##
## Análises com duas covariáveis
##
(fit2 <- rpart(O3 ~ temp + humidity,ozone))
plot(fit2)
text(fit2)

## 
x11()
with(ozone, plot(humidity ~temp))
abline(v=67.5, lty=1, lwd=2)                            # 1o nivel
abline(v=58.5, lty=2, col=2, lwd=2)                     # 2o nivel
abline(v=79.5, lty=2, col=2, lwd=2)                     # 2o nivel
segments(67.5, 59.5, 79.5, 59.5, lty=3, col=4, lwd=2)   # 3o nivel
dev.off()


(fit2 <- rpart(O3 ~ temp + humidity, , control = rpart.control(cp = 0.005), data=ozone))
#(fit2 <- rpart(O3 ~ temp + humidity, , control = rpart.control(cp = 0.001), data=ozone))
plot(fit2)
text(fit2)
plotcp(fit2)
##
## etc etc etc ... qual árvore vc escolheria?
##

## agora com todas as covariáveis
(fitall <- rpart(O3 ~ ., ozone))
## agora desenhe voce mesmo a árvore baseada neste "output"
##...
##...
##...
## e depois confira:
plot(fitall)
text(fitall)

plot(fitall,compress=T,uniform=T,branch=0.4)
text(fitall)

## gráficos diagnóstico usuais (como em regressão são possíveis)
plot(jitter(predict(fitall)),residuals(fitall),xlab="Fitted",ylab="Residuals")
abline(h=0)
qqnorm(residuals(fitall))
qqline(residuals(fitall))

## e predições para novos valores das covariáveis tb usam predict()
## por exemplo predizendo no valor médiano das covariáveis
(x0 <- apply(ozone[,-1],2,median))
predict(fitall,data.frame(t(x0)))

## vamos inversitgas a árvore em mais detalhes
## será tulizazada validação cruzada.
## Vamos então adotar uma semente comum para todos
set.seed(2019)
fitall <- rpart(O3 ~ .,ozone,cp=0.001)
printcp(fitall)
plotcp(fitall)

library(rpart.plot)
rpart.plot(fitall, type=3)
fitallr <- prune.rpart(fitall,0.02)
rpart.plot(fitallr, type=3)

## quanlidade do ajuste (equivalente a um R^2)
1-sum(residuals(fitallr)^2)/sum((ozone$O3-mean(ozone$O3))^2)

## instabilidade ...
set.seed(123)
fitall <- rpart(O3 ~ ., ozone[sample(330,165),])
(fitalls <- prune.rpart(fitall,0.02))
x11()
rpart.plot(fitalls, type=3)

