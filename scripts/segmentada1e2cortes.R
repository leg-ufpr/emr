#-----------------------------------------------------------------------
# Extensões de modelos de regressão · CE 092
# leg.ufpr.br/ensino/EMR
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                       Prof. Dr. Paulo Justiniano R. Jr
#
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Ago-27 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

## Aula 28/08/2019
##
## Regressões por partes/segmentadas com 2 pontos de corte
##
rm(list=ls())
df <- read.table("http://www.leg.ufpr.br/~paulojus/CE092/df02.txt", head=TRUE)
(n <- nrow(df))
with(df, plot(Y1 ~ x)) 

## 1 corte: c=1
c=1
## 2 cortes: c1=0.7 e c2=1.5
c1=0.7; c2=1.5

##
## montando data.frame dos dados construnindo as variáveis auxiliares
## (OBS: é possível (e mais "elegante") expecificar os modelos diretamente
##  sem construir as variáveis no data.frame -- VER MAIS ABAIXO)
df <- transform(df,
                ic = ifelse(x < c, 0, 1), 
		xp = ifelse(x < c, 0, x),
		xcp = ifelse(x < c, 0, x-c),
                i1 = ifelse(x < c1, 0, 1), 
		x1 = ifelse(x < c1, 0, x),
		x1p = ifelse(x < c1, 0, x-c1),
                i2 = ifelse(x < c2, 0, 1), 
		x2 = ifelse(x < c2, 0, x),
		x2p = ifelse(x < c2, 0, x-c2)
                )

##
## Ajustes dos modelos 
##
modseg <-  list()
modseg$m0 <- lm(Y1 ~ x, data=df)
modseg$m1 <- lm(Y1 ~ x + xcp, data=df)
modseg$m2 <- lm(Y1 ~ x + x1p + x2p, data=df)

## Gráficos das Regressões ajustadas
## construindo data-frame para predição
ndf <- data.frame(x = with(df, seq(min(x), max(x), l=500)))
ndf <- transform(ndf,
                 ic = ifelse(x < c, 0, 1), 
                 xp = ifelse(x < c, 0, x),
                 xcp = ifelse(x < c, 0, x-c),
                 i1 = ifelse(x < c1, 0, 1), 
                 x1 = ifelse(x < c1, 0, x),
                 x1p = ifelse(x < c1, 0, x-c1),
                 i2 = ifelse(x < c2, 0, 1), 
                 x2 = ifelse(x < c2, 0, x),
                 x2p = ifelse(x < c2, 0, x-c2)
                 )

##
## predições como modelos ajustados
##
ndf$seg0 <- data.frame(with(modseg, predict(m0, newdata=ndf)))
ndf$seg1 <- data.frame(with(modseg, predict(m1, newdata=ndf)))
ndf$seg2 <- data.frame(with(modseg, predict(m2, newdata=ndf)))

##
## visualização das predições
##
with(df, plot(Y1 ~ x)) 
with(ndf, lines(seg0[[1]] ~ x, col=1))
with(ndf, lines(seg1[[1]] ~ x, col=2))
with(ndf, lines(seg2[[1]] ~ x, col=4))
abline(v=c(0.7, 1, 1.5), lty=2, col=c(4,2,4))


##
## Refazendo tudo SEM CRIAR EXPLICITAMENTE AS VARIÁVEIS AUXILIARES
##
## Lendo dados
adf <- read.table("http://www.leg.ufpr.br/~paulojus/CE092/df02.txt", head=TRUE)
## Ajustando modelos
amodseg <-  list()
amodseg$m0 <- lm(Y1 ~ x, data=adf)
amodseg$m1 <- lm(Y1 ~ x + I((x-1)*(x>1)), data=adf)
amodseg$m2 <- lm(Y1 ~ x + I((x-0.7)*(x>0.7)) + I((x-1.5)*(x>1.5)), data=adf)
## Criando data-frames de predição
andf <- data.frame(x = with(df, seq(min(x), max(x), l=500)))
andf$seg0 <- data.frame(with(amodseg, predict(m0, newdata=andf)))
andf$seg1 <- data.frame(with(amodseg, predict(m1, newdata=andf)))
andf$seg2 <- data.frame(with(amodseg, predict(m2, newdata=andf)))
## visualização das predições
with(adf, plot(Y1 ~ x)) 
with(andf, lines(seg0[[1]] ~ x, col=1))
with(andf, lines(seg1[[1]] ~ x, col=2))
with(andf, lines(seg2[[1]] ~ x, col=4))
abline(v=c(0.7, 1, 1.5), lty=2, col=c(4,2,4))


