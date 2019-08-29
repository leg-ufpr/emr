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

rm(list=ls())

dat <- read.table("http://www.leg.ufpr.br/~paulojus/CE092/df03.txt", head=TRUE, sep=",")
ndat <- data.frame(x = seq(0, 2, len=201))

with(dat, plot(y ~ x))

reg0.4 <- lm(y ~ x + I((x-0.4)*(x>=0.4)), data=dat)
ndat$y0.4 <- predict(reg0.4, newdata=ndat)
with(ndat, lines(y0.4 ~x)) 

reg0.5 <- lm(y ~ x + I((x-0.5)*(x>=0.5)), data=dat)
ndat$y0.5 <- predict(reg0.5, newdata=ndat)
with(ndat, lines(y0.5 ~x, col=2)) 

reg0.6 <- lm(y ~ x + I((x-0.6)*(x>=0.6)), data=dat)
ndat$y0.6 <- predict(reg0.6, newdata=ndat)
with(ndat, lines(y0.6 ~x, col=4)) 

c(logLik(reg0.4),logLik(reg0.5),logLik(reg0.6))

regseg.f <- function(k, data, ndata, lines=FALSE, fit=TRUE, ...){
    reg <- lm(y ~ x + I((x-k)*(x>=k)), data=data)
    if(lines){
        ndata$yK <- predict(reg, newdata=ndata)
        lines(yK ~ x, data=ndata, ...)
    }
    if(fit) return(reg)
    else return(invisible())
}

regseg.f(0.4, data=dat, ndata=ndat, lines=TRUE, lwd=2)
regseg.f(0.5, data=dat, ndata=ndat, lines=TRUE, lwd=2, col=5)
regseg.f(0.6, data=dat, ndata=ndat, lines=TRUE, lwd=2, col=4)

Ks <- seq(0, 2, by=0.1)
fitK <- lapply(Ks, regseg.f, data=dat, ndata=ndat)
names(fitK) <- paste("K", Ks, sep="-")

lLs <- sapply(fitK, logLik)

plot(Ks, lLs, ty="l")
lLs[which.max(lLs)]
## experimente novamente com um grid mais fino nos Ks

with(dat, plot(y ~ x))

Bfit <- fitK[[which.max(lLs)]]
ndat$ypred <- predict(Bfit, newdata=ndat)
lines(ypred ~  x, data=ndat, type="l", col=4)

##
## Dados anteriores do curso
##
df <- read.table("http://www.leg.ufpr.br/~paulojus/CE092/df02.txt", head=TRUE, sep="")
df
ndf <- data.frame(x = seq(0, 2, len=201))

df <- transform(df, y = Y1)
with(df, plot(y ~ x))

Ks <- seq(0, 2, by=0.2)
fitK <- lapply(Ks, regseg.f, data=df, ndata=ndf)
names(fitK) <- paste("K",Ks, sep="-")

lLs <- sapply(fitK, logLik)

plot(Ks, lLs, ty="l")
lLs[which.max(lLs)]
## refinar Ks ?

with(df, plot(y ~ x))

ndf$Y <- predict(fitK[[which.max(lLs)]], newdata=ndf)
lines(Y ~  x, data=ndf, type="l", col=4)
