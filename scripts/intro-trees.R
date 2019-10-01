require(faraway)
data(ozone)
head(ozone)

plot(ozone)
summary(ozone)

with(ozone, plot(O3 ~temp))

oztemp <- ozone[, c("O3", "temp")]
oztemp <- with(oztemp, oztemp[order(temp),])

aov(O3 ~ 1, oztemp)
aov(O3 ~ cut(temp, c(20, 50, 100)), oztemp)
aov(O3 ~ cut(temp, c(20, 60, 100)), oztemp)
aov(O3 ~ cut(temp, c(20, 70, 100)), oztemp)

m <- aov(O3 ~ cut(temp, c(20, 50, 100)), oztemp)
names(m)
anova(m)
names(anova(m))
anova(m)[[2]]
anova(m)[["Sum Sq"]][2]

## código "amador", não otimizado!
rssC <- function(pc, y, x)
    anova(aov(O3 ~ cut(temp, c(min(x), pc, max(x)), include=TRUE), oztemp))[["Sum Sq"]][2]

with(oztemp, rssC(50, O3, temp))
with(oztemp, rssC(60, O3, temp))
with(oztemp, rssC(70, O3, temp))

foo <- c(50,60,70)
with(oztemp, sapply(foo, rssC, y=O3, x=temp))

(aptemp <- unique(oztemp$temp))
(cortes <- aptemp[-length(aptemp)]+diff(aptemp)/2)

rss1 <- with(oztemp, sapply(cortes, rssC, y=O3, x=temp))

plot(cortes, rss1, type="b")
(c1 <- cortes[which.min(rss1)])

with(oztemp, plot(O3 ~temp))

#oztemp <- transform(oztemp, I1 = ifelse(temp < c1, 0, 1))
#(fit1 <- lm(O3 ~ I1, data=oztemp))
(fit1 <- lm(O3 ~ ifelse(temp < c1, 0, 1), data=oztemp))

noztemp <- with(oztemp, data.frame(temp=seq(min(temp), max(temp), l=501)))
noztemp <- transform(noztemp, y1 = predict(fit1, newdata=noztemp))

with(noztemp, lines(y1 ~ temp, col=2))


## Partição em segundo nível
oztemp1.1 <- subset(oztemp, temp < c1)
dim(oztemp1.1)

(aptemp <- unique(oztemp1.1$temp))
(cortes1.1 <- aptemp[-length(aptemp)]+diff(aptemp)/2)
rss1.1 <- with(oztemp1.1, sapply(cortes1.1, rssC, y=O3, x=temp))

plot(cortes1.1, rss1.1, type="b")
(c1.1 <- cortes1.1[which.min(rss1.1)])

(fit1.1 <- lm(O3 ~ ifelse(temp < c1.1, 0, 1), data=oztemp1.1))
noztemp1.1 <- with(oztemp1.1, data.frame(temp=seq(min(temp), max(temp), l=501)))
noztemp1.1 <- transform(noztemp1.1, y1 = predict(fit1.1, newdata=noztemp1.1))

#
oztemp1.2 <- subset(oztemp, temp >= c1)
dim(oztemp1.2)

(aptemp <- unique(oztemp1.2$temp))
(cortes1.2 <- aptemp[-length(aptemp)]+diff(aptemp)/2)
rss1.2 <- with(oztemp1.2, sapply(cortes1.2, rssC, y=O3, x=temp))

plot(cortes1.2, rss1.2, type="b")
(c1.2 <- cortes1.2[which.min(rss1.2)])

(fit1.2 <- lm(O3 ~ ifelse(temp < c1.2, 0, 1), data=oztemp1.2))
noztemp1.2 <- with(oztemp1.2, data.frame(temp=seq(min(temp), max(temp), l=501)))
noztemp1.2 <- transform(noztemp1.2, y1 = predict(fit1.2, newdata=noztemp1.2))

with(oztemp, plot(O3 ~ temp))
with(noztemp, lines(y1 ~ temp, col=2))
with(noztemp1.1, lines(y1 ~ temp, col=4))
with(noztemp1.2, lines(y1 ~ temp, col=4))

##
require(rpart)
require(rpart.plot)

fit <- rpart(O3 ~ temp, data=oztemp)
#fit <- rpart(O3 ~ temp, control = rpart.control(cp = 0.30), data=oztemp)
#fit <- rpart(O3 ~ temp, control = rpart.control(cp = 0.05), data=oztemp)
#fit <- rpart(O3 ~ temp, control = rpart.control(cp = 0.01), data=oztemp)
rpart.plot(fit, main = 'Árvore de regressão')

with(oztemp, plot(O3 ~ temp))

grid <- seq(20,100, length=501)
lines(grid, predict(fit,
                    newdata = data.frame(temp = grid)),
      col = 'black', lwd = 2)

fit



