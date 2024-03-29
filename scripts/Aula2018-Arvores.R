require(geoR)
require(mlbench)
require(ggplot2)
require(rpart)
require(rpart.plot)
require(tree)
require(kernlab)
require(ISLR)

### �rvores de classifica��o e regress�o

### Vamos simular dados de um par (x,y) em que a rela��o entre as vari�veis
### � definida por um polin�mio de grau cinco (apenas para ilustra��o).
set.seed(89438)
x <- runif(1000, -4, 3)
y <- 1/20 * (x+4) * (x+2) * (x+1) * (x-1) * (x-3) + 2 + rnorm(1000)
plot(x, y, col = 'blue', las = 1)

### Agora, vamos ajustar diferentes modelos para descrever a rela��o entre
### as vari�veis. 

### Modelo 1: modelo polinomial de grau 5.
fit1 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))
summary(fit1)
grid <- seq(-4, 3, length.out = 1000) ### Grid de valores para plotar o 
### modelo ajustado.
lines(grid, predict(fit1, newdata = data.frame(x = grid)), col = 'red', lwd = 2)

### Modelo 2: modelo polinomial de grau 3.
fit2 <- lm(y ~ x + I(x^2) + I(x^3))
lines(grid, predict(fit2, newdata = data.frame(x = grid)), col = 'red', lwd = 2, lty = 2)

### Modelo 3: regress�o suave usando polin�mios locais. Vamos usar dois diferentes fatores
### de alcance. Primeiro, para f = 0.2:
fit3 <- lowess(y ~ x, f = 0.2)
lines(fit3, col = 'green', lwd = 2)

### Modelo 4: regress�o suave usando polin�mios locais usando f = 0.6.
fit4 <- lowess(y ~ x, f = 0.6)
lines(fit4, col = 'green', lwd = 2, lty = 2)

### Agora, usando �rvores de regress�o. Vamos usar o pacote rpart.
### Vamos ajustar uma sequ�ncia de �rvores de regress�o de tamanho crescente
### (obtidas diminuindo o valor de cp)

### �rvore com dois n�s finais (cp = 0.30).
fit5 <- rpart(y ~ x, control = rpart.control(cp = 0.30))
x11()
par(mfrow = c(1,2))
rpart.plot(fit5, main = '�rvore de regress�o')
plot(x, y, col = 'blue', main = 'Dados e parti��es')
lines(grid, predict(fit5, newdata = data.frame(x = grid)), col = 'black', lwd = 2)
pcorte <- c(1.216984)
abline(v = pcorte, lty = 2)

### �rvore com tr�s n�s finais (cp = 0.08).
fit5.1 <- rpart(y ~ x, control = rpart.control(cp = 0.08))
x11()
par(mfrow = c(1,2))
rpart.plot(fit5.1, main = '�rvore de regress�o')
plot(x, y, col = 'blue', main = 'Dados e parti��es')
lines(grid, predict(fit5.1, newdata = data.frame(x = grid)), col = 'black', lwd = 2)
pcorte <- c(1.216984, -2.500086)
abline(v = pcorte, lty = 2)

### �rvore com quatro n�s finais (cp = 0.023).
fit5.2 <- rpart(y ~ x, control = rpart.control(cp = 0.023))
x11()
par(mfrow = c(1,2))
rpart.plot(fit5.2, main = '�rvore de regress�o')
plot(x, y, col = 'blue', main = 'Dados e parti��es')
lines(grid, predict(fit5.2, newdata = data.frame(x = grid)), col = 'black', lwd = 2)
pcorte <- c(1.216984, -2.500086, 1.581457)
abline(v = pcorte, lty = 2)

### �rvore com cinco n�s finais (cp = 0.02).
fit5.3 <- rpart(y ~ x, control = rpart.control(cp = 0.02))
x11()
par(mfrow = c(1,2))
rpart.plot(fit5.3, main = '�rvore de regress�o')
plot(x, y, col = 'blue', main = 'Dados e parti��es')
lines(grid, predict(fit5.3, newdata = data.frame(x = grid)), col = 'black', lwd = 2)
pcorte <- c(1.216984, -2.500086, 1.581457, 2.733882)
abline(v = pcorte, lty = 2)

### �rvore com dez n�s finais (cp = 0.005).
fit5.4 <- rpart(y ~ x, control = rpart.control(cp = 0.005))
x11()
par(mfrow = c(1,2))
rpart.plot(fit5.4, main = '�rvore de regress�o')
plot(x, y, col = 'blue', main = 'Dados e parti��es')
lines(grid, predict(fit5.4, newdata = data.frame(x = grid)), col = 'black', lwd = 2)
pcorte <- c(1.216984, -2.500086, 1.581457, 2.733882, 1.960434, -0.7068321,
            0.7129562, -3.8053, -3.107402)
abline(v = pcorte, lty = 2)

### Para finalizar, vamos considerar cp = 0.0005.
fit5.5 <- rpart(y ~ x, control = rpart.control(cp = 0.0005))
x11()
par(mfrow = c(1,2))
rpart.plot(fit5.5, main = '�rvore de regress�o')
plot(x, y, col = 'blue', main = 'Dados e parti��es')
lines(grid, predict(fit5.5, newdata = data.frame(x = grid)), col = 'black', lwd = 2)


########################################################################
### Nova simula��o. Agora, um cen�rio mais favor�vel a uma �rvore de 
### regress�o.

set.seed(8247)
med <- cut(x, c(-4, -2, 0, 1, 3), c(4, 0,2, -3))
med <- as.numeric(as.character(med))
y <- rnorm(1000, med, sd = 1)

x11()
plot(x, y, col = 'blue')

### Modelo 1: modelo polinomial de grau 5.
fit1 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))
summary(fit)
grid <- seq(-4, 3, length.out = 1000) ### Grid de valores para plotar o 
### modelo ajustado.
lines(grid, predict(fit1, newdata = data.frame(x = grid)), col = 'red', lwd = 2)

### Modelo 2: modelo polinomial de grau 3.
fit2 <- lm(y ~ x + I(x^2) + I(x^3))
lines(grid, predict(fit2, newdata = data.frame(x = grid)), col = 'red', lwd = 2, lty = 2)

### Modelo 3: regress�o suave usando polin�mios locais. Vamos usar dois diferentes fatores
### de alcance. Primeiro, para f = 0.2:
fit3 <- lowess(y ~ x, f = 0.2)
lines(fit3, col = 'blue', lwd = 2)

### Modelo 4: regress�o suave usando polin�mios locais usando f = 0.6.
fit4 <- lowess(y ~ x, f = 0.6)
lines(fit4, col = 'blue', lwd = 2, lty = 2)

### Modelo 5: �rvore de regress�o.
fit5 <- rpart(y ~ x, control = rpart.control(cp = 0.005))
lines(grid, predict(fit5, newdata = data.frame(x = grid)), col = 'black', lwd = 2)

legend(x = -4, y = 0, col = c('red', 'red', 'blue', 'blue', 'black'),
       lty = c(1,2,1,2,1), legend = paste ('Modelo ',1:5), bty = 'n')

########################################################################

### Ilustrando a import�ncia da poda.

set.seed(1882)
y <- rep(0, 1000)
x1 <- sample(c('A', 'B'), 1000, replace = TRUE)
x2 <- sample(c('C', 'D'), 1000, replace = TRUE)
p1 <- which(x1 == 'A' & x2 == 'C')
p2 <- which(x1 == 'A' & x2 == 'D')
p3 <- which(x1 == 'B' & x2 == 'C')
p4 <- which(x1 == 'B' & x2 == 'D')
y[p1] <- 2 + rnorm(length(p1), 2, 0.5)
y[p2] <- 2 + rnorm(length(p2), 0, 0.5)
y[p3] <- 2 + rnorm(length(p3), 0, 0.5)
y[p4] <- 2 + rnorm(length(p4), 2, 0.5)

dados <- data.frame(y, x1, x2)

ggplot(dados, aes(x=x1,y=y)) +
    theme_bw(base_size = 18) +
    theme(plot.title = element_text(hjust =0.5))+ 
    labs(x = 'x1', y = 'y') + 
    geom_boxplot() + theme(legend.position=c(0.5,0.9), legend.direction = 'horizontal') 

ggplot(dados, aes(x=x2,y=y)) +
    theme_bw(base_size = 18) +
    theme(plot.title = element_text(hjust =0.5))+ 
    labs(x = 'x2', y = 'y') + 
    geom_boxplot() + theme(legend.position=c(0.5,0.9), legend.direction = 'horizontal') 

ggplot(dados, aes(x=x1,y=y, fill=x2)) +
    theme_bw(base_size = 18) +
    theme(plot.title = element_text(hjust =0.5))+ 
    labs(x = 'x1', y = 'y') + 
    geom_boxplot() + theme(legend.position=c(0.5,0.9), legend.direction = 'horizontal') +
    scale_fill_manual(name = "x2", values = c("white", "grey"))

x11()
arv1 <- rpart(y ~ x1 + x2)
summary(arv1)
### Observe que a altera��o na soma de quadrados dos res�duos � m�nima
### na primeira parti��o, mas bem mais expressiva nas duas parti��es 
### seguintes. Se n�o ajust�ssemos uma grande �rvore, seguido pela poda,
### as duas parti��es em quest�o n�o seriam avaliadas.

plot(arv1)
text(arv1, use.n = TRUE)
### Neste gr�fico, a profundidade das parti��es � proporcional � explica��o
### que elas fornecem.



########################################################################
########################################################################
########################################################################
### Aplica��o 2 - �rvores de regress�o

help("wolfcamp")
datawolf <- as.data.frame(wolfcamp)
names(datawolf) <- c('x', 'y', 'data')

### Vamos ajustar uma �rvore de regress�o usando a press�o como vari�vel resposta
### e as coordenadas (x e y) como covari�veis.

x11()
ajuste <- rpart(data ~ x + y, data = datawolf)
rpart.plot(ajuste, cex = 1)
plotcp(ajuste)

### Diagrama de dispers�o.
av1 <- ggplot(datawolf, aes(x = x, y = y, color = data)) + geom_point(size = 3)
av1 + scale_color_gradientn(colours = rev(heat.colors(10))) +
    theme_bw(base_size = 18) +
    theme(plot.title = element_text(hjust =0.5))+ 
    labs(x = 'x', y = 'y') +
    coord_cartesian(xlim = c(-235, 185), ylim = c(-150, 140)) +
    annotate("text", x = 0, y = 0, label= "610", size = 5)

### Agora, novamente produzindo uma sequ�ncia de �rvores de tamanhos crescentes.
### Passo 1:
x11()
ajuste1 <- prune(ajuste, cp = 0.4)
rpart.plot(ajuste1)

ggplot(datawolf, aes(x = x, y = y, color = data)) + geom_point(size = 3) +
    scale_color_gradientn(colours = rev(heat.colors(10))) +
    theme_bw(base_size = 18) +
    theme(plot.title = element_text(hjust =0.5))+ 
    labs(x = 'x', y = 'y') +
    coord_cartesian(xlim = c(-235, 185), ylim = c(-150, 140)) +
    geom_segment(aes(x = 31.68753, y = -180, xend = 31.68753, yend = 180), size = 1.2, col = 'black', data = datawolf) +
    annotate("text", x = -80, y = 0, label= "739", size = 5) +
    annotate("text", x = 130, y = 0, label= "472", size = 5)


### Passo 2:
ajuste2 <- prune(ajuste, cp = 0.1)
rpart.plot(ajuste2)

av3 <- ggplot(datawolf, aes(x = x, y = y, color = data)) + geom_point(size = 3)
av3 + scale_color_gradientn(colours = rev(heat.colors(10))) +
    theme_bw(base_size = 18) +
    theme(plot.title = element_text(hjust =0.5))+ 
    labs(x = 'x', y = 'y') +
    coord_cartesian(xlim = c(-235, 185), ylim = c(-150, 140)) +
    geom_segment(aes(x = 31.68753, y = -180, xend = 31.68753, yend = 180), size = 1.2, col = 'black', data = datawolf) +
    geom_segment(aes(x = -280, y = -1.160868, xend = 31.68753, yend = -1.160868), size = 1.2, col = 'black', data = datawolf) +
    annotate("text", x = -80, y = 100, label= "518", size = 5) +
    annotate("text", x = -80, y = -80, label= "813", size = 5) +
    annotate("text", x = 130, y = 0, label= "472", size = 5)

### Passo 3:
ajuste3 <- prune(ajuste, cp = 0.06)
r3 <- rpart.plot(ajuste3)

av4 <- ggplot(datawolf, aes(x = x, y = y, color = data)) + geom_point(size = 3)
av4 + scale_color_gradientn(colours = rev(heat.colors(10))) +
    theme_bw(base_size = 18) +
    theme(plot.title = element_text(hjust =0.5))+ 
    labs(x = 'x', y = 'y') +
    coord_cartesian(xlim = c(-235, 185), ylim = c(-150, 140)) +
    geom_segment(aes(x = 31.68753, y = -180, xend = 31.68753, yend = 180), size = 1.2, col = 'black', data = datawolf) +
    geom_segment(aes(x = -280, y = -1.160868, xend = 31.68753, yend = -1.160868), size = 1.2, col = 'black', data = datawolf) +
    geom_segment(aes(x = 31.68753, y = 3.025524, xend = 240, yend = 3.025524), size = 1.2, col = 'black', data = datawolf) +
    annotate("text", x = -80, y = 100, label= "518", size = 5) +
    annotate("text", x = -80, y = -80, label= "813", size = 5) +
    annotate("text", x = 130, y = -80, label = "529", size = 5) +
    annotate("text", x = 130, y = 100, label = "390", size = 5)

ajuste4 <- prune(ajuste, cp = 0.02)
r4 <- rpart.plot(ajuste4)

### Passo 4:

av5 <- ggplot(datawolf, aes(x = x, y = y, color = data)) + geom_point(size = 3)
av5 + scale_color_gradientn(colours = rev(heat.colors(10))) +
    theme_bw(base_size = 18) +
    theme(plot.title = element_text(hjust =0.5))+ 
    labs(x = 'x', y = 'y') +
    coord_cartesian(xlim = c(-235, 185), ylim = c(-150, 140)) +
    geom_segment(aes(x = 31.68753, y = -180, xend = 31.68753, yend = 180), size = 1.2, col = 'black', data = datawolf) + 
    geom_segment(aes(x = -130.7734, y = -180, xend = -130.7734, yend = -1.160868), size = 1.2, col = 'black', data = datawolf) +
    geom_segment(aes(x = -280, y = -1.160868, xend = 31.68753, yend = -1.160868), size = 1.2, col = 'black', data = datawolf) +
    geom_segment(aes(x = 31.68753, y = 3.025524, xend = 240, yend = 3.025524), size = 1.2, col = 'black', data = datawolf) +
    annotate("text", x = -200, y = -80, label= "919", size = 5) +
    annotate("text", x = -50, y = -80, label = "767", size = 5) +
    annotate("text", x = 130, y = -80, label = "529", size = 5) +
    annotate("text", x = -80, y = 100, label = "518", size = 5) +
    annotate("text", x = 130, y = 100, label = "390", size = 5)

########################################################################
########################################################################
########################################################################
### Aplica��o 3 - �rvores de regress�o

help("Carseats")

# Sales: unidade vendidas (em milhares) em cada local

# CompPrice: Pre�o cobrado pela concorr�ncia em cada local

# Income: N�vel de renda da comunidade (em milhares de d�lares)

# Advertising: Or�amento de publicidade em cada local (em milhares de d�lares)

# Population: Tamanho da popula��o na regi�o (em milhares)

# Price: Imposto e taxas em cada regi�o

# ShelveLoc: Fator com n�veis ruim, bom e m�dio que indicam a qualidade do 
# local da prateleira para os assentos de carro em cada site

# Age: Idade m�dia da popula��o local

# Education: N�vel Educacional em cada local

# Urban: fator com n�veis No ou yes, se a loja est� em localidade urbana ou rural;

# US Fator com n�veis No e Yes para indicar se a loja est� nos EUA ou n�o.

set.seed(454)
ajuste <- rpart(Sales ~ ., data = Carseats)
### Ajuste da �rvore de regress�o.


### An�lise de custo-complexidade.

printcp(ajuste)
### Tabela de custo-complexidade.
### A coluna "rel error" apresenta a varia��o no erro (relativa � amostra
### original). Em "xerror", a varia��o calculada por valida��o cruzada.
### Observe que, diferentemente da varia��o relativa original, ao usar
### valida��o cruzada o erro estabiliza com o aumento da �rvore, e at�
### mesmo aumenta, em algumas situa��es.

plotcp(ajuste)
### Curva de custo-complexidade.
### Uma �rvore com seis n�s finais � a melhor op��o.

ajuste2 <- rpart(Sales ~ ., data = Carseats, control = rpart.control(cp = 0.017))
### Selecionando a �rvore com seis n�s finais.

summary(ajuste2)
### O resumo do ajuste apresenta informa��es sobre parti��es alternativas e
### secund�rias, import�ncia das vari�veis, m�dia e QMRes em cada n�.

rpart.plot(ajuste2, cex = 0.8)
### A localiza��o das cadeiras nas lojas e as taxas cobradas s�o os fatores
### mais importantes para explicar as vendas. Observe que as vendas s�o
### menores (em m�dia), em lojas em que as cadeiras est�o mal localizadas
### e em regi�es com maiores taxas sobre o produto. Outros fatores como
### or�amento em publicidade, idade e renda da popula��o local tamb�m
### s�o importantes.

boxplot(Carseats$Sales ~ ajuste2$where, ylab = 'Sales', xlab = 'Node')

predict(ajuste2) ### vetor de valores ajustados pelo modelo.

par(cex = 1.4)
plot(predict(ajuste2), residuals(ajuste2), las = 1, pch = 20)
lines(lowess(residuals(ajuste2) ~ predict(ajuste2)), col = 'red', lwd = 2)

### E se tent�ssemos uma regress�o linear?
ajuste.lm <- lm(Sales ~ ., data = Carseats)
summary(ajuste.lm)
### Observe que as vari�veis com efeito significativo s�o as mesmas que
### comp�em a �rvore, com exce��o de "CompPrice".


########################################################################
########################################################################
########################################################################
### Aplica��o 4 - �rvores de classifica��o.

head(iris)
summary(iris)
by(iris,iris$Species,summary)

### Ajustando uma �rvore para a esp�cie em fun��o das dimens�es da planta.
set.seed(97)
arvore1 <- rpart(Species ~ ., data = iris)
arvore1
summary(arvore1)
rpart.plot(arvore1)

### Vamos ilustrar a parti��o fornecida pela �rvore.

par(cex=1.5,las=1)
with(iris, plot(Petal.Length, Petal.Width, type='n', 
                xlab = 'Comprimento da p�tala',ylab = 'Largura da p�tala'))
with(iris[which(iris$Species=='setosa'),], 
     points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='red'))
with(iris[which(iris$Species=='versicolor'),],
     points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='blue'))
with(iris[which(iris$Species=='virginica'),],
     points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='green'))
lines(c(2.45,2.45), c(0,3), col='red', lty=2)
lines(c(0,8), c(1.8,1.8), col='red', lty=2)


### Tabela de confus�o.
preditos <- predict(arvore1, newdata = iris[,1:4], type='class')
table(preditos, iris$Species) 

########################################################################
########################################################################
########################################################################
### Aplica��o 5 - �rvores de classifica��o

### Spam dataset
### Objetivo - ajustar um modelo preditivo que permita identificar e-mails
### do tipo spam.


### Vamos dividir a base em duas: uma para ajuste do modelo e outra para
### valida��o.
data(spam)
set.seed(88)
help(spam)
select <- sample(1:nrow(spam), 1000)
spam_ajuste <- spam[-select,]
spam_predict <- spam[select,]

### Ajuste da �rvore.
arvSpam <- rpart(type ~ ., data = spam_ajuste)
printcp(arvSpam) ### Tabela de custo-complexidade.
plotcp(arvSpam) ### Curva de custo-complexidade.
summary(arvSpam)
x11()
rpart.plot(arvSpam, cex = 1.1)

### Usando a medida de informa��o
arvSpam2 <- rpart(type ~ ., data = spam_ajuste, parms = list(split = 'information'))
x11()
rpart.plot(arvSpam2, cex = 1.1)

### Criando a tabela "de confus�o".
predmat <- predict(arvSpam, newdata = spam_predict)
### Para cada e-mail da base de predi��o temos a probabilidade de ser spam ou n�o. 

pred <- predict(arvSpam, newdata = spam_predict, type = 'class')
### Cada e-mail da base de predi��o � classificado como spam, se P(spam) > 0.5,
### e n�o spam, caso contr�rio.

tabcruz <- table(pred, spam_predict$type)
### Tabela cruzando os tipos de e-mails e as predi��es.

sens <- tabcruz[2,2]/sum(tabcruz[,2]); sens ### Sensibilidade estimada;
### P(classificar como spam | � spam)

espec <- tabcruz[1,1]/sum(tabcruz[,1]); espec ### Especificidade estimada;
### P(n�o classificar como spam | n�o � spam)

### Usamos p = 0.50 como ponto de corte. Poder�amos ter usado algum outro valor.
### Se descartar um e-mail que n�o � spam for mais danoso que n�o descartar um
### spam (especificidade mais importante que sensibilidade), um valor maior
### para p seria apropriado. Por exemplo, para p = 0,70:

pred07 <- ifelse(predmat[,2] > 0.9, 'spam', 'nonspam')

tabcruz <- table(pred07, spam_predict$type)
### Tabela cruzando os tipos de e-mails e as predi��es.

sens <- tabcruz[2,2]/sum(tabcruz[,2]); sens ### Sensibilidade estimada;
### P(classificar como spam | � spam)

espec <- tabcruz[1,1]/sum(tabcruz[,1]); espec ### Especificidade estimada;
### P(n�o classificar como spam | n�o � spam)

### Em rela��o � regra anterior, a especificidade aumenta um pouco, mas a sensibilidade
### cai muito.

### An�lise usando curva ROC (Receiver operating characteristic)
pred2 <- predict(arvSpam, newdata = spam_predict, type = 'prob')[,2]
predroc <- roc(spam_predict$type, pred2, percent = TRUE)
plot(predroc, print.thres = 'all', print.thres.pattern.cex = 0.8,
     print.auc = TRUE, auc.polygon = TRUE)
coords(predroc, 'best')

### Vamos tentar uma regress�o log�stica

ajusteglm <- glm(type ~ ., family = 'binomial', data = spam_ajuste)

### Criando a tabela de confus�o.
glmpred <- predict(ajusteglm, newdata = spam_predict, type = 'response')
glmclass <- ifelse(glmpred > 0.5, 'spam', 'nonspam')
tabglm <- table(glmclass, spam_predict$type)

sens <- tabglm[2,2]/sum(tabglm[,2]); sens ### Sensibilidade estimada;
### P(classificar como spam | � spam)

espec <- tabglm[1,1]/sum(tabglm[,1]); espec ### Especificidade estimada;



########################################################################
########################################################################
########################################################################
### Conditional inference trees

### Voltemos ao exemplo das vendas de cadeirinhas para beb�s.

require(partykit)
help("airquality")

### Regress�o
airq <- subset(airquality, !is.na(Ozone))
airq2 <- airq[, 1:4]

airct <- ctree(Ozone ~ ., data = airq2)
plot(airct)

airct2 <- ctree(Ozone ~ ., data = airq2, control = ctree_control(mincriterion = 0.99))
plot(airct2) ### Adotando n�vel de signific�ncia de 1%.

### C�digos para sum�rio de uma �rvore de regress�o via ctrees. 
sumarioArv <- function(arvore,dados){
     dados$No95 <- predict(arvore, type = 'node')
     dadosaux <- na.omit(dados)
     indicaNo <- which(!duplicated(dadosaux$No95))
     nos <- dadosaux$No95[indicaNo]
     dataPred <- dadosaux[indicaNo,]
     sumario <- cbind(nos,predict(arvore, newdata = dataPred, 
                                  FUN = function(y,w=1) c(mean(y), sd(y), min(y), quantile(y, c(0.05, 0.25, 0.50, 0.75, 0.95)), max(y))))
     colnames(sumario)[c(1:4, 10)] <- c('N�', 'M�dia', 'DP', 'M�nimo', 'M�ximo')
     sumario <- sumario[order(sumario[,'N�']),]
     
     row.names(sumario) <- NULL
     sumario
}
sumarioArv(airct2, airq2)


### Um exemplo com dados censurados.

require(TH.data)
help(GBSG2)
require(survival)
stree <- ctree(Surv(time, cens) ~ ., data = GBSG2)
plot(stree)

### Estimando o tempo mediano de sobrevida em cada n�.

n <- predict(stree, type = "node") ### N�s em que cada paciente foi alocada.
head(n, 20)
pn <- predict(stree, newdata = GBSG2[c(1,7,6,2),], type = "node")
### Sele��o de uma paciente de cada n� para estima��o dos tempos de sobrevida.

survfit(Surv(time, cens) ~ 1, data = GBSG2) ### Geral
survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[1])) ### N� 3
survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[2])) ### N� 4
survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[3])) ### N� 6
survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[4])) ### N� 7
