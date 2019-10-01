require(geoR)
require(mlbench)
require(ggplot2)
require(rpart)
require(rpart.plot)
require(tree)
require(kernlab)
require(ISLR)

### Árvores de classificação e regressão

### Vamos simular dados de um par (x,y) em que a relação entre as variáveis
### é definida por um polinômio de grau cinco (apenas para ilustração).
set.seed(89438)
x <- runif(1000, -4, 3)
y <- 1/20 * (x+4) * (x+2) * (x+1) * (x-1) * (x-3) + 2 + rnorm(1000)
plot(x, y, col = 'blue', las = 1)

### Agora, vamos ajustar diferentes modelos para descrever a relação entre
### as variáveis. 

### Modelo 1: modelo polinomial de grau 5.
fit1 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))
summary(fit1)
grid <- seq(-4, 3, length.out = 1000) ### Grid de valores para plotar o 
### modelo ajustado.
lines(grid, predict(fit1, newdata = data.frame(x = grid)), col = 'red', lwd = 2)

### Modelo 2: modelo polinomial de grau 3.
fit2 <- lm(y ~ x + I(x^2) + I(x^3))
lines(grid, predict(fit2, newdata = data.frame(x = grid)), col = 'red', lwd = 2, lty = 2)

### Modelo 3: regressão suave usando polinômios locais. Vamos usar dois diferentes fatores
### de alcance. Primeiro, para f = 0.2:
fit3 <- lowess(y ~ x, f = 0.2)
lines(fit3, col = 'green', lwd = 2)

### Modelo 4: regressão suave usando polinômios locais usando f = 0.6.
fit4 <- lowess(y ~ x, f = 0.6)
lines(fit4, col = 'green', lwd = 2, lty = 2)

### Agora, usando árvores de regressão. Vamos usar o pacote rpart.
### Vamos ajustar uma sequência de árvores de regressão de tamanho crescente
### (obtidas diminuindo o valor de cp)

### Árvore com dois nós finais (cp = 0.30).
fit5 <- rpart(y ~ x, control = rpart.control(cp = 0.30))
x11()
par(mfrow = c(1,2))
rpart.plot(fit5, main = 'Árvore de regressão')
plot(x, y, col = 'blue', main = 'Dados e partições')
lines(grid, predict(fit5, newdata = data.frame(x = grid)), col = 'black', lwd = 2)
pcorte <- c(1.216984)
abline(v = pcorte, lty = 2)

### Árvore com três nós finais (cp = 0.08).
fit5.1 <- rpart(y ~ x, control = rpart.control(cp = 0.08))
x11()
par(mfrow = c(1,2))
rpart.plot(fit5.1, main = 'Árvore de regressão')
plot(x, y, col = 'blue', main = 'Dados e partições')
lines(grid, predict(fit5.1, newdata = data.frame(x = grid)), col = 'black', lwd = 2)
pcorte <- c(1.216984, -2.500086)
abline(v = pcorte, lty = 2)

### Árvore com quatro nós finais (cp = 0.023).
fit5.2 <- rpart(y ~ x, control = rpart.control(cp = 0.023))
x11()
par(mfrow = c(1,2))
rpart.plot(fit5.2, main = 'Árvore de regressão')
plot(x, y, col = 'blue', main = 'Dados e partições')
lines(grid, predict(fit5.2, newdata = data.frame(x = grid)), col = 'black', lwd = 2)
pcorte <- c(1.216984, -2.500086, 1.581457)
abline(v = pcorte, lty = 2)

### Árvore com cinco nós finais (cp = 0.02).
fit5.3 <- rpart(y ~ x, control = rpart.control(cp = 0.02))
x11()
par(mfrow = c(1,2))
rpart.plot(fit5.3, main = 'Árvore de regressão')
plot(x, y, col = 'blue', main = 'Dados e partições')
lines(grid, predict(fit5.3, newdata = data.frame(x = grid)), col = 'black', lwd = 2)
pcorte <- c(1.216984, -2.500086, 1.581457, 2.733882)
abline(v = pcorte, lty = 2)

### Árvore com dez nós finais (cp = 0.005).
fit5.4 <- rpart(y ~ x, control = rpart.control(cp = 0.005))
x11()
par(mfrow = c(1,2))
rpart.plot(fit5.4, main = 'Árvore de regressão')
plot(x, y, col = 'blue', main = 'Dados e partições')
lines(grid, predict(fit5.4, newdata = data.frame(x = grid)), col = 'black', lwd = 2)
pcorte <- c(1.216984, -2.500086, 1.581457, 2.733882, 1.960434, -0.7068321,
            0.7129562, -3.8053, -3.107402)
abline(v = pcorte, lty = 2)

### Para finalizar, vamos considerar cp = 0.0005.
fit5.5 <- rpart(y ~ x, control = rpart.control(cp = 0.0005))
x11()
par(mfrow = c(1,2))
rpart.plot(fit5.5, main = 'Árvore de regressão')
plot(x, y, col = 'blue', main = 'Dados e partições')
lines(grid, predict(fit5.5, newdata = data.frame(x = grid)), col = 'black', lwd = 2)


########################################################################
### Nova simulação. Agora, um cenário mais favorável a uma árvore de 
### regressão.

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

### Modelo 3: regressão suave usando polinômios locais. Vamos usar dois diferentes fatores
### de alcance. Primeiro, para f = 0.2:
fit3 <- lowess(y ~ x, f = 0.2)
lines(fit3, col = 'blue', lwd = 2)

### Modelo 4: regressão suave usando polinômios locais usando f = 0.6.
fit4 <- lowess(y ~ x, f = 0.6)
lines(fit4, col = 'blue', lwd = 2, lty = 2)

### Modelo 5: árvore de regressão.
fit5 <- rpart(y ~ x, control = rpart.control(cp = 0.005))
lines(grid, predict(fit5, newdata = data.frame(x = grid)), col = 'black', lwd = 2)

legend(x = -4, y = 0, col = c('red', 'red', 'blue', 'blue', 'black'),
       lty = c(1,2,1,2,1), legend = paste ('Modelo ',1:5), bty = 'n')

########################################################################

### Ilustrando a importância da poda.

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
### Observe que a alteração na soma de quadrados dos resíduos é mínima
### na primeira partição, mas bem mais expressiva nas duas partições 
### seguintes. Se não ajustássemos uma grande árvore, seguido pela poda,
### as duas partições em questão não seriam avaliadas.

plot(arv1)
text(arv1, use.n = TRUE)
### Neste gráfico, a profundidade das partições é proporcional à explicação
### que elas fornecem.



########################################################################
########################################################################
########################################################################
### Aplicação 2 - árvores de regressão

help("wolfcamp")
datawolf <- as.data.frame(wolfcamp)
names(datawolf) <- c('x', 'y', 'data')

### Vamos ajustar uma árvore de regressão usando a pressão como variável resposta
### e as coordenadas (x e y) como covariáveis.

x11()
ajuste <- rpart(data ~ x + y, data = datawolf)
rpart.plot(ajuste, cex = 1)
plotcp(ajuste)

### Diagrama de dispersão.
av1 <- ggplot(datawolf, aes(x = x, y = y, color = data)) + geom_point(size = 3)
av1 + scale_color_gradientn(colours = rev(heat.colors(10))) +
    theme_bw(base_size = 18) +
    theme(plot.title = element_text(hjust =0.5))+ 
    labs(x = 'x', y = 'y') +
    coord_cartesian(xlim = c(-235, 185), ylim = c(-150, 140)) +
    annotate("text", x = 0, y = 0, label= "610", size = 5)

### Agora, novamente produzindo uma sequência de árvores de tamanhos crescentes.
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
### Aplicação 3 - árvores de regressão

help("Carseats")

# Sales: unidade vendidas (em milhares) em cada local

# CompPrice: Preço cobrado pela concorrência em cada local

# Income: Nível de renda da comunidade (em milhares de dólares)

# Advertising: Orçamento de publicidade em cada local (em milhares de dólares)

# Population: Tamanho da população na região (em milhares)

# Price: Imposto e taxas em cada região

# ShelveLoc: Fator com níveis ruim, bom e médio que indicam a qualidade do 
# local da prateleira para os assentos de carro em cada site

# Age: Idade média da população local

# Education: Nível Educacional em cada local

# Urban: fator com níveis No ou yes, se a loja está em localidade urbana ou rural;

# US Fator com níveis No e Yes para indicar se a loja está nos EUA ou não.

set.seed(454)
ajuste <- rpart(Sales ~ ., data = Carseats)
### Ajuste da árvore de regressão.


### Análise de custo-complexidade.

printcp(ajuste)
### Tabela de custo-complexidade.
### A coluna "rel error" apresenta a variação no erro (relativa à amostra
### original). Em "xerror", a variação calculada por validação cruzada.
### Observe que, diferentemente da variação relativa original, ao usar
### validação cruzada o erro estabiliza com o aumento da árvore, e até
### mesmo aumenta, em algumas situações.

plotcp(ajuste)
### Curva de custo-complexidade.
### Uma árvore com seis nós finais é a melhor opção.

ajuste2 <- rpart(Sales ~ ., data = Carseats, control = rpart.control(cp = 0.017))
### Selecionando a árvore com seis nós finais.

summary(ajuste2)
### O resumo do ajuste apresenta informações sobre partições alternativas e
### secundárias, importância das variáveis, média e QMRes em cada nó.

rpart.plot(ajuste2, cex = 0.8)
### A localização das cadeiras nas lojas e as taxas cobradas são os fatores
### mais importantes para explicar as vendas. Observe que as vendas são
### menores (em média), em lojas em que as cadeiras estão mal localizadas
### e em regiões com maiores taxas sobre o produto. Outros fatores como
### orçamento em publicidade, idade e renda da população local também
### são importantes.

boxplot(Carseats$Sales ~ ajuste2$where, ylab = 'Sales', xlab = 'Node')

predict(ajuste2) ### vetor de valores ajustados pelo modelo.

par(cex = 1.4)
plot(predict(ajuste2), residuals(ajuste2), las = 1, pch = 20)
lines(lowess(residuals(ajuste2) ~ predict(ajuste2)), col = 'red', lwd = 2)

### E se tentássemos uma regressão linear?
ajuste.lm <- lm(Sales ~ ., data = Carseats)
summary(ajuste.lm)
### Observe que as variáveis com efeito significativo são as mesmas que
### compõem a árvore, com exceção de "CompPrice".


########################################################################
########################################################################
########################################################################
### Aplicação 4 - Árvores de classificação.

head(iris)
summary(iris)
by(iris,iris$Species,summary)

### Ajustando uma árvore para a espécie em função das dimensões da planta.
set.seed(97)
arvore1 <- rpart(Species ~ ., data = iris)
arvore1
summary(arvore1)
rpart.plot(arvore1)

### Vamos ilustrar a partição fornecida pela árvore.

par(cex=1.5,las=1)
with(iris, plot(Petal.Length, Petal.Width, type='n', 
                xlab = 'Comprimento da pétala',ylab = 'Largura da pétala'))
with(iris[which(iris$Species=='setosa'),], 
     points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='red'))
with(iris[which(iris$Species=='versicolor'),],
     points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='blue'))
with(iris[which(iris$Species=='virginica'),],
     points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='green'))
lines(c(2.45,2.45), c(0,3), col='red', lty=2)
lines(c(0,8), c(1.8,1.8), col='red', lty=2)


### Tabela de confusão.
preditos <- predict(arvore1, newdata = iris[,1:4], type='class')
table(preditos, iris$Species) 

########################################################################
########################################################################
########################################################################
### Aplicação 5 - árvores de classificação

### Spam dataset
### Objetivo - ajustar um modelo preditivo que permita identificar e-mails
### do tipo spam.


### Vamos dividir a base em duas: uma para ajuste do modelo e outra para
### validação.
data(spam)
set.seed(88)
help(spam)
select <- sample(1:nrow(spam), 1000)
spam_ajuste <- spam[-select,]
spam_predict <- spam[select,]

### Ajuste da árvore.
arvSpam <- rpart(type ~ ., data = spam_ajuste)
printcp(arvSpam) ### Tabela de custo-complexidade.
plotcp(arvSpam) ### Curva de custo-complexidade.
summary(arvSpam)
x11()
rpart.plot(arvSpam, cex = 1.1)

### Usando a medida de informação
arvSpam2 <- rpart(type ~ ., data = spam_ajuste, parms = list(split = 'information'))
x11()
rpart.plot(arvSpam2, cex = 1.1)

### Criando a tabela "de confusão".
predmat <- predict(arvSpam, newdata = spam_predict)
### Para cada e-mail da base de predição temos a probabilidade de ser spam ou não. 

pred <- predict(arvSpam, newdata = spam_predict, type = 'class')
### Cada e-mail da base de predição é classificado como spam, se P(spam) > 0.5,
### e não spam, caso contrário.

tabcruz <- table(pred, spam_predict$type)
### Tabela cruzando os tipos de e-mails e as predições.

sens <- tabcruz[2,2]/sum(tabcruz[,2]); sens ### Sensibilidade estimada;
### P(classificar como spam | é spam)

espec <- tabcruz[1,1]/sum(tabcruz[,1]); espec ### Especificidade estimada;
### P(não classificar como spam | não é spam)

### Usamos p = 0.50 como ponto de corte. Poderíamos ter usado algum outro valor.
### Se descartar um e-mail que não é spam for mais danoso que não descartar um
### spam (especificidade mais importante que sensibilidade), um valor maior
### para p seria apropriado. Por exemplo, para p = 0,70:

pred07 <- ifelse(predmat[,2] > 0.9, 'spam', 'nonspam')

tabcruz <- table(pred07, spam_predict$type)
### Tabela cruzando os tipos de e-mails e as predições.

sens <- tabcruz[2,2]/sum(tabcruz[,2]); sens ### Sensibilidade estimada;
### P(classificar como spam | é spam)

espec <- tabcruz[1,1]/sum(tabcruz[,1]); espec ### Especificidade estimada;
### P(não classificar como spam | não é spam)

### Em relação à regra anterior, a especificidade aumenta um pouco, mas a sensibilidade
### cai muito.

### Análise usando curva ROC (Receiver operating characteristic)
pred2 <- predict(arvSpam, newdata = spam_predict, type = 'prob')[,2]
predroc <- roc(spam_predict$type, pred2, percent = TRUE)
plot(predroc, print.thres = 'all', print.thres.pattern.cex = 0.8,
     print.auc = TRUE, auc.polygon = TRUE)
coords(predroc, 'best')

### Vamos tentar uma regressão logística

ajusteglm <- glm(type ~ ., family = 'binomial', data = spam_ajuste)

### Criando a tabela de confusão.
glmpred <- predict(ajusteglm, newdata = spam_predict, type = 'response')
glmclass <- ifelse(glmpred > 0.5, 'spam', 'nonspam')
tabglm <- table(glmclass, spam_predict$type)

sens <- tabglm[2,2]/sum(tabglm[,2]); sens ### Sensibilidade estimada;
### P(classificar como spam | é spam)

espec <- tabglm[1,1]/sum(tabglm[,1]); espec ### Especificidade estimada;



########################################################################
########################################################################
########################################################################
### Conditional inference trees

### Voltemos ao exemplo das vendas de cadeirinhas para bebês.

require(partykit)
help("airquality")

### Regressão
airq <- subset(airquality, !is.na(Ozone))
airq2 <- airq[, 1:4]

airct <- ctree(Ozone ~ ., data = airq2)
plot(airct)

airct2 <- ctree(Ozone ~ ., data = airq2, control = ctree_control(mincriterion = 0.99))
plot(airct2) ### Adotando nível de significância de 1%.

### Códigos para sumário de uma árvore de regressão via ctrees. 
sumarioArv <- function(arvore,dados){
     dados$No95 <- predict(arvore, type = 'node')
     dadosaux <- na.omit(dados)
     indicaNo <- which(!duplicated(dadosaux$No95))
     nos <- dadosaux$No95[indicaNo]
     dataPred <- dadosaux[indicaNo,]
     sumario <- cbind(nos,predict(arvore, newdata = dataPred, 
                                  FUN = function(y,w=1) c(mean(y), sd(y), min(y), quantile(y, c(0.05, 0.25, 0.50, 0.75, 0.95)), max(y))))
     colnames(sumario)[c(1:4, 10)] <- c('Nó', 'Média', 'DP', 'Mínimo', 'Máximo')
     sumario <- sumario[order(sumario[,'Nó']),]
     
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

### Estimando o tempo mediano de sobrevida em cada nó.

n <- predict(stree, type = "node") ### Nós em que cada paciente foi alocada.
head(n, 20)
pn <- predict(stree, newdata = GBSG2[c(1,7,6,2),], type = "node")
### Seleção de uma paciente de cada nó para estimação dos tempos de sobrevida.

survfit(Surv(time, cens) ~ 1, data = GBSG2) ### Geral
survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[1])) ### Nó 3
survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[2])) ### Nó 4
survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[3])) ### Nó 6
survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[4])) ### Nó 7
