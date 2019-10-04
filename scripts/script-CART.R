
### Carregando os pacotes necessários no momento.
require(rpart)
require(rpart.plot)
require(car)
require(ROCR)
require(faraway)
require(MASS)
require(randomForest)
require(caret)
require(vegan)
require(party)
require(ipred)

### Primeiro banco de dados.

### Primeiro contato.
#help(iris)
head(iris)
summary(iris)
by(iris,iris$Species,summary)


### Ajustando uma árvore.
set.seed(97)
arvore1=rpart(Species~.,data=iris)
arvore1
arvore1[[1]]
summary(arvore1)
rpart.plot(arvore1)
head(iris)
#iris[2:4,3]=NA
### Avaliando a discriminação produzida pela árvore.
par(cex=1.5,las=1)
with(iris,plot(Petal.Length, Petal.Width,type='n',xlab='Comprimento da pétala',ylab='Largura da pétala'))
with(iris[which(iris$Species=='setosa'),],points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='red'))
with(iris[which(iris$Species=='versicolor'),],points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='blue'))
with(iris[which(iris$Species=='virginica'),],points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='green'))
lines(c(2.45,2.45),c(0,3),col='red',lty=2)
lines(c(0,8),c(1.8,1.8),col='red',lty=2)

### Tabela de confusão.
preditos=predict(arvore1,newdata=iris[,1:4],type='class')
table(preditos,iris$Species) 

#############################################################################################################################################################

### Segundo banco de dados.

### Primeiro contato.
data(pima)
#help(pima)
head(pima)
summary(pima)

### Dados irreais. Possivelmente atribuiu zero a dados perdidos.

pima2=pima
i1=which(pima2$glucose==0)
pima2$glucose[i1]=NA
i2=which(pima2$diastolic==0)
pima2$diastolic[i2]=NA
i3=which(pima2$triceps==0)
pima2$triceps[i3]=NA
i4=which(pima2$insulin==0)
pima2$insulin[i4]=NA
i5=which(pima2$bmi==0)
pima2$bmi[i5]=NA

pima2$test=factor(pima2$test)
names(pima2)=c('Gravidez','Glicose','Diastólica','Triceps','Insulina','IMC','Classificação','Idade','test')

set.seed(2242)
s1=sample(768)
pimaaj=pima2[s1[1:600],] ### 600 observações para ajuste.
pimaval=pima2[s1[601:768],] ### 168 observações para validação.

### Ajuste e algumas saídas
set.seed(5468)
arvore2=rpart(test~.,data=pimaaj)
arvore2
summary(arvore2) ### Ver partições concorrentes e substitutas.
summary(arvore2,cp=0.05)
plot(arvore2)
text(arvore2,use.n=T)

### Análise de custo/complexidade
plotcp(arvore2)
printcp(arvore2)

arvore3=prune.rpart(arvore2,cp=0.02) ### Podando árvore com melhor custo-complexidade.

### Explorando o rpart.plot
rpart.plot(arvore3,type=0,extra=1)
rpart.plot(arvore3,type=0,extra=4)
rpart.plot(arvore3,type=0,extra=1,branch=1,uniform=F)
# help(prp)


### Predições
head(predict(arvore3,newdata=pimaval,type='class'))
head(predict(arvore3,newdata=pimaval,type='prob'))
head(predict(arvore3,newdata=pimaval,type='matrix'))

### Análise da capacidade preditiva
p1=predict(arvore3,newdata=pimaval,type='prob')[,2] ### Probabilidades estimadas para diabetes.
l1=pima$test[as.numeric(names(p1))] ### Diagnóstico dos indivíduos da amostra teste.
pred=prediction(p1,l1)
perfarv=performance(pred,"sens","fpr") ### Sensibilidade e 1-Especificidade.
plot(perfarv,type='b',ylab='Sensibilidade',xlab='1-Especificidade')
abline(0,1,col='red',lty=2)
text(perfarv@x.values[[1]],perfarv@y.values[[1]]+0.025,round(pred@cutoffs[[1]],3))
#help(performance)
aucarv=performance(pred,"auc")
datapred=round(data.frame(perfarv@alpha.values,perfarv@y.values,1-perfarv@x.values[[1]]),3)
names(datapred)=c('p','Sensibilidade','Especificidade')
indicarv1=datapred[which.min(abs(datapred[,2]-datapred[,3])),2:3]

### Teste KS
x <- p1[which(l1=='0')]
y <- p1[which(l1=='1')]
ks.test(x, y)
ksarv=ks.test(x, y)$statistic
plot(ecdf(x),verticals=T,ylab="Probabilidade acumulada",xlab='p',main='')
plot(ecdf(y),verticals=T,add=T,col='red')
legend(x=0.6,y=0.3,lty=1,col=c('black','red'),legend=c('Não diabéticos','Diabéticos'))



### Tentando uma árvore um pouco maior.
arvore3m=prune.rpart(arvore2,cp=0.012)
rpart.plot(arvore3m,type=0,extra=1,branch=1,uniform=F,cex=0.9)
p1=predict(arvore3m,newdata=pimaval,type='prob')[,2] ### Probabilidades estimadas para diabetes.
l1=pima$test[as.numeric(names(p1))] ### Diagnóstico dos indivíduos da amostra teste.
pred=prediction(p1,l1)
perfarvm=performance(pred,"sens","fpr") ### Sensibilidade e 1-Especificidade.
plot(perfarvm,type='b',ylab='Sensibilidade',xlab='1-Especificidade')
abline(0,1,col='red',lty=2)
text(perfarvm@x.values[[1]],perfarvm@y.values[[1]]+0.025,round(pred@cutoffs[[1]],3))
#help(performance)
aucarvm=performance(pred,"auc")
datapred=round(data.frame(perfarvm@alpha.values,perfarvm@y.values,1-perfarvm@x.values[[1]]),3)
names(datapred)=c('p','Sensibilidade','Especificidade')
indicarv=datapred[which.min(abs(datapred[,2]-datapred[,3])),2:3]

### Teste KS
x <- p1[which(l1=='0')]
y <- p1[which(l1=='1')]
ks.test(x, y)
ksarvm=ks.test(x, y)$statistic
plot(ecdf(x),verticals=T,ylab="Probabilidade acumulada",xlab='p',main='')
plot(ecdf(y),verticals=T,add=T,col='red')
legend(x=0.6,y=0.3,lty=1,col=c('black','red'),legend=c('Não diabéticos','Diabéticos'))



### Alguns parâmetros que podem ser alterados.
# help(rpart.control)

set.seed(2242)
arvore4=rpart(test~.,data=pimaaj,parms=list(split='gini')) ### Default.
arvore5=rpart(test~.,data=pimaaj,parms=list(split='information')) ### Trocando a medida de impureza.

par(mfrow=c(1,2))
rpart.plot(arvore4,type=0,extra=1,branch=1,uniform=F,,cex=0.7)
rpart.plot(arvore5,type=0,extra=1,branch=1,uniform=F,cex=0.7)

arvore6=rpart(test~.,data=pimaaj,parms=list(split='gini'),control=rpart.control(minbucket=20,minsplit=75,maxdepth=5)) ### Limitando o número de observações e tamanho da árvore.
rpart.plot(arvore6,type=0,extra=1,branch=1,uniform=F)



### Inserindo custos de má-classificação e probabilidades a priori.

arvore7=rpart(test~.,data=pimaaj,parms=list(prior=c(0.8,0.2))) ### Embutindo probabilidades a priori.
rpart.plot(arvore7,type=0,extra=1,branch=1,uniform=F,cex=0.75)

### As duas árvores seguintes são idênticas. Na primeira, são especificadas perdas L(1,2)=3 e L(2,1)=2 e na segunda as prioris são
# modificadas de forma a equivaler à mesma escolha de perdas (é o que o R faz). Ver página 8 de Tearnau,2013.



par(mfrow=c(1,2))

set.seed(2242)
arvore8=rpart(test~.,data=pimaaj,parms=list(loss=matrix(c(0,2,3,0),2,2,byrow=T))) ### Embutindo matriz de perdas.
plotcp(arvore8)
rpart.plot(arvore8,type=0,extra=1,branch=1)

### Agora, uma escolha mais gritante para ver o impacto na árvore.

arvore10=rpart(test~.,data=pimaaj,parms=list(loss=matrix(c(0,2,7,0),2,2,byrow=T))) ### Embutindo matriz de perdas.
plotcp(arvore10)
rpart.plot(arvore10,type=0,extra=1)

arvore11=rpart(test~.,data=pimaaj,parms=list(loss=matrix(c(0,7,2,0),2,2,byrow=T))) ### Invertendo as perdas.
plotcp(arvore11)
rpart.plot(arvore11,type=0,extra=1)



### Agora, incorporando tanto prioris quanto perda

arvore12=rpart(test~.,data=pimaaj,parms=list(prior=c(1/3,2/3),loss=matrix(c(0,3,2,0)))) ### Embutindo probabilidades a priori e perda.
plotcp(arvore12)
rpart.plot(arvore12,type=0,extra=1)




###########################################################################################################################################################
### Uma árvore de regressão.

data(Salaries)
summary(Salaries)
plot(Salaries)

Salarios2=Salaries
names(Salarios2)=c('Classe','Disciplina','Anos de formado','Anos de Serviço','Sexo','Salario')
levels(Salarios2$Classe)=c('Prof. Assist.','Prof. Assoc.','Prof.')
levels(Salarios2$Disciplina)=c('Teórica','Aplicada')
levels(Salarios2$Sexo)=c('Feminino','Masculino')

set.seed(9860)
arvore13=rpart(Salario~.,data=Salarios2)
arvore13
rpart.plot(arvore13,type=0,extra=1,digits=6)
meanvar(arvore13,xlab='Média do nó',ylab='Variância do nó') ### Gráfico de variâncias vs médias dos nós.
plot(predict(arvore13),residuals(arvore13,type='pearson'),xlab='Valores ajustados',ylab='Resíduos de Pearson') ### Gráfico de resíduos.

arvore14=rpart(log(Salario)~.,data=Salarios2)
rpart.plot(arvore14,type=0,extra=1,digits=6)
meanvar(arvore14,xlab='Média do nó',ylab='Variância do nó') ### Gráfico de variâncias vs médias dos nós.
cor(Salarios2$Salario,predict(arvore14)) ### Correlação - valores observados e valores ajustados.
plot(Salarios2$Salario,exp(predict(arvore14)),xlab='Salários',ylab='Valores ajustados') ### Gráfico - valores observados e valores ajustados.
plot(predict(arvore14),residuals(arvore14,type='pearson'),xlab='Valores ajustados',ylab='Resíduos de Pearson') ### Gráfico de resíduos.
plotcp(arvore14)
varImp(arvore14)
#plot(sort(varImp(arvore14)$Overall,decreasing=T),type='h',xaxt='n',xlab='Variável',ylab='Informação') ### Importância das variáveis - pacote caret.
#axis(1,1:5,rownames(varImp(arvore14))[order(varImp(arvore14)$Overall,decreasing=T)])

arvore15=prune.rpart(arvore14,cp=0.025)
rpart.plot(arvore15,uniform=F,,type=0,extra=1,digits=6)
round(tapply(Salarios2$Salario,arvore15$where,mean)) ### Salários médios por nó.
round(tapply(Salarios2$Salario,arvore15$where,sd)) ### Desvios padrões dos salários por nó.
meanvar(arvore15,xlab='Média do nó',ylab='Variância do nó') ### Gráfico de variâncias vs médias dos nós.
plot(predict(arvore15),residuals(arvore15,type='pearson'),xlab='Valores ajustados',ylab='Resíduos de Pearson') ### Gráfico de resíduos.

boxplot(Salarios2$Salario~arvore15$where,col='orange',xlab='Nó',ylab='Salário')
boxplot(log(Salarios2$Salario)~arvore15$where,col='orange',xlab='Nó',ylab='Salário')

#######################################################################################################################################################

### Ilustração da instabilidade das árvores

matvar=matrix(c(1,0.8,0.7,0.5,0.8,1,0.8,0.9,0.7,0.8,1,0.6,0.5,0.9,0.6,1),4,4,byrow=T)
mu=c(0,0,0,0)

set.seed(918)
dados=round(data.frame(mvrnorm(100,mu,matvar)),2)
names(dados)=c('Y','X1','X2','X3')

par(mfrow=c(2,3),mar=c(2,2,2,2),cex=0.9)
for(i in 1:6){
r1=rpart(Y~.,data=dados[sample(1:100,replace=T),],control=rpart.control(cp=0.05))
plot(r1)
text(r1)
}

