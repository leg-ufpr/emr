
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


### Um pouco de bagging

b1=bagging(test~.,data=pimaaj,coob=T)

### Vamos avaliar o efeito do número de reamostragens.
sequencia=seq(10,200,10)
erro=numeric()
set.seed(7845)
for(i in 1:length(sequencia))
erro[i]=bagging(test~.,data=pimaaj,nbagg=sequencia[i],coob=T)$err
plot(sequencia,erro,type='b',xlab='Número de reamostragens',ylab='Taxa de má-classificação')

b2=bagging(test~.,data=pimaaj,coob=T,nbagg=50)
#summary(b2)

pred1=predict(b2, newdata=pimaval,type='class',aggregation='majority') ### Predizendo pela maioria de votos.
pred2=predict(b2, newdata=pimaval,type='prob',aggregation='average')[,2] ### Predizendo pela média das probabilidades.

### Análise da capacidade preditiva

l1=pimaval[,'test'] ### Diagnóstico dos indivíduos da amostra teste.
pred=prediction(pred2,l1) 
perfbag=performance(pred,"sens","fpr") ### Sensibilidade e 1-Especificidade

### Curva ROC
plot(perfbag,type='b',ylab='Sensibilidade',xlab='1-Especificidade')
abline(0,1,col='red',lty=2)
text(perfbag@x.values[[1]],perfbag@y.values[[1]]+0.025,round(pred@cutoffs[[1]],3))
aucbag=performance(pred,"auc")


### Sensibilidade e Especificidade

datapred=round(data.frame(perfbag@alpha.values,perfbag@y.values,1-perfbag@x.values[[1]]),3)
names(datapred)=c('p','Sensibilidade','Especificidade')
indicbag=datapred[which.min(abs(datapred[,2]-datapred[,3])),2:3]



### Teste KS
x <- pred2[which(l1=='0')]
y <- pred2[which(l1=='1')]
ks.test(x, y)
ksbag=ks.test(x, y)$statistic
plot(ecdf(x),verticals=T,ylab="Probabilidade acumulada",xlab='p',main='')
plot(ecdf(y),verticals=T,add=T,col='red')
legend(x=0.6,y=0.3,lty=1,col=c('black','red'),legend=c('Não diabéticos','Diabéticos'))
 


#######################################################################################################################################################

### Um exemplo com dados de sobrevivência.

data(lung)
summary(lung)
head(lung)

lung2=lung[,-1] ### Extraindo da base o código da instituição.

lung2$status[which(lung2$status==1)]=0
lung2$status[which(lung2$status==2)]=1

names(lung2)=c('Tempo','Status','Idade','Sexo','Perf. Fisica','Bem estar 1','Bem estar 2','Calorias','Perda de peso')
lung2$Sexo=as.factor(lung2$Sexo) ### Convertendo sexo para fator.
levels(lung2$Sexo)=c('Masculino','Feminino')

set.seed(98)
arvore16=rpart(Surv(Tempo,Status)~.,data=lung2)
summary(arvore16)
printcp(arvore16)
plotcp(arvore16) ### Problema...

arvore17=prune.rpart(arvore16,cp=0.015) ### Escolhendo um ponto de corte 'subjetivo'.
rpart.plot(arvore17,type=0,extra=1,uniform=F) ### Os valores que aparecem são *risco de base (nó inicial).
arvore17[[1]] ### Um sumário do modelo obtido.

arvore17$where ### Indicador dos nós finais.
s1=survfit(Surv(Tempo,Status)~arvore17$where,data=lung2)
plot(s1,xlab='Dias',ylab='Sobrevida',col=c('black','red','blue','green'))
legend(x=850,y=1,legend=c('Nó 3','Nó 4','Nó 6','Nó 7'),col=c('black','red','blue','green'),lty=1)
### Análise gráfica da árvore selecionada.


#######################################################################################################################################################

### Usando inference conditional trees.

### Um exemplo por simulação.

### Especificação da matriz de variâncias e covariâncias.
mat=diag(20)
mat[1,2]=0.3
mat[2,1]=0.3
mat[1,7]=0.25
mat[7,1]=0.25
mat[1,15]=0.2
mat[15,1]=0.2

set.seed(387)
dados=data.frame(mvrnorm(500,mu=rep(0,20),Sigma=mat)) ### Simulação de 5000 dados distrib. Normal (mu=0,sigma=mat).

dados[,1]=factor(dados[,1]>0)
dados[,2]=factor(dados[,2]>0)
dados[,7]=factor(dados[,7]>(-0.5))
dados[,15]=factor(dados[,15]>0.5)
### Binarizando as variáveis 2, 7 e 15, as únicas correlacionadas com a resposta (x1).

arvore18=rpart(X1~.,data=dados,control=rpart.control(cp=0.005))
printcp(arvore18)
plot(arvore18)
text(arvore18)
### Qual o problema dessa árvore?

set.seed(1304)
arvore19=ctree(X1~.,data=dados,control=ctree_control(mincriterion = 0.4))
plot(arvore19)


### Aplicando com os dados pima:

arvore20=ctree(test~.,data=pimaaj)
plot(arvore20)

### Vamos avaliar a capacidade preditiva.

predict(arvore20,newdata=pimaval,type='prob')

p1=predict(arvore20,newdata=pimaval,type='prob') ### Probabilidades estimadas para diabetes.
p1=data.frame(matrix(unlist(p1),ncol=2,byrow=T))[,2]
l1=pimaval[,'test'] ### Diagnóstico dos indivíduos da amostra teste.
pred=prediction(p1,l1)

perfct=performance(pred,"sens","fpr") ### Sensibilidade e 1-Especificidade.
plot(perfct,type='b',ylab='Sensibilidade',xlab='1-Especificidade')
abline(0,1,col='red',lty=2)
text(perfct@x.values[[1]],perfct@y.values[[1]]+0.025,round(pred@cutoffs[[1]],3))
#help(performance)
aucctree=performance(pred,"auc")
datapred=round(data.frame(perfct@alpha.values,perfct@y.values,1-perfct@x.values[[1]]),3)
names(datapred)=c('p','Sensibilidade','Especificidade')
indicct=datapred[which.min(abs(datapred[,2]-datapred[,3])),2:3]


### Teste KS
x <- p1[which(l1=='0')]
y <- p1[which(l1=='1')]
ks.test(x, y)
ksctree=ks.test(x, y)$statistic
plot(ecdf(x),verticals=T,ylab="Probabilidade acumulada",xlab='p',main='')
plot(ecdf(y),verticals=T,add=T,col='red')
legend(x=0.6,y=0.3,lty=1,col=c('black','red'),legend=c('Não diabéticos','Diabéticos'))


### Usando conditional trees no problema de sobrevivência.

ctreesurv=ctree(Surv(Tempo,Status)~.,data=lung2)
plot(ctreesurv)
ctreesurv@cond_distr_response
nodes(ctreesurv, unique(where(ctreesurv)))
predict(ctreesurv) ### Predição - tempos medianos


#######################################################################################################################################################

### Usando o randomForest

forest1=randomForest(test~.,data=pimaaj) ### Não admite dados missing.
forest1=randomForest(test~.,na.action = na.omit,data=pimaaj) 

pima3=rfImpute(test~.,data=pima2)
set.seed(2242)
s1=sample(768)
pimaaj3=pima3[s1[1:600],] ### 600 observações para ajuste.
pimava3=pima3[s1[601:768],] ### 168 observações para validação.

forest1=randomForest(test~.,na.action = na.omit,data=pimaaj3,proximity=T) 
names(forest1)
forest1$confusion ### Tabela de confusão
forest1$err.rate ### Taxa de erro acumulada.
plot(forest1)
forest1$mtry ### Número de variáveis amostradas para partição em cada nó.
forest1$ntree ### Número de árvores construídas.
forest1$importance ### Medida de importância para cada variável.
#forest1$proximity ### Matrix de proximidades.
forest1$proximity[1:5,1:5]

MDSplot(forest1,pimaaj3$test) ### Escalonamento multidimensional - graficando as proximidades.

p1=predict(forest1,newdata=pimava3,type='prob')[,2]
l1=pima$test[as.numeric(names(p1))] ### Diagnóstico dos indivíduos da amostra teste.
pred=prediction(p1,l1)
perfran=performance(pred,"sens","fpr") ### Sensibilidade e 1-Especificidade.
plot(perfran,type='b',ylab='Sensibilidade',xlab='1-Especificidade')
abline(0,1,col='red',lty=2)
text(perfran@x.values[[1]],perfran@y.values[[1]]+0.025,round(pred@cutoffs[[1]],3))
#help(performance)
aucforest=performance(pred,"auc")
datapred=round(data.frame(perfran@alpha.values,perfran@y.values,1-perfran@x.values[[1]]),3)
names(datapred)=c('p','Sensibilidade','Especificidade')
indicforest=datapred[which.min(abs(datapred[,2]-datapred[,3])),2:3]



### Teste KS
x <- p1[which(l1=='0')]
y <- p1[which(l1=='1')]
ks.test(x, y)
ksforest=ks.test(x, y)$statistic
plot(ecdf(x),verticals=T,ylab="Probabilidade acumulada",xlab='p',main='')
plot(ecdf(y),verticals=T,add=T,col='red')
legend(x=0.6,y=0.3,lty=1,col=c('black','red'),legend=c('Não diabéticos','Diabéticos'))

##########################################################################################################################################################

### Usando florestas de árvores condicionais.

cforest1=cforest(test~.,data=pimaaj)
v1=varimp(cforest1)  ### Medidas de importância não condicionadas (podem apresentar correlações espúrias).
### Exemplo - Conhecimento e tamanho dos pés.

cforest2=cforest(test~.,data=pimaaj3) 
v2=varimp(cforest2,conditional=T)  ### Usando importância condicional.
v2=c(-1.818182e-04,  5.636364e-03,  1.665335e-18,  0.000000e+00,  1.090909e-03,  1.454545e-03,  1.454545e-03,  1.818182e-04)

varimpAUC(cforest1) ### Medida de importância baseada na área sob a curva ROC.
varimpAUC(cforest2,conditional=T) ### O mesmo, agora condicionando.


par(mfrow=c(1,2),las=2,cex=1.2)
plot(sort(v1,decreasing=T),type='h',xaxt='n',xlab='Variável',ylab='Informação',ylim=c(0,0.105),main='Não condicionada') ### Importância das variáveis - pacote caret.
axis(1,1:8,names(v1)[order(v1,decreasing=T)])
plot(sort(v2,decreasing=T),type='h',xaxt='n',xlab='Variável',ylab='Informação',ylim=c(0,0.006),main='Condicionada') ### Importância das variáveis - pacote caret.
axis(1,1:8,names(v1)[order(v2,decreasing=T)])

p1=predict(cforest1,newdata=pimaval,type='prob') ### Probabilidades estimadas para diabetes.
p1=data.frame(matrix(unlist(p1),ncol=2,byrow=T))[,2]
l1=pimaval[,'test'] ### Diagnóstico dos indivíduos da amostra teste.
pred=prediction(p1,l1)

perfrct=performance(pred,"sens","fpr") ### Sensibilidade e 1-Especificidade.
plot(perfrct,type='l',ylab='Sensibilidade',xlab='1-Especificidade')
auccforest=performance(pred,"auc")
datapred=round(data.frame(perfrct@alpha.values,perfrct@y.values,1-perfrct@x.values[[1]]),3)
names(datapred)=c('p','Sensibilidade','Especificidade')
indiccforest=datapred[which.min(abs(datapred[,2]-datapred[,3])),2:3]



### Teste KS
x <- p1[which(l1=='0')]
y <- p1[which(l1=='1')]
ks.test(x, y)
kscforest=ks.test(x, y)$statistic
plot(ecdf(x),verticals=T,ylab="Probabilidade acumulada",xlab='p',main='')
plot(ecdf(y),verticals=T,add=T,col='red')
legend(x=0.6,y=0.3,lty=1,col=c('black','red'),legend=c('Não diabéticos','Diabéticos'))


############################################################################################################################################################

### Ajuste de uma regressão logistica, para fins de comparação.

ajustelog=stepAIC(glm(test~.,data=na.omit(pimaaj),family='binomial'))
predlog=predict(ajustelog,newdata=pimaval,type='response')

l1=pimava3[names(predlog),'test'] ### Diagnóstico dos indivíduos da amostra teste.
pred=prediction(predlog,l1)

perfrlog=performance(pred,"sens","fpr") ### Sensibilidade e 1-Especificidade.
plot(perfrlog,type='l',ylab='Sensibilidade',xlab='1-Especificidade')
auclog=performance(pred,"auc")
datapred=round(data.frame(perfrlog@alpha.values,perfrlog@y.values,1-perfrlog@x.values[[1]]),3)
names(datapred)=c('p','Sensibilidade','Especificidade')
indiclog=datapred[which.min(abs(datapred[,2]-datapred[,3])),2:3]


### Teste KS
x <- predlog[which(l1=='0')]
y <- predlog[which(l1=='1')]
ks.test(x, y)
kslog=ks.test(x, y)$statistic
plot(ecdf(x),verticals=T,ylab="Probabilidade acumulada",xlab='p',main='')
plot(ecdf(y),verticals=T,add=T,col='red')
legend(x=0.6,y=0.3,lty=1,col=c('black','red'),legend=c('Não diabéticos','Diabéticos'))


### Ajuste de uma regressão logistica + árvore.

ajustelog2=glm(test~Glicose*IMC+Idade+Classificação+IMC+Gravidez,data=pimaaj3,family='binomial')
predlog2=predict(ajustelog2,newdata=pimava3,type='response')

l1=pimava3[names(predlog2),'test'] ### Diagnóstico dos indivíduos da amostra teste.
pred=prediction(predlog2,l1)

perfrlog2=performance(pred,"sens","fpr") ### Sensibilidade e 1-Especificidade.
plot(perfrlog2,type='l',ylab='Sensibilidade',xlab='1-Especificidade')
auclog2=performance(pred,"auc")
datapred2=round(data.frame(perfrlog2@alpha.values,perfrlog2@y.values,1-perfrlog2@x.values[[1]]),3)
names(datapred2)=c('p','Sensibilidade','Especificidade')
indiclog2=datapred2[which.min(abs(datapred2[,2]-datapred2[,3])),2:3]

### Teste KS
x <- predlog2[which(l1=='0')]
y <- predlog2[which(l1=='1')]
ks.test(x, y)
kslog2=ks.test(x, y)$statistic
plot(ecdf(x),verticals=T,ylab="Probabilidade acumulada",xlab='p',main='')
plot(ecdf(y),verticals=T,add=T,col='red')
legend(x=0.6,y=0.3,lty=1,col=c('black','red'),legend=c('Não diabéticos','Diabéticos'))




############################################################################################################################################################

### Juntando os resultados:

compred=data.frame(matrix(c(aucarv@y.values,ksarv,aucarvm@y.values,ksarvm,aucbag@y.values,ksbag,aucctree@y.values,ksctree,aucforest@y.values,ksforest,auccforest@y.values,kscforest,auclog@y.values,kslog,auclog2@y.values,kslog2),nrow=8,ncol=2,byrow=T))
sensespec=rbind(indicarv1,indicarv,indicbag,indicct,indicforest,indiccforest,indiclog,indiclog2)
d1=data.frame(compred,sensespec)
names(compred)=c('AUC','KS')
rownames(compred)=c('Árvore VC','Árvore sat','Bagging','Conditional tree','Random Forest','Conditional RF','Regressão Logística',"Reg. Log. + árvore")
compred

plot(perfarv,type='l',ylab='Sensibilidade',xlab='1-Especificidade')
plot(perfarvm,type='l',ylab='Sensibilidade',xlab='1-Especificidade',add=T,col='red')
plot(perfbag,type='l',ylab='Sensibilidade',xlab='1-Especificidade',add=T,col='blue')
plot(perfct,type='l',ylab='Sensibilidade',xlab='1-Especificidade',add=T,col='green')
plot(perfran,type='l',ylab='Sensibilidade',xlab='1-Especificidade',add=T,col='orange')
plot(perfrct,type='l',ylab='Sensibilidade',xlab='1-Especificidade',add=T,col='magenta')
plot(perfrlog,type='l',ylab='Sensibilidade',xlab='1-Especificidade',add=T,col='grey')
plot(perfrlog2,type='l',ylab='Sensibilidade',xlab='1-Especificidade',add=T,col='brown')

legend(x=0.85,y=0.7,legend=c('Árvore VC','Árvore sat','Bagging','Conditional tree','Random Forest','Conditional RF','Regressão Logística'),lty=1,bty='n',col=c('black','red','blue','green','orange','magenta','grey','Regressão Logística'))


############################################################################################################################################################


### Um pouco de multivariada.



### Leitura e preparação dos dados 

dados=read.csv2('c://Paraná//Marcia//Dadosmarcia2.csv',header=T) ### Leitura dos dados.
head(dados)
summary(dados)
data2=ordered(dados$Data,levels=levels(dados$Data)[c(12,7,1,5,4,11,8,2,13,6,9,3,10)])
dados$Data=data2
b1=by(dados[,14:34],dados[,'Especie'],summary)

### Apenas separando as datas por mês e ano, atendendo a pesquisadora.
data=strsplit(as.character(dados[,3]),split="/")
mes=numeric()
ano=numeric()
for(i in 1:492){
mes[i]=data[[i]][1]
ano[i]=data[[i]][2]
}


dados=data.frame(dados,mes,ano)
names(dados)
especies=dados[,14:34]
especies2=especies[,c(4,7,11,16,17,19,20)]
indiczero=which(rowSums(especies)==0)
indiczero2=which(rowSums(especies2)==0)


dadoss0=dados[-indiczero,]
especiess0=especies[-indiczero,]
dadoss02=dados[-indiczero2,]
especiess02=especies2[-indiczero2,]


require(mvpart)
distancias=gdist(especiess0,method='bray',full=TRUE,sq=TRUE) ### Matriz de distâncias considerando todas as espécies.
distancias2=gdist(especiess02,method='bray',full=TRUE,sq=TRUE) ### Matriz de distâncias apenas para as espécies mais frequentes.

round(distancias[1:15,1:15],3)
round(distancias2[1:15,1:15],3)

### Usando as contagens
### Esta árvore é das contagens com todas as espécies
set.seed(63458)
ajuste6=mvpart(distancias~Especie+mes+ano+CP+Sexo+Dieta+pH+Cond+OD.mg.l+OD.+Temp,data=dadoss0,bars = F, text.add = T) ### Árvore para todas as espécies.
ajuste62=mvpart(distancias2~Especie+mes+ano+CP+Sexo+Dieta+pH+Cond+OD.mg.l+OD.+Temp,data=dadoss02,bars = F, text.add = T) ### Árvore apenas para as espécies mais frequentes.
plotcp(ajuste62)


### NMDS - Escalonamento multidimensional (apenas para as sete espécies mais frequentes).

require(vegan)

m1=metaMDS(especiess02,zerodist='add')

v1=vegdist(especiess02)

nos=ajuste62$where
par(cex=1.2)
plot(jitter(m1$points[which(nos==4),],a=0.02),col='red',pch=20,cex=1.2,xlim=c(min(m1$points[,1]),max(m1$points[,1])),ylim=c(min(m1$points[,2]),max(m1$points[,2])),xlab='Dimensão 1',ylab='Dimensão 2')
points(jitter(m1$points[which(nos==5),],a=0.02),col='black',pch=20,cex=1.2)
points(jitter(m1$points[which(nos==7),],a=0.02),col='green',pch=20,cex=1.2)
points(jitter(m1$points[which(nos==8),],a=0.02),col='cyan',pch=20,cex=1.2)
points(jitter(m1$points[which(nos==10),],a=0.02),col='blue',pch=20,cex=1.2)
points(jitter(m1$points[which(nos==11),],a=0.02),col='magenta',pch=20,cex=1.2)

points(colMeans(m1$points[which(nos==4),])[1],colMeans(m1$points[which(nos==4),])[2],col='red',cex=5,pch=3)
points(colMeans(m1$points[which(nos==5),])[1],colMeans(m1$points[which(nos==5),])[2],col='black',cex=5,pch=3)
points(colMeans(m1$points[which(nos==7),])[1],colMeans(m1$points[which(nos==7),])[2],col='green',cex=5,pch=3)
points(colMeans(m1$points[which(nos==8),])[1],colMeans(m1$points[which(nos==8),])[2],col='cyan',cex=5,pch=3)
points(colMeans(m1$points[which(nos==10),])[1],colMeans(m1$points[which(nos==10),])[2],col='blue',cex=5,pch=3)
points(colMeans(m1$points[which(nos==11),])[1],colMeans(m1$points[which(nos==11),])[2],col='magenta',cex=5,pch=3)

text(m1$species[,1],m1$species[,2],rownames(m1$species))
lines(c(0,0),c(-2,2),col='red',lty=2)
lines(c(-2,2),c(0,0),col='red',lty=2)
legend(x=1.7,y=1.2,pch=20,bty='n',legend=c('Nó 1','Nó 2','Nó 3','Nó 4','Nó 5','Nó 6'),col=c('red','black','green','cyan','blue','magenta'))



#################################### Roda tudo
###################################


### Com o xvmult, deu 6.

### Para verificar a correspondência dos nós com o restante dos gráficos...
summary(especiess0[which((dadoss0$Especie%in%c('BF','Hm')) & (dadoss0$Cond>=52)),]) ### Vermelho, nó 1
summary(especiess0[which((dadoss0$Especie%in%c('ABL','AF')) & (dadoss0$Cond>=52)),]) ### Amarelo, nó 2
summary(especiess0[which((dadoss0$Especie%in%c('BF','Hm','Tc')) & (dadoss0$Cond<52)),]) ### Verde, nó 3
summary(especiess0[which((dadoss0$Especie%in%c('ABL','AF')) & (dadoss0$Cond<52)),]) ### Anil, nó 4
summary(especiess0[which(dadoss0$Especie=='CA'),]) ### Azul, nó 5.
summary(especiess0[which(dadoss0$Especie%in%c('Mc','Tg')),]) ### Rosa, nó 6.

dist2=dist(distancias2)
n1=nmds(dist2,mindim=2,maxdim=3)


plotcp(ajuste6)

### Gráficos de barras
nos=ajuste6$where
sumarios=by(especiess0,nos,mean)
sds=by(especiess0,nos,sd)

### Sumário (médias e desvios padrões por nó)
matrizsum=matrix(0,21,6)
matrizsd=matrix(0,21,6)
for(i in 1:6){
matrizsum[,i]=sumarios[[i]]
matrizsd[,i]=sds[[i]]}
matrizsum=round(matrizsum,3)
matrizsd=round(matrizsd,4)
rownames(matrizsum)=1:21
colnames(matrizsum)=paste('Nó',1:6)
rownames(matrizsd)=1:21
colnames(matrizsd)=paste('Nó',1:6)

### Médias e desvios padrões - geral
mean(dados[,14:34])
sd(dados[,14:34])

matrizsum2=matrizsum[-11,]

### Opção 2 - Arco íris
par(cex=1.15,mar=c(5,4,2,2))

barplot(t(matrizsum2),beside=T,col=rainbow(6),legend=T,xlab='Espécie de Parasita',ylab='Contagem média',args.legend=list(x=130,bty='n',cex=0.85,legend=paste('Nó',1:6),col=rainbow(6)))
### Gráficos de barras considerando todas as espécies.

barplot(t(matrizsum[11,]),beside=T,col=rainbow(6),legend=T,xlab='',ylab='Contagem média',args.legend=list(x=1.46,y=600,bty='n',cex=0.85,legend=paste('Nó',1:6),col=rainbow(6)),ylim=c(0,300))



##### Colocar sensibilidade e especificidade.
