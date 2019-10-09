#-----------------------------------------------------------------------
# Extensões de modelos de regressão · CE 092
# web.leg.ufpr.br/ensino/EMR
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                       Prof. Dr. Paulo Justiniano R. Jr
# Comandos copiados/adaptados de Faraway 
# https://people.bath.ac.uk/jjf23/ELM/
#
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Out-10 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

##
## Intro a árvores de classificação
##

## Exemplo:
## motivação: predizer a espécie de um particular espécime de canguru de um museu
## opção de 3 espécies e Macho/Femea
## aqui considera apenas a classificação da espécie
## preditoras são medidas de crânio
data(kanga, package="faraway")
## exemplar que se deseja classificar:
x0 <- c(1115,NA,748,182,NA,NA,178,311,756,226,NA,NA,NA,48,1009,NA,204,593)
##notar missing
## duas opções: árvore que inclui attributos missing
##              arvore só com observações disponíveis
## escolha racai sobre possível "informatividade" do missing

## opção aqui: excluir as variáveis faltantes no exemplar a ser classificado
## e tb "sexo"
kanga <- kanga[,c(T,F,!is.na(x0))]
head(kanga)

## vendo qtos faltantes ainda sobram  em cada variável
apply(kanga,2,function(x) sum(is.na(x)))

## Opções:
## 1. discretizar variável e missing como um nível
##    - adequado se suspeitar ser informativo
## 2. imputação
##   - pode introduzir vícios...
## 3. Construir a árvore normalmente ignorando missing.
##   - quando for classificar e encontrar missing há das opções:
##      I. tomar a classe com maioria de votos
##     II. utilizar "surrogate" (ver summary())

## vamos examinar mais de perto as variáveis faltantes.
plot(na.omit(kanga)[,-1]) ## ver relações com palate.width e mandible.length
round(cor(kanga[,-1],use="pairwise.complete.obs")[,c(3,9)],2)

## Duas opções: 
dim(na.omit(kanga[,-c(4,10)]))  ## remover as 2 variáveis e os NA's
dim(na.omit(kanga))             ## remover os NA's

## opta-se pela 1a opção dada a "redundância" das variáveis
df <- na.omit(kanga[,-c(4,10)])

## uma visualização 
ggplot2::ggplot(df,
       aes(x=zygomatic.width, y=foramina.length, shape=species, color=species)) +
    geom_point() +
    theme(legend.position = "top", legend.direction = "horizontal",
          legend.title=element_blank())

## e vamos contruir uma árvore estendida
set.seed(2019)
require(rpart)
require(rpart.plot)
fitK <- rpart(species ~ ., data=df,cp=0.001)
printcp(fitK)
plotcp(fitK)
rpart.plot(fitK)

(fitKp <- prune(fitK,cp=0.01))
plot(fitKp, compress=T, uniform=T, branch=0.4)
text(fitKp)

## avaliando as classificações
(tt <- table(actual=df$species, predicted=predict(fitKp, type="class")))
1-sum(diag(tt))/sum(tt)  ## taxa de erro

## Obs:
## tamanho pode ser confundidor (sexo) e atraapalhar as classificações
## há muita redundância e relações entre variáveis
## - algorítmos podem incluir combinações lineares
## - aqui vamos utilizar compomentes principais das classificadas, 
##  considerando que o foco é a predição e não a interpretabilidade

pcK <- princomp(df[,-1])
## montando novo data-frame de "dados"
pcdf <- data.frame(species=df$species, pcK$scores)

## e lá vamos nós de novo...
pcfit <- rpart(species ~ ., pcdf, cp=0.0005)
printcp(pcfit)
plotcp(pcfit)

(pcfit <- prune.rpart(pcfit,0.0400))
pcfit
plot(pcfit)
## note que o 1o fica de fora! determina tamanho mas não discrimina

## taxa de erro com novas variáveis dadas pelos componentes
(tt <- table(df$species,predict(pcfit,type="class")))
1-sum(diag(tt))/sum(tt)

## e agora para predizer ...
## tem que transformar para componentes!
nx0 <- x0[!is.na(x0)]
nx0 <- nx0[-c(3,9)]
nx0 <- (nx0-pcK$center)/pcK$scale
ndf <- data.frame(nx0 %*% pcK$loadings)

(x0pred <- predict(pcfit, ndf))
x0pred[which.max(predict(pcfit, ndf))]

## um "concorrente neste caso : análise de discriminante linear 
library(MASS)
ldamod <- lda(species ~ ., df)
(tt <- table(df$species,predict(ldamod)$class))
1-sum(diag(tt))/sum(tt)

