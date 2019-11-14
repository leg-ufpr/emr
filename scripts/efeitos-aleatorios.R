#-----------------------------------------------------------------------
# Extensões de modelos de regressão · CE 092
# web.leg.ufpr.br/ensino/EMR
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                       Prof. Dr. Paulo Justiniano R. Jr
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Nov-12 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Pacotes.

library(tidyverse)
library(lme4)     # Funções para modelos de efeitos aleatórios.
library(lmerTest) # Complementar a lme4.
library(emmeans)  # Estimated marginal means.

#=======================================================================
# Caso 1. Experimento em blocos incompletos.

# Dados contidos no labestData.
# da <- as_tibble(labestData::PimentelPg185)

url <- "https://raw.githubusercontent.com/pet-estatistica/labestData/devel/data-raw/PimentelPg185.txt"
da <- read_tsv(url)
da <- da %>%
    mutate_at(c("bloc", "trat"), "factor")
attr(da, "spec") <- NULL

str(da)

# São 6 repetições de cada tratamento.
# O tamanho do bloco é 3.
addmargins(xtabs(~trat + bloc, data = da))

# Quantas vezes cada par ocorre junto.
by(data = da$trat,
   INDICES = da$bloc,
   FUN = function(x) {
       apply(combn(sort(x), m = 2),
             MARGIN = 2,
             FUN = paste0,
             collapse = "_")
   }) %>%
    flatten_chr() %>%
    table()

# Obtém as coordenadas radiais para gráfico de conexão.
da <- da %>%
    do({
        k <- nlevels(.$trat)
        a <- seq(0, 2 * pi, length.out = k + 1)[-(k + 1)]
        cbind(.,
              data.frame(coord_x = sin(a)[as.integer(.$trat)],
                         coord_y = cos(a)[as.integer(.$trat)]))
    })

# Gráfico de conexão.
ext <- c(-1.2, 1.2)
ggplot(data = da,
       mapping = aes(coord_x, coord_y)) +
    facet_wrap(facets = ~bloc, nrow = 2) +
    geom_point() +
    geom_polygon(fill = NA, color = "orange") +
    geom_label(mapping = aes(label = trat)) +
    expand_limits(x = ext, y = ext) +
    coord_equal()

# Visualização dos dados.
ggplot(data = da,
       mapping = aes(x = trat, y = y, color = bloc)) +
    geom_point() +
    stat_summary(mapping = aes(group = 1),
                 geom = "line",
                 fun.y = "mean") +
    geom_text(mapping = aes(label = bloc),
              hjust = 0,
              nudge_x = 0.02,
              show.legend = FALSE)

library(igraph)

edg <- by(data = da$trat,
          INDICES = da$bloc,
          FUN = combn,
          m = 2) %>%
    flatten_int()

ghp <- graph(edg, directed = FALSE)
plot(ghp,
     layout = layout_in_circle,
     edge.curved = FALSE)

#-----------------------------------------------------------------------
# Ajuste dos modelos.

# Ajuste do modelo de efeitos fixos para blocos.
m0 <- lm(y ~ bloc + trat,
         data = da,
         contrasts = list(bloc = contr.sum))

# Quadro de teste de F.
anova(m0)

# Estimativas dos parâmetros.
summary(m0)

# Médias ajustadas marginais.
emm_m0 <- emmeans(m0, specs = ~trat)
emm_m0

# Constrastes par a par, ou seja, contrastes de Tukey.
contrast(emm_m0, method = "tukey")

# Ajuste do modelo de efeito aleatório de bloc.
mm0 <- lmer(y ~ (1 | bloc) + trat, data = da)

# Quadro de teste de Wald.
anova(mm0)

# Estimativas dos parâmetros.
summary(mm0, correlation = FALSE)

# Extração das estimativas dos parâmetros de efeito fixo.
fixef(mm0)

# Extração das predições dos coeficientes de efeito aleatório.
ranef(mm0)

# Componentes de variância.
VarCorr(mm0)

# Médias marginais ajustadas.
emm_mm0 <- emmeans(mm0, specs = ~trat)
emm_mm0

# Constrastes par a par, ou seja, contrastes de Tukey.
contrast(emm_mm0, method = "pairwise")

# O efeito de encolhimento (shirinkage).
ef_mm0 <- c(ranef(mm0)$bloc[, 1])
ef_m0 <- coef(m0)[m0$assign == 1]
ef_m0 <- c(ef_m0, bloc10 = -sum(ef_m0))

# Estimativas ao lado das predições para o efeito de blocos.
cbind(ef_m0, ef_mm0)

ef <- tibble(bloc = names(ef_m0),
             ef_m0 = as.vector(ef_m0),
             ef_mm0 = as.vector(ef_mm0)) %>%
    gather(key = "ef", value = "val", -bloc)

ggplot(data = ef,
       mapping = aes(x = ef, y = val, group = bloc)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, lty = 2, size = 0.25) +
    labs(x = "Modelo usado para estimar/predizer o efeito",
         y = "Valor estimado/predito para o efeito")

#=======================================================================
# Caso 2. Experimento com muitos avaliadores.

# 42 provadores (aleatório) e efeito de concentração (fixo).
url <- "http://leg.ufpr.br/~walmes/data/frango_sensorial.txt"
aval <- read_tsv(url, comment = "#")
attr(aval, "spec") <- NULL
aval <- aval %>%
    mutate(conc = factor(conc),
           aval = factor(aval))
str(aval)

aval_l <- aval %>%
    gather(key = "resp", value = "value", -(1:3))

# Reposta categórica (vamos ignorar isso por enquanto).
ggplot(data = aval_l,
       mapping = aes(x = conc, y = value, color = aval, group = aval)) +
    facet_wrap(facets = ~resp, scale = "free_y") +
    geom_point() +
    geom_line()

# Criando uma nota média.
aval_g <- aval_l %>%
    group_by(aval, conc) %>%
    summarise(value = mean(value))
aval_g

# Ajuste do modelo de efeitos fixos para avaliador.
m0 <- lm(value ~ aval + conc,
         data = aval_g,
         contrasts = list(aval = contr.sum))

# Ajuste do modelo de efeito aleatório de bloc.
mm0 <- lmer(value ~ (1 | aval) + conc, data = aval_g)

# O efeito de encolhimento (shirinkage).
ef_mm0 <- c(ranef(mm0)$aval[, 1])
ef_m0 <- coef(m0)[m0$assign == 1]
ef_m0 <- c(ef_m0, aval42 = -sum(ef_m0))

# Estimativas ao lado das predições para o efeito de blocos.
cbind(ef_m0, ef_mm0)[order(ef_m0), ]

# ATTENTION: devido à resposta ser discreta e limitada em alguns níveis,
# vários avaliadores tiveram a mesma estimativa/predição devido ao
# padrão de avaliação igual.

ef <- tibble(bloc = names(ef_m0),
             ef_m0 = as.vector(ef_m0),
             ef_mm0 = as.vector(ef_mm0)) %>%
    gather(key = "ef", value = "val", -bloc)

ggplot(data = ef,
       mapping = aes(x = ef, y = val, group = bloc)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, lty = 2, size = 0.25) +
    labs(x = "Modelo usado para estimar/predizer o efeito",
         y = "Valor estimado/predito para o efeito")

summary(mm0)

#=======================================================================
# Caso 3. Dados longitudinais.

url <- "http://leg.ufpr.br/~walmes/data/gado_crescimento.txt"
gado <- read_tsv(url, comment = "#")
attr(gado, "spec") <- NULL
str(gado)

gado <- gado %>%
    filter(estacao %in% c("seca1")) %>%
    mutate_at(c("vaca", "helmintico", "estacao"), "factor")

# Tem estrutura de tratamentos.
xtabs(~helmintico, data = gado)

ggplot(data = gado,
       mapping = aes(x = mes,
                     y = peso,
                     color = vaca,
                     group = vaca)) +
    facet_wrap(facets = ~estacao) +
    geom_point(show.legend = FALSE) +
    geom_line(show.legend = FALSE)

# Sem acomodar o efeito de vaca.
m0 <- lm(peso ~ helmintico * mes, data = gado)
anova(m0)

# Acomodar o efeito de vaca com efeitos fixos.
m1 <- lm(peso ~ (vaca + helmintico) * mes,
         data = gado,
         contrast = list(vaca = contr.sum))
anova(m1)

# ATTENTION: algumas vacas tem apenas uma observação do valor de peso, o
# que impede estimar a curva de crescimento, com declaração de
# interação, em um modelo de efeitos fixos.

anova(m0, m1)

# Acomodar o efeito de vaca com efeitos aleatórios.
mm0 <- lmer(peso ~ (1 + mes | vaca) + helmintico * mes,
            data = gado)

summary(mm0)

# Cria malha para a predição.
pred <- gado %>%
    distinct(vaca, helmintico) %>%
    mutate(mes = list(seq(5, 9, by = 1))) %>%
    unnest()

pred$m0 <- predict(m0, newdata = pred)
pred$m1 <- predict(m1, newdata = pred)
pred$mm0 <- predict(mm0, newdata = pred)

# Não tem efeito algum de vaca porque não foi declarado.
ggplot(data = pred,
       mapping = aes(x = mes, y = m0, color = vaca, group = vaca)) +
    facet_wrap(facets = ~helmintico) +
    geom_line()

# Efeito de vaca acomodado via efeitos fixos.
ggplot(data = pred,
       mapping = aes(x = mes, y = m1, color = vaca, group = vaca)) +
    facet_wrap(facets = ~helmintico) +
    geom_line()

# Efeito de vaca acomodado via efeitos aleatórios.
ggplot(data = pred,
       mapping = aes(x = mes, y = mm0, color = vaca, group = vaca)) +
    facet_wrap(facets = ~helmintico) +
    geom_line()

# Comparando as estimativas/predições para os efeitos aleatórios.
ef_m1 <- tibble(b0 = coef(m1)[m1$assign == 1],
                b1 = coef(m1)[m1$assign == 4])
ef_m1 <- rbind(ef_m1,
               -colSums(ef_m1))

ef_mm0 <- as_tibble(ranef(mm0)$vaca)
names(ef_mm0) <- names(ef_m1)

ef_m1$vaca <- levels(gado$vaca)
ef_m1$ef <- "fixo"

ef_mm0$vaca <- rownames(ranef(mm0)$vaca)
ef_mm0$ef <- "aleatório"

ef <- bind_rows(ef_m1, ef_mm0) %>%
    mutate_at(c("ef", "vaca"), "factor") %>%
    mutate(ef = fct_rev(ef)) %>%
    gather(key = "param", value = "value", b0, b1)

ggplot(data = ef,
       mapping = aes(x = ef, y = value, group = vaca)) +
    facet_wrap(facets = ~param, scale = "free_y") +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, lty = 2, size = 0.25) +
    labs(x = "Modelo usado para estimar/predizer o efeito",
         y = "Valor estimado/predito para o efeito")

# Como obter as curvas populacionais estimadas para cada tratamento em
# cada modelo?  No de efeitos aleatórios é simples. Mas e no de efeitos
# fixos?

# Qual a razão para os NA no modelo de efeitos fixos?
cbind(m1 = coef(m1)[!(m1$assign %in% c(1, 4))],
      mm0 = fixef(mm0))

#=======================================================================
# Caso 4. Hierarquia mais profunda.

url <- "http://leg.ufpr.br/~walmes/data/farms.txt"
farm <- read_tsv(url, comment = "#")
attr(farm, "spec") <- NULL
str(farm)

farm <- farm %>%
    mutate_at(c("farm", "block", "trt"), "factor")

mm0 <- lmer(resp ~ (1 | farm/block) + trt, data = farm)
summary(mm0)

em_mm0 <- emmeans(mm0, specs = ~trt)
em_mm0
contrast(em_mm0, method = "pairwise")

# ATTENTION: esse modelo ignora a estrutura hierárquica de casualização.
m0 <- lm(resp ~ farm/block + trt, data = farm)
summary(m0)

em_m0 <- emmeans(m0, specs = ~trt)
em_m0
contrast(em_m0, method = "pairwise")

# Esteja atento aos erros padrões das médias e dos contrastes entre
# médias.

#-----------------------------------------------------------------------
