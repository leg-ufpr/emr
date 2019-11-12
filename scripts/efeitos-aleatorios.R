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
library(lme4)
library(emmeans)
library(lmerTest)

#-----------------------------------------------------------------------
# Caso 1. Experimento em blocos incompletos.

# Dados contidos no labestData.
da <- as_tibble(labestData::PimentelPg185)

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

# Ajuste do modelo de efeitos fixos para blocos.
m0 <- lm(y ~ bloc + trat, data = da)

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

#-----------------------------------------------------------------------
# Caso 2. Experimento com muitos avaliadores.

# 42 provadores (aleatório) e efeito de concentração (fixo).
url <- "http://leg.ufpr.br/~walmes/data/frango_sensorial.txt"
aval <- read_tsv(url, comment = "#")
attr(aval, "spec") <- NULL
str(aval)

#-----------------------------------------------------------------------
# Caso 3. Dados longitudinais.

url <- "http://leg.ufpr.br/~walmes/data/gado_crescimento.txt"
gado <- read_tsv(url, comment = "#")
attr(gado, "spec") <- NULL
str(gado)

gado <- gado %>%
    mutate_at(c("vaca", "helmintico", "estacao"), "factor")

xtabs(~vaca + estacao, data = gado)

ggplot(data = gado,
       mapping = aes(x = mes,
                     y = peso,
                     color = vaca,
                     group = vaca)) +
    facet_wrap(facets = ~estacao) +
    geom_point(show.legend = FALSE) +
    geom_line(show.legend = FALSE)

#-----------------------------------------------------------------------
# Caso 4. Hierarquia mais profunda.

url <- "http://leg.ufpr.br/~walmes/data/farms.txt"
farm <- read_tsv(url, comment = "#")
attr(farm, "spec") <- NULL
str(farm)

farm <- farm %>%
    mutate_at(c("farm", "block", "trt"), "factor")

#-----------------------------------------------------------------------
# Caso 5. Parcelas subsubdivididas.

url <- "http://leg.ufpr.br/~walmes/data/sistema_gesso_solo.txt"
gesso <- read_tsv(url, comment = "#")

#-----------------------------------------------------------------------
# Caso 6. Efeitos aleatórios cruzados.

url <- "http://leg.ufpr.br/~walmes/data/mosca_algodao_aval.txt"
mosca <- read_tsv(url, comment = "#")

#-----------------------------------------------------------------------
