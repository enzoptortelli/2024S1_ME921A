library(tidyverse)
library(cluster)
library(factoextra)
library(knitr)
library(kableExtra)
library(ggbiplot)
library(vtable)

rm(list = ls(all.names = TRUE))

dados_originais <- read_csv("data/penguin.zip")

db_pre_processado <- dados_originais %>%
  drop_na %>%
  filter(flipper_length_mm > 0 & flipper_length_mm < 5000 & sex == 'MALE') %>%
  select(!sex) %>%
  rename(compri_culmen = culmen_length_mm,
         prof_culmen = culmen_depth_mm,
         compri_nadadeira = flipper_length_mm,
         massa_corporal = body_mass_g)

kable(db_pre_processado %>%
        summary(.),
      format = 'latex'
)

db <- db_pre_processado %>%
  mutate(compri_nadadeira = compri_nadadeira / 10, massa_corporal = massa_corporal / 100) %>%
  scale(., scale = FALSE)

summary(db)

modelPCA <- princomp(db)

summary(modelPCA)

fviz_pca_biplot(modelPCA, label = 'var') +
  labs(title = 'Biplot - PCA', x = 'Comp.1', y = 'Comp.2')

fviz_screeplot(modelPCA) +
  labs(x = 'Componentes', y = 'Porcentagem da variancia explicada')


db_ppca <- modelPCA$scores[, 1:2]
fviz_nbclust(db_ppca, cluster::pam, method = "wss") +
  labs(title = 'Otimizacao do numero de clusters',
       x = 'Numero de clusters',
       y = 'Soma de quadrados interno total')

pmodel_pam <- pam(db_ppca, 3)
fviz_cluster(pmodel_pam, ggtheme = theme_bw())
fviz_silhouette(pmodel_pam,
                title = 'Grafico de silhueta dos clusters',
                subtitle = 'Largura media das silhuetas: 0.61',
                ylab = 'Largura da silhueta')

