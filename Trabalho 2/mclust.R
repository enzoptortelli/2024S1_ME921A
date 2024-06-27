library(mclust)

png('plots\\pairs.png')
pairs(ENEM_2023)
dev.off()

modelBIC <- mclustBIC(ENEM_2023, 1:15)
png('plots\\bic.png')
plot_bic <- plot(modelBIC, what = 'BIC')
dev.off()

modelICL <- mclustICL(ENEM_2023, 1:15)
png('plots\\icl.png')
plot(modelICL)
dev.off()

model_mclust <- Mclust(ENEM_2023, G = 7, modelNames = "EVV")
png('plots\\classification.png')
plot(model_mclust, what = 'classification')
dev.off()

png('plots\\uncertPlot.png')
uncerPlot(model_mclust$z)
dev.off()


db_mclust <- ENEM_2023 %>% mutate(Cluster = as.factor(model_mclust$classification))


violin_nota <- ggplot(
  ENEM_2023 %>% pivot_longer(cols = everything(), names_to = 'Prova', values_to = 'Nota'),
  aes(x = Prova, y = Nota)
) +
  geom_violin(draw_quantiles = 0.5, fill = 'grey') +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas por prova') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )

bar_num_obs_cluster <- ggplot(db_mclust, aes(Cluster, fill = Cluster)) +
  geom_bar() +
  theme_bw() +
  labs(title = 'Número de observações em cada um dos clusters',
       y = 'Número de observações') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )

gauss_violin_lc <- ggplot(db_mclust %>% select(LC, Cluster), aes(x = Cluster, y = LC, group = Cluster, fill = Cluster)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(breaks=1:7, drop = FALSE) +
  ylim(0, 1000) +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas na prova de Linguagem e Código por cluster (GMM)',
       y = 'Nota') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )

gauss_violin_cn <- ggplot(db_mclust %>% select(CN, Cluster), aes(x = Cluster, y = CN, group = Cluster, fill = Cluster)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(breaks=1:7, drop = FALSE) +
  ylim(0, 1000) +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas na prova de Ciências da Natureza por cluster (GMM)',
       y = 'Nota') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )

gauss_violin_ch <- ggplot(db_mclust %>% select(CH, Cluster), aes(x = Cluster, y = CH, group = Cluster, fill = Cluster)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(breaks=1:7, drop = FALSE) +
  ylim(0, 1000) +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas na prova de Ciências Humanas por cluster (GMM)',
       y = 'Nota') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )

gauss_violin_mt <- ggplot(db_mclust %>% select(MT, Cluster), aes(x = Cluster, y = MT, group = Cluster, fill = Cluster)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(breaks=1:7, drop = FALSE) +
  ylim(0, 1000) +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas na prova de Matemática por cluster (GMM)',
       y = 'Nota') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )

gauss_violin_redacao <- ggplot(db_mclust %>% select(REDAÇÃO, Cluster), aes(x = Cluster, y = REDAÇÃO, group = Cluster, fill = Cluster)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(breaks=1:7, drop = FALSE) +
  ylim(0, 1000) +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas na prova de Redação por cluster (GMM)',
       y = 'Nota') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )


ggsave('plots/gauss_violin_mt.png', plot = gauss_violin_mt)
ggsave('plots/gauss_violin_ch.png', plot = gauss_violin_ch)
ggsave('plots/gauss_violin_cn.png', plot = gauss_violin_cn)
ggsave('plots/gauss_violin_lc.png', plot = gauss_violin_lc)
ggsave('plots/gauss_violin_redacao.png', plot = gauss_violin_redacao)
ggsave('plots/bar_num_obs_cluster.png', plot = bar_num_obs_cluster)

ggsave('plots/violin_nota.png', plot = violin_nota)
