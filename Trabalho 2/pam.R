library(cluster)
library(factoextra)

nb_clust_plot <- fviz_nbclust(ENEM_2023, cluster::pam, method = 'wss')
ggsave('plots/nbclust.png', plot = nb_clust_plot)

model_pam <- pam(ENEM_2023, 7)

silhouette <- fviz_silhouette(model_pam)
ggsave('plots/silhouette.png', plot = silhouette)

db_pam <- ENEM_2023 %>% mutate(Cluster = as.factor(model_pam$clustering))

pam_violin_lc <- kmedoids_violin_LC <- ggplot(db_pam %>% select(LC, Cluster), aes(x = Cluster, y = LC, group = Cluster, fill = Cluster)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(breaks=1:7, drop = FALSE) +
  ylim(0, 1000) +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas na prova de Linguagem e Código por cluster (K-medoids)',
       y = 'Nota') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )

pam_violin_mt <- ggplot(db_pam %>% select(MT, Cluster), aes(x = Cluster, y = MT, group = Cluster, fill = Cluster)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(breaks=1:7, drop = FALSE) +
  ylim(0, 1000) +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas na prova de Matemática por cluster (K-medoids)',
       y = 'Nota') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )

pam_violin_cn <- ggplot(db_pam %>% select(CN, Cluster), aes(x = Cluster, y = CN, group = Cluster, fill = Cluster)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(breaks=1:7, drop = FALSE) +
  ylim(0, 1000) +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas na prova de Ciências da Natureza por cluster (K-medoids)',
       y = 'Nota') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )

pam_violin_ch <- ggplot(db_pam %>% select(CH, Cluster), aes(x = Cluster, y = CH, group = Cluster, fill = Cluster)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(breaks=1:7, drop = FALSE) +
  ylim(0, 1000) +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas na prova de Ciências Humanas por cluster (K-medoids)',
       y = 'Nota') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )

pam_violin_redacao <- ggplot(db_pam %>% select(REDAÇÃO, Cluster), aes(x = Cluster, y = REDAÇÃO, group = Cluster, fill = Cluster)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(breaks=1:7, drop = FALSE) +
  ylim(0, 1000) +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas na prova de Redação por cluster (K-medoids)',
       y = 'Nota') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )

ggsave('plots/pam_violin_mt.png', plot = pam_violin_mt)
ggsave('plots/pam_violin_ch.png', plot = pam_violin_ch)
ggsave('plots/pam_violin_cn.png', plot = pam_violin_cn)
ggsave('plots/pam_violin_lc.png', plot = pam_violin_lc)
ggsave('plots/pam_violin_redacao.png', plot = pam_violin_redacao)

reetiquetar <- function(x) {
  print(x)
  if(x == 1) return(6)
  if(x == 2) return(3)
  if(x == 3) return(4)
  if(x == 4) return(7)
  if(x == 5) return(2)
  if(x == 6) return(5)
  if(x == 7) return(1)
}

summ1 <- db_mclust %>% group_by(Cluster) %>% summarise(n = n()) %>% mutate(Método = 'GMM')
summ2 <- db_pam %>% group_by(Cluster) %>% summarise(n = n()) %>% mutate(Método = 'K-medoids')

num_obs_cluster <- ggplot(union(summ1, summ2),
       aes(x = Cluster, y = n, fill = Método)) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  labs(title = 'Número de observações em cada um dos grupos por método de clusterização',
       y = 'Número de observações') +
  theme(plot.title = element_text(size=11, face = 'bold'))

ggsave('plots/num_obs_cluster.png', num_obs_cluster)

model_kmeans <- kmeans(ENEM_2023, 7)
db_kmeans <- ENEM_2023 %>% mutate(Cluster = as.factor(model_kmeans$cluster))

ggplot(db_dbscan %>% select(LC, Cluster), aes(x = Cluster, y = LC, group = Cluster, fill = Cluster)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(breaks=1:7, drop = FALSE) +
  ylim(0, 1000) +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas na prova de Linguagem e Código por cluster (K-medoids)',
       y = 'Nota') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )

ggplot(db_dbscan %>% select(MT, Cluster), aes(x = Cluster, y = MT, group = Cluster, fill = Cluster)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(breaks=1:7, drop = FALSE) +
  ylim(0, 1000) +
  theme_bw() +
  labs(title = 'Gráfico de violino das notas na prova de Linguagem e Código por cluster (K-medoids)',
       y = 'Nota') + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11, face = 'bold')
  )


