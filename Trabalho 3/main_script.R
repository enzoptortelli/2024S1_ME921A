set.seed(204256)

#--- Carregando bibliotecas ---#
library(tidyverse)
library(mixer)
library(network)
library(mclust)
library(RColorBrewer)


#--- Lendo dados ---#
liders_gringos <- c('Biden', 'Sunak', 'Meloni', 'Macron', 'Scholz')
#liders_gringos <- c('Meloni')

nw_matrix <- read_csv('network_matrix.csv', n_max = 22) %>%
  rename(Nome = 1) %>%
  filter(!(Nome %in% liders_gringos)) %>%
  select(!all_of(liders_gringos)) %>%
  select(!Nome) %>%
  as.matrix(.)

head_state <- read_csv('pais_chefe.csv') %>% filter((Nome %in% colnames(nw_matrix)))


#--- Etiquetas --#
colnames(nw_matrix) <- head_state$País # A visualização das etiquetas no gráfico fica ruim se colocarmos textos muito grandes
rownames(nw_matrix) <- colnames(nw_matrix)

head_label <- head_state$País


nw_matrix_person <- nw_matrix # mas queremos que, tirando no gráfico de network, as etiquetas sejam os nomes dos chefes de estado
colnames(nw_matrix_person) <- head_state$Nome
rownames(nw_matrix_person) <- colnames(nw_matrix_person)

heatmap(nw_matrix_person, Rowv = NA, Colv = NA, scale = "none")


#--- Fazendo o modelo ---#
nw_model <- network(nw_matrix)

nw_layout <- network.layout.fruchtermanreingold(nw_model, layout.par=NULL)
plot(nw_model, mode = "fruchtermanreingold", label = head_label, coord = nw_layout)


mx_param_est <- mixer(nw_matrix, qmin = 1, qmax = 5, method = "variational") # falar sobre isso

mx_model <- getModel(mx_param_est)
num_clusters <- mx_model$q


prob_obs_cluster <- t(mx_model$Taus)
prob_obs_cluster
mx_model$Pis # colocar essas tabelas
obs_cluster <- map(prob_obs_cluster)


color_pallet <- brewer.pal(max(num_clusters, 3), "YlOrBr") %>% .[1:num_clusters]
color_obs <- color_pallet[obs_cluster] # Etiquetas de acordo com cluster
plot(nw_model, coord = nw_layout, vertex.col = color_obs, label = head_label)
legend("topright", col = color_pallet, pch = 20, legend = 1:num_clusters)

map(prob_obs_cluster)


#--- ATENÇÃO: NÃO TENTE ENTENDER ---#
result <- tibble(País = head_state$País,
                 Nome = head_state$Nome,
                 Cluster_pertencimento = obs_cluster)



k = ncol(result)
for(j in (1:num_clusters + ncol(result))) {
  result[, paste('Seguindo', j - k, sep = '_')] <- 0
  for(i in 1:nrow(result)) {
    result[i, j] <- nw_matrix[i, j - k  == obs_cluster] %>% sum(.)
  }
}

k = ncol(result)
for(j in (1:num_clusters + ncol(result))) {
  result[, paste('Seguidores', j - k, sep = '_')] <- 0
  for(i in 1:nrow(result)) {
    result[i, j] <- nw_matrix[j - k  == obs_cluster, i] %>% sum(.)
  }
}


#--- Gráficos ---#
t <- result %>%
  pivot_longer(cols = c('Seguindo_1', 'Seguindo_2', 'Seguindo_3', 'Seguidores_1', 'Seguidores_2', 'Seguidores_3'), names_to = 'Cluster', values_to = 'Número')

ggplot(t %>%
         select(Nome, Cluster, Número) %>%
         filter(Cluster %in% c('Seguindo_1', 'Seguindo_2', 'Seguindo_3')) %>%
         mutate(Cluster = as.character(parse_number(Cluster))),
       aes(x = reorder(Nome, -Número), y = Número, fill = Cluster)) +
  scale_fill_manual(values = color_pallet) +
  geom_bar(position="stack", stat="identity") +
  labs(title = 'Número de pessoas seguidas por cada chefe de governo separado por cluster',
       x = 'Chefe de estado',
       y = 'Seguindo') +
  theme_bw()

ggplot(t %>%
         select(Nome, Cluster, Número) %>%
         filter(Cluster %in% c('Seguidores_1', 'Seguidores_2', 'Seguidores_3')) %>%
         mutate(Cluster = as.character(parse_number(Cluster))),
       aes(x = reorder(Nome, -Número), y = Número, fill = Cluster)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = color_pallet) +
  labs(title = 'Número de pessoas que seguem cada chefe de governo separado por cluster',
       x = 'Chefe de estado',
       y = 'Seguidores') +
  theme_bw()

ggplot(result %>%
         mutate(Cluster = as.factor(Cluster_pertencimento),
                Seguindo = Seguindo_1 + Seguindo_2 + Seguindo_3) %>%
         group_by(Cluster) %>%
         summarise(Seguindo = mean(Seguindo)),
       aes(x = Cluster, y = Seguindo, fill = Cluster)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = color_pallet) +
  labs(title = 'Média por cluster da quantidade de líderes seguidos',
       x = 'Cluster',
       y = 'Seguindo') +
  theme_bw() +
  theme(legend.position = 'none')

ggplot(result %>%
         mutate(Cluster = as.factor(Cluster_pertencimento),
                Seguidores = Seguidores_1 + Seguidores_2 + Seguidores_3) %>%
         group_by(Cluster) %>%
         summarise(Seguidores = mean(Seguidores)),
       aes(x = Cluster, y = Seguidores, fill = Cluster)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = color_pallet) +
  labs(title = 'Média por cluster da quantidade de líderes seguidos',
       x = 'Cluster',
       y = 'Seguidores') +
  theme_bw() +
  theme(legend.position = 'none')


ggplot(tibble(Nome = head_state$Nome,
              P1 = prob_obs_cluster[, 1],
              P2 = prob_obs_cluster[, 2],
              P3 = prob_obs_cluster[, 3]) %>%
         mutate(Incerteza = 1 - pmax(P1, P2, P3)),
       aes(x = reorder(Nome, Incerteza), y = Incerteza)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  labs(title = 'Incerteza assoaciada à classificação de cada observação',
       x = 'Chefe de governo') +
  theme_bw()
  



