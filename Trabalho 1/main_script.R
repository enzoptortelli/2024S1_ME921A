library(tidyverse)
library(cluster)
library(factoextra)
library(knitr)

#Passo-a-passo
# 1 - limpar os dados e escalonar/centralizar os dados
# 2 - pca
# 3 - clustering
# 4 - silhueta ou plotar no cartesiano
# dúvida: qual a necessidade de um algoritmo de clusterização quando temos o gráfico biplot em mãos?

# Estou assumindo que não há, nos dados, uma espécie que tenha sido amostrada apenas indivíduos fêmeas.

# --- PINGUIM
# selecionar apenas um sexo. Tomar cuidado com observacoes absurdas.
dados_originais <- read_csv("data/penguin.zip")

# Limpeza dos dados
db <- dados_originais %>%
  drop_na %>%
  filter(flipper_length_mm > 0 & flipper_length_mm < 5000 & sex %in% c('FEMALE', 'MALE')) %>%
  mutate(flipper_length_mm = flipper_length_mm / 10, body_mass_g = body_mass_g / 100)
  

# PCA

biplot(modelPCA <- princomp(db %>%
                              filter(sex == 'MALE') %>%
                              select(!sex) %>%
                              scale(., scale = FALSE))
       )

plot(modelPCA, main = "Screenplot")


# Clustering
db_ppca <- modelPCA$scores[, 1:2]
fviz_nbclust(db_ppca, cluster::pam, method = "wss")

pmodel_pam <- pam(db_ppca, 3)

plot(pmodel_pam)




















elbow_pam <- function(dados, k_max) {
  result_matrix <- matrix(data = 1:k_max, nrow = k_max, ncol = 2)
  
  for(i in 2:k_max) {
    model <- pam(dados, i)
    result_matrix[i, 2] <- mean(silhouette(model)[, 3])
  }
  
  return(result_matrix)
}


#ggplot(db1, aes(x = child_mort, y = total_fer, color = as.factor(t1$cluster))) +
 # geom_point() +
  #theme_bw()
