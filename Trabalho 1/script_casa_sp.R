library(tidyverse)
library(cluster)
library(factoextra)


db <- read_csv("data/archive.zip")

# pensar num jeito de normalizar os valores de preco, condominio e tamanho.
# o que acontece com os valores binários (suites, elevador, ...)

db %>%
  select(child_mort, gdpp) %>%
  mutate(gdpp = gdpp / 1000) %>%
  ggplot(data = ., aes(x = child_mort, y = gdpp)) +
  geom_point()

db %>%
  select(child_mort, total_fer) %>%
  ggplot(data = ., aes(x = child_mort, y = total_fer)) +
  geom_point()

db1 <- db %>%
  mutate(total_fer = total_fer * 10, gdpp = gdpp / 1000)

fviz_nbclust(db1[, 2:10], cluster::pam, method = "wss")

model_pam <- pam(db1[, 2:10], 5)

model_hier <- hclust(dist(db1[, 2:10]))

