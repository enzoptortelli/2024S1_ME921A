db2 <- db %>%
  group_by(Team, Opponent, `Dragons For`, `Dragons Against`) %>%
  mutate(geTotal = sum(`Gold Earned`),
         csTotal = sum(`Creep Score`),
         wpTotal = sum(`Ward Interactions`)) %>%
  ungroup()


db2 <- db2 %>%
  mutate(geShare = `Gold Earned` / geTotal,
         csShare = `Creep Score` / csTotal,
         wpShare = `Ward Interactions` / wpTotal)

db2.1 <- db2 %>%
  group_by(Player, Position) %>%
  summarise(mean(`Champion Damage Share`),  mean(geShare), mean(csShare), mean(wpShare)) %>%
  ungroup() %>%
  select(!Player)

m2 <- as.matrix(db2.1 %>% select(!Position))
rownames(m2) <- db2.1$Position

hier <- hclust(dist(m2))
