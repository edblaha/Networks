# Olympic co-medalling network, 1896-2016 (TidyTuesday 2021-07-27)
# Nodes = National Olympic Committees (NOCs)
# Edges = two NOCs medalled in the same event at the same Games

library(tidyverse)
library(igraph)
library(countrycode)

olympics <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv")

podiums <- olympics |>
  filter(!is.na(medal)) |>
  distinct(games, event, noc)#used bc of team events- AI suggested it as a way to collapse lists of every medalist in team events


nodes <- podiums |>
  count(noc, name = "podiums") |>
  rename(id = noc) |>
  mutate(country = countrycode(id, "ioc", "country.name",              
                               custom_match = c(URS = "USSR",
                                                USA = "USA",
                                                GBR = "UK",
                                                GDR = "East Germany",
                                                FRG = "West Germany",
                                                EUA = "United Team of Germany",
                                                TCH = "Czechoslovakia",
                                                YUG = "Yugoslavia",
                                                SCG = "Serbia & Montenegro",
                                                EUN = "Unified Team",
                                                ANZ = "Australasia",
                                                BOH = "Bohemia",
                                                RU1 = "Russian Empire",
                                                WIF = "West Indies Federation",
                                                UAR = "United Arab Republic",
                                                PRK = "N. Korea"),
                               warn = FALSE),
         country = coalesce(country, id))

nodes
#AI assisted changing names to full country names from codes automatically


#links between countries on podium together
links <- podiums |>
  inner_join(podiums, by = c("games", "event"), relationship = "many-to-many") |>
  filter(noc.x < noc.y) |>
  count(from = noc.x, to = noc.y, name = "shared")

#weighting using cosine similarity (Claude showed me how, need to capture proportion of podiums shared rather than raw count)
links <- links |>
  left_join(select(nodes, id, podiums), by = c("from" = "id")) |>
  left_join(select(nodes, id, podiums), by = c("to" = "id")) |>
  mutate(weight = shared / sqrt(podiums.x * podiums.y)) |>
  select(from, to, shared, weight)

#Pair that shared at least 2 podiums and have a weight of at least 0.055
links <- links |>
  filter(shared >= 3, weight >= 0.055)

links |>
  arrange(desc(weight))


#Keep only countries that appear in the edgelist
nodes <- nodes |>
  filter(id %in% c(links$from, links$to))



net <- graph_from_data_frame(d=links, vertices=nodes, directed=F)
net



#Groups of countries that tend to medal together 

set.seed(50)
V(net)$color <- as.numeric(membership(cluster_louvain(net)))


V(net)$eigen <- eigen_centrality(net)$vector


nice_layout <- layout_with_fr(net, weights = E(net)$weight)


png("olympic_network.png", width = 3200, height = 3200, res = 250)


par(mar = c(0, 0, 2, 0))

plot.igraph(net,
            layout=nice_layout,
            main="Olympic co-medalling network, 1896-2016",
            vertex.label=V(net)$country,
            vertex.label.family="Helvetica",
            vertex.label.font=2,
            vertex.label.cex=0.55,
            vertex.label.color="grey20",
            vertex.label.dist=0.9,
            vertex.size=6 + V(net)$eigen*12,
            vertex.frame.color=NA,
            edge.width=E(net)$weight*10,
            edge.color=adjustcolor("grey50", 0.3),
            edge.curved=0.15)

dev.off()
