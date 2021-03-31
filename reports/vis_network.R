### Reconstruct a reply graph network ###

library(tidyverse)
library(visNetwork)

# Size
n <- 400
## Base Edges
set.seed(42)
edges <- data %>% select(hashtag_list) %>% sample_n(n)

## Base Nodes with value mapped to global dataset
nodes <- data.frame(id = edges %>% unlist() %>% unique()) %>%
  mutate(value = map_int(id, get_hashtag_frequency))

## Base Nodes with value mapped to local sample dataset
# nodes <- data.frame(edges %>% unlist() %>% table()) %>% 
#   setNames(c("id", "value"))

## Add Edge Variables
edges <- edges %>%
  group_by(from, to) %>%
  mutate(value = n()) %>% # scale width
  unique()

head(edges)

# Add Node Variables
# (Degree of Centrality only for tidygraph objects, parsing?)
nodes <- nodes %>%
  mutate(label = id) %>%
  mutate(title = paste0("<p><b>Occurrences:</b><br>", value, "</p>"))

head(nodes)

## Legend Nodes
# Scaling for Number of occurrences size
# Sizing does not work with value for some reason...
# lnodes <- data.frame(id = 1:5) %>%
#   mutate(value = nodes$value %>% fivenum()) %>%
#   mutate(label = value)


#### Render Graph ####
network <- visNetwork(nodes, edges,
                      width = "100%", height = "650px",
                      background = "white"
) %>%
  visEdges(smooth = TRUE) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 2, hover = FALSE),
    nodesIdSelection = list(enabled = TRUE, main = "Select hashtag")
  ) %>%
  visInteraction(
    navigationButtons = FALSE,
    keyboard = FALSE,
    dragNodes = TRUE,
    dragView = TRUE,
    zoomView = TRUE
  ) %>%
  visPhysics(stabilization = list(
    enabled = FALSE, # Faster loading, physics less stable at the beginning
    iterations = 1000,
    solver = "barnesHut"
  )) %>%
  visLayout(randomSeed = 42) %>%
  visConfigure(enabled = TRUE)

# visIgraphLayout()

network
visSave(network, file = "hashtag_network.html")

# %>%
#

# visLegend(addNodes = lnodes, useGroups = FALSE, main = "Occurrences")
#   for faster performance