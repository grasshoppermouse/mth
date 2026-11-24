# Hierarchical clustering

# Top 100 words
text2vec100 <- word_vectors[mth200udpipe$lemma[1:100], ]

# L2-normalize each row so every word vector has a length of 1
row_norms <- sqrt(rowSums(text2vec100^2))
norm_vectors <- text2vec100 / row_norms

# Cosine similarity matrix via dot product of unit vectors
cos_sim <- tcrossprod(norm_vectors)

# Convert to cosine distance
cos_dist <- as.dist(1 - cos_sim)

# Cluster
hc <- hclust(cos_dist, method = "ward.D2")

# Cut the tree into k clusters
# create a named vec to serve as a dict
hc_clusters <- setNames(cutree(hc, k = 6), rownames(text2vec100))

phy <- ape::as.phylo(hc)
g <- igraph::as.igraph(phy, directed = TRUE)

# 2. Attach branch lengths for node heights
E(g)$weight <- phy$edge.length

# Assign clusters and word frequencies
V(g)$cluster <- as.factor(hc_clusters[V(g)$name])
names(mth200udpipe$n) <- mth200udpipe$lemma
V(g)$n <- mth200udpipe$n[V(g)$name]

p <-
  ggraph(g, layout = "dendrogram", circular = TRUE, length = weight) +
  geom_edge_link(alpha = 0.6) +
  geom_node_point(
    aes(filter = leaf, colour = cluster, size = log10(n)),
    size = 1.5,
    # color = "steelblue"
  ) +
  geom_node_text(
    aes(
      x = x * 1.05, # Push labels 10% outward radially
      y = y * 1.05,
      label = name,
      colour = cluster,
      size = log10(n),
      filter = leaf,
      angle = node_angle(x, y)
    ),
    hjust = "outward",
    # size = 4.5
  ) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(60, 60, 60, 60), legend.position = "none")
ggsave("hclust.svg", p, width = 6, height = 6)
