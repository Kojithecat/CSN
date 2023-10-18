library(igraph)
library(igraphdata)
library(clustAnalytics)
data(karate,package="igraphdata")
wc <- walktrap.community(karate)
modularity(wc)
#[1] 0.3532216
unname(membership(wc)) ## try membership(wc)
#[1] 1 1 2 1 5 5 5 1 2 2 5 1 1 2 3 3 5 1 3 1 3 1 3 4 4 4 3 4 2 3 2 2 3 3
plot(wc, karate)

data(karate,package="igraphdata")
fc <- fastgreedy.community(karate)
dendPlot(fc)

as_adjacency_matrix(as.undirected(karate,mode = "each"))

evaluate_significance(karate,alg_list=list(Louvain=cluster_louvain,
                                             "label prop"= cluster_label_prop,
                                             walktrap=cluster_walktrap),
                        gt_clustering=V(karate)$Faction)

B <- matrix(c(1, 0.2, 0.2, 1), ncol=2)
G <- barabasi_albert_blocks(m=4, p=c(0.5, 0.5), B=B, t_max=100,
                              type="Hajek", sample_with_replacement = FALSE)
plot(G, vertex.color=(V(G)$label),vertex.label=NA,vertex.size=10)


#intersection: count all elements of A that are in B

#union: sum of cardinalities - intersection


