# STA 352 HW1 Zishan (Bruce) Shao

# (a) betweeness: edge & vertex
# define the graph
library(igraph)
#g <- graph.formula(1-2,1-3,2-3,2-4,3-5,4-5)
g <- graph.formula(A-B,A-C,B-C,B-D,C-E,D-E)
E(g)
plot(g, vertex.color="dodgerblue", vertex.label.color="white", edge.color="red", edge.width=2)

# find out the betweeness of edges and nodes
betweenness(g)
E(g)
edge.betweenness(g)


# (b) Compute the local clustering coefficients for the nodes.
transitivity(g, "local")


# (c) Compute the global clustering coefficients for the graph.
transitivity(g)

