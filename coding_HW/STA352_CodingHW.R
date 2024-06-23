

## STA352 Coding Homework
## Zishan (Bruce) Shao

## Q1: Consider the following network
# (a) Give the number of edges and the number of vertices in the network
library(igraph)
g<-graph.formula(1-3,1-4,1-5,1-6,2-4,2-5,2-6,3-5,4-6) # create the graph

vcount(g) # provide count of vertices
ecount(g) # provide count of edges
V(g)
E(g)


# (b): degree distribution of network
degree(g) # calculate degrees for each of th vertices
sort(degree(g))
degree_distribution(g)


# (c): average nearest neighbor degree for vertices 2,3, and 4
knn(g)$knn # construct a nearest neighbor network
neighborhood(g,order=1,nodes=1,mindist=1)
lapply(neighborhood(g,order=1,mindist=1),degree,g=g)
sapply(lapply(neighborhood(g,order=1,mindist=1),degree,g=g), mean)


# (d):
A<-get.adjacency(g)
A<-A[order(rownames(A)),order(rownames(A))]
g<-graph_from_adjacency_matrix(A,mode="undirected")	

distances(g)
diameter(g) # get the diameter
# get the shortest path (explore the pair with no 2,3,4 vertices)
which(shortest.paths(g)==diameter(g),arr.ind=TRUE) 


# (e):
g_less <- delete_edges(g, get.edge.ids(g, c(1,4)))
A_less<-get.adjacency(g_less)
A_less<-A_less[order(rownames(A_less)),order(rownames(A_less))]
g_less<-graph_from_adjacency_matrix(A_less,mode="undirected")	

diameter(g_less) # proven to increase by 1


# (f):
closeness(g)
apply(distances(g),1,sum)
1/apply(distances(g),1,sum) # proven that it is 1, 3


# (g) 
betweenness(g) # vertex betweeness --> proved


# (h)
E(g)
edge_betweenness(g) # edge betweeness --> proved


# (i)
table(sapply(cliques(g),length))
cliques(g,min=3,max=3)


# (j)
gtemp<-g 
gtemp[1,2]<-1
cliques(gtemp,min=4,max=4) 


# (k)
max_cliques(g,min=2,max=2) 




## Q2: 

# (a)
edge_density(g)
ecount(g)/choose(vcount(g),2)

# (b)
transitivity(g)

# (c)
choose(degree(g),2)
sum(choose(degree(g),2))
transitivity(g,type="local")  # This proves my result


# (d)
ge<-make_ego_graph(g,nodes=1)[[1]]
#(ecount(ge)-vcount(ge)+1)/choose(vcount(ge)-1,2)
plot(ge)


# (e)
# vertice connectivity
vertex_connectivity(g)
components(g-vertices(c(1,5)))

# (f)
# edge connectivity
edge_connectivity(g)
components(g-edges(1:3, 3:5))




## Q3:

# Define the directed graph here
library(igraph)
gd <- graph.formula(1-+3, 1-+4, 1++5, 1-+6, 2-+4, 2++5, 2-+6, 4++6, 5-+3)

igraph.options(vertex.color="dodgerblue", vertex.size=20, 
               vertex.label.cex=1.25, vertex.label.color="white",
               edge.color="red", edge.arrow.size=1, edge.width=2)

A <- get.adjacency(gd)
A <- A[order(rownames(A)), order(rownames(A))]
gd <- graph_from_adjacency_matrix(A) 

# Create a tkplot and save the plot ID
tkp.id <- tkplot(gd)

# Use the plot ID to manipulate the plot
L <- tk_coords(tkp.id, norm=TRUE) * 1.5
L[, 1] <- L[, 1] * 1.5 
# plot(gd, layout=L, rescale=FALSE)


# (a)
plot(gd,layout=L,rescale=FALSE)
dyad.census(gd)
triad_census(gd)  

# (b)

# (c)
plot(induced.subgraph(gd, c(1,3,5)))


# (d)
plot(induced.subgraph(gd, c(3,4,6)))
plot(induced.subgraph(gd, c(5,6,4)))


# (e)
reciprocity(gd,mode="default")


# (f)
components(gd,mode="strong")
sort(components(gd,mode="strong")$membership)










