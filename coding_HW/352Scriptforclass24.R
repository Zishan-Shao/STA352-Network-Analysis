library(igraph)
gd<-graph.formula(1-+2,1-+4,2-+5,3-+5,3++6,2-+6,6-+4,5++6) # entering order, 1 enter 2, then 1 enter 4 etc

igraph.options(vertex.color="dodgerblue",vertex.size=20, 
vertex.label.cex=1.25,vertex.label.color="white",
edge.color="red",edge.arrow.size=1,edge.width=2)

A<-get.adjacency(gd)
A<-A[order(rownames(A)),order(rownames(A))]
gd<-graph_from_adjacency_matrix(A)	

tkplot(gd)
L<-tk_coords(6,norm=TRUE)*1.5;L[,1]<-L[,1]*1.5 
plot(gd,layout=L,rescale=FALSE)

gu<-as.undirected(gd) 
plot(gu,layout=L,rescale=FALSE)

g<-gu 

vcount(g)
ecount(g)
V(g)
E(g)

degree(g)
sort(degree(g))
degree_distribution(g)

knn(g)$knn
neighborhood(g,order=1,nodes=1,mindist=1)
lapply(neighborhood(g,order=1,mindist=1),degree,g=g)
sapply(lapply(neighborhood(g,order=1,mindist=1),degree,g=g), mean)

distances(g)
diameter(g)
which(shortest.paths(g)==diameter(g),arr.ind=TRUE)

closeness(g)
apply(distances(g),1,sum)
1/apply(distances(g),1,sum)

betweenness(g)
edge_betweenness(g)
E(g) 

table(sapply(cliques(g),length))
cliques(g,min=3,max=3)

gtemp<-g 
gtemp[2,3]<-1
cliques(gtemp,min=4,max=4) 
max_cliques(gtemp,min=2,max=2) 

edge_density(g)
ecount(g)/choose(vcount(g),2)

transitivity(g)
choose(degree(g),2)
sum(choose(degree(g),2))

triad_census(g)
6/15

transitivity(g,type="local")    

ge<-make_ego_graph(gu,nodes=6)[[1]]
(ecount(ge)-vcount(ge)+1)/choose(vcount(ge)-1,2)
plot(gE)

vertex_connectivity(g)
components(g-vertices(c(2,6)))

edge_connectivity(g)
E(g)
components(g-edges(1:2))

plot(gd,layout=L,rescale=FALSE)
dyad.census(gd)
triad_census(gd)  

plot(induced.subgraph(gd, c(2,3,4)))
plot(induced.subgraph(gd, c(3,5,6)))
plot(induced.subgraph(gd, c(1,2,4)))

reciprocity(g2,mode="default")

components(g2,mode="strong")
sort(components(g2,mode="strong")$membership)

