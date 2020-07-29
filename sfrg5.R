library(igraph)
g1 <- sample_gnm(10, 20)
tkplot(g1, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')

distances(g1, v=V(g1), to =V(g1), mode = c("all", "out",
  "in"), weights = NULL, algorithm = c("automatic", "unweighted",
  "dijkstra", "bellman-ford", "johnson"))

adj = matrix(c(0,1,0,0,0,1,1,0,1,0,0,0,0,1,0,1,0,0,0,0,1,0,1,0,0,0,0,1,0,1,1,0,0,0,1,0),6,6,byrow=TRUE)
adj

g2 = graph_from_adjacency_matrix(adj)
tkplot(g2, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')

distances(g2, v=V(g2), to =V(g2), mode = c("all", "out",
  "in"), weights = NULL, algorithm = c("automatic", "unweighted",
  "dijkstra", "bellman-ford", "johnson"))

k = gorder(g2)      
m = k/2
m