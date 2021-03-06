library(igraph)


g2<-make_graph(c(1,6,1,2,1,5,1,4,2,3,2,4,2,9,3,8,3,9,4,7,4,8,5,6,5,9,6,7,7,8), directed = FALSE)
l<-matrix(c(3,7.5,7.5,7.25,2,7,6.5,6,0,2.5,6,4.5,8,3,0.5,1,7,0),9,2, byrow = TRUE)
plot.igraph(g2,  layout = l)

#adjacency matrix of g2
A = matrix(c(0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,0,0,1,0,1,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,0,1,0,0,0,0,1,0,0,1,1,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,0,0,0,0), 9, 9, byrow = TRUE)
A

ev<-eigen(A)
vectors<-ev$vectors
vectors

v2<-vectors[,2]
v3<-vectors[,3]

j<-cbind(v2,v3)
j
plot.igraph(g2, layout = j)