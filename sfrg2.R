#install.packages("igraph")

library(igraph)
n<-8

g <- make_ring(n)
tkplot(g, vertex.shape='circle',
     vertex.size=15,
     vertex.color='gold')
A<-as_adjacency_matrix(g)
ev<-eigen(A)
ev



g1 <- sample_gnm(10, 20)
tkplot(g1, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')
A1<-as_adjacency_matrix(g1)
ev<-eigen(A1)
ev$values
A1

tg1<-minimum.spanning.tree(g1)
tkplot(tg1, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')
tA1<-as_adjacency_matrix(tg1)
tA1

#construct the Dartboad graph G(n,k)
#k=number rays
#n=number of circles

#first the grid
k<-5
n<-7
gr<- make_lattice( c(k,n+1) )
#layout_on_grid(db)
tkplot(gr, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')
#Agr<-as_adjacency_matrix(gr)
#Agr

#cylinder
ne<-c()
for(i in 1:(n+1)){ne<-c(ne,(i-1)*k+1,i*k)}
cy<-add_edges(gr,ne)

tkplot(cy, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')

#dartboard
cl<-function(v){if(v<= k) {1} else {v}}
vl<-lapply(1:length(V(gr)),cl)
dbtemp<- simplify(contract.vertices(cy, vl), remove.loops=TRUE)
db<-delete_vertices(dbtemp,c(2:k))   

tkplot(db, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')
#Adb<-as_adjacency_matrix(db)
#Adb

tdb<-minimum.spanning.tree(db)
tkplot(tdb, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')

#randomize labels on the nodes
#run it until you get something satisfactory
db2 <- permute(db, sample(vcount(db)))
tkplot(db2, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')

tdb2<-minimum.spanning.tree(db2)
tkplot(tdb2, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')
