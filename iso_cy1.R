rm(list=ls())
library(igraph)

g1 <- sample_gnm(20, 30)
tkplot(g1, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')

Di= distances(g1, v=V(g1), to =V(g1), weights = NULL, algorithm ="dijkstra")
# Di is the distances matrix in G=g1

nG=length(V(g1)) #number of vertices of G
#class(Di)
#dim(Di)

m=3
k = 2*m  
#we will be looking for isometric cycles of length k=2m 


vertGk<-c()

for (i in V(g1)) {for (j in V(g1)) {
    if (Di[i, j] == m) {vertGk<-c(vertGk,c(i,j))}
  }
}


vertGkm<-matrix(vertGk,ncol=2,byrow=TRUE)
vertGkm

nGk=nrow(vertGkm) #number of vertices of Gk

edgGk <-c()

for (i in 1:(nGk-1)) {
  for (j in (i+1):nGk) {
    if (Di[vertGkm[i,1], vertGkm[j,1]] == 1) {
    if (Di[vertGkm[i,2], vertGkm[j,2]] == 1) {
      edgGk <-c(edgGk, c(i,j))
    }
  }
}
}

edgGkm<-matrix(edgGk,ncol=2,byrow=TRUE)
edgGkm

eGk=nrow(edgGkm) #number of edges of Gk
eGk

Gk<-graph_from_edgelist(edgGkm,directed=FALSE)
tkplot(Gk, vertex.shape='circle',
       vertex.size=15,
       vertex.color='pink')

DiGk= distances(Gk, v=V(Gk), to =V(Gk), weights = NULL, algorithm ="dijkstra")


dtr=c()  #distance to reverse in Gk: distance from (u,v) to (v,u)
for (u in V(Gk)){
pu=c(vertGkm[u,])
ru=which(vertGkm[1:nGk, 1]==pu[2] & vertGkm[1:nGk,2]==pu[1])
dtr<- c(dtr,DiGk[u,ru])}

dtr
which(dtr==m)

u=5
pu=c(vertGkm[u,])
ru=which(vertGkm[1:nGk, 1]==pu[2] & vertGkm[1:nGk,2]==pu[1])

pu #pair corresponding to vertex u
ru #vertex corresponding to reverse of the pair pu

vertGkm[ru,] #reverse of the pair pu

asp=all_shortest_paths(Gk, u, to = ru, weights = NULL)

np=length(asp$res)
np

vertGkm[asp$res[[1]],] # a cycle of length 6

vertGkm[asp$res[[2]],] # another? cycle of length 6



#TRY now cycles length 8:


m=4
k = 2*m  
#we will be looking for isometric cycles of length k=2m 


vertGk<-c()

for (i in V(g1)) {for (j in V(g1)) {
  if (Di[i, j] == m) {vertGk<-c(vertGk,c(i,j))}
}
}


vertGkm<-matrix(vertGk,ncol=2,byrow=TRUE)
vertGkm

nGk=nrow(vertGkm) #number of vertices of Gk

edgGk <-c()

for (i in 1:(nGk-1)) {
  for (j in (i+1):nGk) {
    if (Di[vertGkm[i,1], vertGkm[j,1]] == 1) {
      if (Di[vertGkm[i,2], vertGkm[j,2]] == 1) {
        edgGk <-c(edgGk, c(i,j))
      }
    }
  }
}

edgGkm<-matrix(edgGk,ncol=2,byrow=TRUE)
edgGkm

eGk=nrow(edgGkm) #number of edges of Gk
eGk

Gk<-graph_from_edgelist(edgGkm,directed=FALSE)
tkplot(Gk, vertex.shape='circle',
       vertex.size=15,
       vertex.color='pink')

DiGk= distances(Gk, v=V(Gk), to =V(Gk), weights = NULL, algorithm ="dijkstra")


dtr=c()  #distance to reverse in Gk: distance from (u,v) to (v,u)
for (u in V(Gk)){
  pu=c(vertGkm[u,])
  ru=which(vertGkm[1:nGk, 1]==pu[2] & vertGkm[1:nGk,2]==pu[1])
  dtr<- c(dtr,DiGk[u,ru])}

dtr
which(dtr==m)

u=6
pu=c(vertGkm[u,])
ru=which(vertGkm[1:nGk, 1]==pu[2] & vertGkm[1:nGk,2]==pu[1])

pu #pair corresponding to vertex u
ru #vertex corresponding to reverse of the pair pu

vertGkm[ru,] #reverse of the pair pu

asp=all_shortest_paths(Gk, u, to = ru, weights = NULL)

np=length(asp$res)
np

