#install.packages("igraph")
#rm(list=ls())

library(igraph)

g1 <- sample_gnm(10, 20)
tkplot(g1, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')
A1<-as_adjacency_matrix(g1)
A1

tg1<-minimum.spanning.tree(g1)
tkplot(tg1, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')
tgA1<-as_adjacency_matrix(tg1)
tgA1


#-----------------------------------------
# function to find the a spanning tree congestion in a graph

#first, given a graph, a spanning tree and an edge of of the 
#spanning tree, calculate the congestion of the edge
edg.congestion<-function(g,tg,ed){
        rtg<-delete.edges(tg,ed)
        ved<-ends(tg,ed)
        scp<-subcomponent(rtg,ved[1])
        cong=0
        for(eg in E(g)){
                if(length(intersect(ends(g,eg),scp))==1){
                cong<-cong+1}} 
        return(cong)}
#-------------------------------------
#it works

E(tg1)
edg.congestion(g1,tg1,E(tg1)[1])  

#---------------------------------------
#second, given a graph and a spanning tree write a list with congestion
#of all edges of the tree
tree.congestion<-function(g,tg){
        sapply(E(tg),edg.congestion,g=g,tg=tg)}
#-----------------------------
#it works

tree.congestion(g1,tg1)

#-------------------------------
# function to find edge degree for all edges of a tree
# the degree of an edge in a tree is the the larger (in number of vertices) 
# of the two components obtained by removing it

#small function for size of component
mysc<-function(var1,var2){length(subcomponent(var1,var2))}

#first, function for degree of one edge
edg.deg<-function(tg,ed){
        ao<-ends(tg,ed)
        rtg<-delete.edges(tg,ed)
        tc<-sapply(ao,mysc,var1=rtg)
        max(tc)}
#second, function of degrees of all edges
edgs.degs<-function(var){sapply(E(var),edg.deg,tg=var)}
#---------------------------------------
#it works

EDs<-edgs.degs(tg1)
E(tg1)
EDs
######-----------------------------------------

# APPLICATIONS

#####------------------------------------------
# Rectangular Grid
k<-5
n<-6
g2<- make_lattice( c(k,n) )
#layout_on_grid(db)
tkplot(g2, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')

tg2<-minimum.spanning.tree(g2)
tkplot(tg2, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')

cong<-tree.congestion(g2,tg2)
cong
max(cong)
#---
g3 <- permute(g2, sample(vcount(g2)))
tg3<-minimum.spanning.tree(g3)
tkplot(tg3, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')

cong<-tree.congestion(g3,tg3)
cong
max(cong)
#---

#####-------------------------------
# Dartboad graph G(n,k)
#k=number rays
#n=number of circles

#first the grid
k<-7
n<-5
gr<- make_lattice( c(k,n+1) )
#layout_on_grid(db)
#tkplot(gr, vertex.shape='circle',
#       vertex.size=15,
#       vertex.color='gold')
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


# Adb<-as_adjacency_matrix(db)
# ev<-eigen(Adb)
# vectors<-ev$vectors
# vectors

vcir<-c()
for(j in 1:k){vcir<-c(vcir,cos(2*pi*(j-1)/k), sin(2*pi*(j-1)/k))}
vcir<-array(vcir,c(k,2))

cir<-c(0,0)
for(i in 1:n){cir<-rbind(cir,i^2*vcir)}

tkplot(db, tk_set_coords = cir, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')
#Adb<-as_adjacency_matrix(db)
#Adb

rglplot(db, tk_set_coords=cir)


tdb<-minimum.spanning.tree(db)
tkplot(tdb, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')
#----
cong<-tree.congestion(db,tdb)
cong
max(cong)
#---

#----
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
#----
cong<-tree.congestion(db2,tdb2)
cong
max(cong)
#---
