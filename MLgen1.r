#rm(list=ls())

library(igraph)
g1<- sample_gnm(10, 20)
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

edg.congestion<-function(g,tg,ed){
        rtg<-delete.edges(tg,ed)
        ved<-ends(tg,ed)
        scp<-subcomponent(rtg,ved[1])
        cong=0
        for(eg in E(g)){
                if(length(intersect(ends(g,eg),scp))==1){
                cong<-cong+1}} 
        return(cong)}

E(tg1)
edg.congestion(g1,tg1,E(tg1)[1])
edg.congestion(g1,tg1,E(tg1)[9])  

tree.congestion<-function(g,tg){
        sapply(E(tg),edg.congestion,g=g,tg=tg)}
lc<-tree.congestion(g1,tg1)
max(lc)


#---
g2 <- permute(g1, sample(vcount(g1)))
tg2<-minimum.spanning.tree(g2)
tkplot(tg2, vertex.shape='circle',
       vertex.size=15,
       vertex.color='gold')

cong<-tree.congestion(g2,tg2)
cong
max(cong)
#---

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

wghts<-sample(c(1,5),20,replace=T)
wghts
wg1<-g1
E(wg1)$weight<-wghts
print(E(wg1)$weight)
tkplot(wg1, vertex.shape='circle',
       vertex.size=15,
       edge.label=wghts,
       vertex.color='gold')

wtg1<-minimum.spanning.tree(wg1)
tkplot(wtg1, vertex.shape='circle',
       vertex.size=15,
       edge.label=wghts,
       vertex.color='gold')

cong<-tree.congestion(g1,wtg1)
cong

A1<-as_adjacency_matrix(g1)
A1

wA1<-as_adjacency_matrix(wg1,attr="weight")
wA1
