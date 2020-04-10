#install.packages("igraph")

library(igraph)

### Isomorphic planar graphs with nonisomorphic duals

g1<-make_graph(~A-B-C-A,B-D,C-E,D-E-F-D)
tkplot(g1,vertex.color='gold')



######
g <- make_lattice( c(6,6) )
layout_on_grid(g)
g2 <- make_lattice( c(5,5))
layout_on_grid(g2)
plot(g, layout=layout_on_grid(g, dim = 2)*1, vertex.size=5,
       vertex.shape='circle', 
        vertex.label=NA, vertex.color='blue',  edge.width=1, edge.color='blue')
#par(new=TRUE)

plot(g2, layout=layout_on_grid(g2, dim = 2)*.4-c(.8,.8), vertex.size=5,
       vertex.shape='square', 
     vertex.label=NA, vertex.color='red', edge.width=0,
     edge.color='red',rescale=F, add=T)


#rglplot plots in 3D

######
g3 <- make_lattice( c(4,4,4) )
layout_on_grid(g3, dim = 3)

gr3pl<-rglplot(g3, layout=layout_on_grid(g3, dim = 3),vertex.size=5,
        vertex.shape='circle', 
        vertex.label=NA, vertex.color='blue',  
        edge.width=1, edge.color='blue')



pdf(filename="")
plot(gr3pl)
dev.off()


#####
g <- sample_gnp(10, 2/10)
l <- layout_with_mds(g)
plot(g, layout=l, vertex.label=NA, vertex.size=3)
######



# all vertex shapes, minus "raster", that might not be available
shapes <- setdiff(shapes(), "")
g <- make_ring(length(shapes))
set.seed(42)
plot(g, vertex.shape=shapes, vertex.label=shapes, vertex.label.dist=1,
     vertex.size=15, vertex.size2=15,
     vertex.pie=lapply(shapes, function(x) if (x=="pie") 2:6 else 0),
     vertex.pie.color=list(heat.colors(5)))

# add new vertex shape, plot nothing with no clipping
add_shape("nil")
plot(g, vertex.shape="nil")