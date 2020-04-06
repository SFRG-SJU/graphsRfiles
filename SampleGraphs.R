#install.packages("igraph")
library(igraph)
g1<-make_graph(c(1, 2, 2, 3, 3, 4, 5, 6), directed = FALSE)

plot(g1)
rglplot(g1)
tkplot(g1)

g2<-make_graph(c("A", "B", "B", "C", "C", "D"), directed = FALSE)
rglplot(g2)

solids <- list(make_graph("Tetrahedron"),
               make_graph("Cubical"),
               make_graph("Octahedron"),
               make_graph("Dodecahedron"),
               make_graph("Icosahedron"))
for(i in 1:5){tkplot(solids[[i]])}

graph <- make_graph( ~ A-B-C-D-A, E-A:B:C:D,
                     F-G-H-I-F, J-F:G:H:I,
                     K-L-M-N-K, O-K:L:M:N,
                     P-Q-R-S-P, T-P:Q:R:S,
                     B-F, E-J, C-I, L-T, O-T, M-S,
                     C-P, C-L, I-L, I-P)
plot(graph)
rglplot(graph)
tkplot(graph)

