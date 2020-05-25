library(igraph)
library(dplyr)


#function to calculate congestion for individual edge
edg.congestion<-function(g,tg,ed){
        rtg<-delete.edges(tg,ed)
        ved<-ends(tg,ed)
        scp<-subcomponent(rtg,ved[1])
        cong=0
        for(eg in E(g)){
                if(length(intersect(ends(g,eg),scp))==1){
                        cong<-cong+1}} 
        return(cong)}

#function to calculate total congestion for a tree
tree.congestion<-function(g,tg){
        sapply(E(tg),edg.congestion,g=g,tg=tg)}


#function for random weighted tree
random_weighted_tree<-function(gr){
        #initializes random weights
        w<-sample(c(1:10),20,replace=T)
        wg1<-gr
        #adds random weights to edges in graph
        E(wg1)$weight<-w
        #creates mst for random weighted graph
        wtg1<-minimum.spanning.tree(wg1)
        #calculates congestion
        con<-tree.congestion(gr, wtg1)
        max_con = max(con)
        #creates vector of weights, and max congestion 
        #to add to the dataframe
        vec <- c(w, max_con)
        return(vec)
}

#function to return best 3 trees out of 500 random weighted trees
top_3_trees<-function(g){
    #initialize vectors for top 3 trees and tree df
    tree1<-rep(99, 21)
    tree2<-rep(99, 21)
    tree3<-rep(99, 21)
    tree_df <- data.frame(matrix(NA, nrow=3, ncol=21))
    #create 500 trees with the best 3 stored as vectors
    for (i in 1:500) {
        tree <- data.frame(matrix(NA, nrow=3, ncol=21))
        vec<- random_weighted_tree(g1)
        if (vec[21]<tree1[21]) {
            tree3<-tree2
            tree2<-tree1
            tree1<-vec
        } else if (vec[21]>=tree1[21] && vec[21]<tree2[21]) {
            tree3<-tree2
            tree2<-vec
        } else if (vec[21]>=tree1[21] && vec[21]>=tree2[21] && vec[21]<tree3[21]) {
            vec<-tree3
        }
    
    }
    #put vectors into the df
    tree_df[1,] <- tree1
    tree_df[2,] <- tree2
    tree_df[3,] <- tree3
    return(tree_df)
}

#create df for results
df <- data.frame(matrix(NA, nrow=30, ncol=22))
graph_num = 0
#create 10 graphs, get top 3 trees, and add to df
for (i in 1:10) {
    graph_num = graph_num+1
    tree_num = graph_num*3
    g<- sample_gnm(10,20)
    trees <- top_3_trees(g)
    trees[22] <- graph_num
    vec1<-trees[1,]
    vec2<-trees[2,]
    vec3<-trees[3,]
    i = tree_num-2
    j = tree_num-1
    k = tree_num
    df[i,]<-vec1
    df[j,]<-vec2
    df[k,]<-vec3
}
#rename columns
colnames(df)[colnames(df) == 'X21'] <- 'Congestion'
colnames(df)[colnames(df) == 'X22'] <- 'Graph #'
head(df)

write.csv(df, file= 'top3trees.csv')