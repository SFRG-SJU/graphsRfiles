library(igraph)
library(dplyr)


#create graph to use
g1<- sample_gnm(20, 50)


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
        w<-sample(c(1,5),50,replace=T)
        wg1<-gr
        #adds random weights to edges in graph
        E(wg1)$weight<-w
        #creates mst for random weighted graph
        wtg1<-minimum.spanning.tree(wg1)
        #calculates congestion
        con<-tree.congestion(gr, wtg1)
        max_con = max(con)
        #creates vector of weights, congestion, and max congestion 
        #to add to the dataframe
        vec <- c(w, con, max_con)
        return(vec)
}

#initializes dataframe of appropriate size
df <- data.frame(matrix(NA, nrow=500, ncol=70))
#adds the combined vector to each row of datafram
#one row for each random weighted tree
#may take a while to complete
for (i in 1:500) {
        vec<-random_weighted_tree(g1)
        df[i,] <- vec
}

#DONT RUN UNTIL PREVIOUS LOOP COMPLETES
#determines whether congenstion is small (1) or large (0)
for (i in 1:500) {
        if (df[i, 70] < 18) {
                df[i, 71] <- 1
        } else {
                df[i, 71] <- 0
        }
}

head(df)

#counts the number of small vs large congestion trees
count<-table(df['V71'])
count

write.csv(df, file= 'congestion.csv')