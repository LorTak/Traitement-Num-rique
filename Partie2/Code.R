#Importation des bibliothèques
#configuration du répertoire de travail
classic30<-readMat("classic30.mat")
classic30$classid
factor(classic30$classid)
nbclasses=length(levels(factor(classic30$classid)))
kmeans<-kmeans(classic30$dtm,nbclasses)
table(classic30$classid,kmeans$cluster)               

mat<-as.matrix(classic30$dtm)
mat<-mat%*%t(mat)
mat[mat!=0]<-1
graph<-graph_from_adjacency_matrix(mat, mode="undirected")
plot(graph)
cluster_fast_greedy<-cluster_fast_greedy(graph)
plot(cluster_fast_greedy,graph)
table(classic30$classid,cluster_fast_greedy$membership)
table(kmeans$cluster,cluster_fast_greedy$membership)
