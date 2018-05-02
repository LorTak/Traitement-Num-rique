# Projet de Hamid OuledAli

# Chargement des données

data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.data",sep = ",")

# On enleve les données

cible <- data$whether.he.she.donated.blood.in.March.2007
data$whether.he.she.donated.blood.in.March.2007 <- NULL

# 1)

str(data)
summary(data)
ncol(data)
nrow(data)
pairs(data,col = cible+1)
pairs(red,col = cible+1)
boxplot(data)


# Non les données ne sont pas homogène on va surement devoir centrer et réduire les données Monetary CC blood a beaucoup de valeurs 
# extrêmes

#2)
install.packages("FactoMineR")
library(FactoMineR)

res.pca <- PCA(data)

# On observe que les 2 premiers axes factorielles représente plus de 90 % de l'inertie
# Le premiere aces est porter par frequency et time.month et le second sur recently.month

pairs(res.pca$ind$coord)
red <- res.pca$ind$coord

crit <-c("complete","ward.D2","ward.D","average")

d <-dist(data)
d <- dist(red)
d <-dist(red[,1:2])
pairs(scale(data),col = cible+1)

par(mfrow = c(2,2))
for (c in crit){
  h <- hclust(d,method= c)
  classe = cutree(h,2)
  t <- table(classe,cible)
  plot(red,col=cible+1)
}

pairs(red,col=cible+1)

km <- kmeans(data,2,nstart = 100)
table(km$cluster,cible)

km <- kmeans(red,2,nstart = 100)
table(km$cluster,cible)

km <- kmeans(red[,1:2],2,nstart = 100)
table(km$cluster,cible)









