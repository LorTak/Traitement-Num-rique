
# Projet de Hamid OuledAli et Vincent HUDRY

# Chargement des donnÃ©es

data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.data",sep = ",")

# On enleve les donnÃ©es

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


# Non les donnÃ©es ne sont pas homogÃ¨ne on va surement devoir centrer et rÃ©duire les donnÃ©es Monetary CC blood a beaucoup de valeurs 
# extrÃªmes

#2)
install.packages("FactoMineR")
library(FactoMineR)

res.pca <- PCA(data)

# On observe que les 2 premiers axes factorielles reprÃ©sente plus de 90 % de l'inertie
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

#

data1 <-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=",",header = FALSE)
cible1 <- data1[,1]
data1[,1]<- NULL

names(data1)<-c("Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","NonFlavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")

str(data1)

summary(data1)
ncol(data1)
nrow(data1)
pairs(data1,col= cible1)
pairs(red,col = cible1)
boxplot(data1)

PCA<-PCA(data1[,-5])
plot(PCA)



res.pca <- PCA(data1)

# On observe que les 2 premiers axes factorielles reprÃ©sente plus de 90 % de l'inertie
# Le premiere aces est porter par frequency et time.month et le second sur recently.month

pairs(res.pca$ind$coord)
red <- res.pca$ind$coord

crit <-c("complete","ward.D2","ward.D","average")

d1 <-dist(data1)
d1 <- dist(red)
d1 <-dist(red[,1:2])
pairs(scale(data1),col = cible1+1)

par(mfrow = c(2,2))
for (c in crit){
  h <- hclust(d1,method= c)
  classe = cutree(h,2)
  t <- table(classe,cible1)
  plot(red,col=cible1+1)
}

pairs(red,col=cible1+1)

km <- kmeans(data1,2,nstart = 100)
table(km$cluster,cible1)

km <- kmeans(red,2,nstart = 100)
table(km$cluster,cible1)

km <- kmeans(red[,1:2],2,nstart = 100)
table(km$cluster,cible1)


