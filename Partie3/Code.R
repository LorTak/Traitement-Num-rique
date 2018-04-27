#Chargement des bibliothèques et configuration du répertoire de travail
workbook<-loadWorkbook("CCPP/CCPP/Folds5x2_pp.xlsx")
table1<-readWorksheet(workbook,1)
table2<-readWorksheet(workbook,2)
table3<-readWorksheet(workbook,3)
table4<-readWorksheet(workbook,4)
table5<-readWorksheet(workbook,5)
tableT<-rbind(table1,table2,table3,table4,table5)
remove(table1,table2,table3,table4,table5)
summary(tableT)
tableT200<-tableT[1:200,]

plot(tableT200[,"AP"],col="black")
points(tableT200[,"AT"],col="red")
points(tableT200[,"V"],col="green")
points(tableT200[,"RH"],col="blue")
points(tableT200[,"PE"],col="yellow")

PCA(tableT)

lm1<-lm(tableT200[,"PE"]~tableT200[,"V"])
lm2<-lm(tableT200[,"PE"]~tableT200[,"AT"])
plot(tableT200[,"V"],tableT200[,"PE"])
abline(lm1,col="red")
abline(lm2,col="blue")
plot(tableT200[,"AT"],tableT200[,"PE"])
abline(lm1,col="red")
abline(lm2,col="blue")
print(lm1)
print(lm2)
plot(tableT[,"V"],tableT[,"PE"])
abline(lm1,col="red")
abline(lm2,col="blue")
plot(tableT[,"AT"],tableT[,"PE"])
abline(lm1,col="red")
abline(lm2,col="blue")

mse<-function(error){
  mean(error^2)
}
mae<-function(error){
  mean(abs(error))
}
mselm1<-mse(lm1$residuals)
maelm1<-mae(lm1$residuals)
mselm2<-mse(lm2$residuals)
maelm2<-mae(lm2$residuals)
