
 #install.packages("dummies") 
library(dummies) 

#Reading data
S=read.csv("dataqol_classif_imp.csv")
X=S[,2:29]


Xd <- dummy.data.frame(X, sep = ".", dummy.classes='ALL') 

write.table(Xd,file='Xd.txt',sep=" ",row.names=F,col.names=F)
