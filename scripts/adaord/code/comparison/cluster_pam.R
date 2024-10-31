
library(cluster)

#Reading data
S=read.csv("dataqol_classif_imp.csv")
X=S[,2:29]


dd <- daisy(X, type = list(ordratio = 1:28))

k=4

pamx <- pam(dd, k)

