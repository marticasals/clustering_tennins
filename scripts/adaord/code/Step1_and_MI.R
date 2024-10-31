
#The clustPOM package is located here
#https://github.com/vuw-clustering/clustord
#Load all the functions we will be needing.
source("R/clustering.R")
source("R/ordinalmodels.R")
source("R/generatestart.R")
source("R/likelihoods_memberships.R")
source("R/utils.R")
source("R/rowclustering_lm_daniel_osm.R")
source("R/utils_daniel_osm.R")


###### Questionnaires Responses Of Patients Affected By Breast Cancer ################################
#We read the data: https://cran.r-project.org/web/packages/ordinalClust/ordinalClust.pdf page 9
library(ordinalClust)
y.mat <- dataqol.classif
#The first column is ID and the last column is death or not. We remove it.
y.mat <- y.mat[,-1]
y.mat <- y.mat[,-ncol(y.mat)]
y.mat <- as.matrix(y.mat)
#We also remove the last two question because they have more levels (q=7) than the other 28 (q=4)
y.mat <- y.mat[,-c(ncol(y.mat)-1,ncol(y.mat))]
head(y.mat)
table(y.mat)
dim(y.mat)
# y.mat is our data set.

#We remove the individuals 36 and 79 because they have lots of NA
index.remove <- c(which(dimnames(y.mat)[[1]]=="36"), which(dimnames(y.mat)[[1]]=="79"))
y.mat <- y.mat[-index.remove,]
dim(y.mat) #38 28

#Impute
library("ForImp")
y.mat.imp <- ForImp(y.mat[1:37,])
y.mat.imp <- rbind(y.mat.imp,y.mat[38,])
y.mat <- y.mat.imp
rm(y.mat.imp)
table(y.mat)
dim(y.mat)

q <- length(table(y.mat)) # number of categories
print(paste("q=",q,sep=""))

long.df <- data.frame(Y=factor(as.vector(y.mat)),ROW=rep(1:nrow(y.mat),times=ncol(y.mat)),
                      COL=rep(1:ncol(y.mat),each=nrow(y.mat)))


AIC <- array(NA,9)
BIC <- array(NA,9)
names(AIC) <- paste0("R",2:10)
names(BIC) <- paste0("R",2:10)

for (r in 5:5)
{
  row.results <- rowclustering("Y~row+column", model="OSM", nclus.row=r, long.df=long.df)
  AIC[r-1] <-  row.results$criteria$AIC
  BIC[r-1] <-  row.results$criteria$BIC
}
AIC
BIC

#R=14 was the best
phi.est <- c(0.0000000, 0.4431103, 0.8216237, 1.0000000)
v.est <- c()
v.est[1] <- 1
for (k in 2:(q-1)) v.est[k] <- 1 + ((q-1)*phi.est[k])
v.est[q] <- q



y.mat.ord<-y.mat
for (k in 1:q) y.mat.ord[which(y.mat==k)] <- v.est[k]

table(y.mat)
table(y.mat.ord)


setwd("/Users/fernandani/Dropbox/Marsden2017-20/Ongoing_Papers/201907_Archetypes/03.Program/Clustering/")
write.csv(y.mat,"dataqol_classif_imp.csv")
write.csv(y.mat.ord,"dataqol_classif_def.csv")
save.image(file = "dataqol_workspace.RData")



