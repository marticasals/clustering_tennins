#STEP 2 for computing ADA with dataqol

source("LFunctions_to_calculate_real_archetypes_with_swap2.R")
source("LstepArchetypesMod.R")                              
source("LstepArchetypoids.R")

#Reading data
S=read.csv("dataqol_classif_def.csv")
X=S[,2:29]

library(archetypes)


#############
#ADA
#############
nrep=20
set.seed(1234)
asd <- stepLArchetypoids3(X,k= 1:10,norep=nrep)
screeplot(asd) 


xx=c()
l=length(asd)

for (i in 1:l){
xx[i]=asd[[i]][[2]]
}

plot <- graphics::plot 
plot(1:l,xx,type='b',xlab='Archetypoids', ylab='RSS',cex.lab=1.5,cex=1.5,cex.axis=1.5)


 X[asd[[2]][[1]],]

 X[asd[[3]][[1]],]
 X[asd[[4]][[1]],]

 X[asd[[5]][[1]],]


Y=read.csv("dataqol_classif_imp.csv")
Y=Y[,2:29]

Y[c(30,27),]

Y[c(30,33,29),]

Y[c(30,33,29,36),]

Y[c(30,33,29,36,27),]

Y[c(30,33,29,36,4,10),]



#################
#Ternary plot, k = 3
#################

#alphas
huge=200
k=3

  n <- ncol(t(X))
  x_gvv <- rbind(t(X), rep(huge, n))
  zs=x_gvv [,asd[[k]][[1]]]
	
  ae <- matrix(0, nrow = k, ncol = n)
  
  for (j in 1 : n){
   ae[, j] = coef(nnls(zs, x_gvv[,j]))
  }




#ternary plot
library(vcd)

ternaryplot(t(ae),grid=T,dimnames=c(1,2,3),col=1,cex=.6,main='ADA')

#############
#starplot with k = 4
##############
#alphas
huge=200
k=4

  n <- ncol(t(X))
  x_gvv <- rbind(t(X), rep(huge, n))
  zs=x_gvv [,c(30,33,29,36)]
	
  ae <- matrix(0, nrow = k, ncol = n)
  
  for (j in 1 : n){
   ae[, j] = coef(nnls(zs, x_gvv[,j]))
  }


stars(t(ae),draw.segments=T,labels=1:38)

