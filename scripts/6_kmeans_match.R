################################################################################
# Clustering per match
################################################################################

rm(list=ls())

##-- Load packages -------------------------------------------------------------
library(data.table)
library(ggplot2)
library(dplyr)
library(compareGroups)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path)) # Only with Rstudio

##-- Paths ---------------------------------------------------------------------
path_des <- '../Data/disaggregated/'
path_agr <- '../Data/raw_aggregated/'
path_fin <- '../Data/final_data/'
path_fig <- 'figures/'
path_rep <- '../Data/Reports/'
path_rda <- '../Data/Rdata/'

file_name <- paste0(path_fin,'d_match.txt')
d <-  fread(file_name, header = T, sep=" ")

################################################################################
# Preprocessing
################################################################################
##-- Variables for performing the clustering
var_clust <- c('played_minutes','Point','RallyCount','Dist', # Load variables
               'rally_min','Dist_min')                       # Intensity  

##-- Variables to compare between clusterings
var_int   <- c(var_clust,'Speed','surface',
               'PercFirstServe','PercSecondServe','PercDoubleFault') # Intensity 

# Not scaled descriptive
dd_m0 <- d %>% filter(Gender=='M') %>% select(all_of(var_int)) # Men
dd_w0 <- d %>% filter(Gender=='W') %>% select(all_of(var_int)) # Women

# scaled clustering
dd_m <- d %>% filter(Gender=='M') %>% select(all_of(var_clust)) %>% scale()# Men
dd_w <- d %>% filter(Gender=='W') %>% select(all_of(var_clust)) %>% scale()# Women

################################################################################
# Clustering per match --> Men
################################################################################
set.seed(12345)

# Elbow men --> k=4
k.max <- 10
wss <- sapply(1:k.max,function(k){kmeans(dd_m, k, nstart=50, iter.max = 15 )$tot.withinss})
png(filename = paste0(path_fig,'elbow_matches.png'),width = 720, height = 480)
plot(1:k.max, wss,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
abline(v=4,lty=3)
dev.off()

# Number of clusters=4
km_m <- kmeans(dd_m, centers =  4, nstart=50, iter.max = 15)
dd_m0$cluster <- km_m$cluster
res <- compareGroups(cluster ~ ., data=dd_m0)
export2html(file = paste0(path_fig,'table_match.html'))
createTable(res)

# ################################################################################
# # Clustering per match --> Women
# ################################################################################
# set.seed(12345)
# 
# # Elbow women --> k=4
# k.max <- 10
# wss <- sapply(1:k.max,function(k){kmeans(dd_w, k, nstart=50, iter.max = 15 )$tot.withinss})
# plot(1:k.max, wss,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
# 
# # Number of clusters=4
# km_w <- kmeans(dd_w, centers =  4, nstart=50, iter.max = 15)
# dd_w0$cluster <- km_w$cluster
# res <-compareGroups(cluster ~ ., data=dd_w0)
# res
# createTable(res)
# 
