################################################################################
# Archetype per player
# https://www.r-bloggers.com/2012/07/archetypal-analysis/
# The R code is at the end of the page
################################################################################

rm(list=ls())

##-- Load packages -------------------------------------------------------------
library(data.table)
library(ggplot2)
library(dplyr)
library(compareGroups)
library(archetypes)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path)) # Only with Rstudio

##-- Paths ---------------------------------------------------------------------
path_des <- '../Data/disaggregated/'
path_agr <- '../Data/raw_aggregated/'
path_fin <- '../Data/final_data/'
path_fig <- '../Data/Reports/figures/'
path_rep <- '../Data/Reports/'
path_rda <- '../Data/Rdata/'

file_name <- paste0(path_fin,'d_player.txt')
d <-  fread(file_name, header = T, sep=" ")

################################################################################
# Preprocessing
################################################################################
##-- Variables involved in clustering archetypes
# Load variables
var_clust <- c('played_minutes','Point','RallyCount','Dist','rally_match','Dist_match', 
                'rally_min','Dist_min')
## Intensity
var_int   <- c(var_clust,'Speed','PercFirstServe','PercSecondServe','PercDoubleFault','Player') # Intensity 

# Not scaled exploratory analysis
dd_m0 <- d %>% filter(Gender=='M') %>% select(all_of(var_int)) # Men
dd_w0 <- d %>% filter(Gender=='W') %>% select(all_of(var_int)) # Women

# Scaled clustering
dd_m <- d %>% filter(Gender=='M') %>% select(all_of(var_clust)) %>% scale()# Men
dd_w <- d %>% filter(Gender=='W') %>% select(all_of(var_clust)) %>% scale()# Women

################################################################################
# Archetypes per player --> Men
################################################################################
##-- Archetypes
set.seed(12345)
aa <- stepArchetypes(dd_m, k=1:10, nrep=5)
screeplot(aa)

##-- Four clusters -------------------------------------------------------------
aa_4 <- bestModel(aa[[4]]) 

##-- Characteristics
aa_4$archetypes

##-- Archetypes
pp_pos1 <- which.max(coef(aa_4)[,1]) # Prototypes cluster 1
pp_pos2 <- which.max(coef(aa_4)[,2]) # Prototypes cluster 2
pp_pos3 <- which.max(coef(aa_4)[,3]) # Prototypes cluster 3
pp_pos4 <- which.max(coef(aa_4)[,4]) # Prototypes cluster 4
dd_m0[pp_pos1,]
dd_m0[pp_pos2,]
dd_m0[pp_pos3,]
dd_m0[pp_pos4,]

pp_pos1 <- order(coef(aa_4)[,1],decreasing = TRUE)[1:3] # Prototypes cluster 1
pp_pos2 <- order(coef(aa_4)[,2],decreasing = TRUE)[1:3] # Prototypes cluster 2
pp_pos3 <- order(coef(aa_4)[,3],decreasing = TRUE)[1:3] # Prototypes cluster 3
pp_pos4 <- order(coef(aa_4)[,4],decreasing = TRUE)[1:3] # Prototypes cluster 4
dd_m0[pp_pos1,]
dd_m0[pp_pos2,]
dd_m0[pp_pos3,]
dd_m0[pp_pos4,]

##-- Assign to clusters
dd_m0$cluster <- apply(coef(aa_4),1,which.max)
dd_m0$Player[dd_m0$cluster==1]
dd_m0$Player[dd_m0$cluster==2]
dd_m0$Player[dd_m0$cluster==3]
dd_m0$Player[dd_m0$cluster==4]
