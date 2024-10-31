################################################################################
# Clean data
################################################################################

rm(list=ls())

##-- Load packages -------------------------------------------------------------
library(data.table)
library(ggplot2)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path)) # Only with Rstudio

##-- Paths ---------------------------------------------------------------------
path_des <- '../Data/disaggregated/'
path_agr <- '../Data/raw_aggregated/'
path_fin <- '../Data/final_data/'
path_fig <- '../Data/Reports/figures/'
path_rep <- '../Data/Reports/'
path_rda <- '../Data/Rdata/'

##-- 2016 US open feet to meter ------------------------------------------------
feet_to_meter <- 3.2808399
file_name <- paste0(path_agr,'d_agr_2016.txt')
d_temp <- read.table(file_name,header = TRUE,sep=' ')
sel_usopen <- d_temp$Slam=='usopen'
d_temp$P1Dist[sel_usopen] <- d_temp$P1Dist[sel_usopen]/feet_to_meter
d_temp$P2Dist[sel_usopen] <- d_temp$P2Dist[sel_usopen]/feet_to_meter
ggplot(d_temp,aes(x=RallyCount,y=P1Dist, colour=Slam)) + geom_point()
write.table(x         = d_temp,
            file      = file_name, 
            row.names = FALSE,
            col.names = TRUE)

##-- Only one file -------------------------------------------------------------
SELECTED_YEARS <- 2017 # 2016:2018
dd_list <- vector("list", length = length(SELECTED_YEARS))
d <- data.frame()
for(year in SELECTED_YEARS){
  d <- rbind(d,read.table(paste0(path_agr,'d_agr_',year,'.txt'),header = TRUE))
}
# dd_2016 <- read.table(paste0(path_agr,'d_agr_2016.txt'),header = TRUE)
# dd_2017 <- read.table(paste0(path_agr,'d_agr_2017.txt'),header = TRUE)
# dd_2018 <- read.table(paste0(path_agr,'d_agr_2018.txt'),header = TRUE)
# d <- rbind(dd_2016,dd_2017,dd_2018)  

dim(d)
sel_na_rally  <- is.na(d$RallyCount)
sel_na_P1dist <- is.na(d$P1Dist)
sel_na_P2dist <- is.na(d$P2Dist)
sel_0_rally   <- !is.na(d$RallyCount) & d$RallyCount==0
sel_0_P1Dist  <- !is.na(d$P1Dist)     & d$P1Dist==0
sel_0_P2Dist  <- !is.na(d$P2Dist)     & d$P2Dist==0

d_miss <- data.frame(type=c('sel_na_rally', 'sel_na_P1dist', 'sel_na_P2dist', 
                            'sel_0_rally',  'sel_0_P1Dist',  'sel_0_P2Dist'),
                     missing=c(sum(sel_na_rally), sum(sel_na_P1dist), sum(sel_na_P2dist), 
                               sum(sel_0_rally),  sum(sel_0_P1Dist),  sum(sel_0_P2Dist)))

dd <- d[!(sel_na_rally | sel_na_P1dist | sel_na_P2dist |
          sel_0_rally  | sel_0_P1Dist  | sel_0_P2Dist),]
dim(dd)
##-- Table slam x year ---------------------------------------------------------
table(dd$Slam,dd$year,dd$Gender)

##-- Speed
summary(dd$Speed)
dd$Speed [is.na(dd$Speed)]  <- mean(dd$Speed,  na.rm=T)
dd$Speed1[is.na(dd$Speed1)] <- mean(dd$Speed1, na.rm=T)
dd$Speed2[is.na(dd$Speed2)] <- mean(dd$Speed2, na.rm=T)

##-- Rename variables ----------------------------------------------------------

##-- Write final data per match ------------------------------------------------
file_name <- paste0(path_fin,'d_match.txt')
write.table(x         = dd,
            file      = file_name, 
            row.names = FALSE,
            col.names = TRUE)
