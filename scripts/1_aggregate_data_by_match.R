################################################################################
# Script that aggregates data
# This script aggregates data by matches and provide the number of missing data 
# (missing or zero) by year and tournament for the relevant variables
################################################################################

rm(list=ls())

##-- Load packages -------------------------------------------------------------
library(data.table)
library(ggplot2)
library(dplyr)
library(rstudioapi) 

##-- Paths ---------------------------------------------------------------------
path_des <- '../Data/disaggregated/'
path_agr <- '../Data/raw_aggregated/'
path_fig <- '../Data/Reports/figures/'
path_rep <- '../Data/Reports/'
path_rda <- '../Data/Rdata/'

setwd(dirname(getActiveDocumentContext()$path)) # Only with Rstudio

# ------------------------------------------------------------------------------
# AGREGGATE NUMBER OF REGISTERS
# ------------------------------------------------------------------------------
YEARS <- 2011:2021
for(year in YEARS){
  file_name  <- paste0(path_des,'registres_',year,'.txt')
  d_reg_temp <- read.table(file_name, header = TRUE, sep = ' ')
  d_reg_temp$tournament <- rownames(d_reg_temp)
  d_reg_temp$year       <- year
  if(year==YEARS[1]){
    d_reg <- d_reg_temp
  } else{
    d_reg <- rbind(d_reg,d_reg_temp)
  }
}
d_reg$tournament <- factor(d_reg$tournament,
                           levels=c('ausopen','frenchopen',
                                    'wimbledon','usopen'))


# ------------------------------------------------------------------------------
# AGREGGATE DATA
# ------------------------------------------------------------------------------
##-- Parametres ----------------------------------------------------------------
VARS <- c('RallyCount',
          'P1DistanceRun','P2DistanceRun',
          'ServeNumber','ServeIndicator',
          'Speed_KMH')
TOURNAMENTS <- c('ausopen','frenchopen','wimbledon','usopen')
TYPE <- c('perc_missing','perc_zero','perc_total')    # Indicators: perc_total = perc_zero + perc_missing
missings_dades0 <- expand.grid(VARS,TOURNAMENTS,TYPE) # YEARS
colnames(missings_dades0) <- c('vars','tournament','type')
missings_dades0$value <- NA

##-- Aggregate data ------------------------------------------------------------
ini <- TRUE
for(year in YEARS){
  print(year)
  fitxer <- paste0(path_des,"d_", year, ".txt")
  bb     <- read.table(fitxer, header = TRUE, sep= " ")
  dd     <- as.data.table(bb)
  if(!'RallyCount' %in% names(dd)) dd$RallyCount <- dd$Rally
  if(!'P1DistanceRun' %in% names(dd)){
    dd$P1DistanceRun <- NA
    dd$P2DistanceRun <- NA
  }
  if(!'ServeNumber' %in% names(dd)){
    dd$ServeNumber <- NA
  }
  ##-- Missing data indicators -------------------------------------------------
  miss_temp <- missings_dades0
  miss_temp$year <- year
  n_row <- nrow(miss_temp)
  for(i in 1:n_row){
    v  <- miss_temp$vars[i]
    to <- miss_temp$tournament[i]
    ty <- miss_temp$type[i]
    
    sel_miss <- miss_temp$vars       == v  &
                miss_temp$tournament == to &
                miss_temp$type       == ty
    sel_dd   <- dd$slam == to
    
    if(ty==TYPE[1]){
      miss_temp$value[sel_miss] <- 100*sum(is.na(dd[sel_dd,v,with=FALSE]) | 
                                             dd[sel_dd,v,with=FALSE]=='',na.rm=TRUE)/nrow(dd[sel_dd,])
    }
    if(ty==TYPE[2]){
      miss_temp$value[sel_miss] <- 100*sum(dd[sel_dd,v,with=FALSE]==0 | 
                                             dd[sel_dd,v,with=FALSE]=='0',na.rm=TRUE)/nrow(dd[sel_dd,])
    }
    if(ty==TYPE[3]){       # sum of two previous
      miss_temp$value[sel_miss] <- 100*sum(is.na(dd[sel_dd,v,with=FALSE]) | 
                                             dd[sel_dd,v,with=FALSE]=='',na.rm=TRUE)/nrow(dd[sel_dd,]) + 
                                   100*sum(dd[sel_dd,v,with=FALSE]==0 | 
                                             dd[sel_dd,v,with=FALSE]=='0',na.rm=TRUE)/nrow(dd[sel_dd,])
    }
  }
  if(ini){
    miss <- miss_temp
    ini  <- FALSE
  }else{
    miss <- rbind(miss,miss_temp)
  }
  ##-- FINAL Indicators of missing data ----------------------------------------
  
  dd$distance_exist_1 <- 1-as.numeric(dd$P1DistanceRun == 0 | is.na(dd$P1DistanceRun))
  dd$distance_exist_2 <- 1-as.numeric(dd$P2DistanceRun == 0 | is.na(dd$P2DistanceRun))
  dd$points           <- as.numeric(dd$PointsNumber)
  
  ##-- Aggregate data per match (match_id) -------------------------------------
  dd2= dd[ , .(RallyCount   = sum(RallyCount, na.rm= T), 
               P1Dist       = .N * (sum(P1DistanceRun, na.rm= T)/sum(distance_exist_1)),
               P2Dist       = .N * (sum(P2DistanceRun, na.rm= T)/sum(distance_exist_2)), 
               Point        = .N, 
               Speed        = mean(Speed_KMH[Speed_KMH !=0], na.rm= T),
               Speed1       = mean(Speed_KMH[Speed_KMH !=0 & ServeIndicator == 1], na.rm = TRUE),
               Speed2       = mean(Speed_KMH[Speed_KMH !=0 & ServeIndicator == 2], na.rm = TRUE),
               FirstServe1  = sum(ServeNumber == 1 & ServeIndicator == 1, na.rm= T), 
               SecondServe1 = sum(ServeNumber == 2 & ServeIndicator == 1, na.rm= T), 
               DoubleFault1 = sum(ServeNumber == 0 & ServeIndicator == 1, na.rm= T), 
               FirstServe2  = sum(ServeNumber == 1 & ServeIndicator == 2, na.rm= T), 
               SecondServe2 = sum(ServeNumber == 2 & ServeIndicator == 2, na.rm= T), 
               DoubleFault2 = sum(ServeNumber == 0 & ServeIndicator == 2, na.rm= T),
               Time         = tail(ElapsedTime, n=1),
               matchnum     = unique(match_num), 
               Slam         = unique(slam),
               Gender       = substr(unique(match_num), 1, 1), 
               player1      = tolower(unique(player1)),
               player2      = tolower(unique(player2))), 
           by = match_id]
  
  ##-- New variables -----------------------------------------------------------
  
  ##-- Transform duration of the match to minutes
  dd2$split_time    <- strsplit(dd2$Time, ":")                           # 2:20:30 --> c(2,20,30)
  dd2$total_minutes <- unlist(lapply(dd2$split_time, function(x) {       # c(2,20,30) --> 140.5
    (as.numeric(x[1]) * 60) + as.numeric(x[2]) + (as.numeric(x[3]) / 60)
  }))
  dd2$played_minutes <- 0.2 * dd2$total_minutes                          # 20% of time played
  
  ##-- Standardize variable by time
  dd2$rally_min  <- with(dd2,RallyCount/played_minutes)
  dd2$P1dist_min <- with(dd2,P1Dist    /played_minutes)
  dd2$P2dist_min <- with(dd2,P2Dist    /played_minutes)
  
  ##-- Grouping distances and percentages
  dd2$Dist        <- with(dd2,(P1Dist + P2Dist)/2)          # mean distance by player
  dd2$Dist_min    <- with(dd2,Dist/played_minutes)          # distance by minute for each player
  
  dd2$FirstServe  <- with(dd2,FirstServe1 + FirstServe2)    # Total number of first serves
  dd2$SecondServe <- with(dd2,SecondServe1 + SecondServe2)  # Total number of 2nd serves
  dd2$DoubleFault <- with(dd2,DoubleFault1 + DoubleFault2)  # Total number of double faults
  
  dd2$FirstServe  <- with(dd2,FirstServe1 + FirstServe2)    # Total number of first serves
  dd2$SecondServe <- with(dd2,SecondServe1 + SecondServe2)  # Total number of 2nd serves
  dd2$DoubleFault <- with(dd2,DoubleFault1 + DoubleFault2)  # Total number of double faults
  
  ##-- Year
  dd2$year <- year
  
  ##-- Gender
  dd2$Gender <- factor(ifelse(dd2$Gender=='1','M',
                       ifelse(dd2$Gender=='2','W',as.character(dd2$Gender))))
  
  ##-- Surface
  dd2$surface <- ifelse(dd2$Slam== "ausopen", "Hard", ifelse(dd2$Slam == "frenchopen", "Clay", 
                                                      ifelse(dd2$Slam == "usopen",     "Hard", 
                                                      ifelse(dd2$Slam == "wimbledon" , "Grass ", NA_character_))))
  
  ##-- Serve percentage
  # Player 1
  dd2 <- dd2 %>%  mutate(PercFirstServe1  = FirstServe1  / (FirstServe1 + SecondServe1 + DoubleFault1) * 100)
  dd2 <- dd2 %>%  mutate(PercSecondServe1 = SecondServe1 / (FirstServe1 + SecondServe1 + DoubleFault1) * 100)
  dd2 <- dd2 %>%  mutate(PercDoubleFault1 = DoubleFault1 / (FirstServe1 + SecondServe1 + DoubleFault1) * 100)
  # Player 2
  dd2 <- dd2 %>%  mutate(PercFirstServe2  = FirstServe2  / (FirstServe2 + SecondServe2 + DoubleFault2) * 100)
  dd2 <- dd2 %>%  mutate(PercSecondServe2 = SecondServe2 / (FirstServe2 + SecondServe2 + DoubleFault2) * 100)
  dd2 <- dd2 %>%  mutate(PercDoubleFault2 = DoubleFault2 / (FirstServe2 + SecondServe2 + DoubleFault2) * 100)
  
  # Global
  dd2 <- dd2 %>%  mutate(PercFirstServe  = FirstServe  / (FirstServe + SecondServe + DoubleFault) * 100)
  dd2 <- dd2 %>%  mutate(PercSecondServe = SecondServe / (FirstServe + SecondServe + DoubleFault) * 100)
  dd2 <- dd2 %>%  mutate(PercDoubleFault = DoubleFault / (FirstServe + SecondServe + DoubleFault) * 100)
  
  
  # Save data ------------------------------------------------------------------
  file_name <- paste0(path_agr,'d_agr_',year,'.txt')
  write.table(x         = apply(dd2,2,as.character), # character to avoid problems
              file      = file_name, 
              row.names = FALSE,
              col.names = TRUE)
  
}

##-- Save missing and zero indicators ------------------------------------------
file_name <- paste0(path_agr,'missings.txt')
# miss$tournament <- factor(miss$tournament,levels=TOURNAMENTS)
write.table(x         = miss,
            file      = file_name, 
            row.names = FALSE,
            col.names = TRUE)

save.image(paste0(path_rda,"01_aggregated_data.RData"))

# Per continuar, mirar script "dades_agregades_complet_original"


