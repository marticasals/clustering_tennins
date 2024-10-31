################################################################################
# Aggregate data by player
################################################################################

rm(list=ls())

##-- Load packages -------------------------------------------------------------
library(data.table)
library(ggplot2)
library(dplyr)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path)) # Only with Rstudio

##-- Paths ---------------------------------------------------------------------
path_des <- '../Data/disaggregated/'
path_agr <- '../Data/raw_aggregated/'
path_fin <- '../Data/final_data/'
path_fig <- '../Data/Reports/figures/'
path_rep <- '../Data/Reports/'
path_rda <- '../Data/Rdata/'

file_name <- paste0(path_fin,'d_match.txt')
dd_match <-  fread(file_name, header = T, sep=" ")

##-- Player 1 ------------------------------------------------------------------
dd_P1 = dd_match[ , .(n             =.N,
                      RallyCount    = sum (RallyCount,     na.rm = T)/2, # Rally count is for two players
                      Dist          = sum (P1Dist,         na.rm = T),
                      Point         = sum (Point,          na.rm = T), 
                      Speed         = mean(Speed1,         na.rm = T),
                      FirstServe    = sum (FirstServe1,    na.rm = T), 
                      SecondServe   = sum (SecondServe1,   na.rm = T), 
                      DoubleFault   = sum (DoubleFault1,   na.rm = T), 
                      played_minutes= sum (played_minutes, na.rm = T),
                      Gender        = unique(Gender)
                 
), by= player1]
dd_P1$Player  <- dd_P1$player1
dd_P1$player1 <- NULL

##-- Player 2 ------------------------------------------------------------------
dd_P2 = dd_match[ , .(n             =.N,
                      RallyCount    = sum (RallyCount,     na.rm = T)/2, # Rally count is for two players
                      Dist          = sum (P2Dist,         na.rm = T),
                      Point         = sum (Point,          na.rm = T), 
                      Speed         = mean(Speed2,         na.rm = T),
                      FirstServe    = sum (FirstServe2,    na.rm = T), 
                      SecondServe   = sum (SecondServe2,   na.rm = T), 
                      DoubleFault   = sum (DoubleFault2,   na.rm = T), 
                      played_minutes= sum (played_minutes, na.rm = T), 
                      Gender        = unique(Gender)
                      
), by= player2]
dd_P2$Player  <- dd_P2$player2
dd_P2$player2 <- NULL

##-- All players (with duplicates) ---------------------------------------------
dd_players0 <- rbind(dd_P1,dd_P2)

##-- All players (w/o duplicates) ----------------------------------------------
dd_players <- dd_players0[ , .(n           = sum (n),
                               RallyCount  = sum (RallyCount, na.rm = T), 
                               Dist        = sum (Dist,       na.rm = T),
                               Point       = sum (Point,      na.rm = T), 
                               Speed       = mean(Speed,      na.rm = T),
                               FirstServe  = sum (FirstServe, na.rm = T), 
                               SecondServe = sum (SecondServe,na.rm = T), 
                               DoubleFault = sum (DoubleFault,na.rm = T), 
                               played_minutes= sum (played_minutes,       na.rm = T), 
                               Gender      = unique(Gender)
), by= Player]


##-- Number of players
table(dd_players$Gender)

##-- Remove empty players ------------------------------------------------------
dd_players <- dd_players[trimws(Player) != ""]

##-- Standardized variables ----------------------------------------------------
dd_players[,rally_min  := RallyCount/played_minutes]
dd_players[,rally_match:= RallyCount/n]

dd_players[,Dist_min   := Dist/played_minutes]
dd_players[,Dist_match := Dist/n]

dd_players[,Point_min   := Point/played_minutes]
dd_players[,Point_match := Point/n]

dd_players[,played_minutes_match  := played_minutes/n]

##-- Serve percentage ----------------------------------------------------------
dd_players <- dd_players %>%  mutate(PercFirstServe  = FirstServe  / (FirstServe + SecondServe + DoubleFault) * 100)
dd_players <- dd_players %>%  mutate(PercSecondServe = SecondServe / (FirstServe + SecondServe + DoubleFault) * 100)
dd_players <- dd_players %>%  mutate(PercDoubleFault = DoubleFault / (FirstServe + SecondServe + DoubleFault) * 100)

summary(dd_players)

##-- Write final data per player -----------------------------------------------
file_name <- paste0(path_fin,'d_player.txt')
write.table(x         = dd_players,
            file      = file_name, 
            row.names = FALSE,
            col.names = TRUE)
