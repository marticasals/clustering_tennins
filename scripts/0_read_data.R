################################################################################
# Script that reads the repository databases and groups them for each year
# This script read the data from the Jeff repository and converts it into local
# files and provides the number of registers and columns for each year and 
# tournament. The files are saved in the directory ../Data/disaggregated/
################################################################################

##-- Load libraries ------------------------------------------------------------
library(openxlsx)
library(rstudioapi) 

setwd(dirname(getActiveDocumentContext()$path)) # Only with Rstudio

##-- Parameters ----------------------------------------------------------------
tour_names <- c('ausopen','frenchopen','usopen','wimbledon') # Four tournaments
YEAR       <- 2011:2021                                      # Years of interest
url        <- "https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/" # URL where data comes from
path_dis   <- "../Data/disaggregated/" # Where to save disaggregated data


##-- Read data -----------------------------------------------------------------
for (year in YEAR) {

  d_registres       <- matrix('', ncol=2, nrow=4)
  d_registres_point <- matrix('', ncol=2, nrow=4)
  d_registres_match <- matrix('', ncol=2, nrow=4)
 
  for(i in 1:length(tour_names)) {
    cat('Year:',year,'Tournament:',tour_names[i],'\n')
    
    if(!(i==4 & year==2020)){ # Lack of wimblendon 2020
      name_file_point   <- paste(url,year,'-',tour_names[i],'-points.csv', sep="")
      name_file_matches <- paste(url,year,'-',tour_names[i],'-matches.csv',sep="") 
      
      filesp <- read.table(name_file_point,   skip = 0, header = TRUE, sep =',')             # point by point file
      filesm <- read.table(name_file_matches, skip = 0, header = TRUE, sep =',', quote = "") # match file
    
      d_aux  <- merge(x = filesp , y = filesm, all.x = TRUE, by= "match_id")                 # merge both datasets
    
      ##-- Registers per year -------------------------------------------------- 
      x <- matrix(dim(d_aux),  ncol = 2, byrow = TRUE)
      y <- matrix(dim(filesp), ncol = 2, byrow = TRUE)
      z <- matrix(dim(filesm), ncol = 2, byrow = TRUE)
    
      if(i==1){ 
        d                 <- d_aux
        d_registres       <- x
        d_registres_point <- y
        d_registres_match <- z
      
      }else{ 
        d <- merge(x = d,y = d_aux,all = TRUE)
        d_registres       <- rbind(d_registres,x)
        d_registres_point <- rbind(d_registres_point,y)
        d_registres_match <- rbind(d_registres_match,z)
      } 
    }else{
      d_registres       <- rbind(d_registres,0)
      d_registres_point <- rbind(d_registres_point,0)
      d_registres_match <- rbind(d_registres_match,0)
    }
  }
  
  ##-- Save data ---------------------------------------------------------------
  file_name <- paste0(path_dis,'d_',year,'.txt')     
  write.table(x         = d, 
              file      = file_name, 
              row.names = TRUE, 
              col.names = TRUE)
  
  ##-- Save number of registers and variables ----------------------------------
  matrix_registers <- matrix(c(d_registres,
                               d_registres_point, 
                               d_registres_match), ncol=6, nrow=4,
                             dimnames = list(tour_names,
                                             c("row_d",       "col_d", 
                                               "row_points",  "col_points", 
                                               "row_matches", "col_matches")))
  file_name <- paste0(path_dis,'registres_',year,'.txt') 
  write.table(x         = matrix_registers, 
              file      = file_name, 
              row.names = TRUE, col.names = TRUE)
}

