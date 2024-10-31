################################################################################
# Exploratory descriptive analysis
################################################################################

rm(list=ls())

##-- Load packages -------------------------------------------------------------
library(data.table)
library(ggplot2)
library(rstudioapi) 

##-- Paths ---------------------------------------------------------------------
path_des <- '../Data/disaggregated/'
path_agr <- '../Data/raw_aggregated/'
path_fig <- '../Data/Reports/figures/'
path_rep <- '../Data/Reports/'
path_rda <- '../Data/Rdata/'

setwd(dirname(getActiveDocumentContext()$path)) # Only with Rstudio

##-- Load data -----------------------------------------------------------------
load(paste0(path_rda,"01_aggregated_data.RData"))

################################################################################
# REGISTERS
################################################################################
color_scale <- c('grey','brown','darkgreen','darkblue')

# PLOT NUMBER OF REGISTERS (POINTS)
ggplot(d_reg,aes(x = year,y = row_d, colour = tournament)) + 
  geom_line() +
  geom_point() +
  facet_wrap(.~tournament) +
  xlab('Year') + ylab('Number of Registers') +
  scale_color_manual(values=color_scale)+
  theme(legend.position = 'none')
file_name <- paste0(path_fig,'registres.png') 
ggsave(filename = file_name, width = 4.5, height = 3)

# PLOT COLUMNS (VARIABLES)
ggplot(d_reg,aes(x = year,y = col_d, colour = tournament)) + 
  geom_line() +
  geom_point() +
  facet_wrap(.~tournament) +
  xlab('Year') + ylab('Number of Columns') +
  scale_color_manual(values=color_scale)+
  theme(legend.position = 'none')
file_name <- paste0(path_fig,'variables.png') 
ggsave(filename = file_name, width = 4.5, height = 3)


################################################################################
# MISSING DATA AND ZEROS
################################################################################
##-- Missings
ggplot(miss[miss$type=='perc_missing',], 
       aes(x=year,y=value,
           colour = tournament,
           shape  = tournament)) +
  geom_line(alpha=0.5) +
  geom_point(alpha=0.5) +
  facet_wrap(.~vars) + 
  xlab('Year') + ylab('% missings') +
  scale_color_manual(values=color_scale) +
  theme(legend.position = 'bottom')

file_name <- paste0(path_fig,'missings.png') 
ggsave(filename = file_name, width = 5, height = 3)

##-- Zeros
ggplot(miss[miss$type=='perc_zero',], 
       aes(x=year,y=value,
           colour= tournament,
           shape = tournament)) +
  geom_line(alpha=0.5) +
  geom_point(alpha=0.5) +
  facet_wrap(.~vars) + 
  xlab('Year') + ylab('% zeros') +
  scale_color_manual(values=color_scale)+
  theme(legend.position = 'bottom')

file_name <- paste0(path_fig,'zeros.png') 
ggsave(filename = file_name, width = 5, height = 3)

##-- Missings i zeros
ggplot(miss[miss$type=='perc_total',], 
       aes(x=year,y=value,
           colour = tournament,
           shape = tournament)) +
  geom_line(alpha=0.5) +
  geom_point(alpha=0.5) +
  facet_wrap(.~vars) + 
  xlab('Year') + ylab('% zeros or missings') +
  scale_color_manual(values=color_scale)+
  theme(legend.position = 'bottom')

file_name <- paste0(path_fig,'zeros_missings.png') 
ggsave(filename = file_name, width = 5, height = 3)

# Tables missings for distance
sel_total1   <- miss$type=='perc_total' & miss$vars=='RallyCount'
sel_total2   <- miss$type=='perc_total' & miss$vars=='P1DistanceRun'

with(miss[sel_total1,],  tapply(value,list(tournament,year),mean)) # There is no data in Aus open 2016
with(miss[sel_total2,],  tapply(value,list(tournament,year),mean)) 

# Data avalilable
with(miss[sel_total & miss$year %in% 2016:2018,],  
     round(100-tapply(value,list(tournament,year),mean),1))


# ------------------------------------------------------------------------------
# DESCRIPTIVE RALLY COUNT VS DISTANCE YEARS 2016 VS 2021
# ------------------------------------------------------------------------------
path_agr <- '../Data/raw_aggregated/'
path_rep <- '../Data/Reports/'
path_fig <- '../Data/Reports/figures/'
TOURNAMENTS <- c('ausopen','frenchopen','wimbledon','usopen')

for(year in 2016:2021){
  print(year)
  dd2 <- read.table(paste0(path_agr,'d_agr_',year,'.txt'),header = TRUE)
  dd2$genere <- factor(substr(dd2$matchnum, 1, 1))
  dd2$up <- as.numeric(dd2$P1Dist/dd2$RallyCount>5.5)
  dd2$Slam <- factor(dd2$Slam,levels=TOURNAMENTS)
  
  # slope_1 <- coef(lm(P1Dist~RallyCount,dd2[dd2$up==1,]))[2]
  # slope_0 <- coef(lm(P1Dist~RallyCount,dd2[dd2$up==0,]))[2]
  # slope_1/slope_0
  
  gg <- ggplot(dd2,aes(x=RallyCount,y=P1Dist,col=Slam)) +
    geom_point(alpha=0.6) +
    xlab('Shot count') + ylab('Distance') + ggtitle(year) +
    scale_color_manual(breaks = TOURNAMENTS,
                       values=color_scale)
  
  assign(paste0('gg_',year),gg)
  file_name <- paste0(path_fig,'rally_distance_',year,'.png') 
  ggsave(filename = file_name, plot = gg, width = 5, height = 3)

}



