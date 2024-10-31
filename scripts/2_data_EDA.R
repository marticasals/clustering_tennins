################################################################################
# Exploratory descriptive analysis
################################################################################

rm(list=ls())

##-- Load packages -------------------------------------------------------------
library(data.table)
library(ggplot2)

##-- Paths ---------------------------------------------------------------------
path_des <- '../Data/disaggregated/'
path_agr <- '../Data/raw_aggregated/'
path_fig <- '../Data/Reports/figures/'
path_rep <- '../Data/Reports/'
path_rda <- '../Data/Rdata/'

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
       aes(x=year,y=value,colour=tournament)) +
  geom_line(alpha=0.5) +
  geom_point(alpha=0.5) +
  facet_wrap(.~vars) + 
  xlab('Year') + ylab('% missings') +
  scale_color_manual(values=color_scale)+
  theme(legend.position = 'bottom')

file_name <- paste0(path_fig,'missings.png') 
ggsave(filename = file_name, width = 5, height = 3)

##-- Zeros
ggplot(miss[miss$type=='perc_zero',], 
       aes(x=year,y=value,colour=tournament)) +
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
       aes(x=year,y=value,colour=tournament)) +
  geom_line(alpha=0.5) +
  geom_point(alpha=0.5) +
  facet_wrap(.~vars) + 
  xlab('Year') + ylab('% zeros or missings') +
  scale_color_manual(values=color_scale)+
  theme(legend.position = 'bottom')

file_name <- paste0(path_fig,'zeros_missings.png') 
ggsave(filename = file_name, width = 5, height = 3)

# ------------------------------------------------------------------------------
# DESCRIPTIVE RALLY COUNT VS DISTANCE YEARS 2016 VS 2021
# ------------------------------------------------------------------------------
library(ggplot2)
path_agr <- 'C:/Users/jcortes/My Drive/Docencia/TFM TFG/ZZZ_OLD/TFG_Sara_Pascasi_Tennis/Dades/agregades_brutes/'
path_rep <- 'C:/Users/jcortes/My Drive/Docencia/TFM TFG/ZZZ_OLD/TFG_Sara_Pascasi_Tennis/Dades/Reports/'
path_fig <- 'C:/Users/jcortes/My Drive/Docencia/TFM TFG/ZZZ_OLD/TFG_Sara_Pascasi_Tennis/Dades/Reports/figures/'
TOURNAMENTS <- c('ausopen','frenchopen','wimbledon','usopen')
setwd(path_agr)


for(year in 2016:2021){
  print(year)
  dd2 <- read.table(paste0('d_agr_',year,'.txt'),header = TRUE)
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
  # par(mfrow=c(1,2))
  # plot(dd2$P1Dist~dd2$RallyCount,main=year,
  #      col=dd2$genere,xlim=c(0,2100))
  # plot(dd2$P1Dist~dd2$RallyCount,main=year,
  #      col=as.numeric(factor(dd2$Slam)),xlim=c(0,2100))
  
  #plot(dd2$P2Dist~dd2$RallyCount,col=dd2$up+1)
}
save.image(file = paste0(path_rep,'dades.Rdata'))

##-- Exploration


hist(dd2$ratio,br=20)
cutpoint <- ifelse(year==2016,5.5,ifelse(year==2016,5,5))
dd2$up <- ifelse(dd2$ratio>cutpoint,'high_ratio','low_ratio')

##-- match_id
dd2$match_id[!is.na(dd2$up) & dd2$up=='high_ratio']
dd2$match_id[!is.na(dd2$up) & dd2$up=='low_ratio']

# genere
barplot(prop.table(table(dd2$up,dd2$Genere),2))

# torneig
barplot(prop.table(table(dd2$up,dd2$Slam),2))
table(dd2$up,dd2$Slam,useNA = 'ifany')


# View(dd2,'dd2')
# lm(P1Dist~RallyCount,dd2[dd2$up==0,])
# lm(P1Dist~RallyCount,dd2[dd2$up==1,])
# View(dd[sel_dd,c('match_id','distance_exist_1',
#                  'P1DistanceRun','genere')],'us')  
# 
# dddd <-  dd[sel_dd , .(RallyCount   = sum(RallyCount, na.rm= T), 
#                        P1Dist       = .N * (sum(P1DistanceRun, na.rm= T)/sum(distance_exist_1)),
#                        P2Dist       = .N * (sum(P2DistanceRun, na.rm= T)/sum(distance_exist_2))), 
#             by = match_id]
