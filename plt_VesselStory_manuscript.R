#PLOTS FOR SanctSound vessel manuscript

# INPUT:  output of 1b_processVesselDetectionsAIS.R (combined data products together)
# OUTPUT: Graphics for SanctSound vessel manuscript

library(ggplot2)
library(viridis)
library(dplyr)
library(scales)
library(gridExtra)
library(ggpubr)
library(scatterpie)
library(lubridate)

rm(list=ls())

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=10, face="bold")
  )
#-----------------------------------------------------------------------------------------
#read in files ####
#-----------------------------------------------------------------------------------------
wd = "E:\\RESEARCH\\SanctSound\\data2\\combineFiles1_SPLShips\\"# Summary2019Month_ver2022-01-21.csv
setwd(wd)
infile = paste0(wd,"Summary 2019Month_ver2022-01-21.csv") #choose.files() #output from 1b_process_VesselDetectionsAIS.R
output4 = read.csv(infile)
as.data.frame(colnames(output4))
output4[, 64:66] = as.numeric( unlist( output4[, 64:66] ) )
orgData = output4

#all site lat/longs
siteLoc = read.csv("E:\\RESEARCH\\SanctSound\\data\\SiteLocations.csv")
siteLoc = siteLoc[,1:3]

#2020 data for comparison
infile =  paste0(wd,"Summary 2020Month_ver2022-02-06.csv") #choose.files() #Summary 2020Month_ver2021-10-06.csv
out2020in = read.csv(infile)

#metrics to add
output4$NoiseAdd   = output4$YVess125_med - output4$NVess125_med
output4$LOA_PropL  = output4$LOA_L_UV_sum/ (output4$LOA_S_UV_sum + output4$LOA_M_UV_sum + output4$LOA_L_UV_sum)
output4$ALL_UV_sum = output4$LOA_S_UV_sum + output4$LOA_M_UV_sum + output4$LOA_L_UV_sum

out2020in$NoiseAdd   = out2020in$YVess125_med - out2020in$NVess125_med
out2020in$LOA_PropL  = out2020in$LOA_L_UV_sum/ (out2020in$LOA_S_UV_sum + out2020in$LOA_M_UV_sum + out2020in$LOA_L_UV_sum)
out2020in$ALL_UV_sum = out2020in$LOA_S_UV_sum + out2020in$LOA_M_UV_sum + out2020in$LOA_L_UV_sum

#-----------------------------------------------------------------------------------------
##Summary of sampling across 2019, 2020- tile plot (Figure S1) ####
#USE THIS GRAPHIC TO DECIDE HOW TO truncate data to just a month of data for analyses below
#-----------------------------------------------------------------------------------------
output4$Yr = 2019
out2020in$Yr = 2020
outALL = rbind(output4,out2020in)

ggplot(outALL, aes(as.factor(Mth), Site, fill=as.numeric(as.character(Days)) ) ) +
  geom_tile()+
  xlab("")+ 
  facet_wrap(~Yr)+
  scale_fill_gradient2(low="white", high="blue", name = "days sampled")+
  theme_minimal() +
  theme( legend.position  = "bottom") 
# in illustrator add red border to months sampled in graphics

#-----------------------------------------------------------------------------------------
#only keep specific sites and months
keep = as.data.frame ( rbind(
  c("SB01",03,"Y"), c("SB02",03,"Y"), c("SB03",03,"Y"),
  c("PHRB",1,"N") , c("HI03",1,"N"),  c("HI01",1,"N"), c("HI04",12,"N"),
  c("OC04",08,"N"), c("OC03",11,"N"), c("OC02",03,"Y"), c("OC01",11,"N"),
  c("MB03",03,"Y"), c("MB02",03,"N"), c("MB01",03,"N"),
  c("GR03",5,"N"), c("GR02",5,"N"), c("GR01",5,"N"),
  c("FK04",3,"N") , c("FK03",1,"N"),  c("FK02",1,"N") , c("FK01",1,"N"),
  c("CI05",4,"Y"),  c("CI04",4,"N"),  c("CI03",11,"N"), c("CI02",6,"N"),   c("CI01",4,"N")) )

colnames(keep) = c("Site","Mth","ShipLane") 
keep$Mth =  as.numeric( as.character(keep$Mth) )
keep$Site = as.character(keep$Site) 
keep$ShipLane = as.character(keep$ShipLane)

outputKeep = NULL
for (kk in 1:nrow(keep)){
  tmp = output4[ output4$Site == keep[kk,1] & output4$Mth == keep[kk,2],] 
  tmp$ShipLane = keep[kk,3]
  outputKeep = rbind(outputKeep,tmp)
}
colnames(outputKeep) = (c(colnames(output4),"ShipLane") )
outputKeep$Site = as.character(outputKeep$Site ) 

#-----------------------------------------------------------------------------------------
## AIS traffic size categories- (Figure 2- map pies) ####
#----------------------------------------------------------------------------------------
# AIS COMPOSITION ON MAP... for these months
cData = merge(outputKeep, siteLoc, by.x = "Site")
as.data.frame(colnames(cData))
cData = cbind( cData[1],cData[64:66], cData[104:105])
cData$Total = cData$LOA_S_UV_sum + cData$LOA_M_UV_sum + cData$LOA_L_UV_sum
cData$Small  = cData$LOA_S_UV_sum/cData$Total
cData$Medium = cData$LOA_M_UV_sum/cData$Total
cData$Large  = cData$LOA_L_UV_sum/cData$Total
cData$x = seq(1,nrow(cData), by = 1)
cData$y = seq(1,nrow(cData), by = 1)
cData$norTotal = (cData$Total  - min(cData$Total)) / (max(cData$Total) -  min(cData$Total) )
cData$norTotal = (cData$Total  - min(cData$Total)) / (max(cData$Total) -  min(cData$Total) )
cData$SiteCnt = paste0(cData$Site, " (", cData$Total, ")")

P = ggplot() + geom_scatterpie(aes(x=x, y=y) , data=cData, cols=c("Small","Medium","Large" )) + coord_equal() +
  geom_text(data = cData, aes(x=x, y=y, label = SiteCnt ), vjust = 0, nudge_y = 0.5, size = 2) +theme_minimal() 
P 
#copied to illustrator to put this graphics together-- because so much overlap in sites on this graphic... really only works for within sanctuary comparisons
world <- map_data('world')
p = ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="gray90", color="black") +
  geom_point(data = cData, aes(x=lon, y=lat), color = "red",  size = 1) +
  geom_text(data = cData,  aes(x=lon, y=lat,  label = Site ), size = 2)  +
  coord_quickmap()+
  xlim(-165,-60) + ylim(10, 55) +
  theme_minimal()
p 
#copied to illustrator to put this graphics together-- because so much overlap in sites on this graphic... really only works for within sanctuary comparisons
#make inset map for PM08



#### Import High-Resolution Shoreline ####
# Select the File named "GADM/ENP_gadm36_USA_2.RData"
shorelineFN <- file.choose()#"./GADM/ENP_gadm36_USA_2.RData"
# This loads a spatial polygon named shapeWGS that has all of the US shoreline in high-ish resolution
load(shorelineFN) 
# Clip to your region of interest defined above
minLat  <- 18.255
maxLat  <- 28.05
minLong <- (-176)
maxLong <- (-155)
land <- fortify(shapeWGS) %>% 
  dplyr::filter(long>(minLong-1),long<(maxLong+1),lat>minLat-1,lat<maxLat+1) 
p1 = ggplot() +
  geom_map(map = world,color=NA, fill=NA) +
  geom_polygon(data=land,aes(x=long,y=lat, group=group), fill = "darkgrey") +
  coord_sf(xlim = c(minLong,maxLong), 
           ylim = c(minLat,maxLat), expand = FALSE) +
  theme_classic() +
  xlab("") + 
  ylab("") + 
  geom_point(data = cData, aes(x=lon, y=lat), color = "red",  size = 1) +
  geom_point(data = siteLoc, aes(x=-175.55000, y=27.71667), color = "red",  size = 3) +
  theme(panel.grid.major = element_line(color = gray(.8), size = 0.2)) 
p1

#----------------------------------------------------------------------------
  ## Multiple metrics all sites-  bubble chart (Figure 3) ####
#----------------------------------------------------------------------------
# AMOUNT VESSEL TRAFFIC: AIS detections vs Vessel detections
as.data.frame(colnames(outputKeep))
outputKeep$Sant = substr(outputKeep$Site,1,2)
ggplot(outputKeep, aes(x = TotalVesselDet_cnt_mean, y = LOA_ALL_UV_mean) ) +
  geom_errorbar (aes(ymin=LOA_ALL_UV_mean- LOA_ALL_UV_SD/sqrt(Days), 
                     ymax=LOA_ALL_UV_mean+ LOA_ALL_UV_SD/sqrt(Days)), 
                 colour="gray", width=0) +
  geom_errorbarh(aes(xmin=TotalVesselDet_cnt_mean- TotalVesselDet_cnt_SD/sqrt(Days), 
                     xmax=TotalVesselDet_cnt_mean+ TotalVesselDet_cnt_SD/sqrt(Days)), 
                 colour="gray", width=0) +
  ylim(0,25)+   xlim(0,25)+
  geom_point(aes(size = (LOA_ALL_OPHRS_mean)*60, color = Sant) ) +
  geom_text(aes(label=Site), hjust=1.2, vjust=0, size = 3)+
  xlab("LISTEN: daily count of detections") + ylab("AIS: daily unique vessel count") +
  labs(size = "AIS operational hours")+
  theme_minimal(base_size = 18)+
  theme( plot.title=element_text(size=18, face="bold"), 
         legend.position = "bottom")
#copied to illustrator to edit legend etc

log10(outputKeep$LOA_ALL_OPHRS_mean +1)

#----------------------------------------------------------------------------
## Multiple metrics all sites-  bubble chart (Figure 4) ####
#----------------------------------------------------------------------------
#order site by percent of day
tmp = outputKeep %>% arrange(PropVessel_daily_mean)
outputKeep$Site = factor(outputKeep$Site, levels = tmp$Site, ordered=T)
outputKeep$PropL = (outputKeep$LOA_L_UV_sum/ (outputKeep$LOA_S_UV_sum +outputKeep$LOA_M_UV_sum +outputKeep$LOA_L_UV_sum))*100
idx = which(colnames(outputKeep)== "LOA_PropL" )
colnames(outputKeep)[idx] = "AIS traffic >100 m"
as.data.frame(colnames (outputKeep) )

ggplot(outputKeep, aes(y = PrpHRSVESS*100, x = NoiseAdd, color = ShipLane, label= Site) ) +
  geom_point(aes(size = PropL) ) +
  scale_size(range = c(.3, 10), name="Proportion of Traffic large (>100m)") +
  scale_color_manual(values=c("#999999", "#E69F00") ) +
  ylim(c(0,100)) +
  labs(title = "", 
       subtitle = "",
       caption = "")+
  geom_vline(xintercept = 1)+
  geom_text(aes(label=Site), hjust=1, vjust=0, size = 3)+
  theme_minimal(base_size = 18) +
  theme( plot.title=element_text(size=18, face="bold"), 
         legend.position = "none",
         plot.subtitle  = element_text(color = "dark gray", size = 10, face = "italic"),
         plot.caption  = element_text(color = "dark gray",  size = 10, face = "italic") )
#copied to illustrator to edit legend etc

#----------------------------------------------------------------------------
## Category through the season... GR01, SB02 (Figure 4B) ####
#----------------------------------------------------------------------------
GR01 = output4[ output4$Site == "GR01" | output4$Site == "SB03", ]
ggplot(GR01, aes(y=PrpHRSVESS*100, x=NoiseAdd, color = as.factor(Mth) ) ) +
  geom_point( aes(size = ALL_UV_sum ))+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 25)+
  geom_text(aes(label=Mth), hjust=0, vjust=-1, size = 3)+
  ylim(c(0,100)) +
  xlim(c(-5,10)) +
  theme_minimal(base_size = 18) +
  theme( plot.title=element_text(size=18, face="bold"), 
         plot.subtitle  = element_text(color = "dark gray", size = 10, face = "italic"),
         plot.caption  = element_text(color = "dark gray",  size = 10, face = "italic") )

#----------------------------------------------------------------------------
## Duration of quiet- by category (Figure 5) ####
#  When is it quiet- within a 24 hour day?
#----------------------------------------------------------------------------
outputKeep$cat[ outputKeep$PrpHRSVESS < .25 & outputKeep$NoiseAdd < 1]  = "LL_LexLdom"
outputKeep$cat[ outputKeep$PrpHRSVESS > .25 & outputKeep$NoiseAdd > 1 ] = "HH_HexHdom"
outputKeep$cat[ outputKeep$PrpHRSVESS < .25 & outputKeep$NoiseAdd > 1 ] = "HL_HexLdom"
outputKeep$cat[ outputKeep$PrpHRSVESS > .25 & outputKeep$NoiseAdd < 1 ] = "LH_LexHdom"
outputKeep = outputKeep[!is.na( outputKeep$cat),]
as.data.frame(colnames (outputKeep) )

#use hourly data for these plots
inFiles = list.files (path = wd, pattern = "Hr")
inSites = sapply(strsplit(inFiles,"_"), `[`, 1)
inFilesF = list.files (path = wd, pattern = "Hr", full.names = T)

HRsumALL = NULL
for (ss in 1:length(unique(outputKeep$Site)) ) {
  tSite = outputKeep$Site[ss]
  
  #get hourly data for the site...
  idx = which( inSites == tSite)
  tmpHR = read.csv( inFiles[idx] )
  tmpHR$HR = hour (tmpHR$DateTime )
  tmpHR$MTH = month(tmpHR$DateTime )
  
  #truncate to month of interest for the site
  mth = keep$Mth[keep$Site == tSite]
  tmpHR = tmpHR[ tmpHR$MTH == mth,]
  
  hrs = unique(tmpHR$HR)
  HRsum = NULL
  
  for (hh in 1:length(unique(tmpHR$HR)) ){
    #get the metrics for all the hours
    tmp = tmpHR[tmpHR$HR == hrs[hh],]
   tsd =  sd(tmp$QuietDur, na.rm = T)/60
   tmu =  mean(tmp$QuietDur, na.rm = T)/60 
   tse = ( sd(tmp$QuietDur, na.rm = T)/sqrt(nrow(tmp)) )/60
                                            
   #convert to minutes and save
    HRsum = rbind(HRsum, c(as.character(tSite) ,mth, hrs[hh], 
                           tmu, tsd,tse ) )
    
  }
  colnames( HRsum ) = c("site","Month", "Hour","mean", "sd", "se")
 
  
  
  # HRsum =  as.data.frame(HRsum)
  # HRsum$mean = as.numeric(as.character(HRsum$mean))
  # HRsum$sd   = as.numeric(as.character(HRsum$sd))
  # HRsum$Hour = as.numeric(as.character(HRsum$Hour))
  # #plot of quiet by hour of the day for a given site
  # ggplot(data=HRsum, aes( x = (Hour), y = mean) ) +
  #   geom_bar(stat="identity", color="black", position=position_dodge(), width=0.65, size=0.3)+
  #   geom_errorbar(aes(ymin=mean, ymax=mean+sd), position=position_dodge(.5), width=.2) +
  #   
  #   coord_polar(theta = "x",start=0) +
  #   ylim(c(0,70)) + 
  #   labs( x = "", y = "",
  #         title = "",
  #         caption = "", 
  #         subtitle = "") +
  #   theme_light()+
  #   theme( legend.position = "none",axis.text.y = element_text(size = 12,colour="black"),
  #          axis.text.x=element_text(size = 10,colour="black"))
  
  HRsumALL = rbind( HRsumALL,HRsum )
  
}

#formatting...
HRsumALL = as.data.frame(HRsumALL)
HRsumALL$mean = as.numeric(as.character(HRsumALL$mean))
HRsumALL$sd =  as.numeric(as.character(HRsumALL$sd ))
HRsumALL$se =  as.numeric(as.character(HRsumALL$se ))
HRsumALL$Hour =  as.numeric(as.character(HRsumALL$Hour ))

#HRsumALL = HRsumALL[!is.nan( HRsumALL$Hour),]

#all sites
ggplot(data=HRsumALL, aes( x = (Hour), y = mean) ) +
  geom_bar(stat="identity", color="light gray", position=position_dodge(), width=0.65, size=0.3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(.5), width=.2) +
  theme_minimal()+
  facet_wrap(~site)+
  labs( x = "", y = "", title = "", caption = "", subtitle = "") +
  coord_polar(theta = "x",start=0) +
  theme( legend.position = "none",axis.text.y = element_text(size = 12, colour="black"),
         axis.text.x=element_text(size = 10,colour="black"))

#plot of quiet by hour of the day for a given site
PHH1 = ggplot(data=HRsumALL[HRsumALL$site == "SB03",], aes( x = (Hour), y = mean) ) +
  geom_bar(stat="identity", color="light gray", position=position_dodge(), width=0.65, size=0.3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(.5), width=.2) +
  theme_minimal()+
  labs( x = "", y = "", title = "SB03", caption = "", subtitle = "") +
  ylim(0,60)+
  coord_polar(theta = "x",start=0) +
  theme( legend.position = "none",axis.text.y = element_text(size = 12, colour="black"),
         axis.text.x=element_text(size = 10,colour="black"))
# HH- same across hours- continuous

PHH2 = ggplot(data=HRsumALL[HRsumALL$site == "FK03",], aes( x = (Hour), y = mean) ) +
  geom_bar(stat="identity", color="light gray", position=position_dodge(), width=0.65, size=0.3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(.5), width=.2) +
  theme_minimal()+
  labs( x = "", y = "",title = "FK03", caption = "", subtitle = "") +
  ylim(0,60)+
  coord_polar(theta = "x",start=0) +
  theme( legend.position = "none",axis.text.y = element_text(size = 12, colour="black"),
         axis.text.x=element_text(size = 10,colour="black"))
# HH- difference across hours- clumping

PLH3 = ggplot(data=HRsumALL[HRsumALL$site == "OC01",], aes( x = (Hour), y = mean) ) +
  geom_bar(stat="identity", color="light gray", position=position_dodge(), width=0.65, size=0.3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(.5), width=.2) +
  theme_minimal()+
  labs( x = "", y = "",  title = "OC01",caption = "",  subtitle = "") +
  coord_polar(theta = "x",start=0) +
  ylim(0,60)+
  theme( legend.position = "none",axis.text.y = element_text(size = 12, colour="black"),
         axis.text.x=element_text(size = 10,colour="black"))
# LH- same across hours- some clumping

PHL4 = ggplot(data=HRsumALL[HRsumALL$site == "HI03",], aes( x = (Hour), y = mean) ) +
  geom_bar(stat="identity", color="light gray", position=position_dodge(), width=0.65, size=0.3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(.5), width=.2) +
  theme_minimal()+
  ylim(0,60)+
  labs( x = "", y = "", title = "HI03", caption = "",  subtitle = "") +
  coord_polar(theta = "x",start=0) +
  theme( legend.position = "none",axis.text.y = element_text(size = 12, colour="black"),
         axis.text.x=element_text(size = 10,colour="black"))
# LH- difference across hours- continuous

ggarrange(PHL4,PHH1,PLH3, PHH2)

# !!!! CHECK ERROR BARS FOR SB03

#----------------------------------------------------------------------------
## Change in condition- by category (Figure 6) ####
#  how did vessel presence and sound level change?
#----------------------------------------------------------------------------
#which sites have April data
t1 = ( unique(out2020in$Site[out2020in$Mth == 4] ) )
t2 = ( unique(output4$Site[output4$Mth == 4] ) )
t1[ t1 %in% t2 ]


keep = as.data.frame ( rbind(
  c("CI01",4,"N","LL"), c("CI05",4,"N","HH"), 
  c("FK01",4,"N","LH"), c("FK02",4,"N","LL"),
  c("GR01",4,"N","LL"), 
  c("MB02",4,"N","HL"), c("MB01",4,"N","HL"),
  c("SB01",4,"Y","HH"), c("SB02",4,"Y","HH"),  c("SB03",4,"Y", "HH") ))
colnames(keep) = c("Site","Mth","ShipLane","Category") 
keep$Mth =  as.numeric( as.character(keep$Mth) )
keep$Site = as.character(keep$Site) 
keep$ShipLane = as.character(keep$ShipLane)
keep$Category = as.character(keep$Category)

#2019 truncate data
#---------------------------------------------
outputKeep = NULL
for (kk in 1:nrow(keep)){
  tmp = output4[ output4$Site == keep[kk,1] & output4$Mth == keep[kk,2],] 
  tmp$ShipLane = keep[kk,3]
  tmp$Category = keep[kk,4]
  outputKeep = rbind(outputKeep, tmp )
}
colnames(outputKeep) = c( colnames(output4) , "ShipLane","Category")
outputKeep$Site = as.character(outputKeep$Site) 
as.data.frame(colnames (outputKeep) )
#2020 truncate data
#---------------------------------------------
outputKeep20 = NULL
for (kk in 1:nrow(keep)){
  tmp = out2020in[ out2020in$Site == keep[kk,1] & out2020in$Mth == keep[kk,2],] 
  if (nrow(tmp) > 0){ 
    tmp$ShipLane = keep[kk,3]
    tmp$Category = keep[kk,4]
    outputKeep20 = rbind(outputKeep20, tmp )
  }
}
outputKeep20$Site = as.character(outputKeep20$Site)
as.data.frame(colnames (outputKeep20) )
#select certain variables
#---------------------------------------------
myvars <- c("Site", "PrpHRSVESS", "NoiseAdd", "ShipLane", "Yr", 'LOA_PropL',"OL_median_125", "Category")
out2020 =  outputKeep20[myvars]
out2019 =  outputKeep[myvars]
outputChange = rbind(out2020, out2019)  
outputChange$Site_yr = paste0(outputChange$Site,"_", outputChange$Yr )

p  = ggplot(outputChange, aes(y = PrpHRSVESS*100, x = OL_median_125, color = Category, label= Site_yr) ) +
  geom_point(aes(size = LOA_PropL) ) +
  scale_size(range = c(.1, 24), name="Proportion of Traffic large (>100m)") +
  #scale_color_manual(values=c("#999999", "#E69F00") ) +
  xlab("Low-Frequency Sound Level (125 Hz octave band)") + ylab("Percent of hours with vessel noise") +
  geom_text(aes(label=Site_yr), hjust=1, vjust=0, size = 3) +
  xlim(c(80,100)) +
  theme_minimal(base_size = 18) +
  theme( plot.title=element_text(size=18, face="bold"), 
         legend.position = "none",
         plot.subtitle  = element_text(color = "dark gray", size = 10, face = "italic"),
         plot.caption  = element_text(color = "dark gray",  size = 10, face = "italic") )
plot(p)

#!!!make bubbles a little bigger
# INTERPRETATION (not what I was thinking in terms of categories (ugh)
# MB02-- increase dominance; FK02/CI01/SB02/SB03-- increase level

















