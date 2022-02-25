#PLOTS FOR VESSEL STORY MAP
# INPUT:  output of 1b_processVesselDetectionsAIS.R (combined data products together)
# OUTPUT: Graphics for each section of story map
library(ggplot2)
library(viridis)
library(dplyr)
library(scales)
library(gridExtra)
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
#read in files
#-----------------------------------------------------------------------------------------
setwd("E:\\RESEARCH\\SanctSound\\data2\\combineFiles1_SPLShips\\")# Summary2019Month_ver2022-01-21.csv
infile = choose.files()
output4 = read.csv(infile)
as.data.frame(colnames(output4))
output4$Site
output4[, 64:66] = as.numeric( unlist( output4[, 64:66] ) )
orgData = output4

#all site lat/longs
siteLoc = read.csv("E:\\RESEARCH\\SanctSound\\data\\SiteLocations.csv")
siteLoc = siteLoc[,1:3]
#2020 data for comparison
infile = choose.files() #Summary 2020Month_ver2021-10-06.csv
out2020in = read.csv(infile)
#which sites have April data
t1 = ( unique(out2020in$Site[out2020in$Mth == 4] ) )
t2 = ( unique(output4$Site[output4$Mth == 4] ) )
t1[ t1 %in% t2 ]

#-----------------------------------------------------------------------------------------
##Summary of sampling across 2019- tile plot
#-----------------------------------------------------------------------------------------
ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(Days)) ) ) +
  geom_tile()+
  xlab("Month in 2019")+ 
  scale_fill_gradient2(low="white", high="blue")+
  theme( legend.title = element_blank() ) +
  ggtitle("Days sampled 2019")
#USE THIS GRAPHIC TO DECIDE HOW TO truncate data to just a month of data for analyses below
#-----------------------------------------------------------------------------------------
#Noise added, ALL SITES, ALL MONTHS (vessel-non vessel hours)
output4$NoiseAdd   = output4$YVess125_med - output4$NVess125_med
output4$LOA_PropL  = output4$LOA_L_UV_sum/ (output4$LOA_S_UV_sum + output4$LOA_M_UV_sum + output4$LOA_L_UV_sum)
output4$ALL_UV_sum = output4$LOA_S_UV_sum + output4$LOA_M_UV_sum + output4$LOA_L_UV_sum

#-----------------------------------------------------------------------------------------
##Sound Levels with/without vessels-- all sites tile plots, histogram
#-----------------------------------------------------------------------------------------
#ALL days, ALL SITES, ALL MONTHS
ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(OL_median_125)) ) ) +
  geom_tile()+
  xlab("Month in 2019")+ 
  theme( legend.title = element_blank() ) +
  ggtitle("Octave Band Sound Pressure Level [125 Hz]")
#Non-vessel days, ALL SITES, ALL MONTHS
ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(NVess125_med)) ) ) +
  geom_tile()+
  xlab("Month in 2019")+ 
  theme( legend.title = element_blank() ) +
  ggtitle("Octave Band Sound Pressure Level (no vessel hours) [125 Hz]")
#Vessel days, ALL SITES, ALL MONTHS
ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(YVess125_med)) ) ) +
  geom_tile()+
  xlab("Month in 2019")+ 
  theme( legend.title = element_blank() ) +
  ggtitle("Octave Band Sound Pressure Level (vessel hours) [125 Hz]")

#USE IN OSM PPT
ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(NoiseAdd)) ) ) +
  geom_tile()+
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred")+
  xlab("2019")+ 
  theme( legend.title = element_blank() ) #+
  #ggtitle("Noise added when vessel noise dominant")

#distributions of sound levels with and without vessels present
mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
hgA <- hist(output4$NVess125_med, breaks = 20, plot = FALSE) # Save first histogram data
hgB <- hist(output4$YVess125_med, breaks = 20, plot = FALSE) # Save 2nd histogram data
plot(hgA, col = c1) # Plot 1st histogram using a transparent color
plot(hgB, col = c2, add = TRUE) # Add 2nd histogram using different color

#-----------------------------------------------------------------------------------------
## AIS traffic size categories- stacked bar graphs
#COMPARISION OF TRAFFIC COMPOSITION ACROSS ALL SITES-- Stacked Bar sorted by lowest to highest % large
#only keep specific sites and months
#-----------------------------------------------------------------------------------------
#  (output4$LOA_S_UV_sum[1] + output4$LOA_M_UV_sum[1] + output4$LOA_L_UV_sum[1])
keep = as.data.frame ( rbind(
  c("SB01",03), c("SB02",03), c("SB03",03),
  c("PHRB",1) , c("HI03",1),  c("HI01",1),
  c("OC04",04), c("OC03",11), c("OC02",04), c("OC01",04),
  c("MB03",03), c("MB02",03), c("MB01",03),
  c("GR03",11), c("GR02",11), c("GR01",11),
  c("FK04",1) , c("FK03",1),  c("FK02",1) , c("FK01",1),
  c("CI05",4), c("CI04",4), c("CI02",6), c("CI01",4)) )
colnames(keep) = c("site","mth") 
keep$mth =  as.numeric( as.character(keep$mth) )
keep$site = as.character(keep$site) 

outputKeep = NULL
for (kk in 1:nrow(keep)){
  tmp = output4[ output4$Site == keep[kk,1] & output4$Mth == keep[kk,2],] 
  outputKeep = rbind(outputKeep,tmp)
}
colnames(outputKeep) = colnames(output4)
outputKeep$Site = as.character(outputKeep$Site ) 
# write.csv(outputKeep, "E:\\RESEARCH\\SanctSound\\analysis\\Vessel_story\\Summary_vesselData.csv") 

#across sites graphic- STACKED bars for vessel composition across sites
tmp = outputKeep %>% arrange(LOA_PropL)
outputKeep$Site = factor(outputKeep$Site, levels = tmp$Site, ordered=T)
output4m = reshape2 :: melt(outputKeep, id.vars = "Site", measure.vars = c("LOA_S_UV_sum","LOA_M_UV_sum","LOA_L_UV_sum" ))
# add month to the site name-- color the text?

ggplot(output4m, aes(fill=variable, y=value, x=Site)) + 
  geom_bar(position="fill", stat="identity",width = .75)+
  xlab("")+ ylab("")+
  scale_fill_viridis(discrete = T, name = "Vessel Size", labels = c("Small (<20m)", "Medium (20-100m)", "Large (>100m)")) +
  ggtitle("Composition of AIS Vessel Traffic")+AS
  #labs(subtitle = "Data source: Jan 2019 (HI,FK); Apr 2019 (CI); Nov 2019 (SB,OC,MB)")+
  theme_minimal()+
  theme(axis.text.x   = element_text(angle = 45), 
        plot.subtitle = element_text(color = "black", size = 14, face = "italic"),legend.position = "none" )
#NOTE: export and overlay in illustrator with sound levels


## AIS COMPOSITION ON MAP... for these months
#http://www.spectdata.com/index.php/2018/10/25/how-to-use-ggplot-to-plot-pie-charts-on-a-map/
#combining data
cData = merge(outputKeep, siteLoc, by.x = "Site")
as.data.frame(colnames(cData))
cData = cbind( cData[1],cData[64:66], cData[102:103])
cData$Total = cData$LOA_S_UV_sum + cData$LOA_M_UV_sum + cData$LOA_L_UV_sum
cData$Small  = cData$LOA_S_UV_sum/cData$Total
cData$Medium = cData$LOA_M_UV_sum/cData$Total
cData$Large  = cData$LOA_L_UV_sum/cData$Total

# need a matrix with repeat lat lon f

library(scatterpie)
#https://cran.r-project.org/web/packages/scatterpie/vignettes/scatterpie.html
#cDatam = reshape2 :: melt(cData, id.vars = c("Site","lat","lon"), measure.vars = c("Small","Medium","Large" ))

sum( cData$Total )
cData$x = seq(1,nrow(cData), by = 1)
cData$y = seq(1,nrow(cData), by = 1)

cData$norTotal = (cData$Total  - min(cData$Total)) / (max(cData$Total) -  min(cData$Total) )
cData$norTotal = (cData$Total  - min(cData$Total)) / (max(cData$Total) -  min(cData$Total) )

cData$SiteCnt = paste0(cData$Site, " (", cData$Total, ")")
ggplot() + geom_scatterpie(aes(x=lon, y=lat), data=cData, cols=c("Small","Medium","Large" ))  + coord_equal()
#too much overlap...

P = ggplot() + geom_scatterpie(aes(x=x, y=y, r=norTotal) , data=cData, cols=c("Small","Medium","Large" )) + coord_equal() +
  geom_text(data = cData, aes(x=x, y=y, label = Site ), vjust = 0, nudge_y = 0.5, size = 2)  
P + geom_scatterpie_legend(cData$norTotal, x=20, y=5)
#too big a size difference...

P = ggplot() + geom_scatterpie(aes(x=x, y=y) , data=cData, cols=c("Small","Medium","Large" )) + coord_equal() +
  geom_text(data = cData, aes(x=x, y=y, label = SiteCnt ), vjust = 0, nudge_y = 0.5, size = 2)  
P #copied to illustrator to put this graphics together-- because so much overlap in sites on this graphic... really only works for within sanctuary comparisons


world <- map_data('world')
p = ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="gray90", color="black") +
  geom_point(data = cData, aes(x=lon, y=lat), color = "red", size = 1) +
  geom_text(data = cData, aes(x=lon, y=lat, label = Site ), size = 2)  +
  coord_quickmap()+
  xlim(-160,-60) + ylim(10, 55) +
  theme_minimal()
p #copied to illustrator to put this graphics together-- because so much overlap in sites on this graphic... really only works for within sanctuary comparisons

p2 = p + geom_scatterpie(aes(x=lon, y=lat, r=norTotal), data=cData, cols=c("Small","Medium","Large" ), alpha=.8)+
  geom_scatterpie_legend(cData$norTotal, x=-140, y=40)
p2 # too much overlap to see anything


#-------------------- ---------------------------------------------------------------------
#SOUND LEVELS AND AIS traffic- scatter plots with error bars
#-------------------- ---------------------------------------------------------------------
# overlay graphics in illustrator: sound levels ordered by proportion large vessels?
ggplot(outputKeep, aes(x=Site, y=OL_median_125 )) + 
  geom_point(size = 5) +
  ggtitle("125Hz median Sound Level")+
  #labs(subtitle = "Data source: Jan 2019 (HI,FK); Apr 2019 (CI); Nov 2019 (SB,OC,MB)")+
  scale_y_continuous(position = "right")+
  theme_minimal()+
  theme(axis.text.x   = element_text(angle = 45), 
        plot.subtitle = element_text(color = "black", size = 14, face = "italic") )

ggplot(outputKeep, aes(x=Site, y=OL_median_125 )) + 
  geom_point(size = 5) +
  geom_errorbar(aes(ymin=OL_median_125-OL_sd_125, ymax=OL_median_125+OL_sd_125), width=.2,
                position=position_dodge(.9)) +
  ggtitle("125Hz median Sound Level")+
  #labs(subtitle = "Data source: Jan 2019 (HI,FK); Apr 2019 (CI); Nov 2019 (SB,OC,MB)")+
  scale_y_continuous(position = "right")+
  theme_minimal()+
  theme(axis.text.x   = element_text(angle = 45), 
        plot.subtitle = element_text(color = "black", size = 14, face = "italic") )
#NOTE: export and overlay in illustrator with stacked bars for AIS size

#-----------------------------------------------------------------------------------------
#Size pies for specific sites- pie charts, tile plots (not used)
#-----------------------------------------------------------------------------------------
output4m = reshape2 :: melt(outputKeep, id.vars = "Site", measure.vars = c("LOA_S_UV_sum","LOA_M_UV_sum","LOA_L_UV_sum" ))
pdata = output4m[output4m$Site == "SB03",]
pdata$percent2 = pdata$value/ sum(pdata$value) *100
sum(pdata$percent2)
pdata$value/3 + c(0, cumsum(pdata$value)[-length(pdata$value)] )
percent( pdata$percent2)
p1 = ggplot(pdata, aes(x="", y=value, fill=variable))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  theme_minimal()+
  xlab("") + ylab("")+
  blank_theme +
  scale_fill_viridis(discrete = T, name = "Vessel Size", labels = c("Small (<20m)", "Medium (20-100m)", "Large (>100m)")) +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank()) +
  ggtitle(paste0("SB03- November 2019 (",as.character(sum(pdata$value))," vessels)" ) )


pdata = output4m[output4m$Site == "GR01",]
pdata$percent2 = pdata$value/ sum(pdata$value) *100
sum(pdata$percent2)
pdata$value/3 + c(0, cumsum(pdata$value)[-length(pdata$value)] )
percent( pdata$percent2)
p2 = ggplot(pdata, aes(x="", y=value, fill=variable))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  theme_minimal()+
  xlab("") + ylab("")+
  blank_theme +
  scale_fill_viridis(discrete = T, name = "Vessel Size", labels = c("Small (<20m)", "Medium (20-100m)", "Large (>100m)")) +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank()) +
  ggtitle(paste0("GR01- November 2019 (",as.character(sum(pdata$value))," vessels)" ) )

pdata = output4m[output4m$Site == "FK03",]
pdata$percent2 = pdata$value/ sum(pdata$value) *100
sum(pdata$percent2)
pdata$value/3 + c(0, cumsum(pdata$value)[-length(pdata$value)] )
percent( pdata$percent2)
p3 = ggplot(pdata, aes(x="", y=value, fill=variable))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  theme_minimal()+
  xlab("") + ylab("")+
  blank_theme +
  scale_fill_viridis(discrete = T, name = "Vessel Size", labels = c("Small (<20m)", "Medium (20-100m)", "Large (>100m)")) +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank()) +
  ggtitle(paste0("FK03- January 2019 (",as.character(sum(pdata$value))," vessels)" ) )
grid.arrange(p1,p2,p3)

# tile plots (not used) 
ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(ALL_UV_sum)) ) ) +
  geom_tile()+
  xlab("Month in 2019")+ 
  scale_fill_gradient2(low="white", high="blue")+
  theme( legend.title = element_blank() ) +
  ggtitle("AIS- total vessels")

ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(LOA_PropL)) ) ) +  
  geom_tile()+
  xlab("Month in 2019")+ 
  scale_fill_gradient2(low="white", high="black")+
  theme( legend.title = element_blank() ) +
  ggtitle("AIS- Proportion large vessels")

#-----------------------------------------------------------------------------------------
## Vessel noise presence- export to make gauge charts in google sheets, bar charts with error
#  PropVessel_daily_mean == because an average of all days, not quite add up to 100%
#  output4$PercentVessel_daily_mean + output4$PercentQuiet_daily_mean
#-----------------------------------------------------------------------------------------
keep = as.data.frame ( rbind(
  c("SB03",03),c("FK03",03), c("MB02",03), c("PHRB",01) ) )
colnames(keep) = c("site","mth") 
keep$mth =  as.numeric( as.character(keep$mth) )
keep$site = as.character(keep$site) 
outputKeep = NULL
for (kk in 1:nrow(keep)){
  tmp = output4[ output4$Site == keep[kk,1] & output4$Mth == keep[kk,2],] 
  outputKeep = rbind(outputKeep,tmp)
}
colnames(outputKeep) = colnames(output4)
outputKeep$Site = as.character(outputKeep$Site )
outputKeep$PropVessel_daily_mean
outputKeep$PrpHRSVESS *100

#all sites-- bar chart
keep = as.data.frame ( rbind(
  c("SB01",11), c("SB02",11), c("SB03",11),
  c("PHRB",1) , c("HI03",1),  c("HI01",1),
  c("OC04",11), c("OC03",11), c("OC02",11), c("OC01",11),
  c("MB03",01), c("MB02",11), c("MB01",11),
  c("GR03",11), c("GR02",11), c("GR01",11),
  c("FK04",1) , c("FK03",1),  c("FK02",1) , c("FK01",1),
  c("CI04",11), c("CI03",11), c("CI02",11)) )
colnames(keep) = c("site","mth") 
keep$mth =  as.numeric( as.character(keep$mth) )
keep$site = as.character(keep$site) 
outputKeep = NULL
for (kk in 1:nrow(keep)){
  tmp = output4[ output4$Site == keep[kk,1] & output4$Mth == keep[kk,2],] 
  outputKeep = rbind(outputKeep,tmp)
}
colnames(outputKeep) = colnames(output4)
outputKeep$Site = as.character(outputKeep$Site ) 
#order site by percent of day
tmp = outputKeep %>% arrange(PropVessel_daily_mean)
outputKeep$Site = factor(outputKeep$Site, levels = tmp$Site, ordered=T)

#decide which variables to keep
# % of day, monthly average-- can I add error bars-- using the daily mean
output4m = reshape2 :: melt(outputKeep, id.vars = "Site", measure.vars = c("PropVessel_daily_mean" ))
output4m$SD = outputKeep$PropVessel_daily_SD

ggplot(output4m, aes(fill=variable, y=value, x=Site)) + 
  geom_bar( stat="identity", color="black", width = .7)+
  geom_errorbar(aes(ymin=value, ymax=value+SD), width=.2,
                position=position_dodge(.9)) +
  coord_flip() +
  labs(y = "Percent of day dominated by vessel noise", subtitle="Data source: November 2019, except January for PHRB and HI") + 
  xlab("") +
  scale_fill_viridis(discrete = T, name = "% day") +
  ggtitle("Vessel Noise Presence") +
  theme_classic() +
  theme(legend.position = "none",plot.subtitle = element_text(color = "dark gray", size = 10, face = "italic") )

#USE IN OSM PPT--- across all site months
#PrpHRSVESS*100  PropVessel_daily_mean
ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(PrpHRSVESS*100 )) ) ) +
  geom_tile()+
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred")+
  xlab("2019")+ 
  theme( legend.title = element_blank() ) #+
#ggtitle("Noise added when vessel noise dominant")

# Prop of hours with vessels present, over month (USE THIS!)
tmp = outputKeep %>% arrange(PrpHrsQUIET)
outputKeep$Site = factor(outputKeep$Site, levels = tmp$Site, ordered=T)
output4m = reshape2 :: melt(outputKeep, id.vars = "Site", measure.vars = c("PrpHRSVESS"))
ggplot(output4m, aes(fill=variable, y=value, x=Site)) + 
  geom_bar( stat="identity", width = .7)+
  ggtitle("Monthly Vessel Noise Presence") +
  coord_flip() +
  ylim(c(0,1))+
  labs(y = "Proportion of Hours", caption="November 2019 for all sites, except January 2019 for PHRB and HI") + xlab("") +
  theme(legend.position = "none", panel.background = element_rect(fill = "transparent"), 
        plot.background  = element_rect(fill = "transparent", color = NA) ,
        panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),)

# OTHER ALL DATA-- tile plots (not used)
ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(PropVessel_daily_mean)) ) ) +
  geom_tile()+
  xlab("Month in 2019")+ 
  scale_fill_gradient2(low="white", high="black")+
  theme( legend.title = element_blank() ) +
  ggtitle("% of hour with vessel noise")

ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(PrpHRSVESS)) ) ) +
  geom_tile()+
  xlab("Month in 2019")+ 
  scale_fill_gradient2(low="white", high="blue")+
  theme( legend.title = element_blank() ) +
  ggtitle("Proportion of hours with vessel noise")

#-----------------------------------------------------------------------------------------
#MULTIPLE DIMENSIONS
#AIS =  vessel composition
#VESS= prop hours with vessels in month
#PROP= prop radials >10 km OR ship lane within 10 km
#SPL = noise added
#individual site graphic- series of three graphics (SPL, AIS, VESS)

# Multiple metrics: no ship lane site-- FK03
#----------------------------------------------------------------------------------------------------
#sound level- bar
capt = "Florida Keys (03) January 2019"
tmp = output4[ output4$Site == "FK03" & output4$Mth == 1,] 
tmpm = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("YVess125_med","NVess125_med" ))
tmpsd = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("YVess125_sd","NVess125_sd" ))
tmpm = cbind(tmpm,tmpsd$value)
colnames(tmpm)[4] = "sd"
tmpm$variable2 = c("vessel","no vessel")
p1= ggplot(tmpm, aes(x =variable2, y=value, fill = variable2) )  + 
  geom_bar(stat="identity", width = .7) +
  geom_text(aes(label=round(value)), vjust=1.6, color="black", size=5)+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) +
  coord_cartesian(ylim=c(70,100))+
  scale_fill_brewer(palette="Paired") +
  scale_x_discrete(limits=c("vessel", "no vessel")) +
  labs(title="Sound Level Metric: hours with and without vessel noise detected", 
       x="", y = "Median Sound Level in 125 Hz octave band", caption = capt) +
  theme(legend.position = "none", panel.background = element_rect(fill = "transparent"), 
        plot.background  = element_rect(fill = "transparent", color = NA) ,
        panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),)

#vessel presence- donuts
tmpm = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("PrpHRSVESS","PrpHrsQUIET" ))
tmpm$variable2 = c("vessel","no vessel")
tmpm$fraction = tmpm$value / sum(tmpm$value)
tmpm$ymax = cumsum(tmpm$fraction)
tmpm$ymin = c(0, head(tmpm$ymax, n=-1))
tmpm$labelPosition <- (tmpm$ymax + tmpm$ymin) / 2
tmpm$label <- paste0(tmpm$variable2, "\n ", round(tmpm$value *100) ,"%")

p2 = ggplot(tmpm, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=variable2)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  scale_fill_brewer(palette="Paired") +
  scale_color_brewer(palette=2) +
  xlim(c(2, 4)) +
  theme_void() +
  labs(caption = capt)+
  theme(legend.position = "none")+
  ggtitle("Vessel Noise Metric: % of hours with vessel detections")

tmpm = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("LOA_S_UV_sum","LOA_M_UV_sum" ,"LOA_L_UV_sum"))
tmpm$Vessel = c("small","medium","large")
tmpm$Label[1]= round( (tmpm$value[1]/ sum(tmpm$value) )*100) 
tmpm$Label[2]= round( (tmpm$value[2]/ cumsum(tmpm$value)[3] )*100)
tmpm$Label[3]= round( (tmpm$value[3]/ cumsum(tmpm$value)[3] )*100)
p3 = ggplot(tmpm, aes(x="", y=value, fill=Vessel)) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  #scale_fill_brewer(palette="Blues")+
  blank_theme+
  labs(caption = capt)+
  ggtitle("Vessel Traffic  Metric: composition of AIS vessels") +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = paste0(Label,"%") ), size=5)
grid.arrange(p2,p3,p1,nrow=1)


# Multiple metrics: ship lane site-- SB03
#-------------------------------------------------------------------------------------------------
capt = "Stellwagon (03) January 2019"
tmp = output4[ output4$Site == "SB03" & output4$Mth ==1,] 
tmpm = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("YVess125_med","NVess125_med" ))
tmpsd = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("YVess125_sd","NVess125_sd" ))
tmpm = cbind(tmpm,tmpsd$value)
colnames(tmpm)[4] = "sd"
tmpm$variable2 = c("vessel","no vessel")
p1= ggplot(tmpm, aes(x =variable2, y=value, fill = variable2) )  + 
  geom_bar(stat="identity", width = .7) +
  geom_text(aes(label=round(value)), vjust=1.6, color="black", size=5)+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) +
  coord_cartesian(ylim=c(70,100))+
  scale_fill_brewer(palette="Paired") +
  scale_x_discrete(limits=c("vessel", "no vessel")) +
  labs(title="Sound Level Metric: hours with and without vessel noise detected", 
       x="", y = "Median Sound Level in 125 Hz octave band", caption = capt) +
  theme(legend.position = "none", panel.background = element_rect(fill = "transparent"), 
        plot.background  = element_rect(fill = "transparent", color = NA) ,
        panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),)

#vessel presence- donuts
tmpm = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("PrpHRSVESS","PrpHrsQUIET" ))
tmpm$variable2 = c("vessel","no vessel")
tmpm$fraction = tmpm$value / sum(tmpm$value)
tmpm$ymax = cumsum(tmpm$fraction)
tmpm$ymin = c(0, head(tmpm$ymax, n=-1))
tmpm$labelPosition <- (tmpm$ymax + tmpm$ymin) / 2
tmpm$label <- paste0(tmpm$variable2, "\n ", round(tmpm$value *100) ,"%")

p2 = ggplot(tmpm, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=variable2)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  scale_fill_brewer(palette="Paired") +
  scale_color_brewer(palette=2) +
  xlim(c(2, 4)) +
  theme_void() +
  labs(caption = capt)+
  theme(legend.position = "none")+
  ggtitle("Vessel Noise Metric: % of hours with vessel detections")

tmpm = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("LOA_S_UV_sum","LOA_M_UV_sum" ,"LOA_L_UV_sum"))
tmpm$Vessel = c("small","medium","large")
tmpm$Label[1]= round( (tmpm$value[1]/ cumsum(tmpm$value)[3] )*100) 
tmpm$Label[2]= round( (tmpm$value[2]/ cumsum(tmpm$value)[3] )*100)
tmpm$Label[3]= round( (tmpm$value[3]/ cumsum(tmpm$value)[3] )*100)
p3 = ggplot(tmpm, aes(x="", y=value, fill=Vessel)) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  #scale_fill_brewer(palette="Blues")+
  blank_theme+
  labs(caption = capt)+
  ggtitle("Vessel Traffic  Metric: composition of AIS vessels") +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = paste0(Label,"%") ), size=5)
grid.arrange(p1,p2,p3,nrow=1)

# Multiple metrics: CI01
#-------------------------------------------------------------------------------------------------
capt = "Channel Islands (01) April 2019"
tmp$LOA_ALL_OPHRS_mean
tmp$LOA_ALL_OPHRS_SD
tmp$LOA_ALL_UV_mean
tmp$LOA_ALL_UV_SD

tmp = output4[ output4$Site == "CI01" & output4$Mth ==4,] 
tmpm = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("YVess125_med","NVess125_med" ))
tmpsd = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("YVess125_sd","NVess125_sd" ))
tmpm = cbind(tmpm,tmpsd$value)
colnames(tmpm)[4] = "sd"
tmpm$variable2 = c("vessel","no vessel")
p1= ggplot(tmpm, aes(x =variable2, y=value, fill = variable2) )  + 
  geom_bar(stat="identity", width = .7) +
  geom_text(aes(label=round(value)), vjust=1.6, color="black", size=5)+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) +
  coord_cartesian(ylim=c(70,100))+
  scale_fill_brewer(palette="Paired") +
  scale_x_discrete(limits=c("vessel", "no vessel")) +
  labs(title="Sound Level Metric: hours with and without vessel noise detected", 
       x="", y = "Median Sound Level in 125 Hz octave band", caption = capt) +
  theme(legend.position = "none", panel.background = element_rect(fill = "transparent"), 
        plot.background  = element_rect(fill = "transparent", color = NA) ,
        panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),)

#vessel presence- donuts
tmpm = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("PrpHRSVESS","PrpHrsQUIET" ))
tmpm$variable2 = c("vessel","no vessel")
tmpm$fraction = tmpm$value / sum(tmpm$value)
tmpm$ymax = cumsum(tmpm$fraction)
tmpm$ymin = c(0, head(tmpm$ymax, n=-1))
tmpm$labelPosition <- (tmpm$ymax + tmpm$ymin) / 2
tmpm$label <- paste0(tmpm$variable2, "\n ", round(tmpm$value *100) ,"%")

p2 = ggplot(tmpm, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=variable2)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  scale_fill_brewer(palette="Paired") +
  scale_color_brewer(palette=2) +
  xlim(c(2, 4)) +
  theme_void() +
  labs(caption = capt)+
  theme(legend.position = "none")+
  ggtitle("Vessel Noise Metric: % of hours with vessel detections")

tmpm = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("LOA_S_UV_sum","LOA_M_UV_sum" ,"LOA_L_UV_sum"))
tmpm$Vessel = c("small","medium","large")
tmpm$Label[1]= round( (tmpm$value[1]/ cumsum(tmpm$value)[3] )*100) 
tmpm$Label[2]= round( (tmpm$value[2]/ cumsum(tmpm$value)[3] )*100)
tmpm$Label[3]= round( (tmpm$value[3]/ cumsum(tmpm$value)[3] )*100)
p3 = ggplot(tmpm, aes(x="", y=value, fill=Vessel)) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  #scale_fill_brewer(palette="Blues")+
  blank_theme+
  labs(caption = capt)+
  ggtitle("Vessel Traffic  Metric: composition of AIS vessels") +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = paste0(Label,"%") ), size=5)
grid.arrange(p1,p2,p3,nrow=1)


# Multiple metrics: GR01-- no vessels
#----------------------------------------------------------------------------------------------------
capt = "Gray's Reef (site 01), May 2019"
tmp = output4[ output4$Site == "GR01" & output4$Mth ==5,] 
tmpm = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("YVess125_med","NVess125_med" ))
tmpsd = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("YVess125_sd","NVess125_sd" ))
tmpm = cbind(tmpm,tmpsd$value)
colnames(tmpm)[4] = "sd"
tmpm$variable2 = c("vessel","no vessel")
p1= ggplot(tmpm, aes(x =variable2, y=value, fill = variable2) )  + 
  geom_bar(stat="identity", width = .7) +
  geom_text(aes(label=round(value)), vjust=1.6, color="black", size=5)+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) +
  coord_cartesian(ylim=c(70,100))+
  scale_fill_brewer(palette="Paired") +
  scale_x_discrete(limits=c("vessel", "no vessel")) +
  labs(title="Sound Level Metric: hours with and without vessel noise detected", 
       x="", y = "Median Sound Level in 125 Hz octave band", caption = capt) +
  theme(legend.position = "none", panel.background = element_rect(fill = "transparent"), 
        plot.background  = element_rect(fill = "transparent", color = NA) ,
        panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),)

#vessel presence- donuts
tmpm = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("PrpHRSVESS","PrpHrsQUIET" ))
tmpm$variable2 = c("vessel","no vessel")
tmpm$fraction = tmpm$value / sum(tmpm$value)
tmpm$ymax = cumsum(tmpm$fraction)
tmpm$ymin = c(0, head(tmpm$ymax, n=-1))
tmpm$labelPosition <- (tmpm$ymax + tmpm$ymin) / 2
tmpm$label <- paste0(tmpm$variable2, "\n ", round(tmpm$value *100) ,"%")

p2 = ggplot(tmpm, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=variable2)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  scale_fill_brewer(palette="Paired") +
  scale_color_brewer(palette=2) +
  xlim(c(2, 4)) +
  theme_void() +
  labs(caption = capt)+
  theme(legend.position = "none")+
  ggtitle("Vessel Noise Metric: % of hours with vessel detections")

tmpm = reshape2 :: melt(tmp, id.vars = "Site", measure.vars = c("LOA_S_UV_sum","LOA_M_UV_sum" ,"LOA_L_UV_sum"))
tmpm$Vessel = c("small","medium","large")
tmpm$Label[1]= round( (tmpm$value[1]/ cumsum(tmpm$value)[3] )*100) 
tmpm$Label[2]= round( (tmpm$value[2]/ cumsum(tmpm$value)[3] )*100)
tmpm$Label[3]= round( (tmpm$value[3]/ cumsum(tmpm$value)[3] )*100)
p3 = ggplot(tmpm, aes(x="", y=value, fill=Vessel)) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  #scale_fill_brewer(palette="Blues")+
  blank_theme+
  labs(caption = capt)+
  ggtitle("Vessel Traffic  Metric: composition of AIS vessels") +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = paste0(Label,"%") ), size=5)
grid.arrange(p1,p2,p3,nrow=1)


## Multiple metrics all sites-  bubble chart
#----------------------------------------------------------------------------
#Y = VESS, X = SPL, Size = AIS, color = PROP
#truncate to data of interest AND add shipping lane to the sites info
#----------------------------------------------------------------------------
#c("OC01",4,"N"),c("OC04",4,"N"),
keep = as.data.frame ( rbind(
  c("SB01",3,"Y"), c("SB02",3,"Y"),  c("SB03",3,"Y"),
  c("HI03",1,"N"), c("HI01",1,"N"),
  c("OC02",4,"Y"), 
  c("MB01",3,"N"), c("MB02",3,"N"),  c("MB03",3,"Y"),
  c("GR01",5,"N"), c("GR02",5,"N"),  c("GR03",5,"N"),
  c("FK01",3,"N"), c("FK02",3,"N"),  c("FK03",3,"N"),  c("FK04",3,"N"),
  c("CI05",4,"Y"), c("CI01",4,"N"),  c("CI02",6,"N"),  c("CI04",4,"N") ) )
colnames(keep) = c("Site","Mth","ShipLane") 
keep$Mth =  as.numeric( as.character(keep$Mth) )
keep$Site = as.character(keep$Site) 
keep$ShipLane = as.character(keep$ShipLane)

outputKeep = NULL
for (kk in 1:nrow(keep)){
  tmp = output4[ output4$Site == keep[kk,1] & output4$Mth == keep[kk,2],] 
  tmp$ShipLane = keep[kk,3]
  outputKeep = rbind(outputKeep, tmp )
}
colnames(outputKeep) = c( colnames(output4) , "ShipLane")
outputKeep$Site = as.character(outputKeep$Site) 
as.data.frame(colnames(output4))
#order site by percent of day
tmp = outputKeep %>% arrange(PropVessel_daily_mean)
outputKeep$Site = factor(outputKeep$Site, levels = tmp$Site, ordered=T)
outputKeep$PropL = (outputKeep$LOA_L_UV_sum/ (outputKeep$LOA_S_UV_sum +outputKeep$LOA_M_UV_sum +outputKeep$LOA_L_UV_sum))*100
colnames(outputKeep)[74] = "AIS traffic >100 m"
as.data.frame(colnames (outputKeep) )


ggplot(outputKeep, aes(y = PrpHRSVESS*100, x = NoiseAdd, color = ShipLane, label= Site) ) +
  geom_point(aes(size = PropL,) ) + #ALL_UV_sum-- not very informative
  scale_size(range = c(.1, 24), name="Proportion of Traffic large (>100m)") +
  scale_color_manual(values=c("#999999", "#E69F00") ) +
  ylim(c(0,100)) +
  xlab("") + ylab("") +
  labs(title = "", 
       subtitle = "",
       caption = "")+
  geom_text(aes(label=Site), hjust=2, vjust=0, size = 3)+
  theme_minimal(base_size = 18) +
  theme( plot.title=element_text(size=18, face="bold"), 
         legend.position = "none",
         plot.subtitle  = element_text(color = "dark gray", size = 10, face = "italic"),
         plot.caption  = element_text(color = "dark gray",  size = 10, face = "italic") )

#only acoustic variables in plot-- 
ggplot(outputKeep, aes(y = PrpHRSVESS*100, x = NoiseAdd, color = ShipLane, label= Site) ) +
  geom_point(aes(size =TotalVesselDet_cnt_mean,) ) + 
  # Size is an estimate of the number of vessel detections
  #TotalVesselDet_cnt_mean-- 
  #VesselDet_meanHourly_cnt_mean
  scale_size(range = c(.1, 24), name="Proportion of Traffic large (>100m)") +
  scale_color_manual(values=c("#999999", "#E69F00") ) +
  ylim(c(0,100)) +
  xlab("") + ylab("") +
  labs(title = "", 
       subtitle = "",
       caption = "")+
  geom_text(aes(label=Site), hjust=2, vjust=0, size = 3)+
  theme_minimal(base_size = 18) +
  theme( plot.title=element_text(size=18, face="bold"), 
         legend.position = "none",
         plot.subtitle  = element_text(color = "dark gray", size = 10, face = "italic"),
         plot.caption  = element_text(color = "dark gray",  size = 10, face = "italic") )


# AMOUNT VESSEL TRAFFIC: AIS detections vs Vessel detections
#IN PPT
as.data.frame(colnames(output4))
outputKeep$Sant = substr(outputKeep$Site,1,2)
ggplot(outputKeep, aes(x = TotalVesselDet_cnt_mean, y = LOA_ALL_UV_mean) ) +
  geom_errorbar (aes(ymin=LOA_ALL_UV_mean- LOA_ALL_UV_SD/sqrt(Days), 
                     ymax=LOA_ALL_UV_mean+ LOA_ALL_UV_SD/sqrt(Days)), 
                 colour="gray", width=0) +
  geom_errorbarh(aes(xmin=TotalVesselDet_cnt_mean- TotalVesselDet_cnt_SD/sqrt(Days), 
                     xmax=TotalVesselDet_cnt_mean+ TotalVesselDet_cnt_SD/sqrt(Days)), 
                 colour="gray", width=0) +
  ylim(0,25)+   xlim(0,25)+
  geom_point(aes(size = LOA_ALL_OPHRS_mean, color = Sant) ) +
  #geom_smooth(method = "lm",formula = y ~ x, size = 1, se = FALSE, color = "black")+
  geom_text(aes(label=Site), hjust=1.2, vjust=0, size = 3)+
  xlab("LISTEN: mean daily count of detections") + ylab("AIS: mean daily unique vessel count") +
  labs(size = "AIS operational hours in day")+
  theme_minimal(base_size = 18)+
  theme( plot.title=element_text(size=18, face="bold"), 
         legend.position = "bottom")
outputKeep$LOA_ALL_UV_SD/sqrt(outputKeep$Days)
#SIZE: PropVessel_daily_mean OR LOA_ALL_OPHRS_mean
#labs(size = "% of day with vessel noise")

# Duration of VESSEL TRAFFIC: AIS OPHRS (daily mean) vs Vessel duration (percent of day)
#NOTE USED!
as.data.frame(colnames(output4))
output4$PropVessel_daily_mean #% of day
outputKeep$LOA_ALL_OPHRS_meanPer = (outputKeep$LOA_ALL_OPHRS_mean/24)*100


ggplot(outputKeep, aes(x = PropVessel_daily_mean, y = LOA_ALL_OPHRS_meanPer) ) +
  #geom_errorbar (aes(ymin=LOA_ALL_OPHRS_mean- LOA_ALL_OPHRS_SD/sqrt(Days), ymax=LOA_ALL_OPHRS_mean+ LOA_ALL_OPHRS_SD/sqrt(Days)), colour="gray", width=.1) +
  geom_errorbarh(aes(xmin=PropVessel_daily_mean- PropVessel_daily_SD/sqrt(Days), xmax=PropVessel_daily_mean+ PropVessel_daily_SD/sqrt(Days)), colour="gray", width=.1) +
  geom_point(aes(size = PropVessel_daily_mean) ) +
  geom_smooth(method = "lm",formula = y ~ x, size = 1, se = FALSE, color = "black")+
  geom_text(aes(label=Site), hjust=1.2, vjust=0, size = 3)+
  xlab("Listen: mean % day") + ylab("AIS: mean % day") +
  labs(size = "% of day with vessel noise")+
  theme_minimal(base_size = 18)+
  theme( plot.title=element_text(size=18, face="bold"), 
         legend.position = "bottom",
         plot.subtitle  = element_text(color = "dark gray", size = 10, face = "italic"),
         plot.caption  = element_text(color = "dark gray",  size = 10, face = "italic") )


#-----------------------------------------------------------------------------------------
#LISTENING RANGE-- CI sites only
#-----------------------------------------------------------------------------------------
OUT = readxl::read_xlsx("G:\\My Drive\\ActiveProjects\\SANCTSOUND\\LR_125Hz_10dB\\CI_125Hz_LR_SNR_10dB.xlsx")
#listening ranges for MARCH at sites in Channel Islands
usite = unique(OUT$Site)
OUTsum = NULL
for (ss in 1:length(usite)){
  
  tmp = OUT[OUT$Site == usite[ss] & OUT$Month == 10,]
  #summary(tmp)
  avg = mean(tmp$`Listening Range, km`) 
  std = sd(tmp$`Listening Range, km` )
  
  zer = sum(tmp$`Listening Range, km` == 0 )/nrow(tmp)
  greater10km = sum(tmp$`Listening Range, km` >10000 )/nrow(tmp)
  greater10km0 = sum(tmp$`Listening Range, km` >10000 )/ sum(tmp$`Listening Range, km` != 0 )
  
  OUTsum = rbind(OUTsum, c(usite[ss],round(avg), round(std), round(zer,2), round(greater10km, 2),round(greater10km0, 2)  ) )
}
colnames( OUTsum ) = c("SiteSum","avgRange_km","sdRange_km", "Percent0","Percent>10km","Percent>10km0")
OUTsum = as.data.frame( OUTsum )
OUTsum$Location = c("Santa Rosa (N)","San Miguel (W)","SanNic (E)","Santa Rosa (S)", "Anacapa (E)")

keep = as.data.frame ( rbind(
  c("CI01",6), c("CI02",6) ,c("CI03",6), c("CI04",6),c("CI05",6)) )
colnames(keep) = c("site","mth") 
#NOTE: CI01 might be corrupt??
keep$mth =  as.numeric( as.character(keep$mth) )
keep$site = as.character(keep$site) 
outputKeep = NULL
for (kk in 1:nrow(keep)){
  tmp = output4[ output4$Site == keep[kk,1] & output4$Mth == keep[kk,2],] 
  outputKeep = rbind(outputKeep,tmp)
}
colnames(outputKeep) = colnames(output4)
outputKeep$Site = as.character(outputKeep$Site ) 

#order site by percent of day
tmp = outputKeep %>% arrange(PropVessel_daily_mean)
outputKeep$Site2 = factor(outputKeep$Site, levels = tmp$Site, ordered=T)
outputKeep$Site

#add listening range values
OUTsum2 = OUTsum[OUTsum$Site !="CI03",]
outputKeep = cbind(outputKeep,OUTsum2[1:7])
outputKeep$Site
outputKeep$SiteSum

as.data.frame(colnames(outputKeep))
outputKeep$`Percent>10km` = as.numeric(as.character(outputKeep$`Percent>10km`))*100
colnames(outputKeep)[73] = "AIS traffic >100 m"
colnames(outputKeep)[74] = "Total unique AIS vessels"
outputKeep$Location
outputKeep$NoVESS_OL_median_125[4] = outputKeep$OL_median_125[4]
round(outputKeep$OL_median_125 - outputKeep$NoVESS_OL_median_125)
round(outputKeep$OL_median_125)
ggplot(outputKeep, aes(y =`Percent>10km`, x = NoVESS_OL_median_125, label= Location) ) +
  geom_point(aes(size = `Total unique AIS vessels`) ) +
  #scale_color_manual(values=c("#999999", "#E69F00") ) +
  labs(caption = "Summer 2019") +
  xlab("Median Sound Level \n 125 Hz OTB (no vessel periods)") + ylab("Percent of listening ranges >10 km") +
  geom_text(aes(label=Location),hjust=.46, vjust=-1.6, size = 3)+
  theme_minimal(base_size = 12)

cbind(outputKeep$Site, outputKeep$`Percent>10km`, outputKeep$NoVESS_OL_median_125, outputKeep$OL_median_125,  outputKeep$VESS_OL_median_125, outputKeep$`Total unique AIS vessels`)

#compare CI02 (north) with CI01 (SB channel) with
#percent difference in AIS traffic- less traffic
(outputKeep$`Total unique AIS vessels`[2] - outputKeep$`Total unique AIS vessels`[1] )/outputKeep$`Total unique AIS vessels`[1]
#difference sound level- higher sound levels
(outputKeep$NoVESS_OL_median_125[2] - outputKeep$NoVESS_OL_median_125[1] )
#difference ranges > 10-- more ranges out side 10 km
(outputKeep$`Percent>10km`[2] - outputKeep$`Percent>10km`[1] )