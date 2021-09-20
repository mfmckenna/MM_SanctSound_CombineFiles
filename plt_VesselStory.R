#PLOTS FOR VESSEL STORY MAP
# INPUT: output of 1b_processVesselDetectionsAIS.R
# OUTPUT: Graphcs for each section of story map
library(ggplot2)
library(viridis)
library(dplyr)
rm(list=ls())

#read in files
#-----------------------------------------------------------------------------------------
setwd("E:\\RESEARCH\\SanctSound\\data2\\combineFiles1_SPLShips\\")# Summary2019Month_ver2021-09-02.csv
infile = choose.files()
output4 = read.csv(infile)
as.data.frame(colnames(output4))
output4$Site

#sampling across 2019
#-----------------------------------------------------------------------------------------
ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(Days)) ) ) +
  geom_tile()+
  xlab("Month in 2019")+ 
  scale_fill_gradient2(low="white", high="blue")+
  theme( legend.title = element_blank() ) +
  ggtitle("Days sampled 2019")
#USE THIS GRAPHIC TO DECIDE HOW TO truncate data to just a month of data for analyses below


#noise
#-----------------------------------------------------------------------------------------
as.data.frame(colnames(output4))
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
#Noise added, ALL SITES, ALL MONTHS (vessel-non vessel hours)
output4$NoiseAdd = output4$YVess125_med - output4$NVess125_med

ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(NoiseAdd)) ) ) +
  geom_tile()+
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred")+
  xlab("Month in 2019")+ 
  theme( legend.title = element_blank() ) +
  ggtitle("Noise added when vessel noise dominant")

mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
hgA <- hist(output4$NVess125_med, breaks = 20, plot = FALSE) # Save first histogram data
hgB <- hist(output4$YVess125_med, breaks = 20, plot = FALSE) # Save 2nd histogram data
plot(hgA, col = c1) # Plot 1st histogram using a transparent color
plot(hgB, col = c2, add = TRUE) # Add 2nd histogram using different color

#AIS traffic
#-----------------------------------------------------------------------------------------
#(1) individual site graphics- 
#EAST COAST REGION- Traffic bubbles (total operational hours) for March 2019- SB01, GR01, FK03

#COMPARISION OF TRAFFIC COMPOSITION ACROSS ALL SITES-- Stacked Bar sorted by lowest to highest % large
#only keep specific sites and months
output4$LOA_PropL = output4$LOA_L_UV_sum/ (output4$LOA_S_UV_sum + output4$LOA_M_UV_sum + output4$LOA_L_UV_sum)
keep = as.data.frame ( rbind(
             c("SB01",11), c("SB02",11), c("SB03",11),
             c("PHRB",1) , c("HI03",1),  c("HI01",1),
             c("OC04",11), c("OC03",11), c("OC02",11), c("OC01",11),
             c("MB03",11), c("MB02",11), c("MB01",11),
             c("GR03",11), c("GR02",11), c("GR01",11),
             c("FK04",1) , c("FK03",1),  c("FK02",1) , c("FK01",1),
             c("CI05",4), c("CI04",4), c("CI02",4), c("CI01",4)) )
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

#(2) across sites graphic- 
#STACKED bars for vessel composition across sites
tmp = outputKeep %>% arrange(LOA_PropL)
outputKeep$Site = factor(outputKeep$Site, levels = tmp$Site, ordered=T)
output4m = reshape2 :: melt(outputKeep, id.vars = "Site", measure.vars = c("LOA_S_UV_mean","LOA_M_UV_mean","LOA_L_UV_mean" ))

 ggplot(output4m, aes(fill=variable, y=value, x=Site)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ ylab("")+
  scale_fill_viridis(discrete = T, name = "Vessel Size (m)", labels = c("<20 small", "20-100 medium", ">100 large")) +
  ggtitle("Composition of AIS Vessel Traffic")+
  theme_minimal()
## ? Add month to the x-axis with the site?

#(3) Other graphics- tile plots (not used)
output4$ALL_UV_sum = output4$LOA_S_UV_sum = output4$LOA_M_UV_sum + output4$LOA_L_UV_sum
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


#Vessel noise presence
#-----------------------------------------------------------------------------------------
as.data.frame(colnames(output4))
#(1) individual site graphics- horizontal bar graph or gauge
#highlight WCR (inshore sites): OC01, MB02, CI02 November % day with vessel noise
#PropVessel_daily_mean == because an average of all days... not quite add up to 100%
# output4$PercentVessel_daily_mean + output4$PercentQuiet_daily_mean
#make the gauge plot in google sheets

keep = as.data.frame ( rbind(
  c("SB01",11), c("SB02",11), c("SB03",11),
  c("PHRB",1) , c("HI03",1),  c("HI01",1),
  c("OC04",11), c("OC03",11), c("OC02",11), c("OC01",11),
  c("MB03",11), c("MB02",11), c("MB01",11),
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

#(2) across site graphic: decide which variables to keep
# % of day, monthly average
output4m = reshape2 :: melt(outputKeep, id.vars = "Site", measure.vars = c("PropVessel_daily_mean" ))
ggplot(output4m, aes(fill=variable, y=value, x=Site)) + 
  geom_bar( stat="identity", width = .7)+
  coord_flip() +
  ylim(c(0,60))+
  labs(y = "% of day (monthly average)", caption="November 2019 for all sites, except January for PHRB and HI") + xlab("")+
  scale_fill_viridis(discrete = T, name = "% day") +
  ggtitle("Vessel Noise Presence") +
  theme(legend.position = "none", panel.background = element_rect(fill = "transparent"), 
        plot.background  = element_rect(fill = "transparent", color = NA) ,
        panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(color = "grey20", size = 10, hjust = .5, vjust = .5, face = "plain"),)

# Prop of hours with vessels present, over month (USE THIS!)
tmp = outputKeep %>% arrange(PrpHrsQUIET)
outputKeep$Site = factor(outputKeep$Site, levels = tmp$Site, ordered=T)
#output4m = reshape2 :: melt(outputKeep, id.vars = "Site", measure.vars = c("PrpHRSVESS","PrpHrsQUIET"))
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

#LISTENING RANGE-- CI sites only
#-----------------------------------------------------------------------------------------
OUT = readxl::read_xlsx("G:\\My Drive\\ActiveProjects\\SANCTSOUND\\LR_125Hz\\CI_125Hz_LR_SNR_0dB.xlsx")
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

ggplot(outputKeep, aes(y =`Percent>10km`, x = NoVESS_OL_median_125, label= Location) ) +
  geom_point(aes(size = `Total unique AIS vessels`) ) +
  #scale_color_manual(values=c("#999999", "#E69F00") ) +
  labs(caption = "Summer 2019") +
  xlab("Median Sound Level \n 125 Hz OTB (no vessel periods)") + ylab("Percent of listening ranges >10 km") +
  geom_text(aes(label=Location),hjust=.46, vjust=-1.6, size = 3)+
  theme_minimal(base_size = 12)
#NEED TO FIX CI02- san miguel

#MULTIPLE DIMENSIONS
#-----------------------------------------------------------------------------------------
#AIS =  vessel composition
#VESS= prop hours with vessels in month
#PROP= prop radials >10 km OR ship lane within 10 km
#SPL = noise added

#(1) individual site graphic- series of three graphics (SPL, AIS, VESS)
as.data.frame(colnames(output4))
#no ship lane site-- FK03
#sound level- bar
tmp = output4[ output4$Site == "FK03" & output4$Mth ==1,] 
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
       x="", y = "Median Sound Level in 125 Hz octave band", caption = "Florida Keys (03) January 2019") +
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
  labs(caption = "Florida Keys (03) January 2019")+
  theme(legend.position = "none")+
  ggtitle("Vessel Noise Metric: % of hours with vessel detections")

# AIS composition- pie chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

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
  labs(caption = "Florida Keys (03) January 2019")+
  ggtitle("Vessel Traffic  Metric: composition of AIS vessels") +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = paste0(Label,"%") ), size=5)
grid.arrange(p1,p2,p3,nrow=1)

#ship lane site-- SB03
#no ship lane site-- FK03
#sound level- bar
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
       x="", y = "Median Sound Level in 125 Hz octave band", caption = "Stellwagon (03) January 2019") +
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
  labs(caption = "Stellwagon (03) January 2019")+
  theme(legend.position = "none")+
  ggtitle("Vessel Noise Metric: % of hours with vessel detections")

# AIS composition- pie chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

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
  labs(caption = "Stellwagon (03) January 2019")+
  ggtitle("Vessel Traffic  Metric: composition of AIS vessels") +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = paste0(Label,"%") ), size=5)
grid.arrange(p1,p2,p3,nrow=1)


#(2) across site comparison- bubble chart
#Y = VESS, X = SPL, Size = AIS, color = PROP
#truncate to data of interest AND add shipping lane to the sites info
keep = as.data.frame ( rbind(
  c("SB01",3,"Y"), c("SB02",3,"Y"),  c("SB03",3,"Y"),
  c("HI03",1,"N"), c("HI01",1,"N"),
  c("OC04",3,"N"), c("OC02",3,"Y"),  c("OC01",3,"N"),
  c("MB02",3,"N"), c("MB01",3,"N"),
  c("GR03",5,"N"), c("GR02",5,"N"),  c("GR01",5,"N"),
  c("FK04",3,"N"), c("FK03",3,"N"),  c("FK02",3,"N"),  c("FK01",3,"N"),
  c("CI05",4,"Y"), c("CI04",4,"N"),  c("CI01",4,"N"),  c("CI02",4,"N") ) )
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

ggplot(outputKeep, aes(y = PrpHRSVESS, x = NoiseAdd, color = ShipLane, label= Site) ) +
  geom_point(aes(size = `AIS traffic >100 m`) ) +
  scale_color_manual(values=c("#999999", "#E69F00") ) +
  labs(caption = "Month of data in 2019 (HI-Jan, SB/MB-March,CI-April, GR-May)")+
  xlab("Noise Added [125 Hz OTB]") + ylab("Proportion of Hours with Vessel Noise") +
  geom_text(aes(label=Site),hjust=1.5, vjust=0, size = 3)+
  theme_minimal(base_size = 16)
  
  