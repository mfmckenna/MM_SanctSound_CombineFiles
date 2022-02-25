# Animated graphic of COVID change

#reads in output from: 1b_process_VesselDetectionsAIS.R

rm(list=ls())

library(ggplot2)
library(viridis)
library(dplyr)
library(scales)
library(gridExtra)
library(gganimate)

dirW = ("E:\\RESEARCH\\SanctSound\\data2\\combineFiles1_SPLShips\\")

#---------------------------------------------
#2019 data
output4 = read.csv(paste0(dirW, "Summary 2019Month_ver2022-01-21.csv") )
output4$NoiseAdd = output4$YVess125_med - output4$NVess125_med
output4$PropL   = (output4$LOA_L_UV_sum/ (output4$LOA_S_UV_sum +output4$LOA_M_UV_sum + output4$LOA_L_UV_sum) ) *100

#2020 data
out2020in = read.csv(paste0(dirW, "Summary 2020Month_ver2022-02-06.csv") )
out2020in$NoiseAdd = out2020in$YVess125_med - out2020in$NVess125_med
out2020in$PropL   = (out2020in$LOA_L_UV_sum/ (out2020in$LOA_S_UV_sum +out2020in$LOA_M_UV_sum + out2020in$LOA_L_UV_sum) ) *100

#CHECK: which sites have April data
#---------------------------------------------
t1 = ( unique(out2020in$Site[out2020in$Mth == 4] ) )
t2 = ( unique(output4$Site[output4$Mth == 4] ) )
t1[ t1 %in% t2 ]

#selection criteria
#---------------------------------------------
keep = as.data.frame ( rbind(
  c("CI01",4,"N"),c("CI05",4,"N"), 
  c("FK01",4,"N"), c("FK02",4,"N"),
  c("GR01",4,"N"), 
  c("MB02",4,"N"),c("MB01",4,"N"),
  c("SB01",4,"Y"), c("SB02",4,"Y"),  c("SB03",4,"Y") ))
colnames(keep) = c("Site","Mth","ShipLane") 
keep$Mth =  as.numeric( as.character(keep$Mth) )
keep$Site = as.character(keep$Site) 
keep$ShipLane = as.character(keep$ShipLane)

#2019 truncate data
#---------------------------------------------
outputKeep = NULL
for (kk in 1:nrow(keep)){
  tmp = output4[ output4$Site == keep[kk,1] & output4$Mth == keep[kk,2],] 
  tmp$ShipLane = keep[kk,3]
  outputKeep = rbind(outputKeep, tmp )
}
colnames(outputKeep) = c( colnames(output4) , "ShipLane")
outputKeep$Site = as.character(outputKeep$Site) 


#2020 truncate data
#---------------------------------------------
outputKeep20 = NULL
for (kk in 1:nrow(keep)){
  tmp = out2020in[ out2020in$Site == keep[kk,1] & out2020in$Mth == keep[kk,2],] 
  if (nrow(tmp) > 0){ 
    tmp$ShipLane = keep[kk,3]
    outputKeep20 = rbind(outputKeep20, tmp )
  }
}
outputKeep20$Site = as.character(outputKeep20$Site)

#select certain variables
#---------------------------------------------
myvars <- c("Site", "PrpHRSVESS", "NoiseAdd", "ShipLane", "YR_mean", 'PropL',"OL_median_125")
out2020 =  outputKeep20[myvars]
out2019 =  outputKeep[myvars]
outputChange = rbind(out2020, out2019)  


## ANIMATION bubbles moving in 2020 for MB and SB sites
#https://www.r-graph-gallery.com/320-the-basis-of-bubble-plot.html 
#https://towardsdatascience.com/animating-your-data-visualizations-like-a-boss-using-r-f94ae20843e3
#---------------------------------------------
p  = ggplot(outputChange, aes(y = PrpHRSVESS*100, x = OL_median_125, color = ShipLane, label= Site) ) +
  geom_point(aes(size = PropL, alpha = .6) ) +
  scale_size(range = c(.1, 24), name="Proportion of Traffic large (>100m)") +
  scale_color_manual(values=c("#999999", "#E69F00") ) +
  xlab("Low-Frequency Sound Level (125 Hz octave band)") + ylab("Percent of hours with vessel noise") +
  #labs(title    = "",
       #subtitle = "Data source: monthly averages for Mar (SB,MB, OC), May (GR)",
       #caption =  "orange = shiplane within 10 km \n bubble size = Percent of AIS traffic large vessels (>100m)")+
  geom_text(aes(label=Site), hjust=1.5, vjust=0, size = 3) +
  xlim(c(80,100)) +
  theme_minimal(base_size = 18) +
  theme( plot.title=element_text(size=18, face="bold"), 
         legend.position = "none",
         plot.subtitle  = element_text(color = "dark gray", size = 10, face = "italic"),
         plot.caption  = element_text(color = "dark gray",  size = 10, face = "italic") )

plot(p)

anim <- p + 
  # Transition between species, spending 1s in each Species and animating for 
  # 2s between states... can use different types of transitions this is good for categorical variables
  
  transition_states(YR_mean,
                    transition_length = 2,
                    state_length = 1) +
  # Use an easing function to make the movement more natural
  ease_aes('cubic-in-out') +
  # Add a title so audience knows what they're seeing 
  ggtitle('Year = {closest_state}')

# Show in RStudio
anim


#Interpretation
#---------------------------------------------
outputChange[outputChange$Site == "MB02", ] #inshore, fishing?
#louder, more vessel presence, slightly more large
outputChange[outputChange$Site == "GR01", ] #inshore, biology?
#louder, less vessel presence, slightly less large
outputChange[outputChange$Site == "OC01", ] #inshore, tide?
#same, slightly more vessel presence,  less large
outputChange[outputChange$Site == "SB01", ] #biologics, distant source?
#quieter,less vessel presence. less large
