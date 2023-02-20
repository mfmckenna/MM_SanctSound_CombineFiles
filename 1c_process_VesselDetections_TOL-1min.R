# process sanctuary sound vessel detection data and AIS results

#modified 1b-- wanted sound levels only during vessel detections "noise added" 
# using TOLs at 1 minute resolution

#tested for OC02 speed reduction
rm(list=ls())

#-----------------------------------------------------------------------------------------
# MAIN functions-- by site processing
#-----------------------------------------------------------------------------------------
# Reads in both vessel detection 

#outputs 
# a per site .csv file: E:\RESEARCH\SanctSound\data2\combineFiles1_SPLShips
# summary table for average of all data per site (copied here):
# https://docs.google.com/spreadsheets/d/1dw9Vy0dXKW3Q6Ter3t19cmLf_5EJWbmL9nqJsCrNXQE/edit#gid=888829761

#-----------------------------------------------------------------------------------------
# TO ADD (future versions)
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# LOAD Libraries
#-----------------------------------------------------------------------------------------
library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(scales)
library(BBmisc)
library(zoo)

#-----------------------------------------------------------------------------------------
# SETUP parameters
#-----------------------------------------------------------------------------------------
ra = 7 #days for running average
range01 = function(x){(x-min(x))/(max(x)-min(x))} #normalize between 0-1
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
yearflag = 2019
site = "OC02"

# range of dates for output graphics
eDatePlot = '2020-12-31'
sDatePlot = '2018-11-01'

# some analysis and output flags 
flagCSV  = TRUE  #true if you want to output hourly and daily csv files per site
flagPLT  = FALSE  #true if you want to output summary plots of data
flagYear = TRUE   #true writes out a csv of yearly data to be copied into google docs

# OUTPUT details 
#!!!(FUTURE: create site loop here) !!!
# cat("Processing site...", sitesVD[ss], ":",ss, "of", length(sitesVD),"\n")
output  = NULL
tDir   = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site, "\\")
outDir = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site, "\\")
DC = Sys.Date()

#-----------------------------------------------------------------------------------------
# READ IN-- vessel detection
#-----------------------------------------------------------------------------------------
nFilesVD  = length( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVDF = ( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD = basename(inFilesVDF)
sant = sapply(strsplit(inFilesVD[1], "_"), "[[", 2)
depl = sapply(strsplit(inFilesVD, "_"), "[[", 3)
VD=NULL
for (ii in 1:(nFilesVD)) {
  tmp = read.csv(inFilesVDF[ii])
 # names(tmp)
  tmp$Sant = sant
  tmp$Dep  = depl[ii]
  colnames(tmp) = c("ISOStartTime","ISOEndTime","Label","Sant","Depl" )
  
  VD = rbind(VD,tmp)
}
VD$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD$ISOStartTime)), tz = "GMT" )
VD$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD$ISOEndTime)), tz = "GMT" )
VD$Mth = month(VD$Start )
VD$Yr  = year(VD$Start )
VD$Dur = as.numeric(as.character( difftime(VD$End, VD$Start, units = "secs" )) )

VD$Dep  = as.numeric(as.character(VD$Dep))
VD$DepC = c(0,diff(VD$Dep))
indx = which(VD$DepC == 1) #find transitions in deployments
#vessel detections greater than 12 hour!!!
VD[VD$Dur > 3600*12,]
VD$DurH = VD$Dur/3600 

#-----------------------------------------------------------------------------------------
# VESSEL DETECTION DURATIONS- summary
#-----------------------------------------------------------------------------------------
VDmean = aggregate(VD$Dur,    by=list(VD$Mth,VD$Yr), mean, na.rm=T) 
VDsd =  aggregate(VD$Dur,    by=list(VD$Mth,VD$Yr), sd, na.rm=T) 
VDagg = cbind(VDmean, VDsd$x)
ggplot(VD, aes(x=as.factor(Mth), y=(DurH), fill = as.factor(Yr), color = as.factor(Yr) )  )+
  geom_boxplot() +
  theme_minimal()
colnames(VD)

ggplot(VD, aes(DurH,  color = as.factor(Mth) ) ) + stat_ecdf(geom = "point") + 
  facet_wrap(~Yr)+
  theme_minimal()

#-----------------------------------------------------------------------------------------
# ADD time stamps between detections... faster way??
#-----------------------------------------------------------------------------------------
VDall = NULL
for (ii in 1:(nrow(VD) -1 ) )  {
  
  if (VD$DepC [ii+1] == 0) {
    tpL = "ambient"
    tpS = VD$End [ii] + 1
    tpE = VD$Start [ii+1] - 1
    tpD = as.numeric( difftime(tpE, tpS, units = "secs") )
    
    #recombine and build new matrix
    r1 =  cbind.data.frame( VD$Sant[ii], VD$Dep[ii], as.POSIXct(VD$Start[ii]), as.POSIXct(VD$End[ii]), VD$Label[ii], (VD$Dur[ii]) ) 
    colnames(r1)= c("Sanctuary", "Deployment", "Start", "End","Label", "DurS")
    
    r2 =  cbind.data.frame( VD$Sant[ii], VD$Dep[ii], tpS, tpE, tpL, tpD) 
    colnames(r2)= c("Sanctuary", "Deployment", "Start", "End","Label", "DurS")
    
    VDall =  rbind(VDall , rbind.data.frame(r1,r2) )
    
  } else {
    r1 =  cbind.data.frame( VD$Sant[ii], VD$Dep[ii], as.POSIXct(VD$Start[ii]), as.POSIXct(VD$End[ii]), VD$Label[ii], (VD$Dur[ii])) 
    colnames(r1)= c("Sanctuary", "Deployment", "Start", "End","Label", "DurS")
    VDall =  rbind(VDall , r1) 
    
  }
  
}

#-----------------------------------------------------------------------------------------
# IS there a difference in Durations of vessel vs non vessel periods--- some initial graphics
#-----------------------------------------------------------------------------------------
VDallt = VDall[VDall$DurS < (3600*6),]
ggplot(VDallt, aes(x=DurS, color = Label )  )+
  geom_histogram(fill="white", alpha=0.5, position="identity")+
  ggtitle ("Is there a difference in durations of vessel vs non vessel periods?")

ggplot(VDallt, aes(x=DurS, color = Label )  )+
  geom_boxplot()+
  ggtitle ("Is there a difference in durations of vessel vs non vessel periods?")+
  theme_minimal()

# vessel durations are very similar at this site- with longer durations of non-vessel periods
# how does this breakdown by slowdowns?
VDall$Mth = month(VDall$Start)
VDall$Yr = year(VDall$Start)
VDall$Hr = hour(VDall$Start)

#interpretation-- means not ideal
sum1 = aggregate(VDall$DurS,    by=list(VDall$Mth, VDall$Yr, VDall$Label), mean, na.rm=T) 
# expect longer duration of vessel noise in slower period- but maybe not bc lower source level/ shorter detection range
sum12019 = sum1[ sum1$Group.2 == "2019",]
ggplot(sum12019, aes(x=(Group.1), y = x/3600, color =Group.3)  )+
  geom_line()+
  geom_point()+
  theme_minimal() +
  labs(x = "", y="duration Hours")
# not sure what this means... ship durations are longer in the winter??
# convert these monthly values % of the time with/without vessel noise
VDallt = VDall[VDall$DurS < (3600*6),]
ggplot(VDallt, aes(x=as.factor(Mth), y=(DurS)/3600, fill = as.factor(Yr), color = as.factor(Yr) )  )+
  geom_boxplot() +
  labs(x="",y = "Duration-hours")+
  theme_minimal()

summary(VDall)

#-----------------------------------------------------------------------------------------
# Label vessel detections as in or out of slow down
#-----------------------------------------------------------------------------------------
slowStart = as.Date("2021-06-01")
slowEnd   = as.Date("2021-10-31")
slowStartTrim = as.Date("2021-07-01")
slowEndTrim   = as.Date("2021-08-01")
baseStart = as.Date("2019-07-12")
baseEnd   = as.Date("2019-08-01")


VDall$startDay = as.Date(VDall$Start)
min(VDall$startDay)

VDall$Period [VDall$startDay > slowStart & VDall$startDay < slowEnd] = "slowdown"
VDall$Period [VDall$startDay > baseStart & VDall$startDay < baseEnd] = "baseline"
#VDall$Period [VDall$startDay > slowStartTrim & VDall$startDay < slowEndTrim] = "slowdownTrim"

#remove all NA data
VDall2 = VDall[!is.na(VDall$Period), ]
VDall2 = VDall2[(VDall2$Label)== "ship", ]

ggplot(VDall2, aes(x=as.factor(Mth), y=(DurS)/3600, fill = as.factor(Period), color = as.factor(Period) )  )+
  geom_boxplot() +
  labs(x="",y = "Duration-hours")+
  theme_minimal()
# durations were longer during the slowdown-
ggplot(VDall2, aes((DurS)/3600,  color = Period ) ) + stat_ecdf(geom = "point") + 
  theme_minimal()

# Truncate to just July...
VDall2July = VDall2[VDall2$Mth == 7,]

# did the vessel noise perids differ in duration?
VDall2JulyVD = VDall2July[VDall2July$Label == "ship",]
mean( VDall2JulyVD$DurS[VDall2JulyVD$Period == "baseline"] ) /60
sd( VDall2JulyVD$DurS[VDall2JulyVD$Period == "baseline"] ) /60

mean( VDall2JulyVD$DurS[VDall2JulyVD$Period == "slowdown"] )/ 60
sd( VDall2JulyVD$DurS[VDall2JulyVD$Period == "slowdown"] )/ 60
# FINDING: durations were longer during the slowdown-

# did the non-vessel noise periods differ in duration?
VDall2 = VDall[!is.na(VDall$Period), ]
VDall2 = VDall2[(VDall2$Label)== "ambient", ]
VDall2July = VDall2[VDall2$Mth == 7,]

VDall2JulyVD = VDall2July[VDall2July$Label == "ambient",]
mean( VDall2JulyVD$DurS[VDall2JulyVD$Period == "baseline"] ) /60
sd( VDall2JulyVD$DurS[VDall2JulyVD$Period == "baseline"] ) /60
mean( VDall2JulyVD$DurS[VDall2JulyVD$Period == "slowdown"] )/ 60
sd( VDall2JulyVD$DurS[VDall2JulyVD$Period == "slowdown"] )/ 60
# FINDING: durations were longer during the slowdown- not sure why??

#-----------------------------------------------------------------------------------------
# READ IN-- OTB
#-----------------------------------------------------------------------------------------
nFilesPSD   = length( list.files(path=tDir, pattern = "_1min", full.names=TRUE, recursive = TRUE))
inFilesPSDF = ( list.files(path=tDir, pattern = "mean_1min", full.names=TRUE, recursive = TRUE))
inFilesPSD  = basename(inFilesPSDF)
inFilesPSD

#combine all TOLs together
TOLmin = NULL
for (ii in 1:(length(inFilesPSDF)) )  {
  tmpPSD = read.csv(inFilesPSDF[ii]) #this takes a while to load...
  TOLmin = rbind( TOLmin, tmpPSD)
}

TOLmin$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", TOLmin$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" ) 
  
# For each period- find corresponding sound levels and take the median-- takes a bit
VDall3 = VDall[!is.na(VDall$Period), ]
VDall3July = VDall3[VDall3$Mth == 7,]

output = NULL
for (ii in 1:nrow(VDall3July)){
  tmp = TOLmin[ TOLmin$DateF >= VDall3July$Start [ii] & TOLmin$DateF <= VDall3July$End [ii] ,] 
 
  tmpMax =  ( apply(tmp[,2:31],2,max) )
  tmpQuant = as.data.frame( apply(tmp[,2:31],2,quantile) )
  tmpMed = tmpQuant[3,]
  
  tmpo = cbind(tmpMax[8], tmpMed[8])
  colnames(tmpo) = c("TOL_125 max", "TOL_125 median")
  tmpo = cbind(VDall3July[ii,], tmpo)
 
   output = rbind(output,tmpo)
}

#-----------------------------------------------------------------------------------------
# Add AIS data to vessel detections- matching transit times
#-----------------------------------------------------------------------------------------
AIStran = read.csv("F:\\RESEARCH\\SanctSound\\data2\\OC02\\smp_transit_data.csv")
AIStranOC = AIStran[ AIStran$loc_id == "OC02",]

AIStranOC$Start = as.POSIXct( gsub("[+]00", "", AIStranOC$start_time_utc), tz = "GMT" ) 
AIStranOC$End = as.POSIXct( gsub("[+]00", "", AIStranOC$end_time_utc), tz = "GMT" ) 
output$AIS = 0
output$Cargo = 0
output$Tanker = 0
output$Fishing = 0
output$Other = 0
for (ii in 1: nrow(output) ){
  #just getting if an AIS vessel start of transit occured in this vessel detection period
  tmp = AIStranOC[AIStranOC$Start >= output$Start[ii] & AIStranOC$Start <= output$End[ii],] 
  if (  nrow(tmp) > 0 ){
    
    output$AIS[ii]     = nrow(tmp) #any AIS
    output$Cargo[ii]   = length( which(tmp$type == "Cargo") )
    output$Tanker[ii]  = length( which(tmp$type == "Tanker") )
    output$Fishing[ii] = length( which(tmp$type == "Fishing") )
    output$Other[ii]   = nrow(tmp) -  ( output$Cargo[ii] + output$Tanker[ii] + output$Fishing[ii] )
    
    output$mDist[ii] = min(tmp$dist_nm)
  }
}

# assign labels to time periods AND PLOT 
# AIS vessels
output$Category[output$AIS > 0] = "A. AIS vessels"
output$Category[output$AIS == 0 & output$Label == "ship"] = "B. Non-AIS vessels"
output$Category[output$AIS == 0 & output$Label == "ambient"] = "C. Ambient"

unique(output$Category)

# get difference from ambient- in previous sample
output$SNR = NA
output$SNRmax = NA
for (ii in 2:nrow(output)) {
  output$SNR[ii] = output$`TOL_125 median`[ii] - output$`TOL_125 median`[ii-1] 
  output$SNRmax[ii]= output$`TOL_125 max`[ii] - output$`TOL_125 max`[ii-1] 
  
}
output$SNRmax[output$Label == "ambient"] = NA  
output$SNR[output$Label == "ambient"] = NA
hist(output$SNR)
hist(output$SNRmax)

# compare the SNR of ships for the different periods...
ggplot(output, aes(SNR,  color = Period ) ) + 
  stat_ecdf(geom = "step") + 
  ggtitle (paste0("Comparision of vessel detection SNR (n=", nrow(output),")" )) +
  theme_minimal()
#interesting... slowdown is always greater! What is going on here???
ggplot(output, aes(SNRmax,  color = Period ) ) + 
  stat_ecdf(geom = "step") + 
  ggtitle (paste0("Comparision of vessel detection SNR (n=", nrow(output),")" )) +
  theme_minimal()

#-----------------------------------------------------------------------------------------
# Compare slowdown AIS vessel periods to baseline vessel periods
#-----------------------------------------------------------------------------------------
#remove all non AIS samples
outputShip = output[output$Label == "ship",]      #removes ambient
outputShipAIS = outputShip[ outputShip$AIS > 0, ] #removes detections without AIS

#Comparison of just AIS vessel between periods-- 
ggplot(outputShipAIS, aes(`TOL_125 max`,  color = Period ) ) + 
  stat_ecdf(geom = "step") + 
  ggtitle (paste0("Comparision of vessel detections with AIS vessels (n=", nrow(outputShipAIS),")" )) +
  theme_minimal()

#difference from non-ship periods-- remove samples with 0 detections and
#select non-vessel detection periods
outputNShip = output[output$Label == "ambient",]
#remove any non-vessle detection periods with AIS... weird these exist!
outputNShipAIS = outputNShip[ outputNShip$AIS == 0, ]
nrow(outputNShipAIS) # interesting many no ship periods with AIS within 10 km!!

ggplot(outputNShipAIS, aes(`TOL_125 max`,  color = Period ) ) + 
  stat_ecdf(geom = "step") + 
  ggtitle (paste0("Comparision of non-AIS vessel (n=", nrow(outputNShipAIS),")" )) +
  theme_minimal()


# just remove ambient samples with AIS detections to plot all three categories
# COPY THISE PLOTS TO PPT https://docs.google.com/presentation/d/1SJd4pKsQn46464IH-L5fOiqt5-WUT-11MCg4V0IRdeU/edit#slide=id.g13e85015171_0_63

idx = which( output$Label == "ambient" & output$AIS > 0) #558!!
output3 = output[-idx, ]
unique(output3$Category)

ggplot(output3, aes(x=Category, y=`TOL_125 median`, fill = as.factor(Period) )  )+
  geom_boxplot() +
  ggtitle (paste0("Comparision of sound levels (125 TOL) (n=", nrow(output3),")" )) +
  labs(x="",y = "median Sound Level (125 Hz TOL)")+
  theme_minimal()

ggplot(output3, aes(x=Category, y=`TOL_125 max`, fill = as.factor(Period) )  )+
  geom_boxplot() +
  ggtitle (paste0("Comparision of sound levels (125 TOL) (n=", nrow(output3),")" )) +
  labs(x="",y = "max Sound Level (125 Hz TOL)")+
  theme_minimal()

ggplot(output3, aes(x=Category, y=DurS/3600, fill = as.factor(Period) )  )+
  geom_boxplot() +
  ggtitle (paste0("Comparision of duration of events (125 TOL) (n=", nrow(output3),")" )) +
  labs(x="",y = "Duration of event (Hours)")+
  theme_minimal()


#-----------------------------------------------------------------------------------------
# Comparisons-- AIS vessel vs non-vessel (after removing ambient samples with AIS )
#-----------------------------------------------------------------------------------------
output2 = rbind(outputShipAIS, outputNShipAIS)
nrow(output)- nrow(output2)

#combine Label and period into same variable to plot with Labelc
output2$Condition = as.factor(  paste0( output2$Period,"-", output2$Label ) )

#is the difference from ambient less?
ggplot(output2, aes(`TOL_125 median`, fill = Condition, color = Condition )  )+
  stat_ecdf(geom = "step")+
  scale_color_manual(values=c("gray", "black","pink","red"))+
  labs(x="median Sound Level (125 Hz TOL)",y = "Exceedance Probability")+
  theme_minimal()
#is the difference from ambient less?
ggplot(output2, aes(`TOL_125 max`, fill = Condition, color = Condition )  )+
  stat_ecdf(geom = "step")+
  scale_color_manual(values=c("gray", "black","pink","red"))+
  labs(x="max Sound Level (125 Hz TOL)",y = "Exceedance Probability")+
  theme_minimal()

#difference in SNR- greater in slowdown because lower ambient
ggplot(output2, aes(SNR, fill = Condition, color = Condition )  )+
  stat_ecdf(geom = "step")+
  scale_color_manual(values=c("gray", "black","pink","red"))+
  labs(x="SNR (125 Hz TOL)",y = "Exceedance Probability")+
  ggtitle (paste0("Comparision of SNR (125 TOL) (n=", nrow(output3),")" )) +
  theme_minimal()

#difference in SNR- same because baseline had lower ambient louder ships/ slowdown had higher ambient and quieter ships
ggplot(output2, aes(SNRmax, fill = Condition, color = Condition )  )+
  stat_ecdf(geom = "step")+
  scale_color_manual(values=c("gray", "black","pink","red"))+
  labs(x="SNR (125 Hz TOL)",y = "Exceedance Probability")+
  ggtitle (paste0("Comparision of SNRmax (125 TOL) (n=", nrow(output3),")" )) +
  theme_minimal()

# Time sampled in each category-- add to above graphs
sum(output2$DurS[ output2$Condition == "baseline-ambient"])/3600
sum(output2$DurS[ output2$Condition == "baseline-ship"])/3600
sum(output2$DurS[ output2$Condition == "slowdown-ambient"])/3600
sum(output2$DurS[ output2$Condition == "slowdown-ship"])/3600

# SHIFT IN CONDIION

#difference from non-vessel periods
NA_baseline = median(output2$`TOL_125 median`[ output2$Condition == "baseline-ship"], na.rm = T )-
  median(output2$`TOL_125 median`[ output2$Condition == "baseline-ambient"], na.rm = T )

NAmax_baseline = median(output2$`TOL_125 max`[ output2$Condition == "baseline-ship"], na.rm = T )-
  median(output2$`TOL_125 max`[ output2$Condition == "baseline-ambient"], na.rm = T )

NA_slowdown = median(output2$`TOL_125 median`[ output2$Condition == "slowdown-ship"], na.rm = T )-
  median(output2$`TOL_125 median`[ output2$Condition == "slowdown-ambient"], na.rm = T )
 
NAmax_slowdown = median(output2$`TOL_125 max`[ output2$Condition == "slowdown-ship"], na.rm = T )-
  median(output2$`TOL_125 max`[ output2$Condition == "slowdown-ambient"], na.rm = T )

diff_periods = median(output2$`TOL_125 median`[ output2$Condition == "baseline-ship"], na.rm = T )-
  median(output2$`TOL_125 median`[ output2$Condition == "slowdown-ship"], na.rm = T )

# Average duration category
mean(output2$DurS[ output2$Condition == "baseline-ambient"])/3600

durVes_baseline     = mean(output2$DurS[ output2$Condition == "baseline-ship"])/3600
sddurVes_baseline   = (sd(output2$DurS[ output2$Condition == "baseline-ship"])/3600) / nrow(output2)
SamplesVes_baseline = (length(output2$DurS[ output2$Condition == "baseline-ship"]) ) 
TimesVes_baseline   = (sum(output2$DurS[ output2$Condition == "baseline-ship"]) ) /3600
PerTimeVessel_baseline = TimesVes_baseline/ (TimesVes_baseline + ((sum(output2$DurS[ output2$Condition == "baseline-ambient"]) ) /3600)) *100

mean(output2$DurS[ output2$Condition == "slowdown-ambient"])/3600
durVes_Slowdown     = mean(output2$DurS[ output2$Condition == "slowdown-ship"])/3600
sddurVes_Slowdown   = (sd(output2$DurS[ output2$Condition == "slowdown-ship"])/3600)  / nrow(output2)
SamplesVes_Slowdown = (length(output2$DurS[ output2$Condition == "slowdown-ship"]) ) 
TimesVes_Slowdown   = (sum(output2$DurS[ output2$Condition == "slowdown-ship"]) ) /3600
PerTimeVessel_Slowdown = TimesVes_Slowdown/ (TimesVes_Slowdown + ((sum(output2$DurS[ output2$Condition == "slowdown-ambient"]) ) /3600)) *100


#duration of ambient: 0.2 hrs vs 0.4 hrs (longer in 2021-- interesting)
#duration of VD: 0.7 vs 0.7 (no change in duration of VD)

cmplt = as.data.frame( rbind(    c("Baseline-2019", NA_baseline, NAmax_baseline, durVes_baseline, sddurVes_baseline, TimesVes_baseline ), 
                                 c("Slowdown-2021", NA_slowdown, NAmax_slowdown, durVes_Slowdown, sddurVes_Slowdown, TimesVes_Slowdown )) )
colnames(cmplt) = c("Period","NoiseAbove","NoiseAboveMax",  "Duration", "Dursd","DaySampled")

cmplt$NoiseAbove = as.numeric(as.character(cmplt$NoiseAbove ))
cmplt$NoiseAboveMax = as.numeric(as.character(cmplt$NoiseAboveMax ))
cmplt$Duration = as.numeric(as.character(cmplt$Duration ))
cmplt$Dursd = as.numeric(as.character(cmplt$Dursd ))
cmplt$DaySampled = as.numeric(as.character(cmplt$DaySampled ))

ggplot(cmplt, aes(x = NoiseAbove, y = Duration, color = Period, size = DaySampled)) +
  geom_point() + 
  #geom_text(label = floor(cmplt$DaySampled), nudge_y = 0.25, check_overlap = T, label.size = 0.35) +
  geom_pointrange(aes( ymin= Duration - Dursd, ymax= Duration+Dursd ))+
  geom_label(label = paste0("Baseline (2019) ", as.character( round(cmplt$DaySampled[1])) , " hrs") , x = 6, y = .7,color = "gray")+
  geom_label(label = paste0("Slowdown (2021) ", as.character( round(cmplt$DaySampled[2])) , " hrs")  , x = 7.5, y = .8,color = "orange")+
  ylim(c(.5,1))+
  xlim(c(0,10))+
  xlab("Noise above non-vessel (median 125 TOL) ")+
  ylab("Duration of AIS vessel detections (hours) ")+  
  scale_color_manual(values=c('#999999','#E69F00'))+
  theme_minimal() +
  theme(legend.position="none")+ 
  ggtitle("Comparision of vessel noise conditions \n Median Noise Above & Duration")+
  theme(text = element_text(size = 16))
  
ggplot(cmplt, aes(x = NoiseAboveMax, y = Duration, color = Period, size = DaySampled)) +
  geom_point() + 
  #geom_text(label = floor(cmplt$DaySampled), nudge_y = 0.25, check_overlap = T, label.size = 0.35) +
  geom_pointrange(aes( ymin= Duration - Dursd, ymax= Duration+Dursd ))+
  geom_label(label = paste0("Baseline (2019) ", as.character( round(cmplt$DaySampled[1])) , " hrs") ,  x = 11, y = .7,color = "gray")+
  geom_label(label = paste0("Slowdown (2021) ", as.character( round(cmplt$DaySampled[2])) , " hrs")  , x = 11, y = .8,color = "orange")+
  ylim(c(.5,1))+
  xlim(c(0,15))+
  xlab("Noise above non-vessel (max 125 TOL) ")+
  ylab("Duration of AIS vessel detections (hours) ")+  
  scale_color_manual(values=c('#999999','#E69F00'))+
  theme_minimal() +
  theme(legend.position="none")+ 
  ggtitle("Comparision of vessel noise conditions \n Max Noise Above & Duration")+ 
  theme(text = element_text(size = 16))
#-----------------------------------------------------------------------------------------
# pie graphs of difference in ship traffic
#-----------------------------------------------------------------------------------------
dt = data.table(X = c(1,70), Y = c(1,1), Uships = c(33, 31), Names = c("Baseline (2019)","Slowdown (2021)"), ships = rbind(c(10, 11, 78), c(7,10,80))) 
dt = as.data.frame(dt)
colnames(dt) = c("X", "Y", "UniqueShips", "Period", "Small", "Medium", "Large" )

library(scatterpie)
ggplot() +
  geom_scatterpie( aes(x=X, y=Y, r=UniqueShips, group=Period), data=dt,
                  cols=c("Small", "Medium", "Large") )  + coord_equal()+
  geom_text(data = dt, aes(x=X, y=Y-1,  label = Period ), size = 3)  +
  ylab("")+xlab("")+
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
  
  

geom_map(map=world, aes(map_id=region), fill="gray90", color="black") +
  geom_point(data = siteLoc, aes(x=lon, y=lat), color = "red", size = 1) +
  geom_text(data = siteLoc, aes(x=lon, y=lat+.5,  label = Names ), size = 3)  +
  coord_quickmap()+
  xlim(-170,-150) + ylim(18, 30) +
  theme_minimal()
p

p2 = p +  
  
  p2