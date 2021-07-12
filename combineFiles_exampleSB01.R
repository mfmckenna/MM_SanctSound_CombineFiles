#Integrating SanctSound data-- and some initial plots

rm(list=ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)

#SET working directory
#-----------------------------------------------------------------------------------------
site = 'SB'
deply = "SB01"
dir =  paste0("D:\\RESEARCH\\SanctSound\\data\\",site)
dir2 = paste0("D:\\RESEARCH\\SanctSound\\data\\",site,"\\", deply)
setwd(dir)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#READ IN in data
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#SPL-- hourl octave band levels-- can I add deployment to the combined spreadsheet?
#-----------------------------------------------------------------------------------------
multmerge = function(path){
  filenames = list.files(path=path,pattern = "_OL_1h.csv", full.names=TRUE,recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
# list.files(path=dir2,pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)
SPL <- multmerge(dir2)
SPL$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPL$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
SPL$DateFday  = as.Date(SPL$DateF)
#add deployment to the spreadhsheet?-- find breaks in data
SPL$diffT = c("NA", diff(SPL$DateF) )
idx  = which( SPL$diffT > 1 )
deploys = c("01","02","03","04","05","06","07","08","09")
SPL$Deploy = NA
SPL$Deploy[1      :(idx[2]-1)]        = deploys[1]
SPL$Deploy[idx[2] : (idx[3]-1)] = deploys[2]
SPL$Deploy[idx[3] : (idx[4]-1)] = deploys[3]
SPL$Deploy[idx[4] : (idx[5]-1)] = deploys[4]
SPL$Deploy[idx[5] : (idx[6]-1)] = deploys[5]
SPL$Deploy[idx[6] : (idx[7]-1)] = deploys[6]
SPL$Deploy[idx[7] : (idx[8]-1)] = deploys[7]
SPL$Deploy[idx[8] : (idx[9]-1)] = deploys[8]
SPL$Deploy[idx[9] :  nrow(SPL)] = deploys[9]
#Data check plot
ggplot(SPL, aes(DateF, OL_500, color = Deploy))+
  geom_point() +
  theme_minimal()

#Vessel Detections-- start end times
#-----------------------------------------------------------------------------------------
multmerge = function(path){
  filenames = list.files(path=path, pattern = "_ships", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
# list.files(path=dir, pattern = "_ships", full.names=TRUE, recursive = TRUE)
VESS = multmerge(dir2)
#VESS are start and end of detections, leave as is and then find matches in a given hour
VESS$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$ISOStartTime)), tz = "GMT" )
VESS$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$ISOEndTime))  , tz = "GMT" )
VESS$DUR_mins = VESS$DateFe- VESS$DateFs #in minutes!

#Data check plot
ggplot(VESS, aes(DateFs, DUR_mins))+
  geom_point() +
  theme_minimal()

#AIS-- daily metrics 
#-----------------------------------------------------------------------------------------
AIS = read.csv("AIS_SB_2018_10_to_2020_10_v2.csv")
AIS = AIS[AIS$LOC_ID == deply,]
AIS$LOA_ALL_UV = AIS$LOA_S_UV + AIS$LOA_M_UV + AIS$LOA_L_UV + AIS$LOA_NA_UV 
AIS$LOA_ALL_OPHRS = AIS$LOA_S_OPHRS + AIS$LOA_M_OPHRS + AIS$LOA_L_OPHRS + AIS$LOA_NA_OPHRS 
AIS$DateFday     = as.Date(AIS$DATE,format = "%m/%d/%Y")#AIS is daily resolution
AIS$DateFJul     = yday(AIS$DateFday)
AIS$Yr = year(AIS$DateFday)

#Data check plot
AIS = AIS[AIS$Yr > 2018,]
ps = ggplot(AIS, aes(DateFJul, LOA_S_OPHRS, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9'),name = "") +
  #scale_colour_discrete(name = "")+
  ylim(c(0,150))+
  theme(legend.position=c(0.2, 0.9))
pm = ggplot(AIS, aes(DateFJul, LOA_M_OPHRS, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,150))+
  theme(legend.position="none")
pl = ggplot(AIS, aes(DateFJul, LOA_L_OPHRS, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,150))+
  theme(legend.position="none")
pa= ggplot(AIS, aes(DateFJul, LOA_ALL_OPHRS, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") + 
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,150))+
  theme(legend.position="none")

grid.arrange(ps,pm,pl,ncol = 3,nrow = 1)

ps = ggplot(AIS, aes(DateFJul, LOA_S_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9'),name = "") +
  #scale_colour_discrete(name = "")+
  ylim(c(0,50))+
  theme(legend.position=c(0.2, 0.9))
pm = ggplot(AIS, aes(DateFJul, LOA_M_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,50))+
  theme(legend.position="none")
pl = ggplot(AIS, aes(DateFJul, LOA_L_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,50))+
  theme(legend.position="none")
pa= ggplot(AIS, aes(DateFJul, LOA_ALL_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") + 
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,50))+
  theme(legend.position="none")
grid.arrange(ps,pm,pl,ncol = 3,nrow = 1)


ggplot(AIS, aes(DateFJul, LOA_NA_OPHRS, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9'),name = "") +
  #scale_colour_discrete(name = "")+
  ylim(c(0,50))+
  theme(legend.position=c(0.2, 0.9))
ggplot(AIS, aes(DateFJul, LOA_NA_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") + 
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,20))+
  theme(legend.position=c(0.2, 0.9))

#TIDE-- water level (hourly)
#-----------------------------------------------------------------------------------------
multmerge = function(path){
  filenames = list.files(path=path, pattern = "CO-OPS_8443970_", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
# list.files(path=dir, pattern = "CO-OPS_8443970_", full.names=TRUE, recursive = TRUE)
TIDE <- multmerge(dir)
TIDE$DateFday = as.Date(TIDE$Date ,format = "%Y/%m/%d")
TIDE$DateF    = as.POSIXct( paste(TIDE$DateFday, paste(TIDE$`Time (GMT)`,":00", sep = ""), sep=" "), tz = "GMT")
TIDE$changeWL = c("NA", diff(TIDE$`Verified (ft)`))

#Data check plot
ggplot(TIDE, aes(DateFday, as.numeric(as.character(changeWL)) ) )+
  geom_point() +
  theme_minimal()

#WIND-- hourly data
#-----------------------------------------------------------------------------------------
multmerge = function(path){
  filenames = list.files(path=path, pattern = "MetData", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
# list.files(path=dir, pattern = "MetData", full.names=TRUE, recursive = TRUE)
WSPD  <- multmerge(dir)
#WSPD is hourly (not at top of hour, so changed)
endR = nrow(WSPD)
WSPD = WSPD[2:endR,]
WSPD$DateF    = as.POSIXct ( paste( WSPD$`#YY`, "/", WSPD$MM, "/", WSPD$DD, " ", WSPD$hh, ":", 00, ":", 00, sep = ""), tz = "GMT")
WSPD$WSPD = as.numeric(as.character(WSPD$WSPD  ))

#Data check plot
ggplot(WSPD, aes(DateF, WSPD ) )+
  geom_point() +
  theme_minimal()

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#COMBINE: for each SPL time step, find associated metrics AIS, WSPD, TIDE, VESS using formated date
#MERGE: with daily AIS values... each SPL time step has same AIS value!!
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#COMBINE AIS
#-----------------------------------------------------------------------------------------
#repeated values for each hourly SPL for a given day because only one daily value for AIS
SplAis = merge(SPL, AIS, all = FALSE, all.x = TRUE, by = "DateFday" ) 
#SOME CHECKS: values were way off for SB01_04 so checking values
#relationship between 63Hz and unique AIS ships (not on the sample time resolution)
#ggplot(SplAis,aes(LOA_ALL_UV,OL_63 ) ) +
#geom_point(aes(size = LOA_ALL_OPHRS))
#hist(AIS$LOA_ALL_UV)
#hist(SPL$OL_63)

#COMBINE WIND and TIDE
#-----------------------------------------------------------------------------------------
#matches the hourly SPL values because all at the top of the hour
print.POSIXct = function(x,...)print(format(x,"%Y-%m-%d %H:%M:%S"))
cat("Check formats match: AIS-", as.character(SplAis$DateF[2]), " WSPD-", as.character(WSPD$DateF[2]), " TIDE-", as.character(TIDE$DateF[2]))
SplAisWspd     = merge(SplAis, WSPD, all = FALSE, all.x = TRUE, by = "DateF" )
SplAisWspdTide = merge(SplAisWspd, TIDE, all = FALSE, all.x = TRUE, by = "DateF" )

# COMBINE VESSdet 
#-----------------------------------------------------------------------------------------
# is tricky- for each each hour, how many vessels detected, and percent of hour present
# some of the time will spill into the next hour... need to keep track of this might be easier to just sum up before combining?
#reformat so for each hour there is a total vessle time value and total vessel count
VESS$DateFstart = as.Date(VESS$DateFs)
VESS$HrS =hour(VESS$DateFs)
VESS$HrE =hour(VESS$DateFe)
#rm(VESSfor)
VESSfor = as.data.frame( seq(from=as.POSIXct(min(VESS$DateFstart), tz="GMT"), 
                             to=as.POSIXct(max(VESS$DateFstart), tz="GMT"), by="hour") )
VESSfor$totalVessels = 0
VESSfor$totalTime = 0
colnames(VESSfor) = c("DayHr","totalVessels","totalTime")
VESSfor$DayHr = force_tz(VESSfor$DayHr, tzone="GMT")
VESSfor$DateF = as.Date(VESSfor$DayHr)

#clean up VESS... sort and remove duplicates (ugh!!!)
VESSa = arrange(VESS, by_group = VESS$DateFs )
VESS = VESSa
VESSd = dplyr::distinct(VESS)
VESS = VESSd

#for testing.... truncate to 1 day-- not all these work because I re-ordered VESS
#VESSforO = VESSfor
#VESSO = VESS
# 1/19/2019: VESSfor = VESSfor[1689:(1689+48), ]; VESS = VESS[2229:2236,]
# 8/11/2019: VESSfor = VESSfor[6536:(6536+48), ]  VESS = VESS[5560:5577,]
# 6/1/2019:  
#VESSfor = VESSfor[4833:(4833+48), ]
#VESS = VESS[3149:3176,]

#for each vessel detection, put in correct column in VESSfor
for (vv in  1:nrow(VESS)){ #nrow(VESS)  1:nrow(VESS) 
  
  
  tmp = VESS[vv,] #temp matrix
  tmp$DateFend = as.Date(tmp$DateFe) #needed end data in case it goes to the next day!
  
  #find column with matching day/hour
  idx = which( as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" ) == VESSfor$DayHr) 
  #put results in this row: VESSfor[idx,] VESSfor[idx+1,]
  
  stH = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" )
  edH = as.POSIXct ( paste0(tmp$DateFend, " "  , tmp$HrE,":00:00"), tz = "GMT" )
  hrsSpan =  as.numeric(edH - stH) # how many hours does the vessel detection dominate?

  if ((hrsSpan) == 1 )#spans to next hour
  {
    #add info to start hour
    VESSfor$totalVessels[idx]  = VESSfor$totalVessels[idx] + 1 # vessel count
    VESSfor$totalTime[idx]     = VESSfor$totalTime[idx] + difftime(edH, tmp$DateFs, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
    
    #add info to next hour
    VESSfor$totalVessels[idx+1]  = VESSfor$totalVessels[idx+1] + 1 # vessel count
    VESSfor$totalTime[idx+1]     = VESSfor$totalTime[idx+1] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds  difftime(tmp$DateFe, edH, units = "mins")
    
  } else if ((hrsSpan) == 0 ){ #within single hour
    VESSfor$totalVessels[idx]  = VESSfor$totalVessels[idx] + 1 # vessel count
    VESSfor$totalTime[idx]     = VESSfor$totalTime[idx] + difftime(tmp$DateFe,tmp$DateFs,  units = "secs") #time in seconds
    
  } else if  ((hrsSpan) >= 2 ) { #spans two or more hours
    
    midHr = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS+1,":00:00"), tz = "GMT" )
    
    #add info to start hour
    VESSfor$totalVessels[idx]  = VESSfor$totalVessels[idx] + 1 # vessel count
    VESSfor$totalTime[idx]     = VESSfor$totalTime[idx] + difftime(midHr, tmp$DateFs, units = "secs")
    
    #add info to middle hours--- need to loop this though each hour
    for(hh in 1:((hrsSpan)-1) ) {
      VESSfor$totalVessels[idx+hh]  = VESSfor$totalVessels[idx+hh] + 1 # vessel count
      VESSfor$totalTime[idx+hh]     = VESSfor$totalTime[idx+hh] + 3600 #time in seconds, full hour
    }
    
    #add info to last hour
    VESSfor$totalVessels[idx+hrsSpan]  = VESSfor$totalVessels[idx+hrsSpan] + 1 # vessel count
    VESSfor$totalTime[idx+hrsSpan]     = VESSfor$totalTime[idx+hrsSpan] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds
  }
  
  cat("Hours for detection ", vv, ": ", hrsSpan, "hrs", "\n")
  #cat("start hour:", as.character(stH), " Row: ", idx, "VesselTime",  VESSfor$totalTime[idx],"Vessels",  VESSfor$totalVessels[idx], "\n")
}

#sum(as.numeric(VESS$DUR_mins))/ (60)   #total duration of detections on a given day-- not as accurate because some time in last detecion on next day
VESSfor$totalTimeM = VESSfor$totalTime/(60) #number of hours dominated by ships
#hist(VESSfor$totalTimeM)
#max(VESSfor$totalTimeM)
errorC = VESSfor[VESSfor$totalTimeM > 60,] #grrr still errors starting in June
# CHECk: 
hist(VESSfor$totalTime)

VESSfor$DateF = VESSfor$DayHr
SplAisWspdTideVess = merge(SplAisWspdTide, VESSfor, all = FALSE, all.x = TRUE, by = "DateF" )
SplAisWspdTideVess$JulDay = yday(SplAisWspdTideVess$DateFday.x)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#Summarize by day
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
daySum1 = NULL 
uday = unique(SplAisWspdTideVess$DateFday.x)
dIn = SplAisWspdTideVess
for (ii in 1:length(uday)){ # ii = 266
  dtmp = dIn[dIn$DateFday.x == uday[ii],]
  hrsample = nrow(dtmp)
  
  depl = dtmp$Deploy[1]
  
  #AIS-- only first daily value because repeated!!
  Atotal    = dtmp$LOA_S_UV[1] + dtmp$LOA_M_UV[1] + dtmp$LOA_L_UV[1] + dtmp$LOA_NA_UV[1]
  Ahtotal   = dtmp$LOA_S_OPHRS[1] + dtmp$LOA_M_OPHRS[1] + dtmp$LOA_L_OPHRS[1] + dtmp$LOA_NA_OPHRS[1]
  perAtotal = Ahtotal/24 #Operation hours can be greater than hours in a day because of multipe ships in the area
  
  #Vessel detect
  VDtotal = sum(dtmp$totalVessels,na.rm = T)  # not unique because we totaled per hour in previous step and some ships feel in multiple hours (fix??)
  VDperHr = mean(dtmp$totalVessels,na.rm = T) # vessel detections per hour
  VDper   = sum(dtmp$totalTime,na.rm = T)/(60*60*hrsample)  #percent of the day with vessels dominating
  #dtmp$totalTime > 3600
  
  #SPL- median per octave band
  SPLm = apply( dtmp[,4:13] , 2 , quantile , probs = 0.5 , na.rm = TRUE )
  
  #WSPD-- average per day
  tst = all(is.na(dtmp$WSPD))
  if (tst == TRUE){
    WSPDm = NA
  }else {
    WSPDm  = mean(dtmp$WSPD, na.rm = T)
  }
  
  #TIDE-- mean hourly change
  tst = all(is.na(dtmp$changeWL))
  if (tst == TRUE){
    TIDEm = NA
  }else {
    TIDEm  = mean(dtmp$changeWL, na.rm = T)
  }
  
  #Combine to output
  daySum1 = rbind(daySum1,c(as.character(dtmp$DATE[1]), yday(uday[ii]), 
                            Atotal, Ahtotal, perAtotal,
                            VDtotal,VDperHr,VDper,
                            SPLm,WSPDm,TIDEm,
                            nrow(dtmp),depl ) )
  
  rm(dtmp,Atotal, Ahtotal, perAtotal,VDtotal,VDperHr,VDper,SPLm,WSPDm,TIDEm,depl)
}
daySum1 = as.data.frame(daySum1)
colnames(daySum1) = c("Day","JulDay","AIS_UV","AIS_OPHRS","AIS_OPRHS_PercDay","VessD_TV","VessD_VD_meanPerHr","VessD_Time_PerDay",
                      "mOB_16", "mOB_31.5","mOB_63","mOB_125","mOB_250","mOB_500","mOB_1000","mOB_2000","mOB_4000","mOB_8000",
                      "WSPD_mean", "TIDE_mean", "totHRs","deployment")

#as.numeric(as.character(daySum1$JulDay))
daySum1$Day = as.Date( as.POSIXct( daySum1$Day ,format = "%m/%d/%Y") )
daySum1$Year = year(daySum1$Day)


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#VISUALIZE DATA
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#1) summary of vessel detection results
#-----------------------------------------------------------------------------------------
# hist(as.numeric(as.character(daySum1$VessD_Time_PerDay))*100)
ggplot(daySum1, aes(as.numeric(as.character(JulDay)), as.numeric(as.character(VessD_Time_PerDay))*100, color = (as.factor(Year))) ) +
  geom_point(alpha = .3)+
  geom_smooth(method = "lm") +
  #geom_linerange(aes(x=as.numeric(as.character(JulDay)), ymax=as.numeric(as.character(VessD_Time_PerDay))*100, ymin=0),alpha = .3) +
  xlab("Julian Day") +
  ylab("% of day dominated by vessel noise")+
  scale_colour_discrete(name = "Year")+
  ggtitle(paste0(deply,": % of day dominated by vessel noise (Detector)")) +
  theme_minimal()

ggplot(daySum1, aes(as.numeric(as.character(JulDay)), as.numeric(as.character(VessD_VD_meanPerHr)), color = (as.factor(Year))) ) +
  geom_point(alpha = .3)+
  geom_smooth(method = "lm") +
  #geom_linerange(aes(x=as.numeric(as.character(JulDay)), ymax=as.numeric(as.character(VessD_VD_meanPerHr)), ymin=0),alpha = .3) +
  xlab("Julian Day") +
  ylab("Average unique houly vessels per day")+
  scale_colour_discrete(name = "Year")+
  ggtitle(paste0(deply,": Average unique houly vessels per day (Detector)")) +
  theme_minimal()

#2) summary of AIS results
#-----------------------------------------------------------------------------------------
#hist( as.numeric(as.character( daySum1$AIS_OPHRS) ) )
ggplot(daySum1, aes(as.numeric(as.character(JulDay)), as.numeric(as.character( AIS_OPHRS) ), color = (as.factor(Year))) ) +
  geom_point(alpha = .3)+
  geom_smooth(method = "lm") +
  geom_linerange(aes(x=as.numeric(as.character(JulDay)), ymax=as.numeric(as.character( AIS_OPHRS) ), ymin=0),alpha = .3) +
  xlab("Julian Day") +
  ylab("Operation Hours [per day]")+
  scale_colour_discrete(name = "Year")+
  ggtitle(paste(deply)) +
  theme_minimal()

ggplot(daySum1, aes(as.numeric(as.character(JulDay)), as.numeric(as.character( AIS_UV) ), color = (as.factor(Year))) ) +
  geom_point(alpha = .3)+
  geom_smooth(method = "loess") +
  geom_linerange(aes(x=as.numeric(as.character(JulDay)), ymax=as.numeric(as.character( AIS_UV) ), ymin=0),alpha = .3) +
  xlab("Julian Day") +
  ylab("Unique Vessels (AIS)")+
  scale_colour_discrete(name = "Year")+
  ggtitle(paste(deply)) +
  theme_minimal()


#3) relationship between daily vessel detections and AIS data 
#-----------------------------------------------------------------------------------------
#(lots of zeros for AIS, so removed those rows for now)
#daySum2 = daySum1[as.numeric(as.character( daySum1$AIS_UV)) > 0, ] 

ggplot(daySum1, aes(as.numeric(as.character( AIS_UV) ), as.numeric(as.character(VessD_VD_meanPerHr)) ,color = as.factor(Year)) ) +
  geom_point(alpha = .3)+
  geom_smooth(method = "lm") +
  ylab("Average vessles per hour (Detector)") +
  xlab("Unique Vessels (AIS)")+
  scale_colour_discrete(name = "Year")+
  ggtitle(paste0(deply, ": Daily comparision of number of vessels")) +
  theme_minimal()

# THIS DOES NOT LOOK AS EXPPECTED-- not sure why 
#hist(as.numeric(as.character( daySum2$AIS_OPHRS)) ) # wow- most days have operation hours > 50!!!
#hist(as.numeric(as.character( daySum2$VessD_Time_PerDay)) ) #most days have 50% of time dominated by vessel noise, what are the > 100% days?
ggplot(daySum1, aes(as.numeric(as.character( AIS_OPHRS) ), as.numeric(as.character(VessD_Time_PerDay))*100 ,color = as.factor(Year) ) ) +
  geom_point(alpha = .3)+
  geom_smooth(method = "lm") +
  ylab("% of day dominated by vessels (Detector)") +
  xlab("Total Operation Hours (AIS)")+
  scale_colour_discrete(name = "Year")+
  ggtitle(paste0(deply, ": Daily comparision of vessels presence")) +
  theme_minimal()
theme_minimal()

#4) relationship between daily vessel detections and SPL levels 
#-----------------------------------------------------------------------------------------
#by year
ggplot(daySum1, aes(as.numeric(as.character(mOB_63) ), as.numeric(as.character(VessD_Time_PerDay))*100 ,color = as.factor(Year) ) ) +
  geom_point(alpha = .3)+
  geom_smooth(method = "lm") +
  ylab("% of day dominated by vessels (Detector)") +
  xlab("daily median SPL [63 Hz]")+
  scale_colour_discrete(name = "Year")+
  ggtitle(paste0(deply, ": Ship Detector vs SPLs")) +
  theme_minimal()

#by deployment
ggplot(daySum1, aes(as.numeric(as.character(mOB_63) ), as.numeric(as.character(VessD_Time_PerDay))*100 ,color = as.factor(deployment) ) ) +
  geom_point(alpha = .3)+
  geom_smooth(method = "lm") +
  ylab("% of day dominated by vessels (Detector)") +
  xlab("daily median SPL [63 Hz]")+
  scale_colour_discrete(name = "Deployment")+
  ggtitle(paste0(deply, ": Ship Detector vs SPLs")) +
  theme_minimal()

ggplot(daySum1, aes(as.numeric(as.character(JulDay)), as.numeric(as.character(mOB_63)), color = (as.factor(Year))) ) +
  geom_point(alpha = .3)+
  geom_smooth(method = "lm") +
  #geom_linerange(aes(x=as.numeric(as.character(JulDay)), ymax=as.numeric(as.character(VessD_VD_meanPerHr)), ymin=0),alpha = .3) +
  xlab("Julian Day") +
  ylab("median Octave Band Sound Pressure Level (63 Hz)")+
  scale_colour_discrete(name = "Year")+
  ggtitle(paste0(deply,": SPL")) +
  theme_minimal()

ggplot(daySum1, aes(as.numeric(as.character(JulDay)), as.numeric(as.character(mOB_500)), color = (as.factor(Year))) ) +
  geom_point(alpha = .3)+
  geom_smooth(method = "lm") +
  #geom_linerange(aes(x=as.numeric(as.character(JulDay)), ymax=as.numeric(as.character(VessD_VD_meanPerHr)), ymin=0),alpha = .3) +
  xlab("Julian Day") +
  ylab("median Octave Band Sound Pressure Level (500 Hz)")+
  scale_colour_discrete(name = "Year")+
  ggtitle(paste0(deply,": SPL")) +
  theme_minimal()


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#VISUALIZE DATA-- AGU presentation
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#truncate data to 17 May (138) to June 16 (168)
daySum1$JulDay = as.numeric(as.character(daySum1$JulDay))
dataInt = daySum1[daySum1$JulDay >= 137 & daySum1$JulDay <= 163 ,]
dataInt$AIS_OPHRS = as.numeric(as.character(dataInt$AIS_OPHRS))
dataInt$AIS_UV = as.numeric(as.character(dataInt$AIS_UV))
dataInt$VessD_Time_PerDay = as.numeric(as.character(dataInt$VessD_Time_PerDay))
dataInt$mOB_125 = as.numeric(as.character(dataInt$mOB_125))
dataInt$Year = as.factor(dataInt$Year )

pDet = ggplot(dataInt, aes(y= VessD_Time_PerDay*100, x=(Year), color=(Year))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=2,fill=c("#E69F00", "#56B4E9"), color="black") +
  xlab("") +  ylab("") + ggtitle("% of day with close-by vessel noise")+
  ylim(c(0,100))+
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA), 
        plot.margin = unit(c(1,2,2,2), "lines") )  

pAISa = ggplot(dataInt, aes(y= AIS_OPHRS, x=(Year), color=(Year)) ) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=2,fill=c("#E69F00", "#56B4E9"), color="black") +
  xlab("") +  ylab ("") + ggtitle("Operation hours (AIS)")+
  ylim(c(0,100))+
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(1,2,2,2), "lines")  ) 

pSPL = ggplot(dataInt, aes(y=mOB_125,x=(Year), color=(Year)) ) +  
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=2,fill=c("#E69F00", "#56B4E9"), color="black") +
  xlab("") +   ylab ("") + ggtitle("Low frequency sound levels ")+ #median [125 Hz]
  ylim(c(85,100)) + labs(caption = "May 17 - June 12")+
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        plot.caption = element_text(color = "black", face = "italic",hjust = 0),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(1,2,2,2), "lines")  ) 

pAISb = ggplot(dataInt, aes(y= AIS_UV, x=(Year), color=(Year)) ) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=2,fill=c("#E69F00", "#56B4E9"), color="black") +
  xlab("") +  ylab ("") + ggtitle("# of vessels tracking close to recorder")+
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(1,2,2,2), "lines")  )              
library(gridExtra)
grid.arrange(pSPL, pDet, pAISb, ncol = 3, nrow = 1)

