#Integrating SanctSound data-- and some check plots

# write out csv files with full SPL integrated with other metrics
# Tide, wind, sun altitude, AIS, vessel dets, biological dets
# created by region because detectors version are different

rm(list=ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(suncalc)

#SET working directory-- currently runs per site
#-----------------------------------------------------------------------------------------
siteLoc =  c(36.798,-121.976) #c(36.648,-121.9084)--MB02  #c(24.43313,-81.93068- SB01; 
site  = 'MB' 
deply = "MB01"
dir   = paste0("E:\\RESEARCH\\SanctSound\\data\\",site)
dir2  = paste0("E:\\RESEARCH\\SanctSound\\data\\",site,"\\", deply)
setwd(dir)
DC = Sys.Date()
outDir = "E:\\RESEARCH\\SanctSound\\data\\combineFiles_dets"

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#READ IN in data
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#SPL-- hourly octave band levels
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2,pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE))
SPL = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "_OL_1h.csv", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)[ff]
  dname  = sapply (strsplit( fname, "_" ),"[",4 )
  tmp    = rbind(fread(fnameF))
  tmp$deploy = dname
  if (ff > 1) { names(tmp) <- NULL }
  
  SPL = rbind(SPL,tmp)
  rm(fname,fnameF,tmp)
}
SPL$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPL$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
SPL$DateFday  = as.Date(SPL$DateF)
#Data check plot
pSPL = ggplot(SPL, aes(DateF, OL_500, color = deploy))+
  geom_point() +
  xlab("")+
  ylab("SPL- 500Hz OTB") +
  theme_minimal()+
  ggtitle(paste( "Check SPL (", deply, ")" ))
pSPL

#SPL-- hourly broad band levels
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2,pattern = "_BB_1h.csv", full.names=TRUE, recursive = TRUE))
SPLBB = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "_BB_1h.csv", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "_BB_1h.csv", full.names=TRUE, recursive = TRUE)[ff]
  dname  = sapply (strsplit( fname, "_" ),"[",4 )
  tmp    = rbind(fread(fnameF))
  tmp$deploy = dname
  if (ff > 1) { names(tmp) <- NULL }
  
  SPLBB = rbind(SPLBB,tmp)
  rm(fname,fnameF,tmp)
}
SPLBB$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPL$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
SPLBB$DateFday  = as.Date(SPL$DateF)
#Data check plot
pSPLB = ggplot(SPLBB, aes(DateF, `BB_20-24000`, color = deploy))+
  geom_point() +
  xlab("")+
  ylab("SPL-BB") +
  theme_minimal()+
  ggtitle(paste( "Check SPL (", deply, ")" ))
pSPLB

#VESSEL DETECTIONS-- start end times
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
pVes = ggplot(VESS, aes(DateFs, DUR_mins))+
  geom_point() +
  xlab("")+
  ylab("Duration Vessel Detections (mins)") +
  theme_minimal()+
  ggtitle(paste( "Check vessel detections (", deply, ")" ))
pVes

#EXPLOSIONS-- start end times (NOT ALL SITES)
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2,pattern = "Explosion", full.names=TRUE, recursive = TRUE))
multmerge = function(path){
  filenames = list.files(path=path, pattern = "_Explosion", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
# list.files(path=dir, pattern = "_ships", full.names=TRUE, recursive = TRUE)
EXPL = multmerge(dir2)
if (length(EXPL) != 0 ){
  #VESS are start and end of detections, leave as is and then find matches in a given hour
  EXPL$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", EXPL$ISOStartTime)), tz = "GMT" )
  EXPL$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", EXPL$ISOEndTime))  , tz = "GMT" )
  EXPL$DUR_mins = EXPL$DateFe- EXPL$DateFs #in minutes!
  #Data check plot
  pExpl = ggplot(EXPL, aes(DateFs, as.numeric(as.character( DUR_mins)) ) )+
    geom_point() +
    xlab("")+
    ylab("Duration Explosions (mins)") +
    theme_minimal()+
    ggtitle(paste( "Check explosions detections (", deply, ")" ))
  pExpl
}

#Biological Detections-- west coast
#options: dolphins, bluewhale, finwhale, humpbackwhale, seiwhale
#GRRR... column names are different for these outputs!!
#-----------------------------------------------------------------------------------------
#need to find which files are available
lstBio = NULL
#dolphins-- presence in a given hour, 0 or 1, summarize by day
nFiles = length( list.files(path=dir2,pattern = "dolphins", full.names=TRUE, recursive = TRUE))
if (nFiles != 0){ lstBio = c(lstBio,"dolphins") }
#bluewhale-- detections
nFiles = length( list.files(path=dir2,pattern = "bluewhale", full.names=TRUE, recursive = TRUE))
if (nFiles != 0){ lstBio = c(lstBio,"bluewhale") }
#finwhale-- presence in a given day, 0 or 1 
nFiles = length( list.files(path=dir2,pattern = "finwhale", full.names=TRUE, recursive = TRUE))
if (nFiles != 0){ lstBio = c(lstBio,"finwhale") }
#humpbackwhale-- presence in a given day, 0 or 1 
nFiles = length( list.files(path=dir2,pattern = "humpbackwhale", full.names=TRUE, recursive = TRUE))
if (nFiles != 0){ lstBio = c(lstBio,"humpbackwhale") }
#northatlanticrightwhale-- presence in a given day, 0 or 1 
nFiles = length( list.files(path=dir2,pattern = "northatlanticrightwhale", full.names=TRUE, recursive = TRUE))
if (nFiles != 0){ lstBio = c(lstBio,"northatlanticrightwhale") }
#seiwhale-- presence in a given day, 0 or 1 
nFiles = length( list.files(path=dir2,pattern = "seiwhale", full.names=TRUE, recursive = TRUE))
if (nFiles != 0){ lstBio = c(lstBio,"seiwhale") }

#LOOP through detection files by type (!!!NEED to add back in others, only tested for blue whale!!!) 
for (ii in 1:length(lstBio)){
  nFiles = length( list.files(path=dir2,pattern = lstBio[ii], full.names=TRUE, recursive = TRUE))
  bio = NULL
  for (ff in 1 : nFiles){
    fname  = list.files(path=dir2, pattern = lstBio[ii], full.names=FALSE, recursive = TRUE)[ff]
    fnameF = list.files(path=dir2, pattern = lstBio[ii], full.names=TRUE, recursive = TRUE)[ff]
    dname = sapply (strsplit( fname, "_" ),"[",3 )
    tmp  = fread(fnameF,header=F) #remove these after
    tmp$deplm = dname
    bio = rbind(bio,tmp)
    rm(fname,fnameF,tmp)
  }
  #this varies by detection type.... will need to test for other file formats!!!
  if (ncol(bio) == 3) {  #bluewhale-- start and end times, need to convert to hourl presence!!
    colnames(bio)= c( as.character(bio[1,1:2]) ,"deply")
    bio = bio[bio$Presence != "Presence",] #remove headers in the data frame
    bio$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", bio$Time)), tz = "GMT" )
    bio$DayHR    = as.POSIXct( paste0(as.Date( bio$DateFs )," ", hour( bio$DateFs ),":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    bio$DateDay = as.Date( bio$DateFs )
    
  }else if (ncol(bio) == 4){ #atlanticcod-- start and end times, need to convert to hourl presence!!
    colnames(bio)= c( as.character(bio[1,1:3]) ,"deply")
    bio = bio[bio$Presence != "Presence",] #remove headers in the data frame
    bio$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", bio$Begin_datetime_ISO6801)), tz = "GMT" )
    bio$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", bio$End_datetime_ISO6801))  , tz = "GMT" )
    bio$DUR_mins = bio$DateFe- bio$DateFs #in minutes!
    bio$DayHR  = as.POSIXct( paste0(as.Date( bio$DateFs )," ", hour( bio$DateFs ),":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    bio$DateDay = as.Date( bio$DateFs )
  }
  
  names(bio)[2] = paste0(lstBio[ii],"P")
  assign( lstBio[ii] , bio) 
  rm(bio)
  
}

#MERGE all the biologics into one matrix, except cod because need to modify (!!!NEED TO UPDATE!!!!)
bio = bluewhale


#MATCH biological detections with all SPL measurements NOTE: NA are data not processed yet for detections
#-----------------------------------------------------------------------------------------
BioAll = as.data.frame( seq(from=as.Date(min(SPL$DateFday)), to=as.Date(max(SPL$DateFday)), by="day") )
colnames(BioAll) = "Day"

#ADD blue whale calls to the daily presence
BioAll$bluewhaleDet = NA
for (ii in 1:nrow(BioAll)){
  tmp1 = nrow( bluewhale[bluewhale$DateDay == BioAll$Day[ii],] )
  if (tmp1 > 0){ #total detection on that day
    tmp2 = bluewhale[bluewhale$DateDay == BioAll$Day[ii],]
    BioAll$bluewhaleDet[ii] = sum(as.numeric(as.character(tmp2$bluewhaleP)))
  } else { BioAll$bluewhaleDet[ii] = 0 } #no detections on that day
}
BioAllo = BioAll

#GRAPHIC for all presense of biological sounds
BioAllomelt = reshape :: melt(BioAllo, id.vars = "Day", 
                              measure.vars = c("bluewhaleDet" ))
BioAllomelt$value = as.numeric(as.character(BioAllomelt$value) )
pBIO = ggplot(BioAllomelt, aes(Day, variable, fill= (value))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="black") +
  labs(title = paste0(deply,": Check- Identified sounds"),  fill = "Detections") +
  xlab("") +
  ylab("")
pBIO

#AIS-- daily metrics 
#-----------------------------------------------------------------------------------------
AIS = read.csv(paste0("AIS_", site, "_2018_10_to_2020_11.csv"))
AIS = AIS[AIS$LOC_ID == deply,]
AIS$LOA_ALL_UV = AIS$LOA_S_UV + AIS$LOA_M_UV + AIS$LOA_L_UV + AIS$LOA_NA_UV 
AIS$LOA_ALL_OPHRS = AIS$LOA_S_OPHRS + AIS$LOA_M_OPHRS + AIS$LOA_L_OPHRS + AIS$LOA_NA_OPHRS 
AIS$DateFday     = as.Date(AIS$DATE,format = "%m/%d/%Y")#AIS is daily resolution
AIS$DateFJul     = yday(AIS$DateFday)
AIS$Yr = year(AIS$DateFday)

#Data check plot
AIS = AIS[AIS$Yr > 2018,]
pAISs = ggplot(AIS, aes(DateFJul, LOA_S_OPHRS, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9'),name = "") +
  #scale_colour_discrete(name = "")+
  ylim(c(0,150))+
  theme(legend.position="none")
pAISm = ggplot(AIS, aes(DateFJul, LOA_M_OPHRS, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,150))+
  theme(legend.position=c(0.2, 0.9))
pAISl = ggplot(AIS, aes(DateFJul, LOA_L_OPHRS, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,150))+
  theme(legend.position="none")
pAISa= ggplot(AIS, aes(DateFJul, LOA_ALL_OPHRS, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") + 
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,150))+
  xlab("Julian Day")+
  theme(legend.position="none")
grid.arrange(pAISs,pAISm,pAISl,pAISa,ncol = 2,nrow = 2, top = paste("Check- AIS OPHRS (",deply, ")"))

pAISsUV = ggplot(AIS, aes(DateFJul, LOA_S_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9'),name = "") +
  ylim(c(0,50))+
  theme(legend.position=c(0.2, 0.9))
pAISmUV = ggplot(AIS, aes(DateFJul, LOA_M_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,50))+
  theme(legend.position="none")
pAISlUV = ggplot(AIS, aes(DateFJul, LOA_L_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,50))+
  theme(legend.position="none")
pAISaUV = ggplot(AIS, aes(DateFJul, LOA_ALL_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") + 
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,50))+
  xlab("Julian Day")+
  theme(legend.position="none")
grid.arrange(pAISsUV,pAISmUV,pAISlUV,pAISaUV,ncol = 2,nrow = 2,top = paste("Check- AIS UV (",deply, ")") )

#TIDE-- water level (hourly)
#-----------------------------------------------------------------------------------------
multmerge = function(path){
  filenames = list.files(path=path, pattern = "^CO-OPS", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
#list.files(path=dir, pattern = "^CO-OPS", full.names=TRUE, recursive = TRUE)

TIDE <- multmerge(dir)
TIDE$DateFday = as.Date(TIDE$Date ,format = "%Y/%m/%d")
TIDE$DateF    = as.POSIXct( paste(TIDE$DateFday, paste(TIDE$`Time (GMT)`,":00", sep = ""), sep=" "), tz = "GMT")
TIDE$changeWL = c("NA", diff(TIDE$`Verified (ft)`))

#Data check plot
pTide = ggplot(TIDE, aes(DateFday, as.numeric(as.character(changeWL)) ) )+
  geom_point() +
  xlab("")+
  ylab("Hourly change in water level") +
  theme_minimal()+ggtitle(paste("Check- TIDE(",deply, ")"))
pTide

#WIND-- 10 minute data
multmerge = function(path){
  filenames = list.files(path=path, pattern = "MetData", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
WSPD  <- multmerge(dir)
#WSPD is hourly (not at top of hour, so changed)
endR = nrow(WSPD)
WSPD = as.data.frame(WSPD[2:endR,])
WSPD$DateC    =  ( paste( WSPD$`#YY`, "-", WSPD$MM, "-", WSPD$DD, " ", WSPD$hh, ":",  WSPD$mm , sep = ""))
WSPD$DateF = as.POSIXct(WSPD$DateC,"%Y-%m-%d %H:%M",tz ="GMT")
WSPD$WSPD  = as.numeric(as.character(WSPD$WSPD  )) #some days with NAs which( is.na(WSPD$WSPD)) = 9

#Data check plot
pWind = ggplot(WSPD, aes(DateF, WSPD ) )+
  geom_point() +
  xlab("")+
  ylab("Wind Speed (mps) ") +
  theme_minimal()+ggtitle(paste("Check- WIND (",deply, ")"))
pWind

#-----------------------------------------------------------------------------------------
#### CLEAN UP #####
rm(dname,bio,BioAll, endR,ff, ii,nFiles)
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#COMBINE: for each SPL time step, find associated metrics AIS, WSPD, TIDE, VESS using formated date
#MERGE: with daily AIS values... each SPL time step has same AIS value!!
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
print.POSIXct = function(x,...)print(format(x,"%Y-%m-%d %H:%M:%S"))

#COMBINE SPLS
#-----------------------------------------------------------------------------------------
SPLa = merge(SPL, SPLBB[,c(2,4)], all = FALSE, all.x = TRUE, by = "DateF" ) 

#COMBINE AIS
#-----------------------------------------------------------------------------------------
#Repeated values for each hourly SPL for a given day because only one daily value for AIS
SplAis = merge(SPLa, AIS, all = FALSE, all.x = TRUE, by = "DateFday" ) 

#COMBINE SUN ALTITUDE, WIND and TIDE
#-----------------------------------------------------------------------------------------
#matches the hourly SPL values, only gets the value for that hour (want average for the hour as well)
WSPD$Day   = as.Date(WSPD$DateF, format = "%Y-%m-%d")
WSPD$HR    = strftime(WSPD$DateF, format="%H") #hour(WSPD$DateF)
WSPD$DayHR = as.POSIXct( paste0(WSPD$Day," ", WSPD$HR,":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

WSPDavg = aggregate(WSPD$WSPD, by=list(WSPD$DayHR), mean)
colnames(WSPDavg) = c("DateF","avgWSPD")
SplAisWspd     = merge(SplAis, WSPDavg,  all = FALSE, all.x = TRUE, by = "DateF" )#mean at the top of the hour
#SplAisWspd     = merge(SplAisWspd, WSPD, all = FALSE, all.x = TRUE, by = "DateF" )#single value at this 10 minute interval

#SUN ALTITUDE-- append to wind
tmp = getSunlightPosition(SplAisWspd$DateF ,siteLoc[1],siteLoc[2])
SplAisWspd$sunAlt = tmp$altitude

SplAisWspdTide = merge(SplAisWspd, TIDE, all = FALSE, all.x = TRUE, by = "DateF" )
cat("Check formats match: AIS-", as.character(SplAis$DateF[2]), " WSPD-", as.character(WSPD$DateF[2]), " TIDE-", as.character(TIDE$DateF[2]))

rm(WSPD,WSPDavg,TIDE,AIS)

# COMBINE VESSdet 
#-----------------------------------------------------------------------------------------
# for each each hour, how many unique vessels detected, and percent of hour dominated by vessel noise
# some of the time will spill into the next hour, need to keep track of this
# reformat so for each hour there is a total vessel time value and total vessel count
VESS$DateFstart = as.Date(VESS$DateFs)
VESS$HrS =hour(VESS$DateFs)
VESS$HrE =hour(VESS$DateFe)
#make new matrix to fill in the values for each hour
VESSfor = as.data.frame( seq(from=as.POSIXct(min(VESS$DateFstart), tz="GMT"), 
                             to=as.POSIXct(max(VESS$DateFstart), tz="GMT"), by="hour") )
VESSfor$totalVessels = 0
VESSfor$totalTime = 0
colnames(VESSfor) = c("DayHr","totalVessels","totalTime")
VESSfor$DayHr = force_tz(VESSfor$DayHr, tzone="GMT")
VESSfor$DateF = as.Date(VESSfor$DayHr)

#clean up VESS... sort and remove duplicates
VESSa = arrange(VESS, by_group = VESS$DateFs )
VESS = VESSa
VESSd = dplyr::distinct(VESS)
VESS = VESSd

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

VESSfor$totalTimeM = VESSfor$totalTime/(60) #number of hours dominated by ships
errorC = VESSfor[VESSfor$totalTimeM > 60,] # CHECk: hist(VESSfor$totalTime)

VESSfor$DateF = VESSfor$DayHr
SplAisWspdTideVess = merge(SplAisWspdTide, VESSfor, all = FALSE, all.x = TRUE, by = "DateF" )
SplAisWspdTideVess$JulDay = yday(SplAisWspdTideVess$DateFday.x)

rm(VESS,VESSa,VESSd,VESSfor,tmp)

# COMBINE Biologics-- need to check this to see what detection outputs are!!
# west coast- bluewhales call detections
#-----------------------------------------------------------------------------------------
SplAisWspdTideVess$bluewhaleDet = NA
for (ii in 1:nrow(SplAisWspdTideVess)){
  tmp1 = nrow( bluewhale[bluewhale$DayHR == SplAisWspdTideVess$DateF[ii],] )
  if (tmp1 > 0){ #total detection on that day
    tmp2 = bluewhale[bluewhale$DayHR == SplAisWspdTideVess$DateF[ii],]
    SplAisWspdTideVess$bluewhaleDet[ii] = sum(as.numeric(as.character(tmp2$bluewhaleP)))
  } else { 
    SplAisWspdTideVess$bluewhaleDet[ii] = 0 } #no detections on that day
}
SplAisWspdTideVessBio = SplAisWspdTideVess

# COMBINE Explosions-- like vessel detections- start and end times
#-----------------------------------------------------------------------------------------
EXPL$DateFstart = as.Date(EXPL$DateFs)
EXPL$HrS =hour(EXPL$DateFs)
EXPL$HrE =hour(EXPL$DateFe)
#clean up EXPL sort and remove duplicates
EXPLa = arrange(EXPL, by_group = EXPL$DateFs )
EXPL = EXPLa
EXPLd = dplyr::distinct(EXPL)
EXPL = EXPLd

#make new matrix to fill in the values for each hour
EXPLfor = as.data.frame( seq(from=as.POSIXct(min(EXPL$DateFstart), tz="GMT"), 
                             to  =as.POSIXct(max(EXPL$DateFstart), tz="GMT"), by="hour") )
EXPLfor$totalExpl = 0
EXPLfor$totalExplTime = 0
colnames(EXPLfor) = c("DayHr","totalExpl","totalExplTime")
EXPLfor$DayHr = force_tz(EXPLfor$DayHr, tzone="GMT")
EXPLfor$DateF = as.Date(EXPLfor$DayHr)

#for each explosion detection, put in correct column in VESSfor
for (vv in  1:nrow(EXPL)){ #nrow(EXPL)  1:nrow(EXPL) 
  tmp = EXPL[vv,] #temp matrix
  tmp$DateFend = as.Date(tmp$DateFe) #needed end data in case it goes to the next day!
  
  #find column with matching day/hour
  idx = which( as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" ) == EXPLfor$DayHr) 
  
  stH = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" )
  edH = as.POSIXct ( paste0(tmp$DateFend, " "  , tmp$HrE,":00:00"), tz = "GMT" )
  hrsSpan =  as.numeric(edH - stH) # how many hours does the vessel detection dominate?
  
  if ((hrsSpan) == 1 )#spans to next hour
  {
    #add info to start hour
    EXPLfor$totalExpl[idx]  = EXPLfor$totalExpl[idx] + 1 # vessel count
    EXPLfor$totalExplTime[idx]     = totalExplTime$totalTime[idx] + difftime(edH, tmp$DateFs, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
    
    #add info to next hour
    EXPLfor$totalExpl[idx+1]  = EXPLfor$totalExpl[idx+1] + 1 # vessel count
    EXPLfor$totalExplTime[idx+1]     = EXPLfor$totalExplTime[idx+1] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds  difftime(tmp$DateFe, edH, units = "mins")
    
  } else if ((hrsSpan) == 0 ){ #within single hour
    EXPLfor$totalExpl[idx]  = EXPLfor$totalExpl[idx] + 1 # vessel count
    EXPLfor$totalExplTime[idx]     = EXPLfor$totalExplTime[idx] + difftime(tmp$DateFe,tmp$DateFs,  units = "secs") #time in seconds
    
  } else if  ((hrsSpan) >= 2 ) { #spans two or more hours
    
    midHr = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS+1,":00:00"), tz = "GMT" )
    
    #add info to start hour
    EXPLfor$totalExpl[idx]  = EXPLfor$totalExpl[idx] + 1 # vessel count
    EXPLfor$totalExplTime[idx]     = EXPLfor$totalExplTime[idx] + difftime(midHr, tmp$DateFs, units = "secs")
    
    #add info to middle hours--- need to loop this though each hour
    for(hh in 1:((hrsSpan)-1) ) {
      EXPLfor$totalExpl[idx+hh]  = EXPLfor$totalExpl[idx+hh] + 1 # vessel count
      EXPLfor$totalExplTime[idx+hh]     = EXPLfor$totalExplTime[idx+hh] + 3600 #time in seconds, full hour
    }
    
    #add info to last hour
    VESSfor$totalExpl[idx+hrsSpan]  = VESSfor$totalExpl[idx+hrsSpan] + 1 # vessel count
    VESSfor$totalExplTime[idx+hrsSpan]     = VESSfor$totalExplTime[idx+hrsSpan] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds
  }
  
  cat("Hours for detection ", vv, ": ", hrsSpan, "hrs", "\n")
}

EXPLfor$totalExplTimeM = EXPLfor$totalExplTime/(60) #number of hours dominated by ships
errorC = EXPLfor[EXPLfor$totalExplTimeM > 60,] # CHECk: hist(EXPLfor$totalExplTimeM)

EXPLfor$DateF = EXPLfor$DayHr
SplAisWspdTideVessBioExpl = merge(SplAisWspdTideVessBio, EXPLfor, all = FALSE, all.x = TRUE, by = "DateF" )

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#CLEAN UP big matrix-- NEED TO MAKE SURE COLUMNS ARE IN THE SAME ORDER!!!
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
dataALL = SplAisWspdTideVessBioExpl
dataALL$DayHr = dataALL$DateF
#!!! NEED TO MAKE SURE COLUMNS ARE IN THE SAME ORDER!!!
#  Site, Deployment, Day, DayHr, Year, Julian, SPLs, AIS, WSPD, TIDE--2, SunALT, vess det-2, bio det)
head( dataALL )
dCols = data.frame(colnames(dataALL))
dCols
dataALL$Yr = year(dataALL$DateF)
dataALL$Site = deply
dataALL$JulDay = yday(dataALL$DateF)
dataALL$Day = as.Date(dataALL$DateF)

a = as.numeric(which( colnames(dataALL)=="Site" ))
b = as.numeric(which( colnames(dataALL)=="deploy" ))
c = as.numeric(which( colnames(dataALL)=="Day" ))
d = as.numeric(which( colnames(dataALL)=="DayHr" ))
e = as.numeric(which( colnames(dataALL)=="Yr" ))
f = as.numeric(which( colnames(dataALL)=="JulDay" ))
g = as.numeric(grep("^OL", colnames(dataALL) ))
h = as.numeric(grep("^BB", colnames(dataALL) ))
i = as.numeric(grep("^LOA", colnames(dataALL) ))
j = as.numeric(grep("*WSPD", colnames(dataALL) ))
k = as.numeric(grep("Verified ", colnames(dataALL) ))
l = as.numeric(grep("changeWL", colnames(dataALL) ))
m = as.numeric(grep("sunAlt", colnames(dataALL) ))
n = as.numeric(grep("totalTime", colnames(dataALL) ))
o = as.numeric(grep("totalVessels", colnames(dataALL) ))
p = as.numeric(grep("*Det", colnames(dataALL) ))
q1 = as.numeric(grep("*Expl", colnames(dataALL) ))

dataALLc = select( dataALL, c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q1,p) )
colnames(dataALLc)
dCols = data.frame(colnames(dataALLc)) #
dCols

#THIS is hourly summary of data (AIS)... note that some variables are daily so this is not ideal (but ideal for analysis!!)
HrSumALL = dataALLc 

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#SUMMARIZE BY DAY
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
daySum1 = NULL 
uday = unique(HrSumALL$Day)
dIn = HrSumALL
g = as.numeric(grep("^OL", colnames(dIn) ))
h = as.numeric(grep("^BB", colnames(dIn) ))
p = as.numeric(grep("*Det", colnames(dIn) ))
q = as.numeric(grep("*Expl", colnames(dIn) ))

for (ii in 1:length(uday)){ # ii = 1
  dtmp = dIn[dIn$Day == uday[ii],]
  hrsample = nrow(dtmp)
  depl = dtmp$deploy[1]
  
  #AIS-- only first daily value because repeated!!
  Atotal    = dtmp$LOA_S_UV[1] + dtmp$LOA_M_UV[1] + dtmp$LOA_L_UV[1] + dtmp$LOA_NA_UV[1]
  Ahtotal   = dtmp$LOA_S_OPHRS[1] + dtmp$LOA_M_OPHRS[1] + dtmp$LOA_L_OPHRS[1] + dtmp$LOA_NA_OPHRS[1]
  perAtotal = Ahtotal/24 #Operation hours can be greater than hours in a day because of multipe ships in the area
  
  #Vessel detect--
  VDtotal = sum(dtmp$totalVessels,na.rm = T)  # not unique because we totaled per hour in previous step and some ships feel in multiple hours (fix??)
  VDperHr = mean(dtmp$totalVessels,na.rm = T) # vessel detections per hour
  VDper   = sum(dtmp$totalTime,na.rm = T)/(60*60*hrsample)  #percent of the day with vessels dominating
  
  #Expl detect--
  EXtotal = sum(dtmp$totalExpl,na.rm = T)  # not unique because we totaled per hour in previous step and some ships feel in multiple hours (fix??)
  EXperHr = mean(dtmp$totalExpl,na.rm = T) # vessel detections per hour
  EXper   = sum(dtmp$totalExplTime,na.rm = T)/(60*60*hrsample)  #percent of the day with vessels dominating
  
  #SPL-- median per octave band
  tmpSPL = select(dtmp,c(g,h))
  SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
  
  #WSPD-- average per day
  tst = all(is.na(dtmp$avgWSPD))
  if (tst == TRUE){
    WSPDm = NA
  }else {
    WSPDm  = mean(dtmp$avgWSPD, na.rm = T)
  }
  
  #TIDE-- mean hourly change
  tst = all(is.na(dtmp$changeWL))
  if (tst == TRUE){
    TIDEm = NA
  }else {
    TIDEm  = mean(as.numeric(as.character(dtmp$changeWL)), na.rm = T)
  }
  
  # BIO: already did day summary in BioAllo, so append at the end
  # OR Dolphins and Cod- summarise detections in that day and hours sampled
  # dolpinP, atlanticcodP,dolphinsPscale, atlanticcodPscale, Baleen whales- repeated for each hour, so just take first row 
  #tmpBIO = select(dtmp,(p))
  #BIOm = colSums(tmpBIO)
  
  #Combine to output
  daySum1 = rbind(daySum1,
                  c(dtmp$Site[1],dtmp$deploy[1],
                    as.character(dtmp$Day[1]), dtmp$Yr[1], dtmp$JulDay[1],
                    SPLm,
                    dtmp$LOA_S_UV[1], 
                    dtmp$LOA_S_OPHRS[1], 
                    dtmp$LOA_M_UV[1], 
                    dtmp$LOA_M_OPHRS[1], 
                    dtmp$LOA_L_UV[1], 
                    dtmp$LOA_L_OPHRS[1],
                    dtmp$LOA_NA_UV[1],
                    dtmp$LOA_NA_OPHRS[1],
                    Atotal, 
                    Ahtotal,
                    WSPDm,TIDEm,
                    VDtotal,VDperHr,VDper,
                    EXtotal,EXperHr,EXper,
                    nrow(dtmp)) )
  
  rm(dtmp,Atotal, Ahtotal, perAtotal,VDtotal,VDperHr,VDper,SPLm,WSPDm,TIDEm,depl,EXtotal,EXperHr,EXper)
}

#!!! NEED TO MAKE SURE COLUMNS ARE IN THE SAME ORDER!!!
#  Site, Deployment, Day, Year, Julian, SPLs, AIS, WSPD, TIDE--2, vess det-3, bio det, hourssamples)
daySum1 = as.data.frame(daySum1)
colnames(HrSumALL)
colnames(daySum1)[1:5]    = c(colnames(HrSumALL)[1:3],colnames(HrSumALL)[5:6]) #dates
colnames(daySum1)[17:26]  = c(colnames(HrSumALL)[18:27])                       #AIS
colnames(daySum1)[27:28]  = c(colnames(HrSumALL)[28], colnames(HrSumALL)[30])  #WSPD,TIDE
colnames(daySum1)[29:31]  = c("VessD_TV","VessD_VD_meanPerHr","VessD_Time_PerDay")
colnames(daySum1)[32:34]  = c("ExplD_TV","ExplD_VD_meanPerHr","ExplD_Time_PerDay")
colnames(daySum1)[35] = "totHRs"
daySum1$Day = as.Date(daySum1$Day )

# MERGE WITH DAILY BIO VALUES
daySumALL = merge(daySum1,BioAllo,by = "Day")

# MAKE NUMERIC VALUES
daySumALL[3:35] <- lapply( daySumALL[3:35], function(x) as.numeric(as.character(x)) )

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#TRUNCATE by time period of interest
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
analysisPeriods = read.csv("E:\\RESEARCH\\SanctSound\\data\\AnalysisPeriods.csv")
tmpAP = analysisPeriods[analysisPeriods$Site == deply,]
HrTrunData  = HrSumALL[ (HrSumALL$Day >= as.Date(tmpAP$DateStart) & HrSumALL$Day  <= as.Date(tmpAP$DateEnd) ),]
DayTrunData = daySumALL[(daySumALL$Day>= as.Date(tmpAP$DateStart) & daySumALL$Day <= as.Date(tmpAP$DateEnd) ),]

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#WRITE OUT FILES
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
setwd(outDir)
DC = Sys.Date()
fnameAll =  paste0(deply, "_CombinedDataHR_",as.character(min(as.Date(HrSumALL$Day))),"_",
                   as.character(max(as.Date(HrSumALL$Day))), "_v", DC, ".csv")  
write.csv(HrSumALL,fnameAll) 

fnameAllday =  paste0(deply, "_CombinedDataDAY_",as.character(min(as.Date(HrSumALL$Day))),"_",
                      as.character(max(as.Date(HrSumALL$Day))) , "_v", DC, ".csv")   
write.csv(daySumALL,fnameAllday)

fnameTrun =  paste0(deply, "_", tmpAP$Analysis,"DataHR_",as.character(tmpAP$DateStart),"_",as.character(tmpAP$DateEnd) , "_v", DC, ".csv")   
write.csv(HrTrunData,fnameTrun)

fnameTrun =  paste0(deply, "_", tmpAP$Analysis,"DataDay_",as.character(tmpAP$DateStart),"_",as.character(tmpAP$DateEnd) , "_v", DC, ".csv")  
write.csv(DayTrunData,fnameTrun)

#CHECK DATA INPUT PLOT-- too much to plot, need to break up
#grid.arrange(pSPL,pVes, pAISa,pAISaUV,pTide, pWind, ncol = 2, nrow = 3)
pSPLa = grid.arrange(pSPL,pSPLB, pVes, ncol = 3,nrow = 1)   #acoustic data
ggsave(paste0(outDir,"\\",deply,"_SPLSummary_created_",DC,".png"),pSPLa)
pAISt = grid.arrange(pAISa,pAISaUV,     ncol = 2,nrow = 1) #AIS
ggsave(paste0(outDir,"\\",deply,"_AISSummary_created_",DC,".png"),pAISt)
pEVI = grid.arrange(pTide,pWind, ncol = 2,nrow = 1) #environmental data
ggsave(paste0(outDir,"\\",deply,"_ENVSummary_created_",DC,".png"),pEVI)
ggsave(paste0(outDir,"\\",deply,"_BIOSummary_created_",DC,".png"),pBIO)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#VISUALIZE DATA-- similar to AGU presentation, but not all data so plotted by monthly
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#truncate data to 17 May (138) to June 16 (168)
daySum1$JulDay = as.numeric(as.character(daySum1$JulDay))
#dataInt = daySum1[daySum1$JulDay >= 137 & daySum1$JulDay <= 163 ,]
dataInt = daySum1
dataInt$AIS_OPHRS = as.numeric(as.character(dataInt$LOA_ALL_OPHRS))
dataInt$AIS_UV    = as.numeric(as.character(dataInt$LOA_ALL_UV))
dataInt$VessD_Time_PerDay = as.numeric(as.character(dataInt$VessD_Time_PerDay))
dataInt$mOB_125 = as.numeric(as.character(dataInt$mOB_125))
dataInt$Year    = as.factor(dataInt$Year )
dataInt$mth = month(dataInt$Day)
unique(dataInt$mth )
pDet = ggplot(dataInt, aes(y= VessD_Time_PerDay*100, x=as.factor(mth), color=as.factor(mth))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=2, color="black") +
  xlab("") +  ylab("") + ggtitle("% of day with close-by vessel noise (DET)")+
  ylim(c(0,100))+
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA), 
        plot.margin = unit(c(1,2,2,2), "lines") )  

pAISa = ggplot(dataInt, aes(y=AIS_OPHRS, x=as.factor(mth), color=as.factor(mth)) ) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=2, color="black") +
  xlab("") +  ylab ("") + ggtitle("Operation hours (AIS)")+
  ylim(c(0,200))+
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(1,2,2,2), "lines")  ) 

pSPL = ggplot(dataInt, aes(y=mOB_125,x=as.factor(mth), color=as.factor(mth)) ) +  
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=2, color="black") +
  xlab("") +   ylab ("") + ggtitle(paste(deply, "- Low frequency sound levels (125 Hz octave band)"))+ #median [125 Hz]
  ylim(c(85,100)) + #labs(caption = "May 17 - June 12")+
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        plot.caption = element_text(color = "black", face = "italic",hjust = 0),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(1,2,2,2), "lines")  ) 

pAISb = ggplot(dataInt, aes(y= AIS_UV, x=as.factor(mth), color=as.factor(mth)) ) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=2, color="black") +
  xlab("") +  ylab ("") + ggtitle("# of vessels tracking close to recorder (AIS)")+
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(1,2,2,2), "lines")  )              
library(gridExtra)
grid.arrange(pSPL, pDet, pAISb, pAISa, ncol = 1, nrow = 4)
