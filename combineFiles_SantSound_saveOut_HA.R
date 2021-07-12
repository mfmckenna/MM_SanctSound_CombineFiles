#Integrating SanctSound data-- and some check plots

# write out csv files with full SPL integrated with other metrics
# Tide, wind, sun altitude, AIS, vessel dets, biological dets
# created by site because detectors version are different

#this version works for HI01

rm(list=ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(suncalc)

#SET working directory-- currently runs per site
#-----------------------------------------------------------------------------------------
siteLoc = c(20.8075666666666,-156.65615)
site  = 'HI' 
deply = "HI01"
dir   = paste0("D:\\RESEARCH\\SanctSound\\data\\",site)
dir2  = paste0("D:\\RESEARCH\\SanctSound\\data\\",site,"\\", deply)
setwd(dir)
DC = Sys.Date()
outDir = "D:\\RESEARCH\\SanctSound\\data\\combineFiles_dets"

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#PART I:READ IN in data
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#SPL-- hourly octave band levels
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2, pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE))
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
SPL$`yyyy-mm-ddTHH:MM:SSZ`
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
SPLBB$`yyyy-mm-ddTHH:MM:SSZ`
SPLBB$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPLBB$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
SPLBB$DateFday  = as.Date(SPL$DateF)
#Data check plot
SPLBB$`BB_20-24000`
pSPLB = ggplot(SPLBB, aes(DateF, `BB_20-24000`, color = deploy))+
  geom_point() +
  xlab("")+
  ylab("SPL-BB") +
  theme_minimal()+
  ggtitle(paste( "Check SPL (", deply, ")" ))
pSPLB

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
  xlab("Julian Day")+
  scale_color_manual(values=c('#E69F00','#56B4E9'),name = "") +
  #scale_colour_discrete(name = "")+
  ylim(c(0,150))+
  theme(legend.position="none")
pAISm = ggplot(AIS, aes(DateFJul, LOA_M_OPHRS, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  xlab("Julian Day")+
  ylim(c(0,150))+
  theme(legend.position=c(0.2, 0.9))
pAISl = ggplot(AIS, aes(DateFJul, LOA_L_OPHRS, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  xlab("Julian Day")+
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
  xlab("Julian Day")+
  theme(legend.position=c(0.2, 0.9))
pAISmUV = ggplot(AIS, aes(DateFJul, LOA_M_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,50))+
  xlab("Julian Day")+
  theme(legend.position="none")
pAISlUV = ggplot(AIS, aes(DateFJul, LOA_L_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,50))+
  xlab("Julian Day")+
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
#-----------------------------------------------------------------------------------------
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

# DETECTIONS-- biological- snapping shrimp,humpback
#-----------------------------------------------------------------------------------------
#need to find which files are available
lstBio = NULL
nFiles = length( list.files(path=dir2,pattern = "humpbackwhale", full.names=TRUE, recursive = TRUE))
if (nFiles != 0){ lstBio = c(lstBio,"humpbackwhale") }
nFiles = length( list.files(path=dir2,pattern = "1000hz", full.names=TRUE, recursive = TRUE))
if (nFiles != 0){ lstBio = c(lstBio,"1000hz") }
lstBio

#LOOP through detection files by type (wrks: bluewhale,FishCalls) 
for (ii in 1:length(lstBio) ) {
  nFiles = length( list.files(path=dir2,pattern = lstBio[ii], full.names=TRUE, recursive = TRUE) )
  bio = NULL
  cat("Processing...", lstBio[ii],"Files = ",as.character(nFiles),"\n")
  
  if (lstBio[ii] == "humpbackwhale"){
    for (ff in 1 : nFiles){
      fname  = list.files(path=dir2, pattern = lstBio[ii], full.names=FALSE, recursive = TRUE)[ff]
      fnameF = list.files(path=dir2, pattern = lstBio[ii], full.names=TRUE, recursive = TRUE)[ff]
      dname  = sapply (strsplit( fname, "_" ),"[",3 )
      tmp    = read.csv(fnameF) #remove these after
      tmp$deply = dname
      bio = rbind(bio,tmp)
      rm(fname,fnameF,tmp)
      }
  }else{
    for (ff in 1 : nFiles){
      fname  = list.files(path=dir2, pattern = lstBio[ii], full.names=FALSE, recursive = TRUE)[ff]
      fnameF = list.files(path=dir2, pattern = lstBio[ii], full.names=TRUE, recursive = TRUE)[ff]
      dname = sapply (strsplit( fname, "_" ),"[",3 )
      tmp  = read.csv(fnameF) #remove these after
      tmp$deply = dname
      bio = rbind(bio,tmp)
      rm(fname,fnameF,tmp) }
  }
  
  #this varies by detection type.... will need to test for other file formats!!!
  if (lstBio[ii] == 'humpbackwhale') { #this is individual detection times
    colnames(bio)= c( "Time","Det_PropGoogleDet","deply")
    bio$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", bio$Time)), tz = "GMT" )
    bio$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", bio$Time)), tz = "GMT" )
    bio$DayHR    = as.POSIXct( paste0(as.Date( bio$DateFs )," ", hour( bio$DateFs ),":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    bio$DateDay = as.Date( bio$DateFs )
    bio$Type = lstBio[ii]
    bio = bio[,2:8]
    
  }else if (lstBio[ii] == '1000hz'){ 
    names(bio) = c( "Time","Det_SPL200-1000","deply") #rename headers
    bio$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", bio$Time)), tz = "GMT" )
    bio$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", bio$Time)), tz = "GMT" )
    bio$DayHR    = as.POSIXct( paste0(as.Date( bio$DateFs )," ", hour( bio$DateFs ),":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    bio$DateDay = as.Date( bio$DateFs )
    bio$Type = lstBio[ii]
    #only keep certain headings-- reorder to match bluewhale
    bio = bio[,2:8]
  }
  
  assign( lstBio[ii] , bio) 
  bio=as.data.frame(bio)
  #check plot
  pBio = ggplot(bio,aes(DateDay, bio[,1] ) )  +
    geom_point()+
    ylab(paste0(lstBio[ii],"Det"))
  pBio
  
}

# DETECTIONS-- anthropogenic- ships
#-----------------------------------------------------------------------------------------
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
VESS$DayHr    = as.POSIXct( paste0(as.Date( VESS$DateFs )," ", hour( VESS$DateFs ),":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
VESS$DateDay = as.Date( VESS$DateFs )
#Data check plot
pVes = ggplot(VESS, aes(DateFs, DUR_mins))+
  geom_point() +
  xlab("")+
  ylab("Duration Vessel Detections (mins)") +
  theme_minimal()+
  ggtitle(paste( "Check vessel detections (", deply, ")" ))
pVes

VESS$Det = "vessel"
VESS$Presence = 1
VESS$deply = NA
names(VESS)[names(VESS) == "Presence"] <- "vesselDet"
VESS = VESS[,c(11,4:5,7:9)]
VESS$Presence = 1

#-----------------------------------------------------------------------------------------
#### CLEAN UP #####
rm(dname, endR,ff,nFiles,ii)
#### CLEAN UP #####
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#PART II:  COMBINE: for each SPL time step, find associated metrics AIS, WSPD, TIDE, VESS using formated date
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

#Detections by hour-- just update vessels because humpback/ shrimp are hourly values
#-----------------------------------------------------------------------------------------
humpbackwhale$DateF = humpbackwhale$DayHR
`1000hz`$DateF = `1000hz`$DayHR
m1 = merge(SplAisWspdTide,humpbackwhale, all = FALSE, all.x = TRUE, by = "DateF" )
SplAisWspdTidebio = merge(m1,`1000hz`, all = FALSE, all.x = TRUE, by = "DateF" )

#list of possible DayHours to find vessel detections in
tmpBfor = as.data.frame( seq(from=as.POSIXct(min(SPL$DateF), tz="GMT"), 
                             to  =as.POSIXct(max(SPL$DateF), tz="GMT"), by="hour") )

names(tmpBfor) = "DayHr"
tmpBfor$Totals = 0
tmpBfor$TotalTime = 0
#vessels!
for (vv in  1:nrow(VESS)){ #loop through each detection, and put in correct column
  tmp = VESS[vv,] #temp matrix tmp$DayHR
  tmp$HrS = hour(tmp$DateFs)
  tmp$HrE = hour(tmp$DateFe)
  
  #find column with matching day/hour
  idx = which(  tmp$DayHr  == tmpBfor$DayHr) 
  stH = as.POSIXct ( paste0(as.Date(tmp$DateFs), " ", tmp$HrS,":00:00"), tz = "GMT" )
  edH = as.POSIXct ( paste0(as.Date(tmp$DateFe), " ", tmp$HrE,":00:00"), tz = "GMT" )
  hrsSpan =  as.numeric(edH - stH) # how many hours does the vessel detection dominate?
  if ((hrsSpan) == 1 )#spans to next hour
  {
    #add info to start hour
    tmpBfor$Totals[idx]  = tmpBfor$Totals[idx] + 1 #  count
    tmpBfor$TotalTime[idx]  = tmpBfor$TotalTime[idx] + difftime(edH, tmp$DateFs, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
    #add info to next hour
    tmpBfor$Totals[idx+1]    = tmpBfor$Totals[idx+1] + 1 #  count
    tmpBfor$TotalTime[idx+1] = tmpBfor$TotalTime[idx+1] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds  difftime(tmp$DateFe, edH, units = "mins")
    
  } else if ((hrsSpan) == 0 ){ #within single hour
    
    tmpBfor$Totals[idx]     = tmpBfor$Totals[idx] + 1 #  count
    tmpBfor$TotalTime[idx]  = tmpBfor$TotalTime[idx] + difftime(tmp$DateFe,tmp$DateFs,units = "secs") #time in seconds
    
  } else if  ((hrsSpan) >= 2 ) { #spans two or more hours
    
    midHr = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS+1,":00:00"), tz = "GMT" )
    #add info to start hour
    tmpBfor$Totals[idx]    =  tmpBfor$Totals[idx] + 1 # vessel count
    tmpBfor$TotalTime[idx] = tmpBfor$TotalTime[idx] + difftime(midHr, tmp$DateFs, units = "secs")
    #add info to middle hours--- need to loop this though each hour
    for(hh in 1:((hrsSpan)-1) ) {
      tmpBfor$Totals[idx+hh]    = tmpBfor$Totals[idx+hh] + 1 # vessel count
      tmpBfor$TotalTime[idx+hh] = tmpBfor$TotalTime[idx+hh] + 3600 #time in seconds, full hour
    }
    
    #add info to last hour
    tmpBfor$Totals[idx+hrsSpan]    = tmpBfor$Totals[idx+hrsSpan] + 1 # vessel count
    tmpBfor$TotalTime[idx+hrsSpan] = tmpBfor$TotalTime[idx+hrsSpan] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds
  }
}#end loop detections
#rename some columns to help with data merging
tmpBfor$DateF = tmpBfor$DayHr
names(tmpBfor)[2] = "Det_TotalVessels"
names(tmpBfor)[3] = "Det_TotalTimeVessels"
SplAisWspdTideBioVes = merge(SplAisWspdTidebio,tmpBfor, all = FALSE, all.x = TRUE, by = "DateF" )

#-----------------------------------------------------------------------------------------
#CLEAN UP big matrix-- NEED TO MAKE SURE COLUMNS ARE IN THE SAME ORDER!!!
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
dataALL = SplAisWspdTideBioVes

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
o = as.numeric(grep("Det", colnames(dataALL) ))

dataALLc = select( dataALL, c(a,b,c,d,e,f,g,h,i,j,k,l,m,o) )
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

for (ii in 1:length(uday)){ # ii = 1
  dtmp = dIn[dIn$Day == uday[ii],]
  hrsample = nrow(dtmp)
  depl = dtmp$deploy[1]
  
  #AIS-- only first daily value because repeated!!
  Atotal    = dtmp$LOA_S_UV[1] + dtmp$LOA_M_UV[1] + dtmp$LOA_L_UV[1] + dtmp$LOA_NA_UV[1]
  Ahtotal   = dtmp$LOA_S_OPHRS[1] + dtmp$LOA_M_OPHRS[1] + dtmp$LOA_L_OPHRS[1] + dtmp$LOA_NA_OPHRS[1]
  perAtotal = Ahtotal/24 #Operation hours can be greater than hours in a day because of multipe ships in the area
  
  #SPL-- median per octave band
  tmpSPL = select(dtmp,c(g,h))
  SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
  
  #WSPD-- average per day
  tst = all(is.na(dtmp$avgWSPD))
  if (tst == TRUE){ WSPDm = NA }else { WSPDm  = mean(dtmp$avgWSPD, na.rm = T)   }
  
  #TIDE-- mean hourly change
  tst = all(is.na(dtmp$changeWL))
  if (tst == TRUE){ TIDEm = NA }else { TIDEm  = mean(as.numeric(as.character(dtmp$changeWL)), na.rm = T)   }
  
  #DETECTIONS--
  Det_TotalVessels       = sum(dtmp$Det_TotalVessels,na.rm = T)      # not unique because we totaled per hour in previous step and some ships feel in multiple hours (fix??)
  Det_mTotalTimeVessels   = mean(dtmp$Det_TotalTimeVessels,na.rm = T) # mean vessel detections per hour
  Det_TotalTimeVessels   = sum(dtmp$Det_TotalTimeVessels,na.rm = T)/(60*60*hrsample)  #percent of the day with vessels dominating
  Det_PropGoogleDet = mean(as.numeric(as.character(dtmp$Det_PropGoogleDet)))
  `Det_SPL200-1000` = median(as.numeric(as.character(dtmp$`Det_SPL200-1000`)))
  
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
                    Det_TotalVessels,Det_mTotalTimeVessels,Det_TotalTimeVessels,
                    Det_PropGoogleDet,`Det_SPL200-1000` ,
                    nrow(dtmp)) )
  
  rm(dtmp,Atotal, Ahtotal, perAtotal,SPLm,WSPDm,TIDEm,depl)
}

#!!! NEED TO MAKE SURE COLUMNS ARE IN THE SAME ORDER!!!
#  Site, Deployment, Day, Year, Julian, SPLs, AIS, WSPD, TIDE--2, detections, hourssamples)
daySum1 = as.data.frame(daySum1)
colnames(HrSumALL)
colnames(daySum1)[1:5]    = c(colnames(HrSumALL)[1:3],colnames(HrSumALL)[5:6]) #dates
colnames(daySum1)[17:26]  = c(colnames(HrSumALL)[18:27])                       #AIS
colnames(daySum1)[27:28]  = c(colnames(HrSumALL)[28], colnames(HrSumALL)[30])  #WSPD,TIDE
colnames(daySum1)[29:31]  = c("VessD_TV","VessD_VD_meanPerHr","VessD_Time_PerDay")
colnames(daySum1)[32:33]  = c("Det_PropGoogleDet","Det_ShrimpSPL")
colnames(daySum1)[34] = "totHRs"


# MAKE NUMERIC VALUES
daySum1[4:34] <- lapply( daySum1[4:34], function(x) as.numeric(as.character(x)) )
daySumALL = daySum1
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#TRUNCATE by time period of interest
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
analysisPeriods = read.csv("D:\\RESEARCH\\SanctSound\\data\\AnalysisPeriods.csv")
tmpAP = analysisPeriods[analysisPeriods$Site == deply,]
HrTrunData  = HrSumALL[ (HrSumALL$Day >= as.Date(tmpAP$DateStart) & HrSumALL$Day  <= as.Date(tmpAP$DateEnd) ),]
DayTrunData = daySumALL[(as.Date(daySumALL$Day)>= as.Date(tmpAP$DateStart) & as.Date(daySumALL$Day) <= as.Date(tmpAP$DateEnd) ),]

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#WRITE OUT FILES
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
setwd(outDir)
DC = Sys.Date()
fnameAll =  paste0(deply, "_CombinedDataHR_dets_",as.character(min(as.Date(HrSumALL$Day))),"_",
                   as.character(max(as.Date(HrSumALL$Day))), "_v", DC, ".csv")  
write.csv(HrSumALL,fnameAll) 

fnameAllday =  paste0(deply, "_CombinedDataDAY_dets_",as.character(min(as.Date(HrSumALL$Day))),"_",
                      as.character(max(as.Date(HrSumALL$Day))) , "_v", DC, ".csv")   
write.csv(daySumALL,fnameAllday)

fnameTrun =  paste0(deply, "_", tmpAP$Analysis,"DataHR_dets_",as.character(tmpAP$DateStart),"_",as.character(tmpAP$DateEnd) , "_v", DC, ".csv")   
write.csv(HrTrunData,fnameTrun)

fnameTrun =  paste0(deply, "_", tmpAP$Analysis,"DataDay_dets_",as.character(tmpAP$DateStart),"_",as.character(tmpAP$DateEnd) , "_v", DC, ".csv")  
write.csv(DayTrunData,fnameTrun)

#CHECK DATA INPUT PLOT-- too much to plot, need to break up
#grid.arrange(pSPL,pVes, pAISa,pAISaUV,pTide, pWind, ncol = 2, nrow = 3)
pSPLa = grid.arrange(pSPL,pSPLB, ncol = 2,nrow = 1)   #acoustic data
ggsave(paste0(outDir,"\\",deply,"_SPLSummary_created_",DC,".png"),pSPLa)
pAISt = grid.arrange(pAISa,pAISaUV,     ncol = 2,nrow = 1) #AIS
ggsave(paste0(outDir,"\\",deply,"_AISSummary_created_",DC,".png"),pAISt)
pEVI = grid.arrange(pTide,pWind, ncol = 2,nrow = 1) #environmental data
ggsave(paste0(outDir,"\\",deply,"_ENVSummary_created_",DC,".png"),pEVI)
pHB = ggplot(HrSumALL, aes(Day, Det_PropGoogleDet)  )+
  geom_point()+
  ggtitle("Humpback whale Google AI detections")
pSS = ggplot(HrSumALL, aes(Day, `Det_SPL200-1000`)  )+
  geom_point()+
  ggtitle("Snapping Shrimp SPL (200-1000Hz)")
pVD = ggplot(HrSumALL, aes(Day, Det_TotalTimeVessels)  )+
  geom_point()+
  ggtitle("Vessel Detections-- total time")
pDET = grid.arrange(pHB,pSS, pVD, ncol = 1,nrow = 3) #environmental data
ggsave(paste0(outDir,"\\",deply,"_DETSummary_created_",DC,".png"),pDET)
