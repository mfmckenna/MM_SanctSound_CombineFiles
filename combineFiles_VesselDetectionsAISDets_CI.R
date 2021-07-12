# process sancturary sound vessel detection data and AIS results
rm(list=ls())

#-----------------------------------------------------------------------------------------
# MAIN functions-- by site
#-----------------------------------------------------------------------------------------
# reads in output from: process_VesselDetectionsAIS.R to incoporate detections....
# use combineFiles_SantSound_saveOut_MB.R to bring in detection

#combines ALL data for the sanctuary
# 1) SPL, AIS, Vessel detections-- from process_VesselDetectionsAIS.R
# 2)Abiotic-- from combineFiles_SantSound_saveOut_noDet.R, 
#check first to see if files exist, but only short period from Frontiers
# 3) biological detections-- added here!!


# SITE DETAILS.... site because detection files differ
#-----------------------------------------------------------------------------------------
dirSite = "E:\\RESEARCH\\SanctSound\\data\\CI"
sanctuary = "CI"
sites = c("01","02","03","04","05") #list sites
dir1     = "E:\\RESEARCH\\SanctSound\\data2\\combineFileEffort_SplAisVesDet\\"
outDir ="E:\\RESEARCH\\SanctSound\\data2\\combineFiles_bySite\\"

#-----------------------------------------------------------------------------------------
# TO ADD (future versions)
#-----------------------------------------------------------------------------------------
# currently truncates data based on available wind and tide, so if mot all data downloaded it will not merge!!!

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
library(plotly)  #https://www.r-graph-gallery.com/interactive-charts.html

#-----------------------------------------------------------------------------------------
# SETUP parameters
#-----------------------------------------------------------------------------------------
ra = 7 #days for running average
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

#-----------------------------------------------------------------------------------------
# OUTPUT details
#-----------------------------------------------------------------------------------------

DC = Sys.Date()
#range of dates for output graphics
eDatePlot = '2020-12-31'
sDatePlot = '2018-11-01'
flagCSV = TRUE #true if you want to ouptut hourly and daily csv files per site

#-----------------------------------------------------------------------------------------
# FIND AND READ IN-- 1) SPL, AIS, Vessel detections-- from process_VesselDetectionsAIS.R
# process both daily and hourly files
#-----------------------------------------------------------------------------------------
inFiles =       ( list.files(path=dir1, pattern = sanctuary, full.names=TRUE, recursive = TRUE))
#daily files
dyFiles = Filter(function(x) grepl("Day", x), inFiles)
inFiles1 = basename(dyFiles)
x = strsplit(inFiles1,"_")
sitesVD = unique(sapply( x, "[", 1 ))
dataInDay = NULL
tmp = read.csv(dyFiles[1])
cnames = colnames(tmp)
for (ff in 1: length(dyFiles) )
{
  tmp = read.csv(dyFiles[ff],header=F,stringsAsFactors = FALSE)[-1,]
  dataInDay = rbind(dataInDay, tmp)
}
colnames(dataInDay) = cnames
sitesIn = unique(dataInDay$Site)
# length(unique(dataInDay$Day)) - nrow(dataInDay)... repeated values because all sites are combined!!

dataInDay$Day   = as.Date(dataInDay$Day)
dataInDay2 = dataInDay
dataInDay2[,5:ncol(dataInDay)] = lapply(dataInDay[,5:ncol(dataInDay)], function(x) as.numeric(as.character(x)))
dataInDay = dataInDay2
rm(tmp,x,dataInDay2)

#hourly files
hrFiles = Filter(function(x) grepl("Hr", x), inFiles)
inFiles1 = basename(hrFiles)
x = strsplit(inFiles1,"_")
sitesVD = unique(sapply( x, "[", 1 ))
cat(sanctuary, ":", length(sitesVD), " sites to process", "\n")
rm(x)

dataInHr = NULL
tmp = read.csv(hrFiles[1])
cnames = colnames(tmp)
for (ff in 1: length(hrFiles) )
{
  tmp = read.csv(hrFiles[ff],header=F,stringsAsFactors = FALSE)[-1,] #deal with slightly different column names!
  dataInHr = rbind(dataInHr, tmp)
}
colnames(dataInHr) = cnames
sitesIn = unique(dataInHr$Site)
dataInHr$Day = as.Date(dataInHr$Day)
# need to reformat all columns... 
dataInHr$Day = as.Date(dataInHr$Day )
dataInHr$DateF = as.POSIXct(as.character(dataInHr$DateF), tz = "GMT")
dataInHr[,6:ncol(dataInHr)] = lapply(dataInHr[,6:ncol(dataInHr)], function(x) as.numeric(as.character(x)))
rm(tmp,cnames)

#CHECK: use this to compare results with when merging data to make sure not loosing/dropping data!!!!
Jul16Hr = dataInHr[ dataInHr$Day == "2019-07-16",]
Jul16Dy = dataInDay[ dataInDay$Day == "2019-07-16",]

#-----------------------------------------------------------------------------------------
# FIND and READ IN-- wind, tide, detections
#-----------------------------------------------------------------------------------------

#WIND-- mps (10 minutes)
#-----------------------------------------------------------------------------------------
multmerge = function(path){
  filenames = list.files(path=path, pattern = "^SST", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
# list.files(path=dirSite, pattern = "^SST", full.names=TRUE, recursive = TRUE)

WIND <- multmerge(dirSite)
#format date
WIND$Day = as.Date( paste( WIND$YYyr, "/", WIND$MMmo, "/",WIND$DDdy, " ",WIND$hhhr, ":",WIND$mmmn,":00", sep = "") )
WIND$DateF2   = as.POSIXct( paste(WIND$Day, paste(WIND$hhhr, ":",WIND$mmmn,":00", sep = ""), sep=" "), tz = "GMT")
WIND$DateF    = as.POSIXct( paste(WIND$Day, paste(WIND$hhhr, ":00:00", sep = ""), sep=" "), tz = "GMT")
idxNA = which( WIND$WSPDm.s == 99 ) #recode missing data
WIND$WSPDm.s[idxNA] = NA

#Data check plot
pWIND = ggplot(WIND, aes(Day, as.numeric(as.character(WSPDm.s)) ) )+
  geom_point() +
  theme_minimal()
#pWIND  #2018 data looks strange-- clips at 6m/s

#summarize/ merge by day
WINDday = aggregate(as.numeric(as.character( WIND$WSPDm.s)),    by=list(WIND$Day), mean, na.rm=T)     #mean windspeed for day
colnames(WINDday) = c("Day","WSPDms_mean")
#merge with dataInDay 
dataInDay2 = merge(dataInDay, WINDday, by = "Day", sort = FALSE)
#not keeping same order as Day , so sort!
dataInDay2 = dataInDay2[order(dataInDay2$Site,dataInDay2$Day),]

#CHECK:  are there fewer rows in new matrix-- do I not have all the wind data, only data through 2019, so truncates deployment CI03_03!!!
#nrow(dataInDay2)- nrow(dataInDay)
#plot(dataInDay$X)
#plot(dataInDay2$X)
#CHECK:  to make sure wind same for all sites on same day!
# dataInDay2[dataInDay2$Day == "2018-12-15",]
cat("Date range: ",as.character( min(dataInDay2$Day) ), "to ", as.character(max(dataInDay2$Day)) )

#summarize/ merge with dataInHr
WINDhr = WIND[, c(19,20,21,7) ]
WINDhr2 = aggregate(as.numeric(as.character( WINDhr$WSPDm.s)),    by=list(WINDhr$DateF), mean, na.rm=T)     #mean windspeed for day
colnames(WINDhr2) = c("DateF","WSPDms_mean")
WINDhr2$DateF  = as.POSIXct ( WINDhr2$DateF )
#need to fix this because is no wind values... those rows are deleted!!
dataInHr2 = merge(dataInHr, WINDhr2, by = "DateF",all.x = TRUE)

rm(dataInDay, dataInHr, WIND, WINDday, WINDhr, WINDhr2 )

#TIDE-- water level (hourly)
#-----------------------------------------------------------------------------------------
multmerge = function(path){
  filenames = list.files(path=path, pattern = "Hr2", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
# list.files(path=dirSite, pattern = "Hr2", full.names=TRUE, recursive = TRUE)
TIDE <- multmerge(dirSite)
TIDE$Day = as.Date(TIDE$Date ,format = "%Y/%m/%d")
TIDE$DateF    = as.POSIXct( paste(TIDE$Day, paste(TIDE$`Time (GMT)`,":00", sep = ""), sep=" "), tz = "GMT")
TIDE$changeWL = c("NA", diff(TIDE$`Verified (ft)`))

TIDEdaym = aggregate(as.numeric(as.character( TIDE$changeWL) ),    by=list(TIDE$Day), mean, na.rm=T)     #mean waterlvel for day
colnames(TIDEdaym) = c("Day","changeWL_mean")
TIDEdaysd= aggregate(as.numeric(as.character( TIDE$changeWL) ),    by=list(TIDE$Day), sd, na.rm=T)       #sdev waterlevel for day
colnames(TIDEdaysd) = c("Day","changeWL_sd")
#merge with day
dataInDay3 = merge(dataInDay2, TIDEdaym, by = "Day")
dataInDay3 = merge(dataInDay3, TIDEdaysd, by = "Day")

#merge with hour
dataInHr3 = merge(dataInHr2, TIDE, by = "DateF")
rm(dataInDay2,dataInHr2, TIDE,TIDEdaym,TIDEdaysd)

Jul16Hr3 = dataInHr3[ dataInHr3$Day.x == "2019-07-16",] #CHECK!
Jul16Dy3 = dataInDay3[ dataInDay3$Day == "2019-07-16",] #CHECK!

#-----------------------------------------------------------------------------------------
#DETECTIONS-- 
#-----------------------------------------------------------------------------------------
#use this spreadhseet: 
# https://docs.google.com/spreadsheets/d/1QHK7s05MCwwE-QTgc6NwUGF0_mgOI-Tj39UOERoXlnQ/edit#gid=1517962691 (Global)
# c("blue","fin","humpback","midshipman","bocaccio","dolphins","snapping shrimp")  
# "sealions"-- only CI02
# "seabass"-- not ready yet
# c("vessels","explosions","sonar") 
# https://docs.google.com/spreadsheets/d/1cc-pZUB57PcaqgKI51qxxOjW28WdEVggXfds84bY1Fc/edit#gid=749032146 (what I have)
detectionsB = c("bluewhale","midshipman","bocaccio","FishCalls")  
detectionsO = c("Explosions")  

#BLUEWHALE detections: start time only
#-----------------------------------------------------------------------------------------
bFiles = list.files(path=dirSite, pattern = detectionsB[1], full.names=TRUE, recursive = TRUE)
bluewhale = NULL
for (dd in 1:length(bFiles) ){
  tmp = as.data.frame( read.csv(bFiles[dd]) )
  tmpSite  = strsplit  (basename(bFiles[dd]),"_")
  if (nrow(tmp)>0){
    tmp$Site = unique(sapply( tmpSite, "[", 2 ))
    tmp$Dep  = unique(sapply( tmpSite, "[", 3 ))
    #cat(colnames(tmp),"\n")
    if (ncol(tmp)< 4 ){
      bluewhale = rbind(bluewhale, tmp )
      
    } else {cat(paste0("No Detections: ",basename(bFiles[dd]),"\n"))} #skips files without detections
    
  } else   {cat(paste0("No Detections: ",basename(bFiles[dd]),"\n")) } #skips files without detections
  
}
bluewhale$DateDet =  as.POSIXct( substr( gsub("T", " ", bluewhale$Time),start = 1, stop = 19 ), tz = "GMT" )
bluewhale$DateF = as.POSIXct( paste0( substr( gsub("T", " ", bluewhale$Time),start = 1, stop = 14 ), "00:00"), tz = "GMT" )
bluewhale$Day = as.Date(bluewhale$DateF)
bluewhale$bluewhaleDet = 1
#SUMMARIZE: by hour/day
bluewhaleHr = aggregate(bluewhaleDet ~ DateF + Site, data = bluewhale, FUN = length)
bluewhaleDy = aggregate(bluewhaleDet ~ Day + Site,  data = bluewhale, FUN = length)
#MERGE: with data
dataInDay4 = merge( dataInDay3, bluewhaleDy, by=c("Day","Site") , all.x=TRUE)
dataInHr4  = merge( dataInHr3,  bluewhaleHr, by=c("DateF","Site"), all.x=TRUE)
#RECODE: times with no detections are NA and need to be zeros if data exists
dataInDay4$bluewhaleDet[is.na(dataInDay4$bluewhaleDet)] <- 0
idxNA = which (is.na (dataInDay4$OL_31.5))
dataInDay4$bluewhaleDet[idxNA] = NA
dataInHr4$bluewhaleDet[is.na(dataInHr4$bluewhaleDet)] <- 0
idxNA = which (is.na (dataInHr4$OL_31.5))
dataInHr4$bluewhaleDet[is.na(dataInHr4$bluewhaleDet)] <- 0
#CHECK: make sure merged to correct date AND site:  
Jul16Hr4 = dataInHr4[ dataInHr4$Day.x == "2019-07-16",] 
Jul16Dy4 = dataInDay4[ dataInDay4$Day == "2019-07-16",] 

#MIDSHIPMAN detections- Start/End times... GRR different column names AND date formates
#-----------------------------------------------------------------------------------------
bFiles = list.files(path=dirSite, pattern = "midshipman.csv", full.names=TRUE, recursive = TRUE)
midshipman = NULL
for (dd in 1:length(bFiles) ){
  tmp = as.data.frame( read.csv(bFiles[dd], head = FALSE) [-1,] )
  tmpSite  = strsplit  (basename(bFiles[dd]),"_")
  if (nrow(tmp)>0){
    #cat(as.character( tmp$V1[1]),"\n")
    tmp$Site = unique(sapply( tmpSite, "[", 2 ))
    tmp$Dep  = unique(sapply( tmpSite, "[", 3 ))
    #cat(colnames(tmp),"\n")
    #format date b/c different formates
    tstTime = as.POSIXct(  strptime( gsub("-", "/", tmp$V1[1]),format = "%m/%e/%Y %H:%M:%S.%OS" ), tz = "GMT" )
    if ( !is.na(tstTime)== TRUE ){
      tmp$Start = as.POSIXct(  strptime( gsub("-", "/", tmp$V1),format = "%m/%e/%Y %H:%M:%S.%OS" ), tz = "GMT" )
      tmp$End = as.POSIXct(  strptime( gsub("-", "/", tmp$V2),format = "%m/%e/%Y %H:%M:%S.%OS" ), tz = "GMT" )
    } else { cat(tstTime, "Fix", bFiles[dd],"\n") } #no detection files
    midshipman = rbind(midshipman, tmp[,3:6] )
  } else {cat(bFiles[dd],"\n")} #skips files without detections
  
}
midshipman$Dur = midshipman$End - midshipman$Start
midshipman$DateF  = as.POSIXct( paste0( substr( gsub("T", " ", midshipman$Start),start = 1, stop = 14 ), "00:00"), tz = "GMT" )
midshipman$DateFe = as.POSIXct( paste0( substr( gsub("T", " ", midshipman$End),start = 1, stop = 14 ), "00:00"), tz = "GMT" )
midshipman$Day    = as.Date(midshipman$DateF)
midshipman$Daye   = as.Date(midshipman$DateFe)

#use vessel deteciton method to combine with hour files, but multiple sites
#what if I sort by site AND date first-- YES
dataInHr4 = dataInHr4[order(dataInHr4$Site,dataInHr4$DateF),]
dataInHr4$midshipmanDet = 0
dataInHr4$midshipmanDetTime = 0
#hourly summary
for (vv in  1:nrow(midshipman) ){ # vv = 1
  
  tmp = midshipman[vv,] #temp matrix
  tmp$HrS = hour(tmp$DateF)
  tmp$HrE = hour(tmp$End)
  
  #find column with matching day/hour
  idx = which( tmp$DateF == dataInHr4$DateF & tmp$Site == dataInHr4$Site ) #match by day and site!
  
  if (length(idx) > 0){ #in some cases, detections are available but not the SPL data, so can't merge yet (will automatically update when run on all data)
    # how many hours does the detection dominate?
    hrsSpan =  as.numeric( difftime(tmp$DateFe , tmp$DateF,units = "hours") )
    
  }else { 
    cat("No match for detection: ",vv, as.character( tmp$DateFe ),"\n" )
    next
  }
  
  if ((hrsSpan) == 1 )#spans to next hour
  {
    #add info to start hour
    dataInHr4$midshipmanDet[idx]      = dataInHr4$midshipmanDet[idx] + 1 # detection count
    dataInHr4$midshipmanDetTime[idx]  = dataInHr4$midshipmanDetTime[idx] + difftime(tmp$Start, tmp$DateF, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
    
    #add info to next hour-- sometimes not available SPL data because last hours of period, so do a check
    if (is.na( dataInHr4$midshipmanDet[idx+1] ) ) {  }else {
      dataInHr4$midshipmanDet[idx+1]      = dataInHr4$midshipmanDet[idx+1] + 1 # detection count
      dataInHr4$midshipmanDetTime[idx+1]  = dataInHr4$midshipmanDetTime[idx+1] + difftime( tmp$End, tmp$DateFe, units = "secs") #time in seconds  difftime(tmp$DateFe, edH, units = "mins")
    }
    
  } else if ((hrsSpan) == 0 ){ #within single hour
    
    dataInHr4$midshipmanDet[idx]     = dataInHr4$midshipmanDet[idx] + 1 # detection count
    dataInHr4$midshipmanDetTime[idx] = dataInHr4$midshipmanDetTime[idx] + difftime(tmp$End, tmp$Start, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
    
  } else if  ((hrsSpan) >= 2 ) { #spans two or more hours
    
    midHr = as.POSIXct ( paste0(tmp$Day, " ", tmp$HrS+1,":00:00"), tz = "GMT" )
    #add info to start hour
    dataInHr4$midshipmanDet[idx]      = dataInHr4$midshipmanDet[idx] + 1 # detection count
    dataInHr4$midshipmanDetTime[idx]  = dataInHr4$midshipmanDetTime[idx] + difftime(midHr, tmp$Start, units = "secs") #time in that hour
    #add info to middle hours--- need to loop this though each hour
    for(hh in 1:((hrsSpan)-1) ) {
      dataInHr4$midshipmanDet[idx+hh]     = dataInHr4$midshipmanDet[idx+hh] + 1 # vessel count
      dataInHr4$midshipmanDetTime[idx+hh] = dataInHr4$midshipmanDetTime[idx+hh] + 3600 #time in seconds, full hour
    }
    #add info to last hour
    dataInHr4$midshipmanDet[idx+hrsSpan]     = dataInHr4$midshipmanDet[idx+hrsSpan] + 1 # vessel count
    dataInHr4$midshipmanDetTime[idx+hrsSpan] =dataInHr4$midshipmanDetTime[idx+hrsSpan] + difftime(tmp$End, tmp$DateFe, units = "secs") #time in seconds
  }
  
  # cat("Hours for detection ", vv, ": ", hrsSpan, "hrs", "\n")
  
}
#daily summary
dataInDay4 = dataInDay4[order(dataInDay4$Site,dataInDay4$Day),]
midshipman$midshipmanDet = 1
midshipmanDy = aggregate(midshipmanDet ~ Day + Site,  data = midshipman, FUN = length)
dataInDay5   = merge( dataInDay4, midshipmanDy, by=c("Day","Site") , all.x=TRUE)
dataInDay5$midshipmanDet[is.na(dataInDay5$midshipmanDet)] <- 0 #change NAs to 0s
idxNA = which (is.na (dataInDay5$OL_31.5))
dataInDay5$midshipmanDet[idxNA] = NA
dataInDay5 = dataInDay5[order(dataInDay5$Site,dataInDay5$Day),]

#bocaccio detections- start/end times, short detections!
#-----------------------------------------------------------------------------------------
bFiles = list.files(path=dirSite, pattern = "bocaccio", full.names=TRUE, recursive = TRUE)
bocaccio = NULL
for (dd in 1:length(bFiles) ){
  tmp = as.data.frame( read.csv(bFiles[dd], head = FALSE) [-1,] )
  tmpSite  = strsplit  (basename(bFiles[dd]),"_")
  if ( nrow(tmp) >0){
    #cat(as.character( tmp$V1[1]),"\n")
    tmp$Site = unique(sapply( tmpSite, "[", 2 ))
    tmp$Dep  = unique(sapply( tmpSite, "[", 3 ))
    #cat(colnames(tmp),"\n")
    #format date b/c different formates
    tstTime = as.POSIXct(  gsub("Z", "", gsub("T", " ", tmp$V1[1]))  , tz = "GMT" )
    if ( !is.na(tstTime)== TRUE ){
      tmp$Start = as.POSIXct(  gsub("Z", "", gsub("T", " ", tmp$V1))  , tz = "GMT" ) 
      tmp$End   = as.POSIXct(  gsub("Z", "", gsub("T", " ", tmp$V2))  , tz = "GMT" )
    } else { 
      cat(tstTime, "Fix", bFiles[dd],"\n") } #no detection files
    bocaccio = rbind(bocaccio, tmp[,3:6] )
    
  } else {cat(bFiles[dd],"\n")} #skips files without detections
  
}
bocaccio$Dur = bocaccio$End - bocaccio$Start
bocaccio$DateF  = as.POSIXct( paste0( substr( gsub("T", " ", bocaccio$Start),start = 1, stop = 14 ), "00:00"), tz = "GMT" )
bocaccio$DateFe = as.POSIXct( paste0( substr( gsub("T", " ", bocaccio$End),start = 1, stop = 14 ), "00:00"), tz = "GMT" )
bocaccio$Day    = as.Date(bocaccio$DateF)
bocaccio$Daye   = as.Date(bocaccio$DateFe)
dataInHr4$bocaccioDet = 0
#dataInHr4$bocaccioDetTime = 0
# CHECK: duration is really short so-- just do counts: max( bocaccio$Dur )

#hourly summary-- should be no missing SPL data for detections
for (vv in  1:nrow(bocaccio) ){ # vv = 3577
  
  tmp = bocaccio[vv,] #temp matrix
  tmp$HrS = hour(tmp$DateF)
  tmp$HrE = hour(tmp$End)
  
  #find column with matching day/hour
  idx = which( tmp$DateF == dataInHr4$DateF & tmp$Site == dataInHr4$Site ) #match by day and site!
  
  if (length(idx) > 0){ #in some cases, detections are available but not the SPL data, so can't merge yet (will automatically update when run on all data)
    # how many hours does the detection dominate?
    hrsSpan =  as.numeric( difftime(tmp$DateFe , tmp$DateF,units = "hours") )
    
  }else { 
    cat("No match for detection: ",vv, as.character( tmp$DateFe ),"\n" )
    next
  }
  
  if ((hrsSpan) == 1 )#spans to next hour
  {
    #add info to start hour
    dataInHr4$bocaccioDet[idx]      = dataInHr4$bocaccioDet[idx] + 1 # detection count
    #dataInHr4$bocaccioDetTime[idx]  = dataInHr4$bocaccioDetTime[idx] + difftime(tmp$Start, tmp$DateF, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
    
    #add info to next hour-- sometimes not available SPL data because last hours of period, so do a check
    if (is.na( dataInHr4$bocaccioDet[idx+1] ) ) {  }else {
      dataInHr4$bocaccioDet[idx+1]      = dataInHr4$bocaccioDet[idx+1] + 1 # detection count
      #dataInHr4$bocaccioDetTime[idx+1]  = dataInHr4$bocaccioDetTime[idx+1] + difftime( tmp$End, tmp$DateFe, units = "secs") #time in seconds  difftime(tmp$DateFe, edH, units = "mins")
    }
    
  } else if ((hrsSpan) == 0 ){ #within single hour
    
    dataInHr4$bocaccioDet[idx]     = dataInHr4$bocaccioDet[idx] + 1 # detection count
    #dataInHr4$bocaccioDetTime[idx] = dataInHr4$bocaccioDetTime[idx] + difftime(tmp$End, tmp$Start, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
    
  } else if  ((hrsSpan) >= 2 ) { #spans two or more hours
    
    midHr = as.POSIXct ( paste0(tmp$Day, " ", tmp$HrS+1,":00:00"), tz = "GMT" )
    #add info to start hour
    dataInHr4$bocaccioDet[idx]      = dataInHr4$bocaccioDet[idx] + 1 # detection count
    #dataInHr4$bocaccioDetTime[idx]  = dataInHr4$bocaccioDetTime[idx] + difftime(midHr, tmp$Start, units = "secs") #time in that hour
    #add info to middle hours--- need to loop this though each hour
    for(hh in 1:((hrsSpan)-1) ) {
      dataInHr4$bocaccioDet[idx+hh]     = dataInHr4$bocaccioDet[idx+hh] + 1 # vessel count
      #dataInHr4$bocaccioDetTime[idx+hh] = dataInHr4$bocaccioDetTime[idx+hh] + 3600 #time in seconds, full hour
    }
    #add info to last hour
    dataInHr4$bocaccioDet[idx+hrsSpan]     = dataInHr4$bocaccioDet[idx+hrsSpan] + 1 # vessel count
    #dataInHr4$bocaccioDetTime[idx+hrsSpan] =dataInHr4$bocaccioDetTime[idx+hrsSpan] + difftime(tmp$End, tmp$DateFe, units = "secs") #time in seconds
  }
  
  # cat("Hours for detection ", vv, ": ", hrsSpan, "hrs", "\n")
  
}

#daily summary
dataInDay5 = dataInDay5[order(dataInDay5$Site,dataInDay5$Day),]
bocaccio$bocaccioDet = 1
bocaccioDy = aggregate(bocaccioDet ~ Day + Site,  data = bocaccio, FUN = length)
dataInDay6 = merge( dataInDay5, bocaccioDy, by=c("Day","Site") , all.x=TRUE)
dataInDay6$bocaccioDet[is.na(dataInDay6$bocaccioDet)] <- 0 #change NAs to 0s
idxNA = which (is.na (dataInDay6$OL_31.5))
dataInDay6$bocaccioDet[idxNA] = NA
dataInDay6 = dataInDay6[order(dataInDay6$Site,dataInDay6$Day),]


#Explosions detections- Start/End times 
#-----------------------------------------------------------------------------------------
bFiles = list.files(path=dirSite, pattern = "Explosions", full.names=TRUE, recursive = TRUE)
explosions = NULL
for (dd in 1:length(bFiles) ){
  info = readLines(bFiles[dd])
  if ( info[1] != "" ) # file has detections in it!
  {
    tmp = as.data.frame( read.csv(bFiles[dd], head = FALSE) [-1,] )
    #no detections... 0 in V3 column
    if ( ncol(tmp) == 2)  {
      tmpSite  = strsplit  (basename(bFiles[dd]),"_")
      if ( nrow(tmp) >0){
        #cat(as.character( tmp$V1[1]),"\n")
        tmp$Site = unique(sapply( tmpSite, "[", 2 ))
        tmp$Dep  = unique(sapply( tmpSite, "[", 3 ))
        #cat(colnames(tmp),"\n")
        #format date b/c different formates
        tstTime = as.POSIXct(  gsub("Z", "", gsub("T", " ", tmp$V1[1]))  , tz = "GMT" )
        if ( !is.na(tstTime)== TRUE ){
          tmp$Start = as.POSIXct(  gsub("Z", "", gsub("T", " ", tmp$V1))  , tz = "GMT" ) 
          tmp$End   = as.POSIXct(  gsub("Z", "", gsub("T", " ", tmp$V2))  , tz = "GMT" )
        } else { 
          cat(tstTime, "Fix", bFiles[dd],"\n") } #no detection files
        explosions = rbind(explosions, tmp[,3:6] )
        
      } else {cat(bFiles[dd],"\n")} #skips files without detections
      
    }
    
  } else {cat("No Detections: ", bFiles[dd], "\n")}
  
}
explosions$Dur = explosions$End - explosions$Start
explosions$DateF  = as.POSIXct( paste0( substr( gsub("T", " ", explosions$Start),start = 1, stop = 14 ), "00:00"), tz = "GMT" )
explosions$DateFe = as.POSIXct( paste0( substr( gsub("T", " ", explosions$End),start = 1, stop = 14 ), "00:00"), tz = "GMT" )
explosions$Day    = as.Date(explosions$DateF)
explosions$Daye   = as.Date(explosions$DateFe)
explosions$explosionsDet  = 1
dataInHr4$explosionsDet = 0
#hourly summary-- 
for (vv in  1:nrow(explosions) ){ # vv = 1
  
  tmp = explosions[vv,] #temp matrix
  tmp$HrS = hour(tmp$DateF)
  tmp$HrE = hour(tmp$End)
  
  #find column with matching day/hour
  idx = which( tmp$DateF == dataInHr4$DateF & tmp$Site == dataInHr4$Site ) #match by day and site!
  
  if (length(idx) > 0){ #in some cases, detections are available but not the SPL data, so can't merge yet (will automatically update when run on all data)
    # how many hours does the detection dominate?
    hrsSpan =  as.numeric( difftime(tmp$DateFe , tmp$DateF,units = "hours") )
    
  }else { 
    cat("No match for detection: ",vv, as.character( tmp$DateFe ),"\n" )
    next
  }
  
  if ((hrsSpan) == 1 )#spans to next hour
  {
    #add info to start hour
    dataInHr4$explosionsDet[idx]      = dataInHr4$explosionsDet[idx] + 1 # detection count
    #dataInHr4$bocaccioDetTime[idx]  = dataInHr4$bocaccioDetTime[idx] + difftime(tmp$Start, tmp$DateF, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
    
    #add info to next hour-- sometimes not available SPL data because last hours of period, so do a check
    if (is.na( dataInHr4$explosionsDet[idx+1] ) ) {  }else {
      dataInHr4$explosionsDet[idx+1]      = dataInHr4$explosionsDet[idx+1] + 1 # detection count
      #dataInHr4$bocaccioDetTime[idx+1]  = dataInHr4$bocaccioDetTime[idx+1] + difftime( tmp$End, tmp$DateFe, units = "secs") #time in seconds  difftime(tmp$DateFe, edH, units = "mins")
    }
    
  } else if ((hrsSpan) == 0 ){ #within single hour
    
    dataInHr4$explosionsDet[idx]     = dataInHr4$explosionsDet[idx] + 1 # detection count
    #dataInHr4$bocaccioDetTime[idx] = dataInHr4$bocaccioDetTime[idx] + difftime(tmp$End, tmp$Start, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
    
  } else if  ((hrsSpan) >= 2 ) { #spans two or more hours
    
    midHr = as.POSIXct ( paste0(tmp$Day, " ", tmp$HrS+1,":00:00"), tz = "GMT" )
    #add info to start hour
    dataInHr4$explosionsDet[idx]      = dataInHr4$explosionsDet[idx] + 1 # detection count
    #dataInHr4$bocaccioDetTime[idx]  = dataInHr4$bocaccioDetTime[idx] + difftime(midHr, tmp$Start, units = "secs") #time in that hour
    #add info to middle hours--- need to loop this though each hour
    for(hh in 1:((hrsSpan)-1) ) {
      dataInHr4$explosionsDet[idx+hh]     = dataInHr4$explosionsDet[idx+hh] + 1 # vessel count
      #dataInHr4$bocaccioDetTime[idx+hh] = dataInHr4$bocaccioDetTime[idx+hh] + 3600 #time in seconds, full hour
    }
    #add info to last hour
    dataInHr4$explosionsDet[idx+hrsSpan]     = dataInHr4$explosionsDet[idx+hrsSpan] + 1 # vessel count
    #dataInHr4$bocaccioDetTime[idx+hrsSpan] =dataInHr4$bocaccioDetTime[idx+hrsSpan] + difftime(tmp$End, tmp$DateFe, units = "secs") #time in seconds
  }
  
  # cat("Hours for detection ", vv, ": ", hrsSpan, "hrs", "\n")
  
}

#daily summary
dataInDay5   = dataInDay5[order(dataInDay5$Site,dataInDay5$Day),]
explosionsDy = aggregate(explosionsDet ~ Day + Site,  data = explosions, FUN = length)
dataInDay6   = merge( dataInDay6, explosionsDy, by=c("Day","Site") , all.x=TRUE)
dataInDay6$explosionsDet[is.na(dataInDay6$explosionsDet)] = 0 #change NAs to 0s
idxNA = which (is.na (dataInDay6$OL_31.5))
dataInDay6$explosionsDet[idxNA] = NA
dataInDay6 = dataInDay6[order(dataInDay6$Site,dataInDay6$Day),]

#FishCalls detections- start/end times with other parameters
#-----------------------------------------------------------------------------------------
bFiles = list.files(path=dirSite, pattern = "FishCalls", full.names=TRUE, recursive = TRUE)
FishCalls = NULL
for (dd in 1:length(bFiles) ){
  info = readLines(bFiles[dd])
  if ( info[1] != "" ) # file has detections in it!
  {
    tmp = as.data.frame( read.csv(bFiles[dd], head = FALSE) [-1,] )
  
    if ( ncol(tmp) == 10)  {
      tmpSite  = strsplit  (basename(bFiles[dd]),"_")
      if ( nrow(tmp) >0 ){
        tmp$Site = unique(sapply( tmpSite, "[", 2 ))
        tmp$Dep  = unique(sapply( tmpSite, "[", 3 ))
        #format date b/c different formates
        tstTime = as.POSIXct(  gsub("Z", "", gsub("T", " ", tmp$V4[1]))  , tz = "GMT" )
        if ( !is.na(tstTime)== TRUE ){
          tmp$Start = as.POSIXct(  gsub("Z", "", gsub("T", " ", tmp$V4))  , tz = "GMT" ) 
          tmp$End   = as.POSIXct(  gsub("Z", "", gsub("T", " ", tmp$V5))  , tz = "GMT" )
        } else { 
          cat(tstTime, "Fix", bFiles[dd],"\n") } #no detection files
        
        FishCalls = rbind(FishCalls, tmp[,c(11:14,1:3,6:8)] )
        
      } else {cat(bFiles[dd],"\n")} #skips files without detections
      
    }
    
  } else {cat("No Detections: ", bFiles[dd], "\n")}
  
}
tmp = colnames( as.data.frame( read.csv(bFiles[1]) ))
colnames(FishCalls)[5:10] = c(tmp[1:3], tmp[6:8])
FishCalls$Dur = FishCalls$End - FishCalls$Start
FishCalls$DateF  = as.POSIXct( paste0( substr( gsub("T", " ", FishCalls$Start),start = 1, stop = 14 ), "00:00"), tz = "GMT" )
FishCalls$DateFe = as.POSIXct( paste0( substr( gsub("T", " ", FishCalls$End),start = 1, stop = 14 ), "00:00"), tz = "GMT" )
FishCalls$Day    = as.Date(FishCalls$DateF)
FishCalls$Daye   = as.Date(FishCalls$DateFe)
FishCalls$Year = year(FishCalls$DateF)
FishCalls$FishCallsDet = 1
dataInHr4$FishCallsDet = 0
#hourly summary
for (vv in  1:nrow(FishCalls) ){ # vv = 1
  
  tmp = FishCalls[vv,] #temp matrix
  tmp$HrS = hour(tmp$DateF)
  tmp$HrE = hour(tmp$End)
  
  #find column with matching day/hour
  idx = which( tmp$DateF == dataInHr4$DateF & tmp$Site == dataInHr4$Site ) #match by day and site!
  
  if (length(idx) > 0){ #in some cases, detections are available but not the SPL data, so can't merge yet (will automatically update when run on all data)
    # how many hours does the detection dominate?
    hrsSpan =  as.numeric( difftime(tmp$DateFe , tmp$DateF,units = "hours") )
    
  }else { 
    cat("No match for detection: ",vv, as.character( tmp$DateFe ),"\n" )
    next
  }
  
  if ((hrsSpan) == 1 )#spans to next hour
  {
    #add info to start hour
    dataInHr4$FishCallsDet[idx]      = dataInHr4$FishCallsDet[idx] + 1 # detection count
    #dataInHr4$bocaccioDetTime[idx]  = dataInHr4$bocaccioDetTime[idx] + difftime(tmp$Start, tmp$DateF, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
    
    #add info to next hour-- sometimes not available SPL data because last hours of period, so do a check
    if (is.na( dataInHr4$FishCallsDet[idx+1] ) ) {  }else {
      dataInHr4$FishCallsDet[idx+1]      = dataInHr4$FishCallsDet[idx+1] + 1 # detection count
      #dataInHr4$bocaccioDetTime[idx+1]  = dataInHr4$bocaccioDetTime[idx+1] + difftime( tmp$End, tmp$DateFe, units = "secs") #time in seconds  difftime(tmp$DateFe, edH, units = "mins")
    }
    
  } else if ((hrsSpan) == 0 ){ #within single hour
    
    dataInHr4$FishCallsDet[idx]     = dataInHr4$FishCallsDet[idx] + 1 # detection count
    #dataInHr4$bocaccioDetTime[idx] = dataInHr4$bocaccioDetTime[idx] + difftime(tmp$End, tmp$Start, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
    
  } else if  ((hrsSpan) >= 2 ) { #spans two or more hours
    
    midHr = as.POSIXct ( paste0(tmp$Day, " ", tmp$HrS+1,":00:00"), tz = "GMT" )
    #add info to start hour
    dataInHr4$FishCallsDet[idx]      = dataInHr4$FishCallsDet[idx] + 1 # detection count
    #dataInHr4$bocaccioDetTime[idx]  = dataInHr4$bocaccioDetTime[idx] + difftime(midHr, tmp$Start, units = "secs") #time in that hour
    #add info to middle hours--- need to loop this though each hour
    for(hh in 1:((hrsSpan)-1) ) {
      dataInHr4$FishCallsDet[idx+hh]     = dataInHr4$FishCallsDet[idx+hh] + 1 # vessel count
      #dataInHr4$bocaccioDetTime[idx+hh] = dataInHr4$bocaccioDetTime[idx+hh] + 3600 #time in seconds, full hour
    }
    #add info to last hour
    dataInHr4$FishCallsDet[idx+hrsSpan]     = dataInHr4$FishCallsDet[idx+hrsSpan] + 1 # vessel count
    #dataInHr4$bocaccioDetTime[idx+hrsSpan] =dataInHr4$bocaccioDetTime[idx+hrsSpan] + difftime(tmp$End, tmp$DateFe, units = "secs") #time in seconds
  }
  
  # cat("Hours for detection ", vv, ": ", hrsSpan, "hrs", "\n")
  
}

#daily summary
dataInDay6   = dataInDay6[order(dataInDay6$Site,dataInDay6$Day),]
FishCallsDy = aggregate(FishCallsDet ~ Day + Site,  data = FishCalls, FUN = length)
dataInDay6   = merge( dataInDay6, FishCallsDy, by=c("Day","Site") , all.x=TRUE)
dataInDay6$FishCallsDet[is.na(dataInDay6$FishCallsDet)] = 0 #change NAs to 0s
idxNA = which (is.na (dataInDay6$OL_31.5))
dataInDay6$FishCallsDet[idxNA] = NA
dataInDay6 = dataInDay6[order(dataInDay6$Site,dataInDay6$Day),]

#-----------------------------------------------------------------------------------------
#Summarize detections by day
#-----------------------------------------------------------------------------------------
dataInDay6$midshipmanDetTime = 0

uSites = unique(dataInHr4$Site)
for (ss in 1:length(uSites)){
  tmpS = dataInHr4[ dataInHr4$Site == uSites[ss], ]
  uDayS = unique(tmpS$Day.x)
  for (ii in 1:length(uDayS)){
    tmpSD = tmpS[ tmpS$Day.x == uDayS[ii], ]
    idx = which( tmpSD$Day.x[1] == dataInDay4$Day & tmpSD$Site[1] == dataInDay4$Site ) #match by day and site!
    dataInDay6$midshipmanDetTime[idx] = sum(tmpSD$midshipmanDetTime)
  }
    
}

#SAVE OUT... with detections, clean up columns!
#-----------------------------------------------------------------------------------------
# HOURLY, dataInHr4
# CHECK NAs---
dataInHr4[886,]
idxNA = which (is.na (dataInHr4$OL_31.5))
dataInHr4[idxNA,30:35] = NA
dataInHr4[886,]
# REMOVE UNUSED COLUMNS
as.data.frame( colnames(dataInHr4) )
dataInHrOUT = dataInHr4[,c(5,1,21, 2,4, 6:16, 22, 27,29, 17,18, 30:35)]
colnames(dataInHrOUT)[1] = "Date"
colnames(dataInHrOUT)[2] = "DateTime"
colnames(dataInHrOUT)[3] = "JulianDay"
colnames(dataInHrOUT)[5] = "Deployment"
colnames(dataInHrOUT)[18] = "TideHeight_ft"
colnames(dataInHrOUT)[19] = "WaterLevelChange_ft"
colnames(dataInHrOUT)[20] = "VesselDet_count"
colnames(dataInHrOUT)[21] = "VesselDet_duration_sec"
fnameOut =  paste0(outDir, sanctuary, "_CombinedDataDets_Hourly_",as.character(min(as.Date(dataInDay6$Day))),"TO",
                   as.character(max(as.Date(dataInDay6$Day))), "_v", DC, ".csv")  

if(flagCSV == TRUE ){ write.csv(dataInHrOUT,fnameOut)} 

# DAILY
dataInDay6[47,]
idxNA = which (is.na (dataInDay6$OL_31.5))
dataInDay4[idxNA,35:40] = NA
as.data.frame( colnames(dataInDay6) )
dataInDyOUT = dataInDay6[,c(1, 2,4, 16, 5:15, 22:31, 32,33,34, 17,18, 35:40)]
colnames(dataInDyOUT)[1] = "Date"
colnames(dataInDyOUT)[3] = "Deployment"
colnames(dataInDyOUT)[4] = "HoursSampled"
colnames(dataInDyOUT)[27] = "TideHeight_ft_mean"
colnames(dataInDyOUT)[28] = "WaterLevelChange_ft_mean"
colnames(dataInDyOUT)[29] = "VesselDet_count"
colnames(dataInDyOUT)[30] = "VesselDet_duration_sec"

fnameOut =  paste0(outDir, sanctuary, "_CombinedDataDets_Day_",as.character(min(as.Date(dataInDay6$Day))),"TO",
                   as.character(max(as.Date(dataInDay6$Day))), "_v", DC, ".csv")  

if(flagCSV == TRUE ){ write.csv(dataInDyOUT,fnameOut)} 


#SAVE OUT... without detections
#-----------------------------------------------------------------------------------------
fnameOut =  paste0(outDir, sanctuary, "_CombinedData_Day_",as.character(min(as.Date(dataInDay3$Day))),"to",
                   as.character(max(as.Date(dataInDay3$Day))), "_v", DC, ".csv")  

if(flagCSV == TRUE ){ write.csv(dataInDay3,fnameOut)} 

fnameOut =  paste0(outDir, sanctuary, "_CombinedData_Hr_",as.character(min(as.Date(dataInHr3$DateF))),"to",
                   as.character(max(as.Date(dataInHr3$DateF))), "_v", DC, ".csv")  

if(flagCSV == TRUE ){ write.csv(dataInHr3,fnameOut)} 

#SAVE OUT... without detections
#-----------------------------------------------------------------------------------------