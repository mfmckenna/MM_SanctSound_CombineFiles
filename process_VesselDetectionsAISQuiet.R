# process sancturary sound vessel detection data and AIS results
rm(list=ls())
## START LINE 261....
#-----------------------------------------------------------------------------------------
# MAIN functions-- by site
#-----------------------------------------------------------------------------------------
# Reads in both vessel detection and AIS data products
# Calculates daily vessel metrics per day:
# Converts vessel detection files: for each each hour, how many unique vessels detected, and percent of hour dominated by vessel noise
# some of the time will spill into the next hour, need to keep track of this
# reformat so for each hour there is a total vessel time value and total vessel count

#updates with measure of duration of quiet summaries
#-----------------------------------------------------------------------------------------
# TO ADD (future versions)
#-----------------------------------------------------------------------------------------
# output table: add range and save out
# duration of quiet- hour/daily events and average durations, using vessel detection data

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
library(plyr)
#-----------------------------------------------------------------------------------------
# SETUP parameters
#-----------------------------------------------------------------------------------------
ra = 7 #days for running average
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

#-----------------------------------------------------------------------------------------
# OUTPUT details
#-----------------------------------------------------------------------------------------
outDir ="E:\\RESEARCH\\SanctSound\\data2\\combineFileeEffort_SplAisVesDet\\"
DC = Sys.Date()
#range of dates for output graphics
eDatePlot = '2020-12-31'
sDatePlot = '2018-11-01'
flagCSV = TRUE #true if you want to ouptut hourly and daily csv files per site

#-----------------------------------------------------------------------------------------
# READ IN-- vessel detection data
#-----------------------------------------------------------------------------------------
dirVD     = paste0("E:\\RESEARCH\\SanctSound\\data2\\SanctSound_VesselDetection_DataProducts")
nFilesVD  = length( list.files(path=dirVD, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD =     ( list.files(path=dirVD, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD = basename(inFilesVD)
x = strsplit(inFilesVD,"_")
sitesVD = unique(sapply( x, "[", 2 ))

cat(length(sitesVD), " number of sites with vessel detection data", "\n")
rm(x)

#-----------------------------------------------------------------------------------------
# READS IN-- AIS data
#-----------------------------------------------------------------------------------------
dirAIS      = paste0("E:\\RESEARCH\\SanctSound\\data")
nFilesAIS   = length( list.files(path=dirAIS,pattern = "_2018_10_to_2020_11.csv", full.names=TRUE, recursive = TRUE))
inFilesAIS  =       ( list.files(path=dirAIS,pattern = "_2018_10_to_2020_11.csv", full.names=TRUE, recursive = TRUE))
x = strsplit(inFilesAIS,"_")
sanctsAIS = unique(sapply( x, "[", 2 ))
#combine AIS files
multmerge = function(path){
  filenames = list.files(path=path, pattern = "_2018_10_to_2020_11.csv", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
AIS <- multmerge(dirAIS)
sitesAIS = unique(AIS$LOC_ID)
cat(length(sitesAIS), " number of sites with AIS data", "\n")
rm(x)

#-----------------------------------------------------------------------------------------
# PROCESS and COMBINE FILES by site, reads in SPL date when processing each site
#-----------------------------------------------------------------------------------------
output = NULL
for (ss in 1: length(sitesVD)) { #loop through each site ss = 1
  
  cat("Processing...", sitesVD[ss], ":",ss, "of", length(sitesVD),"\n")
  
  sFiles = list.files(path=dirVD, pattern = paste0(sitesVD[ss],".*hips.csv"), full.names=TRUE, recursive = TRUE)
  aData  = AIS[ AIS$LOC_ID == sitesVD[ss], ]
  aData$Day = as.Date(aData$DATE,format = "%m/%d/%Y")#AIS is daily resolution
  deply = sitesVD[ss]
  sanct = substr(sitesVD[ss],1,2)
  
  #----------------------------------------------------------------------------------------- 
  #GET SPL files-- this give us accurate date range for detections
  #---------------------------------------------------------------------------------------
  dirSPL  = paste0("E:\\RESEARCH\\SanctSound\\data\\",sanct,"\\", deply)
  nFiles  = length( list.files(path=dirSPL, pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE))
  
  #CHECK: are there OL SPL files??
  if (nFiles == 0 ){
    cat("No SPL files for ",sitesVD[ss],"\n")
    n
    output = rbind(output, c(sitesVD[ss], length(sFiles), nFiles, NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA,NA,NA ) )
    next
  }else { cat(nFiles, "SPL files for ",sitesVD[ss],"\n") }
  SPL = NULL
  for (ff in 1 : nFiles){
    fname  = list.files(path=dirSPL, pattern = "_OL_1h.csv", full.names=FALSE, recursive = TRUE)[ff]
    fnameF = list.files(path=dirSPL, pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)[ff]
    dname  = sapply (strsplit( fname, "_" ),"[",4 )
    tmp    = rbind(fread(fnameF))
    tmp$deploy = dname
    if (ff > 1) { names(tmp) <- NULL }
    
    SPL = rbind(SPL,tmp)
    rm(fname,fnameF,tmp)
  }
  #CHECK: fix format changes in SPL data headings
  SPL$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPL$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
  SPL$Day   = as.Date(SPL$DateF)
  if ( is.na( SPL$DateF [1]) ){#try another format!!
    SPL$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPL$yyyy_mm_ddTHH_MM_SSZ)), tz = "GMT" )
    SPL$Day   = as.Date(SPL$DateF)
  }
  #CHECK: fix some sites have different OB-- SB01 for sure!! starts at 16 Hz
  #cat( sitesVD[ss], ":", colnames(SPL), "\n")
  if (sitesVD[ss]== "SB01") {
    cat("SPL data has extra column(s)... check!", "\n")
    SPL = SPL[,c(1,3:15)]
  }
  cat( sitesVD[ss], ":", colnames(SPL), "\n")
  
  #APPEND BB data to OL
  nFiles = length( list.files(path=dirSPL,pattern = "_BB_1h.csv", full.names=TRUE, recursive = TRUE))
  SPLBB = NULL
  for (ff in 1 : nFiles){
    fname  = list.files(path=dirSPL, pattern = "_BB_1h.csv", full.names=FALSE, recursive = TRUE)[ff]
    fnameF = list.files(path=dirSPL, pattern = "_BB_1h.csv", full.names=TRUE, recursive = TRUE)[ff]
    dname  = sapply (strsplit( fname, "_" ),"[",4 )
    tmp    = rbind(fread(fnameF))
    tmp$deploy = dname
    if (ff > 1) { names(tmp) <- NULL }
    
    SPLBB = rbind(SPLBB,tmp)
    rm(fname,fnameF,tmp)
  }
  #GRRR DIFFERENT HEADING FORMATS!!!
  colcheck = colnames(SPLBB)
  if (colcheck[1] == "yyyy_mm_ddTHH_MM_SSZ" ){
    #SPLBB$yyyy_mm_ddTHH_MM_SSZ
    SPLBB$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPLBB$`yyyy_mm_ddTHH_MM_SSZ`)), tz = "GMT" )
    SPLBB$DateFday  = as.Date(SPL$DateF)
    
  }else if (colcheck[1] == "yyyy-mm-ddTHH:MM:SSZ" ){
    #SPLBB$`yyyy-mm-ddTHH:MM:SSZ`
    SPLBB$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPLBB$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
    SPLBB$DateFday  = as.Date(SPL$DateF)
  }else { (cat ('BB heading format not found',"\n")) }
 
 
  #Data check plot
  SScol = colnames(SPLBB)[2]
  pSPLB = ggplot(SPLBB, aes(DateF, SScol, color = deploy))+
    geom_point() +
    xlab("")+
    ylab("SPL-BB") +
    theme_minimal()+
    ggtitle(paste( "Check SPL BB (", deply, ")" ))
 # pSPLB-- not very ehlpful because converts to NA
  SPLa = merge(SPL, SPLBB[,c(2,4)], all = FALSE, all.x = TRUE, by = "DateF" ) 
  SPL = SPLa
  
  #CREATE accurate time ranges for data-- hourly
  #---------------------------------------------------------------------------------------
  beginTime =   as.POSIXct( min(SPL$DateF) )
  endTime   =   as.POSIXct( max(SPL$DateF) )
  VESSfor   =   as.data.frame( seq(from=beginTime, to=endTime, by="hour") )
  colnames(VESSfor) = "DateF"
  FullListTimes = merge(VESSfor, SPL, by="DateF", all = TRUE)
  FullListTimes$Site = sitesVD[ss]
  FullListTimes = FullListTimes[,c(16,13,14,1,3:12,15)] #reorder and truncate!
  FullListTimes$Day = as.Date(FullListTimes$DateF)
   
  #create accurate time ranges for data-- DAY
  #---------------------------------------------------------------------------------------
  beginDay = as.Date(beginTime)
  endDay   = as.Date(endTime)
  VESSday  = as.data.frame( seq(from=beginTime, to=endTime, by="day") ) 
  colnames(VESSday) = "Day"
  VESSday$Day = as.Date(VESSday$Day)
  #process SPL data to get daily median values per octave band
  uday = unique(SPL$Day)
  dSPL = NULL 
  gg = c(as.numeric(grep("^OL", colnames(SPL) ) ), 15) #add the BB column!
  for (ii in 1:length(uday)){ # ii = 1
    dtmp = SPL[SPL$Day == uday[ii],]
    hrsample = nrow(dtmp)
    depl = dtmp$deploy[1]
    #SPL-- median per octave band
    tmpSPL = select(dtmp,c(gg))
    SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
    dSPL = rbind(dSPL, c(depl, as.character(uday[ii]),  hrsample, SPLm ) )
    
    rm(dtmp,hrsample,depl,tmpSPL,SPLm)
  }
  colnames(dSPL)[1:3] = c("deply","Day","Hrs")
  dSPL = as.data.frame(dSPL)
  dSPL$Day = as.Date(dSPL$Day )
  
  #merge with full day list
  FullListDay= merge(VESSday, dSPL, by="Day", all = TRUE)
  FullListDay$Site = sitesVD[ss]
  FullListDay = FullListDay[,c(15,2,1,4:14,3)] #reorder and truncate!
  
  rm(dname, VESSday,VESSfor,SPL,dSPL,beginDay,beginTime,deply,endDay,endTime,gg,ii,ff,sanct)
  
  #----------------------------------------------------------------------------------------- 
  #PROCESS VESSEL DETECTION DATA TO daily metircs-- COMBINE deployments
  #-----------------------------------------------------------------------------------------
  VESS=NULL
  for (ff in 1:length(sFiles)){
    fname  = sFiles[ff]
    dname  = sapply (strsplit( basename(fname), "_" ),"[",3 )
    tmp    = cbind(fread(fname,header = FALSE, skip=1),dname)
    VESS   = rbind(VESS,tmp)
    rm(fname,dname,tmp)
  }
  
  #FORMAT data
  #-----------------------------------------------------------------------------------------
  VESS$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$V1)), tz = "GMT" )
  VESS$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$V2))  , tz = "GMT" )
  VESS$Dur_mins = VESS$DateFe- VESS$DateFs #in minutes!
  VESS = VESS[,3:7]
  names(VESS)[1] = "label"
  names(VESS)[2] = "deply"
  VESS$DateFstart = force_tz(as.Date(VESS$DateFs), tzone="GMT")
  VESS$HrS =hour(VESS$DateFs)
  VESS$HrE =hour(VESS$DateFe)
  
  #CHECK plot/ CLEAN up VESS-- sort and remove duplicates
  #-----------------------------------------------------------------------------------------
  pVes = ggplot(VESS, aes(DateFs, Dur_mins))+
    geom_point() +
    xlab("")+
    ylab("Duration Vessel Detections (mins)") +
    theme_minimal()+
    ggtitle(paste( "Check vessel detections (", sitesVD[ss], ")" ))
  VESSa = arrange(VESS, by_group = VESS$DateFs )
  VESS  = VESSa
  VESSd = dplyr::distinct(VESS)
  VESS  = VESSd
  rm(VESSa,VESSd)
  
  library(ggplot2)
  library(scales)
  library(stringr)
  
  #START HERE!!!!
  ggplot(VESS, aes(x=DateFs)) + 
    scale_x_datetime(breaks = date_breaks("1 day")) +
    scale_y_continuous(limits = c(0,48), breaks=seq(0,48,2), 
                       labels=str_pad(seq(0,48,2) %% 24, 2, pad="0")) +
    geom_hline(yintercept=seq(0,48,24)) +
    geom_linerange(aes(ymin = DateFs, ymax = DateFe), color = "red",size = 2) + 
    coord_flip() + ylab("Time (hours)") + 
    ggtitle("Activity During Day")
  
  #-----------------------------------------------------------------------------------------
  #PROCESS each vessel detection, put in correct column in FullListTimes
  #-----------------------------------------------------------------------------------------
  
  #MAKE new matrix to fill in the values for each hour
  #-----------------------------------------------------------------------------------------
  #make new columns to fill in data....
  #vessel metrics
  FullListTimes$totalVessels = 0 #count of vessel dominated perids
  FullListTimes$totalTime    = 0 #total time vessel noise dominates
  #vessel dominant periods
  for (vv in  1:nrow(VESS) ){ # vv = 1
    
    tmp = VESS[vv,] #temp matrix
    tmp$DateFend = as.Date(tmp$DateFe) #needed end data in case it goes to the next day!
    
    #find column with matching day/hour
    idx = which( as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" ) == FullListTimes$DateF) 
    
    # how many hours does the vessel detection dominate?
    stH = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" )
    edH = as.POSIXct ( paste0(tmp$DateFend, " "  , tmp$HrE,":00:00"), tz = "GMT" )
    hrsSpan =  as.numeric( difftime(edH , stH,units = "hours") )
    
    if ((hrsSpan) == 1 )#spans to next hour
    {
      #add info to start hour
      FullListTimes$totalVessels[idx]  = FullListTimes$totalVessels[idx] + 1 # vessel count
      FullListTimes$totalTime[idx]     = FullListTimes$totalTime[idx] + difftime(edH, tmp$DateFs, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
      FullListTimes$checkVD[idx] = tmp$deply
      #add info to next hour
      FullListTimes$totalVessels[idx+1]  = FullListTimes$totalVessels[idx+1] + 1 # vessel count
      FullListTimes$totalTime[idx+1]     = FullListTimes$totalTime[idx+1] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds  difftime(tmp$DateFe, edH, units = "mins")
      FullListTimes$checkVD[idx+1] = tmp$deply
      
    } else if ((hrsSpan) == 0 ){ #within single hour
      
      FullListTimes$totalVessels[idx]  = FullListTimes$totalVessels[idx] + 1 # vessel count
      FullListTimes$totalTime[idx]     = FullListTimes$totalTime[idx] + difftime(tmp$DateFe,tmp$DateFs,  units = "secs") #time in seconds
      FullListTimes$checkVD[idx] = tmp$deply
      
    } else if  ((hrsSpan) >= 2 ) { #spans two or more hours
      
      midHr = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS+1,":00:00"), tz = "GMT" )
      #add info to start hour
      FullListTimes$totalVessels[idx]  = FullListTimes$totalVessels[idx] + 1 # vessel count
      FullListTimes$totalTime[idx]     = FullListTimes$totalTime[idx] + difftime(midHr, tmp$DateFs, units = "secs")
      FullListTimes$checkVD[idx] = tmp$deply
      #add info to middle hours--- need to loop this though each hour
      for(hh in 1:((hrsSpan)-1) ) {
        FullListTimes$totalVessels[idx+hh]  = FullListTimes$totalVessels[idx+hh] + 1 # vessel count
        FullListTimes$totalTime[idx+hh]     = FullListTimes$totalTime[idx+hh] + 3600 #time in seconds, full hour
        FullListTimes$checkVD[idx+hh] = tmp$deply
      }
      #add info to last hour
      FullListTimes$totalVessels[idx+hrsSpan]  = FullListTimes$totalVessels[idx+hrsSpan] + 1 # vessel count
      FullListTimes$totalTime[idx+hrsSpan]     = FullListTimes$totalTime[idx+hrsSpan] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds
      FullListTimes$checkVD[idx+hrsSpan] = tmp$deply
    }
    
    # cat("Hours for detection ", vv, ": ", hrsSpan, "hrs", "\n")
  }
  
  #duration of quiet metrics
  VESSQuiet = NULL
  VESSQuiet$DateFs = VESS$DateFe
  VESSQuiet$DateFe = c(VESS$DateFs[2:nrow(VESS)],NA)
  VESSQuiet = as.data.frame(VESSQuiet)
  VESSQuiet$Dur_mins = VESSQuiet$DateFe - VESSQuiet$DateFs 
  VESSQuiet$DateFstart = force_tz(as.Date(VESSQuiet$DateFs), tzone="GMT")
  VESSQuiet$HrS =hour(VESSQuiet$DateFs)
  VESSQuiet$HrE =hour(VESSQuiet$DateFe)
  VESSQuiet$deply = c( VESS$deply[2:nrow(VESS)], VESS$deply[nrow(VESS)])
  #add results to the FullListTimes matrix
  FullListTimes$totalQuiet  = 0 #how much time in an hour quiet--  this is just the opposite of totalTime column
  FullListTimes$totalTimeQuiet   = 0 #how many quiet periods
  FullListTimes$avgDurQuiet   = 0 #how long are quiet periods- average for hour? NEW!!!

  #quiet periods--- !!!under construction!!!
  for (vv in  1:(nrow(VESSQuiet)-1) ){ # vv = 1
    
    tmp = VESSQuiet[vv,] #temp matrix
    tmp$DateFend = as.Date(tmp$DateFe) #needed end data in case it goes to the next day!
    
    #find column with matching day/hour
    idx = which( as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" ) == FullListTimes$DateF) 
    
    # how many hours does the vessel detection dominate?
    stH = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" )
    edH = as.POSIXct ( paste0(tmp$DateFend, " "  , tmp$HrE,":00:00"), tz = "GMT" )
    hrsSpan =  as.numeric( difftime(edH , stH,units = "hours") )
    
    if ((hrsSpan) == 1 )#spans to next hour
    {
      #add info to start hour
      FullListTimes$totalQuiet[idx]  = FullListTimes$totalQuiet[idx] + 1 # vessel count
      FullListTimes$totalTimeQuiet[idx]     = FullListTimes$totalTimeQuiet[idx] + difftime(edH, tmp$DateFs, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
      
      #add info to next hour
      FullListTimes$totalQuiet[idx+1]  = FullListTimes$totalQuiet[idx+1] + 1 # vessel count
      FullListTimes$totalTimeQuiet[idx+1]   = FullListTimes$totalTimeQuiet[idx+1] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds  difftime(tmp$DateFe, edH, units = "mins")
      
      
    } else if ((hrsSpan) == 0 ){ #within single hour
      
      FullListTimes$totalQuiet[idx]  = FullListTimes$totalQuiet[idx] + 1 # vessel count
      FullListTimes$totalTimeQuiet[idx]     = FullListTimes$totalTimeQuiet[idx] + difftime(tmp$DateFe,tmp$DateFs,  units = "secs") #time in seconds
      
    } else if  ((hrsSpan) >= 2 ) { #spans two or more hours
      
      midHr = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS+1,":00:00"), tz = "GMT" )
      #add info to start hour
      FullListTimes$totalQuiet[idx]  = FullListTimes$totalQuiet[idx] + 1 # vessel count
      FullListTimes$totalTimeQuiet[idx]     = FullListTimes$totalTimeQuiet[idx] + difftime(midHr, tmp$DateFs, units = "secs")
     
      #add info to middle hours--- need to loop this though each hour
      for(hh in 1:((hrsSpan)-1) ) {
        FullListTimes$totalQuiet[idx+hh]  = FullListTimes$totalQuiet[idx+hh] + 1 # vessel count
        FullListTimes$totalTimeQuiet[idx+hh]     = FullListTimes$totalTimeQuiet[idx+hh] + 3600 #time in seconds, full hour
  
      }
      #add info to last hour
      FullListTimes$totalQuiet[idx+hrsSpan]  = FullListTimes$totalQuiet[idx+hrsSpan] + 1 # vessel count
      FullListTimes$totalTimeQuiet[idx+hrsSpan]     = FullListTimes$totalTimeQuiet[idx+hrsSpan] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds
  
    }
    
    # cat("Hours for detection ", vv, ": ", hrsSpan, "hrs", "\n")
  }
  
  # FILL IN NAs for missing data based on SPLs (not sure why some NAs within deplyments... length?)
  #-----------------------------------------------------------------------------------------
  indxNA = which (is.na (FullListTimes$OL_31.5))
  FullListTimes$totalVessels[indxNA] = NA
  FullListTimes$totalTime[indxNA] = NA
  FullListTimes$checkVD[indxNA] = NA
  FullListTimes$totalQuiet[indxNA] = NA
  FullListTimes$totalTimeQuiet[indxNA] = NA
  
  FullListTimes$Tck = FullListTimes$totalTime + FullListTimes$totalTimeQuiet
  hist(FullListTimes$Tck) #something is off...
  
  #FORMAT data-- hourly data (VESSfor)
  #-----------------------------------------------------------------------------------------
  FullListTimes$totalTimeM = FullListTimes$totalTime/(60) 
  errorC = FullListTimes[FullListTimes$totalTimeM > 60,] # CHECk: max(FullListTimes$totalTimeM,na.rm = TRUE)
  
  FullListTimes$JulDay = yday(FullListTimes$DateF)
  FullListTimes$Site   = sitesVD[ss]
  FullListTimes$Day    = as.Date(FullListTimes$DateF)
  
  
  # EXPORT HOURLY vessel detections
  #-----------------------------------------------------------------------------------------
  fnameOut =  paste0(outDir, sitesVD[ss], "_VesselDetectionsHr_",as.character(min(as.Date(FullListTimes$DateF))),"to",
                     as.character(max(as.Date(FullListTimes$DateF))), "_v", DC, ".csv")  
  if(flagCSV == TRUE ){ write.csv(FullListTimes,fnameOut) }
  
  #boxplot of hourly values per day-- does not look great because so many zeros at some sites
  #plt_hr = reshape::melt(FullListTimes, id.vars = c("Day" ), measure.vars = c("totalTime"))
  #plt_hr$value = as.numeric(as.character(plt_hr$value))/(60)
  #ggplot(data=plt_hr, aes(x=(Day), y=value, group=Day) ) +
    #geom_boxplot() +
    #ylab("minutes dominated by vessels") + xlab("")
  pHR = ggplot(FullListTimes, aes(x = Day, y=totalTimeM, color = deploy))+
    geom_point()+ #geom_line(aes(y=rollmean(totalTimeM, 7, na.pad=TRUE)),size=1.5)+
    ylab("minutes dominated by vessels") + xlab("")
  #ggsave( paste0(outDir,sites[ss], "_VesselDetectionPlotHR_", "_v",DC,".png") ,pHR)
  
  #-----------------------------------------------------------------------------------------
  # CALCULATE AND EXPORT DAILY summaries
  #-----------------------------------------------------------------------------------------
  #calculate total vessels by summing detections in each day- so vessels daily detections not double counted in occur in multiple hours
  dys = unique(FullListDay$Day)
  FullListDay$totalVessels = 0
  
  for ( dd in 1:length(dys) ) {
    tmp = (VESS[VESS$DateFstart == dys[dd],]) #dd = 10
    idx = which( dys[dd]  == FullListDay$Day)
    tmp$DayE = as.Date(tmp$DateFe)
    
    if (nrow(tmp) == 0 ){ #no data on that day...
      FullListDay$totalVessels[idx] = 0
    }else {
      tmpc = nrow(tmp)
      
      #does the last one go to next day?
      hrsSpan = as.numeric(tmp$DayE[tmpc] - tmp$DateFstart[tmpc] )
    
      if (hrsSpan == 1 )#spans to next day
      {
        #add info to start day
        FullListDay$totalVessels[idx]   = FullListDay$totalVessels[idx] + tmpc    # vessel count
        #add detection to to next day
        FullListDay$totalVessels[idx+1] = FullListDay$totalVessels[idx+1] + 1     # vessel count
        
      } else { #within single day
        FullListDay$totalVessels[idx]   = FullListDay$totalVessels[idx] + tmpc     # vessel count
      }
    }
    
  }
  indxNAd = which( is.na(FullListDay$OL_31.5) )
  FullListDay$totalVessels[indxNAd] = NA  
  
  #Alternative method for calculating daily values from hourly summeries-- accurate for time metrics, NOT total vessels
  #-----------------------------------------------------------------------------------------
  DtimeSum  = aggregate(FullListTimes$totalTime,    by=list(FullListTimes$Day), sum, na.rm=T)     #total time for the day
  DtimeMean = aggregate(FullListTimes$totalTime,    by=list(FullListTimes$Day), mean, na.rm=T)    #mean per hour for the day
  DvessMean = aggregate(FullListTimes$totalVessels, by=list(FullListTimes$Day), mean,na.rm=T)     #average vessels per hour
  #DvessSum  = aggregate(FullListTimes$totalVessels, by=list(FullListTimes$Day), sum,na.rm=T)     #total vessels per day
  
  VESSforDay = as.data.frame( cbind(DtimeSum, DtimeMean[,2],DvessMean[,2]  ) )
  colnames(VESSforDay) = c("Day","sumTotalTime_sec","HRmeanTotalTime_sec","HRmeanVesselDet_sec")

  
  FullListDay2 = merge(FullListDay, VESSforDay, by = "Day")
  FullListDay2$sumTotalTime_sec[indxNAd] = NA  
  FullListDay2$HRmeanTotalTime_sec[indxNAd] = NA  
  FullListDay2$HRmeanVesselDet_sec[indxNAd] = NA  
  
  # percent of the day with vessel noise dominating... divide seconds by total time in day sampled
  FullListDay2$PerDay = ( FullListDay2$sumTotalTime_sec/(as.numeric(as.character(FullListDay2$Hrs)) *60 *60)) *100
  
  # durations of vessels detections and quieter periods 
  #overall for the site:   #mean(as.numeric( VESS$Dur_mins) ),   #mean(as.numeric( VESSQuiet$Dur_mins),na.rm = TRUE )
  tstQ  = ddply(VESSQuiet, .(DateFstart), summarize, xm= (mean(as.numeric( Dur_mins),na.rm = TRUE )) )
  tstQ2 = ddply(VESSQuiet, .(DateFstart), summarize, xm= (sd(as.numeric( Dur_mins),na.rm = TRUE  )) )
  durQuiet = cbind(tstQ,tstQ2[,2])
  colnames(durQuiet)[1] = "Day"
  colnames(durQuiet)[2] = "meanDurQuiet_min"
  colnames(durQuiet)[3] = "sdDurQuiet_min"
  tstV  = ddply(VESS, .(DateFstart), summarize, xm= (mean(as.numeric( Dur_mins),na.rm = TRUE )) )
  tstV2 = ddply(VESS, .(DateFstart), summarize, xm= (sd  (as.numeric( Dur_mins),na.rm = TRUE )) )
  durVess = cbind(tstV,tstV2[,2])
  colnames(durVess)[1] = "Day"
  colnames(durVess)[2] = "meanDurVess_min"
  colnames(durVess)[3] = "sdDurVess_min"
  #append these to day list
  FullListDay2 = merge(FullListDay2, durVess, by = "Day")
  FullListDay2 = merge(FullListDay2, durQuiet, by = "Day")

  #-----------------------------------------------------------------------------------------
  #COMBINE DAILY VD WITH AIS-- so truncates AIS data in most cases
  #-----------------------------------------------------------------------------------------
  cData = merge(FullListDay2,aData,by="Day")
  cols = c("LOA_S_OPHRS","LOA_M_OPHRS","LOA_L_OPHRS","LOA_NA_OPHRS")
  cData$LOA_ALL_OPHRS = rowSums(cData[,cols])
  cols = c("LOA_S_UV","LOA_M_UV","LOA_L_UV","LOA_NA_UV")
  cData$LOA_ALL_UV    = rowSums(cData[,cols])
  as.data.frame(colnames(cData))
  
  cData = cData[,c(1:20, 23:32)] #columns to sum AIS results
  #rename BB column so they all match!!!
  
  fnameOut =  paste0(outDir, sitesVD[ss], "_VesselDetectionAISDay_",as.character(min(as.Date(cData$Day))),"to",
                     as.character(max(as.Date(cData$Day))), "_v", DC, ".csv")  
  
  if(flagCSV == TRUE ){ write.csv(cData,fnameOut)} 
  
  #PLOT of DAILY CONDITIONS--- points with monthly running average
  
  cDatat = cData
  #-----------------------------------------------------------------------------------------
  #A1) daily Vessel detections
  p1 = ggplot(cDatat,aes(Day,totalVessels, color = deply) ) +
    geom_point(alpha = .2)+   geom_line(aes(y=rollmean(totalVessels, ra, na.pad=TRUE)),size=1) +
    xlab("")+     ylab("Daily vessel detections") + theme_minimal()+
    ylim(c(0, round(max(cDatat$totalVessels, na.rm = TRUE)) ) ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme(legend.position=c(.8, 0.7), axis.text.x = element_text(angle=45, hjust = 1), legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  
  #B1) daily AIS vessels by type
  dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_UV","LOA_M_UV","LOA_L_UV","LOA_ALL_UV" ))
  p3 =  ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
    geom_point(alpha = .2)+   geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=1) +
    ylab("Daily unique vessels")+     xlab("") +     theme_minimal() + 
    ylim(c(0, round(max(cDatat$totalVessels, na.rm = TRUE)) ) ) +
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme( legend.position=c(.8, 0.7), axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  
  #A2) % day dominated by Vessel noise, from total time dominated by vessel noise
  # hist(cDatat$PerDay)
  p2 = ggplot(cDatat,aes(Day,PerDay, color = deply) ) +
    geom_point(alpha = .2)+  geom_line(aes(y=rollmean(PerDay, ra, na.pad=TRUE)),size=1) +
    xlab("")+   ylab("% day dominated by vessel noise") + theme_minimal() +
    ylim(c(0,   round(max(cDatat$PerDay, na.rm = TRUE)) ) ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme(legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0))) 
  
  #B2) total operational hours near site
  dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_OPHRS","LOA_M_OPHRS","LOA_L_OPHRS","LOA_ALL_OPHRS" ))
  dataAISm$Perday = ( (dataAISm$value) / 24)*100 #calcualte percent of day ships operating, can be > 100%
  p5 = ggplot(dataAISm, aes(x=Day, y=Perday, color=factor(variable)) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(Perday, ra, na.pad=TRUE)),size=1) +
    ylab("% of day vessels operating")+     xlab("") +     theme_minimal() + 
    #ylim(c(0, round(max(cDatat$PerDay, na.rm = TRUE)) ) ) +
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot)))+
    theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  
  pALL2 = grid.arrange(p1,p3,p2,p5,nrow=2,ncol=2,top = (paste0( "Daily vessel metrics (", sitesVD[ss], ")" )))
  
  p4 = ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=.7) +
    ylab("Daily operational hours")+     xlab("") +     theme_minimal() + 
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot)))+
    theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  
  ggsave( paste0(outDir,sitesVD[ss], "_DailyVessleMetrics_", "_v",DC,".png") ,pALL2)
  
  #Normalizing values.... scale 0-1
  #-----------------------
  #A1) Total vessels
  cDatat$totalVesselsS = stdize(cDatat$totalVessels,na.rm=T)
  p1 = ggplot(cDatat,aes(Day,totalVesselsS,color = deply) ) +
    geom_point(alpha = .2)+  geom_line(aes(y=rollmean(totalVesselsS, ra, na.pad=TRUE)),size=1) +
    annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max = ", round(max(cDatat$totalVessels,na.rm = T)) ),fontface="bold" ) +
    xlab("")+     ylab("scaled daily vessel detections") + theme_minimal()+
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme(legend.position=c(.8, 0.6), axis.text.x = element_text(angle=45, hjust = 1), legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  #B1) daily AIS vessels by type
  cDatat$LOA_S_UVS = stdize(cDatat$LOA_S_UV,na.rm=T)
  cDatat$LOA_M_UVS = stdize(cDatat$LOA_M_UV,na.rm=T)
  cDatat$LOA_L_UVS = stdize(cDatat$LOA_L_UV,na.rm=T)
  cDatat$LOA_ALL_UVS = stdize(cDatat$LOA_ALL_UV,na.rm=T)
  dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_UVS","LOA_M_UVS","LOA_L_UVS","LOA_ALL_UVS" ))
  p3 =  ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=1) +
    annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max (ALL) = ", round(max(cDatat$LOA_ALL_UV,na.rm = T))),fontface="bold" ) +
    ylab("scaled daily unique vessels")+     xlab("") +   theme_minimal() + 
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme( legend.position=c(.8, 0.6), axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  #A2) % day dominated by Vessel noise, from total time dominated by vessel noise
  cDatat$PerDayS = stdize(cDatat$PerDay,na.rm=T)
  p2 = ggplot(cDatat,aes(Day,PerDayS,color = deply) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(PerDayS, ra, na.pad=TRUE)),size=1) +
    annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max = ", round(max(cDatat$PerDay,na.rm = T)) ,"%"),fontface="bold" ) +
    xlab("")+   ylab("scaled % day dominated by vessel noise") + theme_minimal() +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  #B2) total operational hours near site AND percent of day
  #how do I scale percent of the day???
  cDatat$LOA_S_OPHRSS   = stdize( ((cDatat$LOA_S_OPHRS/24)*100)  ,na.rm=T)
  cDatat$LOA_M_OPHRSS   = stdize( ((cDatat$LOA_M_OPHRS/24)*100)  ,na.rm=T)
  cDatat$LOA_L_OPHRSS   = stdize( ((cDatat$LOA_L_OPHRS/24)*100)   ,na.rm=T)
  cDatat$LOA_ALL_OPHRSS = stdize( ((cDatat$LOA_ALL_OPHRS/24)*100) ,na.rm=T)
  dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_OPHRSS","LOA_M_OPHRSS","LOA_L_OPHRSS","LOA_ALL_OPHRSS" ))
  mx = round(max(((cDatat$LOA_ALL_OPHRS/24)*100),na.rm = T))
  which.max(cDatat$LOA_ALL_OPHRS)
  
  p4 = ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=1) +
    annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max (ALL) = ", mx ,"%"), fontface="bold" ) +
    ylab("scaled % of day vessels operating")+     xlab("") +     theme_minimal() + 
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot)))+
    theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1), legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  
  pALLS = grid.arrange(p1,p3,p2,p4,nrow=2,ncol=2,top = (paste0( "scaled Daily vessel metrics (", sitesVD[ss], ")" )))
  
  ggsave( paste0(outDir,sitesVD[ss], "_DailyVessleMetricsScaled_", "_v",DC,".png"), pALLS)
  
  
  #SUMMARY of metrics by site
  #-----------------------
  a0 = nrow( cDatat[!is.na(cDatat$OL_31.5),] )#length(unique(cDatat$Day)) # number of AIS days with data....
  
  #AIS averages
  a1 = mean(as.numeric(as.character(cDatat$LOA_S_UV)),na.rm = T)
  a2 = mean(as.numeric(as.character(cDatat$LOA_M_UV)),na.rm = T)
  a3 = mean(as.numeric(as.character(cDatat$LOA_L_UV)) ,na.rm = T)
  a4 = mean(as.numeric(as.character(cDatat$LOA_ALL_UV)),na.rm = T)
  
  a5 = mean(as.numeric(as.character(cDatat$LOA_S_OPHRS)),na.rm = T)
  a6 = mean(as.numeric(as.character(cDatat$LOA_M_OPHRS)),na.rm = T)
  a7 = mean(as.numeric(as.character(cDatat$LOA_L_OPHRS)),na.rm = T)
  a8 = mean(as.numeric(as.character(cDatat$LOA_ALL_OPHRS)),na.rm = T)
  
  #percent of day OPHRS
  a13 = mean(as.numeric(as.character( ((cDatat$LOA_S_OPHRS/24)*100))), na.rm = T) 
  a14 = mean(as.numeric(as.character( ((cDatat$LOA_M_OPHRS/24)*100))), na.rm = T)
  a15 = mean(as.numeric(as.character( ((cDatat$LOA_L_OPHRS/24)*100))), na.rm = T)
  a16 = mean(as.numeric(as.character( ((cDatat$LOA_ALL_OPHRS/24)*100))), na.rm = T)
  
  #vessel detection averages-- new variables, not include the hourly summaries
  #cDatat$totalVessels,   cDatat$sumTotalTime_sec,   cDatat$HRmeanTotalTime_sec,   cDatat$HRmeanVesselDet_sec,   cDatat$PerDay
  a12 = mean(as.numeric(as.character(cDatat$totalVessels)),na.rm = T)
  a11 = mean(as.numeric(as.character(cDatat$PerDay)),na.rm = T)
  a9  = mean(as.numeric(as.character(cDatat$sumTotalTime_sec)),na.rm = T)
  # a10 = mean(as.numeric(as.character(cDatat$sumTotalVessels)) ,na.rm = T) #no longer used so NA
  
  #median SPLs
  g = c( as.numeric(grep("^OL", colnames(cDatat) ) ), 14)
  tmpSPL = ( cDatat[,g] )
  w <- which( sapply( tmpSPL, class ) == 'factor' )
  tmpSPL[w] <- lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
  SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
  SPLnames = colnames(tmpSPL)
  
  output = rbind (output, c(cDatat$Site[1], length(sFiles), nFiles, a0, a1,a2,a3,a4, a5,a6,a7,a8, a13,a14,a15,a16, 
                            a9,a11,a12, SPLm ) )
  
  # CLEAN UP
  rm(edH,sFiles,errorC, pALL,pVes,pVESStime,pVESSvess, tmp, VESS, VESSfor, VESSforDay, x, 
     edB,ff,fnameOut,HrS,hrsSpan,idx,inFiles,inFilesB, vv,stH,DtimeMean,DtimeSum,DvessSum,DvessMean, HRsampled, pALL2,pVESSvess2)
  
  
}

output = as.data.frame(output)
colnames(output) = c("Site","shipFiles", "SPLfiles", "TotalDays",
                     "mAIS_S_UV",      "mAIS_M_UV",      "mAIS_L_UV",     "mAIS_A_UV",
                     "mAIS_S_OPHRS",   "mAIS_M_OPHRS",   "mAIS_L_OPHRS",  "mAIS_A_OPHRS",
                     "mAIS_S_PerOPHRS","mAIS_M_PerOPHRS","mAIS_L_PerOPHRS","mAIS_A_PerOPHRS",
                     "mTotalTime","mPerDay","mTotalVesselsday",
                     SPLnames)
