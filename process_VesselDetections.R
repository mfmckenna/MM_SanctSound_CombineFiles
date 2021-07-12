# process sancturary sound vessel detection data
rm(list=ls())

#-----------------------------------------------------------------------------------------
# MAIN function
#-----------------------------------------------------------------------------------------
# Code convert vessel detection files: for each each hour, how many unique vessels detected, and percent of hour dominated by vessel noise
# some of the time will spill into the next hour, need to keep track of this
# reformat so for each hour there is a total vessel time value and total vessel count

#-----------------------------------------------------------------------------------------
# TO ADD (future versions)
#-----------------------------------------------------------------------------------------
#line 82... something seems fishy with this because I have to keep adding hours to the max value!!
# errors on sites 8 HI01... need to look into more

#-----------------------------------------------------------------------------------------
# LOAD Libraries
#-----------------------------------------------------------------------------------------
library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(scales)

#-----------------------------------------------------------------------------------------
# DIRECTORY SET UP
#-----------------------------------------------------------------------------------------
dir   = paste0("E:\\RESEARCH\\SanctSound\\detectorResults\\SanctSound_VesselDetection_DataProducts")
#dir   = paste0("E:\\RESEARCH\\SanctSound\\data")
nFiles = length( list.files(path=dir,pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFiles = ( list.files(path=dir,pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
outDir ="E:\\RESEARCH\\SanctSound\\detectorResults\\SanctSound_VesselDetection_DataProducts\\"
#outDir = "E:\\RESEARCH\\SanctSound\\analysis\\VesselDetections2\\"
DC = Sys.Date()
inFilesB = basename(inFiles)
x = strsplit(inFilesB,"_")
sites = unique(sapply( x, "[", 2 ))

#-----------------------------------------------------------------------------------------
# PROCESS DETECTION FILES by site
#-----------------------------------------------------------------------------------------
for (ss in 1:length(sites)) { #loop through each site ss = 1
  
  
  cat("Processing...", sites[ss], ":",ss, "of", length(sites),"\n")
  sFiles = list.files(path=dir, pattern = paste0(sites[ss],".*hips.csv"), full.names=TRUE, recursive = TRUE)
  VESS=NULL
  
  #COMBINE deployments
  #-----------------------------------------------------------------------------------------
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
  
  #CHECK plot
  #-----------------------------------------------------------------------------------------
  pVes = ggplot(VESS, aes(DateFs, Dur_mins))+
    geom_point() +
    xlab("")+
    ylab("Duration Vessel Detections (mins)") +
    theme_minimal()+
    ggtitle(paste( "Check vessel detections (", sites[ss], ")" ))
  
  VESS$DateFstart = force_tz(as.Date(VESS$DateFe), tzone="GMT")
  VESS$HrS =hour(VESS$DateFs)
  VESS$HrE =hour(VESS$DateFe)

  
  #MAKE new matrix to fill in the values for each hour
  #-----------------------------------------------------------------------------------------
  beginTime =   as.POSIXct( paste0( min(VESS$DateFstart)," ", (VESS$HrS[1]),":00:00"), tz="GMT")
  endTime   =   as.POSIXct(paste0( max(VESS$DateFstart) ," ",  VESS$HrE[nrow(VESS)],":00:00"), tz="GMT")
  VESSfor   = as.data.frame( seq(from=beginTime, to=endTime, by="hour") )
  rm(endTime,beginTime)
  
  VESSfor$totalVessels = 0
  VESSfor$totalTime = 0
  colnames(VESSfor) = c("DayHr","totalVessels","totalTime")
  VESSfor$DayHr = force_tz(VESSfor$DayHr, tzone="GMT")
  VESSfor$DateF = as.Date(VESSfor$DayHr)
  VESSfor$Deply = "noDetection"
  
  #CLEAN up VESS... sort and remove duplicates
  #-----------------------------------------------------------------------------------------
  VESSa = arrange(VESS, by_group = VESS$DateFs )
  VESS  = VESSa
  VESSd = dplyr::distinct(VESS)
  VESS = VESSd
  rm(VESSa,VESSd)
  
  #PROCESS each vessel detection, put in correct column in VESSfor
  #-----------------------------------------------------------------------------------------
  for (vv in  1: nrow(VESS) ){ #nrow(VESS)  1:nrow(VESS) 
    
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
      VESSfor$Deply[idx] = tmp$deply
      #add info to next hour
      VESSfor$totalVessels[idx+1]  = VESSfor$totalVessels[idx+1] + 1 # vessel count
      VESSfor$totalTime[idx+1]     = VESSfor$totalTime[idx+1] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds  difftime(tmp$DateFe, edH, units = "mins")
      VESSfor$Deply[idx+1] = tmp$deply
      
    } else if ((hrsSpan) == 0 ){ #within single hour
      VESSfor$totalVessels[idx]  = VESSfor$totalVessels[idx] + 1 # vessel count
      VESSfor$totalTime[idx]     = VESSfor$totalTime[idx] + difftime(tmp$DateFe,tmp$DateFs,  units = "secs") #time in seconds
      VESSfor$Deply[idx] = tmp$deply
      
    } else if  ((hrsSpan) >= 2 ) { #spans two or more hours
      
      midHr = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS+1,":00:00"), tz = "GMT" )
      
      #add info to start hour
      VESSfor$totalVessels[idx]  = VESSfor$totalVessels[idx] + 1 # vessel count
      VESSfor$totalTime[idx]     = VESSfor$totalTime[idx] + difftime(midHr, tmp$DateFs, units = "secs")
      VESSfor$Deply[idx] = tmp$deply
      
      #add info to middle hours--- need to loop this though each hour
      for(hh in 1:((hrsSpan)-1) ) {
        VESSfor$totalVessels[idx+hh]  = VESSfor$totalVessels[idx+hh] + 1 # vessel count
        VESSfor$totalTime[idx+hh]     = VESSfor$totalTime[idx+hh] + 3600 #time in seconds, full hour
        VESSfor$Deply[idx+hh] = tmp$deply
      }
      
      #add info to last hour
      VESSfor$totalVessels[idx+hrsSpan]  = VESSfor$totalVessels[idx+hrsSpan] + 1 # vessel count
      VESSfor$totalTime[idx+hrsSpan]     = VESSfor$totalTime[idx+hrsSpan] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds
      VESSfor$Deply[idx+hrsSpan] = tmp$deply
    }
    
    #cat("Hours for detection ", vv, ": ", hrsSpan, "hrs", "\n")
  }
  
  #FORMAT data
  #-----------------------------------------------------------------------------------------
  VESSfor$totalTimeM = VESSfor$totalTime/(60) 
  errorC = VESSfor[VESSfor$totalTimeM > 60,] # CHECk: hist(VESSfor$totalTime)
  
  VESSfor$DateF = VESSfor$DayHr
  VESSfor$JulDay = yday(VESSfor$DateF)
  VESSfor$Site = sites[ss]
  
  # PLOTS vessel detections
  #-----------------------------------------------------------------------------------------
  pVESStime = ggplot(VESSfor,aes(DayHr,totalTimeM/60,color = Deply) ) +
    geom_bar(stat="identity", position="dodge")+ #geom_point()+
    xlab("")+
    ylab("") +
    theme_minimal()+
    ylim(c(0,1)) +
    ggtitle(paste0( "Proportion of hour dominated by vessel noise (", sites[ss], ")" ))
  
  pVESSvess = ggplot(VESSfor,aes(DayHr,totalVessels,color = Deply) ) +
    geom_point()+
    xlab("")+
    ylab("") +
    theme_minimal()+
    ggtitle(paste0( "Unique vessel detections per hr (", sites[ss], ")" ))
  
  
  pALL = grid.arrange(pVESStime,pVESSvess,ncol=1, nrow = 2)
  
  # EXPORT HOURLY vessel detections
  #-----------------------------------------------------------------------------------------
  fnameOut =  paste0(outDir, sites[ss], "_VesselDetectionsHr_",as.character(min(as.Date(VESSfor$DayHr))),"to",
                     as.character(max(as.Date(VESSfor$DayHr))), "_v", DC, ".csv")  
  write.csv(VESSfor,fnameOut) 
  ggsave( paste0(outDir,sites[ss], "_VesselDetectionPlotHR_", "_v",DC,".png") ,pALL)
  

  # DAILY summaries and plots-- no deployment information exported
  #-----------------------------------------------------------------------------------------
  VESSfor$Date <- as.Date(VESSfor$DayHr)
  DtimeSum  = aggregate(VESSfor$totalTime, by=list(VESSfor$Date), sum)      #total time
  DtimeMean = aggregate(VESSfor$totalTime, by=list(VESSfor$Date), mean)     #mean per hour
  DvessMean = aggregate(VESSfor$totalVessels, by=list(VESSfor$Date), mean)  #average vessels per hour
  DvessSum  = aggregate(VESSfor$totalVessels, by=list(VESSfor$Date), sum)   #total vessels per day
  HRsampled = aggregate(VESSfor$totalTime, by=list(VESSfor$Date), length)   #total hours sampled
  
  VESSforDay = as.data.frame( cbind(DtimeSum, DtimeMean[,2], DvessSum[,2], DvessMean[,2], HRsampled[,2] ) )
  colnames(VESSforDay) = c("Date","sumTotalTime_sec","meanTotalTime_sec","sumTotalVessels","meanTotalVessels","HRsampled")
  VESSforDay$PerDay = (VESSforDay$sumTotalTime_sec/(VESSforDay$HRsampled *60*60))*100
  VESSforDay$Site = sites[ss]
  
  fnameOut =  paste0(outDir, sites[ss], "_VesselDetectionsDay_",as.character(min(as.Date(VESSfor$DayHr))),"to",
                     as.character(max(as.Date(VESSfor$DayHr))), "_v", DC, ".csv")  
  write.csv(VESSforDay,fnameOut) 
  
  pVESStime = ggplot(VESSforDay,aes(Date,PerDay) ) +
    geom_point()+
    xlab("")+
    ylab("") +
    ylim(c(0,100)) +
    theme_minimal()+
    scale_x_date(date_breaks = "2 month", 
                 labels=date_format("%b-%y"),
                 limits = as.Date(c('2018-11-01','2021-12-31')))+
    theme(axis.text.x = element_text(angle=45, hjust = 1)) +
    ggtitle(paste0( "Percent of day dominated by vessel noise (", sites[ss], ")" ))
  pVESSvess = ggplot(VESSforDay,aes(Date,sumTotalVessels) ) +
    geom_point()+
    xlab("")+
    ylab("") +
    theme_minimal()+
    scale_x_date(date_breaks = "2 month", 
                 labels=date_format("%b-%y"),
                 limits = as.Date(c('2018-11-01','2021-12-31')))+
    theme(axis.text.x = element_text(angle=45, hjust = 1)) +
    ggtitle(paste0( "Total vessel detections per day (", sites[ss], ")" ))
  pVESSvess2 = ggplot(VESSforDay,aes(Date,meanTotalVessels) ) +
    geom_point()+
    xlab("")+
    ylab("") +
    theme_minimal()+
    scale_x_date(date_breaks = "2 month", 
                 labels=date_format("%b-%y"),
                 limits = as.Date(c('2018-11-01','2021-12-31')))+
    theme(axis.text.x = element_text(angle=45, hjust = 1)) +
    ggtitle(paste0( "Hourly average vessel detections per day (", sites[ss], ")" ))
  
  pALL2 = grid.arrange(pVESStime,pVESSvess,pVESSvess2, ncol=1, nrow = 3)
  ggsave( paste0(outDir,sites[ss], "_VesselDetectionPlotDay_", "_v",DC,".png") ,pALL2)
  
  rm(edH,sFiles,errorC, pALL,pVes,pVESStime,pVESSvess, tmp, VESS, VESSfor, VESSforDay, x, 
     edB,ff,fnameOut,HrS,hrsSpan,idx,inFiles,inFilesB, vv,stH,DtimeMean,DtimeSum,DvessSum,DvessMean, HRsampled, pALL2,pVESSvess2)
  
  
}