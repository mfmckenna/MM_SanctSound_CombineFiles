#example graphic for COVID study-- July 2019
rm(list=ls())
library(lubridate)
library(dplyr)
library(ggplot2)

#working directory: 
dir = "D:\\RESEARCH\\SanctSound\\data\\example_MB01_02_July2019"
setwd(dir)
#READ IN in data
SPL   = read.csv("SanctSound_MB01_02_OL_1h.csv")
AIS   = read.csv("AIS_MB_2018_10_to_2020_02.csv")
TIDE  = read.csv("CO-OPS_9413450_wl_MB_20190701.csv") #water levels-- need to get July data!!
WSPD  = read.csv("MB_MetData_2019.csv")
VESS  = read.csv("SanctSound_MB01_02_ships.csv") #start end times

#FORMATE AND TRUNCATE DATA
#SPL is hourly resolution (top of hour)
SPL$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPL$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" )
SPL$DateFday  = as.Date(SPL$DateF)
#AIS is daily resolution
AIS$DateFday     = as.Date(AIS$DATE,format = "%m/%d/%Y")
AIS = AIS[AIS$LOC_ID == "MB01",]
#TIDE is hourly resolution (top of hour)
TIDE$DateFday    = as.Date(TIDE$Date ,format = "%Y/%m/%d")
TIDE$DateF    = as.POSIXct(paste(TIDE$DateFday, paste(TIDE$Time..GMT.,":00", sep = ""), sep=" "), tz = "GMT")
TIDE$changeWL   = c("NA", diff(TIDE$Verified..ft.))
#WSPD is hourly (not at top of hour, so changed)
WSPD$DateF    = as.POSIXct ( paste( WSPD$X.YY, "/", WSPD$MM, "/", WSPD$DD, " ", WSPD$hh, ":", 00, ":", 00, sep = ""), tz = "GMT")
#VESS are start and end of detections, leave as is and then find matches in a given hour
VESS$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$ISOStartTime)), tz = "GMT" )
VESS$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$ISOEndTime)), tz = "GMT" )


#COMBINE: for each SPL time step, find associated metrics AIS, WSPD, TIDE, VESS using formated date
#MERGE: with daily AIS values... each SPL time step has same AIS value!!
SPLAIS = merge(SPL, AIS, all = FALSE, all.x = TRUE, by = "DateFday" ) 
SPLAISWSPD = merge(SPLAIS, WSPD, all = FALSE, all.x = TRUE, by = "DateF" )
SPLAISWSPDTIDE = merge(SPLAISWSPD, TIDE, all = FALSE, all.x = TRUE, by = "DateF" )

# VESS is tricky... for each each hour, how many vessels detected, and percent of hour present
ii = 54
SPLAISWSPDTIDE$VESSdet = NA
for(ii in 1:nrow(SPLAISWSPDTIDE)){
  tst = VESS[ between(VESS$DateFs, SPLAISWSPDTIDE$DateF[ii], SPLAISWSPDTIDE$DateF[ii]+(60*60)-1 ),]
  
  if (nrow(tst) > 0){ 
    cat(as.character(SPLAISWSPDTIDE$DateF[ii])," ", ii, ": ", nrow(tst), "\n") #keep track of where there are matches
    SPLAISWSPDTIDE$VESSdet[ii] =  nrow(tst)
    durT = 0
    for (rr in 1:nrow(tst)){
      durtmp = as.numeric (difftime(tst$DateFe[rr],tst$DateFs[rr],unit="secs") )
      durT = durT + durtmp
      rm(durtmp)
    }
    SPLAISWSPDTIDE$VESSdur[ii] = durT
    rm(durT)
  } else { #no vessels detected in that hour
      SPLAISWSPDTIDE$VESSdet[ii] = NA
      SPLAISWSPDTIDE$VESSdur[ii] = 0}
}
SPLAISWSPDTIDE$VESSdurPer = SPLAISWSPDTIDE$VESSdur/(60*60)
#PLOT: with daily summary of the data in tile format-- just July 2019
jul2019 = SPLAISWSPDTIDE[SPLAISWSPDTIDE$DateFday.x >= as.Date("2019/07/01") & SPLAISWSPDTIDE$DateFday.x < as.Date("2019/08/01"), ]
jul2019$JulDay = yday(jul2019$DateF)
# jul2019$OL_63
# jul2019$LOA_S_UV
# jul2019$LOA_M_UV
# jul2019$LOA_L_UV
# jul2019$VESSdet

jul2019m = reshape :: melt(jul2019, id.vars = "DateFday.x", 
                                   measure.vars = c("LOA_S_UV","LOA_M_UV","LOA_L_UV","VESSdet","VESSdurPer"))
jul2019m$JulDay = yday(jul2019m$DateFday.x)
jul2019m$Year = year(jul2019m$DateFday.x)

#START HERE.... how to visualize!!! 
daySum1 = NULL # summarize by day!
uday = unique(jul2019$JulDay)
for (ii in 1:length(uday)){
  dtmp = jul2019[jul2019$JulDay == uday[ii],]
  
  #AIS
  Atotal  = dtmp$LOA_S_UV[1] + dtmp$LOA_M_UV[1] + dtmp$LOA_L_UV[1] + dtmp$LOA_NA_UV[1]
  Ahtotal = dtmp$LOA_S_OPHRS[1] + dtmp$LOA_M_OPHRS[1] + dtmp$LOA_L_OPHRS[1] + dtmp$LOA_NA_OPHRS[1]
  perAtotal = Ahtotal/24
  
  #vessel detect
  perVD = sum(dtmp$VESSdur)/(60*60*24)
  ttVD = sum(dtmp$VESSdet,na.rm = T)
  
  #SPL- median per octave band
  mSPL = apply( dtmp[,4:13] , 2 , quantile , probs = 0.5 , na.rm = TRUE )

  #Combine to output
  daySum1 = rbind(daySum1,c(as.character(dtmp$DATE[1]), uday[ii], Atotal,Ahtotal,perAtotal,perVD,ttVD,mSPL,nrow(dtmp)) )
  rm(dtmp,Atotal,Ahtotal,perAtotal,perVD,ttVD,mSPL)
}
colnames(daySum1) = c("Day","JulDay","AISt","AISophrs","AISophrsPer","VessDtper","VessDt","mOB_31.5","mOB_63",
                      "mOB_125","mOB_250","mOB_500","mOB_1000","mOB_2000","mOB_4000","mOB_8000","mOB_16000","totHRs")
daySum1 = as.data.frame(daySum1)
as.numeric(as.character(daySum1$JulDay))

ggplot(daySum1, aes(as.numeric(as.character(JulDay)), as.numeric(VessDtper), color = as.numeric(AISt))) +
  geom_point()+
  geom_linerange(aes(x=as.numeric(as.character(JulDay)), ymax=as.numeric(VessDtper), ymin=0)) +
  xlab("Julian Day") +
  ylab("Percent of day dominated by vessels")+
  theme_minimal()
  
ggplot(daySum1, aes(as.numeric(VessDtper), as.numeric(AISophrsPer)) ) +
  geom_point()+
  xlab("Percent of day dominated by vessels") +
  ylab("Percent of day with AIS vessels")+
  theme_minimal()

# tile plot with source in each row... but metrics do not work for this
uSource = unique(jul2019m$variable)
daySum = NULL
for (ii in 1:length(uSource)){
  dtmp = jul2019m[jul2019m$variable == uSource[ii],]
  
  #all days with source of interest
  uday = unique(dtmp$DateFday.x)
  
  for (dd in 1:length(uday)){ #for each day total up the samples with source and total samples
    
    if (uSource[ii] == "VESSdet") {# VESSdet can sum up total for a given day
      dtmp2  = dtmp[dtmp$DateFday.x == uday[dd],]
      daySum = rbind(daySum, c( (as.character( uday[dd])), as.character(uSource[ii]), 
                                sum(as.numeric(dtmp2$value), na.rm = T), 
                                nrow(dtmp2)) )
      
    } else {                       # AIS data is a per day metric- so just switch to total 
      dtmp2  = dtmp[dtmp$DateFday.x == uday[dd],]
      daySum = rbind(daySum, c( as.character( uday[dd]), as.character(uSource[ii]), 
                                as.numeric(dtmp2$value[dd], na.rm = T), 
                                nrow(dtmp2) ) )
    }
    
    
  }
  rm(dtmp,uday,dtmp2)
}

colnames(daySum) = c("Day","variable","samples","total")
daySum = as.data.frame(daySum)
daySum$Day2 = as.Date(daySum$Day)
daySum$JulDay = yday(daySum$Day2)
daySum$Year = year(daySum$Day2)

daySum$perSample = as.numeric(as.character(daySum$samples))/as.numeric(as.character(daySum$total))*100
uvar = unique(daySum$variable)

ggplot(daySum, aes(Day2, variable, fill= as.numeric(perSample))) +
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  #scale_y_discrete(labels = ulabs) +
  labs(fill = "% Daily Samples") +
  xlab("") +
  ylab("")
