# combine abiotic conditions with SanctSound 1_processVesselDetecionsAIS.R

# CURRENTLY, ONLY WORKS FOR SB02!!!


rm(list=ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(suncalc)

outDir = "E:\\RESEARCH\\SanctSound\\data2\\combineFiles2_Abiotic\\"
inDir  = "E:\\RESEARCH\\SanctSound\\data2\\combineFiles1_SPLShips\\"
tDir   = "E:\\RESEARCH\\SanctSound\\data\\"

#-----------------------------------------------------------------------------------------
#INPUT AIS and Vessel Detection metrics (output of 1b_processVesselDetectionsAIS) 
## DAILY DATA
#-----------------------------------------------------------------------------------------
site = "SB02"
sant = "SB"
#INPUT a per site .csv file... 
infile = list.files(inDir, pattern = paste0(site,"_SPLVesselDetectionsAIS_Day"), full.names = T) #choose.files()
output4 = read.csv(infile)
as.data.frame(colnames(output4))
SplVes = output4[output4$Site == "SB02", ]
SplVes$Day   = as.Date(SplVes$Day, format = "%Y-%m-%d")

SplVes = SplVes %>% mutate_if(is.factor,as.character)

#-----------------------------------------------------------------------------------------
#read in abiotic data for the sanctuary
#-----------------------------------------------------------------------------------------
#location
#-----------------------------------------------------------------------------------------
siteLoc = read.csv(paste0(tDir, "SiteLocations.csv"))
sLoc    = siteLoc[as.character(siteLoc$Site) == as.character(site),1:3]

#tide
#-----------------------------------------------------------------------------------------
dirAb = paste0(tDir,sant)
setwd(dirAb)

filenames = list.files(dirAb,pattern = "^CO-OPS")
TIDE = NULL
for (ff in 1:length(filenames)) {
  TIDE = rbind(TIDE, read.csv(filenames[ff]))
}
#formate data
TIDE$DateFday = as.Date(TIDE$Date ,format = "%Y/%m/%d")
TIDE$DateF    = as.POSIXct( paste(TIDE$DateFday, paste(TIDE$`Time (GMT)`,":00", sep = ""), sep=" "), tz = "GMT")
TIDE$changeWL = c("NA", diff(TIDE$Verified..ft.) )
TIDE$changeWL = as.numeric (as.character(TIDE$changeWL))

#Data check plot
pTide = ggplot(TIDE, aes(DateFday, as.numeric(as.character(changeWL)) ) )+
  geom_point() +
  xlab("")+
  ylab("Hourly change in water level") +
  theme_minimal()+ggtitle(paste("Check- TIDE(",site, ")"))
pTide

#aggregate by day
uday = unique(TIDE$DateFday)
TIDEday = NULL
for (dd in 1:length(uday))
{
  tmp = TIDE[TIDE$DateFday== uday[dd],] 
  tmp = tmp[complete.cases(tmp),]
  TIDEday = rbind(TIDEday, c(as.character(uday[dd]), mean(tmp$changeWL), sd(tmp$changeWL), nrow(tmp) ) )
  
}
TIDEday = as.data.frame(TIDEday)
colnames(TIDEday) = c("Day","avgWL","sdWL","nSamples")
TIDEday$Day   = as.Date(TIDEday$Day, format = "%Y-%m-%d")
TIDEday = TIDEday %>% mutate_if(is.factor,as.character)
TIDEday = TIDEday %>% mutate_if(is.character,as.numeric)

#wind
#-----------------------------------------------------------------------------------------
filenames = list.files(dirAb,pattern = "MetData")
WSPD = NULL
for (ff in 1: length (filenames)){
  tfile = paste0(dirAb, "\\", filenames[ff])
  tmp   = fread(tfile)
  tmp2   = tmp[2:nrow(tmp),1:7]
  WSPD   = rbind(WSPD,tmp2)
}
endR = nrow(WSPD)
WSPD = as.data.frame(WSPD[2:endR,])

#formate data
WSPD$DateC    =  ( paste( WSPD$`#YY`, "-", WSPD$MM, "-", WSPD$DD, " ", WSPD$hh, ":",  WSPD$mm , sep = ""))
WSPD$DateF = as.POSIXct(WSPD$DateC,"%Y-%m-%d %H:%M",tz ="GMT")
WSPD$WSPD  = as.numeric(as.character(WSPD$WSPD  )) #some days with NAs which( is.na(WSPD$WSPD)) = 9
max(WSPD$WSPD,na.rm= T) 
WSPD$WSPD[WSPD$WSPD == 99] = NA
plot

pWind = ggplot(WSPD, aes(DateF, WSPD ) )+
  geom_point() +
  xlab("")+
  ylab("Wind Speed (mps) ") +
  theme_minimal()+ggtitle(paste("Check- WIND (",site, ")"))
pWind

#format data
WSPD$Day   = as.Date(WSPD$DateF, format = "%Y-%m-%d")
WSPD$HR    = strftime(WSPD$DateF, format="%H") #hour(WSPD$DateF)
WSPD$DayHR = as.POSIXct( paste0(WSPD$Day," ", WSPD$HR,":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
WSPDavg = aggregate(WSPD$WSPD, by=list(WSPD$DayHR), mean)
colnames(WSPDavg) = c("DateF","avgWSPD")

#aggregate by day
uday = unique(WSPD$Day)
WSPDday = NULL
cutOff = 5.1
for (dd in 1:length(uday))
{
  tmp = WSPD[WSPD$Day== uday[dd],] 
  tmp = tmp[complete.cases(tmp),]
  WSPDday = rbind(WSPDday, c(as.character(uday[dd]), mean(tmp$WSPD, na.action = na.omit), sd(tmp$WSPD), nrow(tmp), nrow(tmp[tmp$WSPD>cutOff, ])/nrow(tmp) ) )
  
}
WSPDday = as.data.frame(WSPDday)
colnames(WSPDday) = c("Day","avgWSPDL","sdWSPD","nSamples","TimeAbove")
WSPDday$Day   = as.Date(WSPDday$Day, format = "%Y-%m-%d")
WSPDday = WSPDday %>% mutate_if(is.factor,as.character)
WSPDday = WSPDday %>% mutate_if(is.character,as.numeric)


#-----------------------------------------------------------------------------------------
#COMBINE WIND and TIDE-- daily time scale
#-----------------------------------------------------------------------------------------
SplVesWspd         = merge(SplVes, WSPDday,      all = FALSE, all.x = TRUE, by = "Day" )#mean at the top of the hour
SplVesWspdTide     = merge(SplVesWspd, TIDEday,  all = FALSE, all.x = TRUE, by = "Day" )#mean at the top of the hour


#SUN ALTITUDE-- append to wind
tmp = getSunlightPosition(SplVesWspdTide$Day ,sLoc$lat, sLoc$lon)
SplVesWspdTide$sunAlt = tmp$altitude


setwd(dirAb)
DC = Sys.Date()
SplVesWspdTide$Site[1]
fnameAll =     paste0(SplVesWspdTide$Site[1], "_CombinedData_SplVesAbiotic_Day_",as.character(min(as.Date(SplVesWspdTide$Day))),"to",
                      as.character(max(as.Date(SplVesWspdTide$Day))), "_ver", DC, ".csv")  
write.csv(SplVesWspdTide,fnameAll) 
setwd(outDir)
write.csv(SplVesWspdTide,fnameAll) 
dcol = as.data.frame(colnames(SplVesWspdTide))

dcol

#-----------------------------------------------------------------------------------------
# EXTRA: DATA EXPLORATION-- SB02 (MIGHT NOT WORK BECAUSE ABOVE CODE CHANGED!)
#-----------------------------------------------------------------------------------------
#March 25-31 201 Vs 2020 julian day 83-90
#WIND SPEED
tmp = SplAisWspdTide[SplAisWspdTide$JulianDay > 82 & SplAisWspdTide$JulianDay < 91,]
tmp$Yr = year(tmp$Day)

ggplot(tmp, aes(as.factor(JulianDay), avgWSPD,fill=as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=1, notch=FALSE) +
  xlab("")+
  ylab("Wind Speed (mps) ") +
  ggtitle(("SB02: March 25-31 [2019=5.8, 2020=7.1]"))+
  theme_minimal()
tmp %>% group_by(Yr) %>% summarize(newvar = mean(avgWSPD,na.rm = T))

