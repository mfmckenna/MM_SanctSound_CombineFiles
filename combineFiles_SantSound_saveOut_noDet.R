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
siteLoc =c(42.436838,	-70.546958)#--SB01
  #c(42.436838,	-70.546958)#--SB01
  #c(24.488800,-81.666316) #--FK02 
  #c(31.396417,-80.8904)   #--GR01  
  #c(20.8075666666666,-156.65615)#--HI01
  #c(34.0438,-120.08105)#--CI0
  #c(24.43313,-81.93068)#--FK04 
  #c(48.4904,-125.0038)#--OC02 
  #c(48.3938,-124.654)#--OC01
  #c(36.798,-121.976)#--MB01 
  #c(36.648,-121.9084)#--MB02
site  = 'SB' 
deply = "SB01"
dir   = paste0("E:\\RESEARCH\\SanctSound\\data\\",site)
dir2  = paste0("E:\\RESEARCH\\SanctSound\\data\\",site,"\\", deply)
setwd(dir)
DC = Sys.Date()
outDir = "E:\\RESEARCH\\SanctSound\\data\\combineFiles"
detFlag = 0
analysisPeriods = read.csv("E:\\RESEARCH\\SanctSound\\data\\AnalysisPeriods.csv")

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#READ IN in data
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
rm(dname, endR,ff,nFiles)
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

#-----------------------------------------------------------------------------------------
#CLEAN UP big matrix-- NEED TO MAKE SURE COLUMNS ARE IN THE SAME ORDER!!!
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
dataALL = SplAisWspdTide
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

dataALLc = select( dataALL, c(a,b,c,d,e,f,g,h,i,j,k,l,m) )
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
#p = as.numeric(grep("*Det", colnames(dIn) ))
#q = as.numeric(grep("*Expl", colnames(dIn) ))

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
                    nrow(dtmp)) )
  
  rm(dtmp,Atotal, Ahtotal, perAtotal,SPLm,WSPDm,TIDEm,depl)
}

#!!! NEED TO MAKE SURE COLUMNS ARE IN THE SAME ORDER!!!
#  Site, Deployment, Day, Year, Julian, SPLs, AIS, WSPD, TIDE--2, vess det-3, bio det, hourssamples)
daySum1 = as.data.frame(daySum1)
colnames(HrSumALL)
colnames(daySum1)[1:5]    = c(colnames(HrSumALL)[1:3],colnames(HrSumALL)[5:6]) #dates
colnames(daySum1)[17:26]  = c(colnames(HrSumALL)[18:27])                       #AIS
colnames(daySum1)[27:28]  = c(colnames(HrSumALL)[28], colnames(HrSumALL)[30])  #WSPD,TIDE
#colnames(daySum1)[29:31]  = c("VessD_TV","VessD_VD_meanPerHr","VessD_Time_PerDay")
#colnames(daySum1)[32:34]  = c("ExplD_TV","ExplD_VD_meanPerHr","ExplD_Time_PerDay")
colnames(daySum1)[29] = "totHRs"
daySum1$Day = as.Date(daySum1$Day )
daySumALL = daySum1 
# MAKE NUMERIC VALUES
daySumALL[4:29] <- lapply( daySumALL[4:29], function(x) as.numeric(as.character(x)) )

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#TRUNCATE by time period of interest
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
tmpAP = analysisPeriods[analysisPeriods$Site == deply,]
HrTrunData  = HrSumALL[ (HrSumALL$Day >= as.Date(tmpAP$DateStart,format = "%m/%d/%Y") & HrSumALL$Day  <= as.Date(tmpAP$DateEnd,format = "%m/%d/%Y") ),]
DayTrunData = daySumALL[(daySumALL$Day>= as.Date(tmpAP$DateStart,format = "%m/%d/%Y") & daySumALL$Day <= as.Date(tmpAP$DateEnd,format = "%m/%d/%Y") ),]

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#WRITE OUT FILES
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
setwd(outDir)
DC = Sys.Date()
fnameAll =     paste0(deply, "_CombinedDataHR_",as.character(min(as.Date(HrSumALL$Day))),"_",
                   as.character(max(as.Date(HrSumALL$Day))), "_v", DC, ".csv")  
write.csv(HrSumALL,fnameAll) 

fnameAllday =  paste0(deply, "_CombinedDataDAY_",as.character(min(as.Date(HrSumALL$Day))),"_",
                      as.character(max(as.Date(HrSumALL$Day))) , "_v", DC, ".csv")   
write.csv(daySumALL,fnameAllday)

tmpAP$DateStart = as.Date(as.Date(tmpAP$DateStart,format = "%m/%d/%Y"),format = "%Y-%m-%d")
tmpAP$DateEnd   = as.Date(as.Date(tmpAP$DateEnd,format = "%m/%d/%Y"),format = "%Y-%m-%d")
as.Date(tmpAP$DateStart,format = "%m/%d/%Y")
fnameTrun =  paste0(outDir, "\\", deply, "_", tmpAP$Analysis,"DataHR_",as.character(tmpAP$DateStart),"_",as.character(tmpAP$DateEnd) , "_v", DC, ".csv")   
write.csv(HrTrunData,fnameTrun)

fnameTrun =  paste0(outDir, "\\", deply, "_", tmpAP$Analysis,"DataDay_",as.character(tmpAP$DateStart),"_",as.character(tmpAP$DateEnd) , "_v", DC, ".csv")  
write.csv(DayTrunData,fnameTrun)

#CHECK DATA INPUT PLOT-- too much to plot, need to break up
#grid.arrange(pSPL,pVes, pAISa,pAISaUV,pTide, pWind, ncol = 2, nrow = 3)
pSPLa = grid.arrange(pSPL,pSPLB, ncol = 2,nrow = 1)   #acoustic data
ggsave(paste0(outDir,"\\",deply,"_SPLSummary_created_",DC,".png"),pSPLa)
pAISt = grid.arrange(pAISa,pAISaUV,     ncol = 2,nrow = 1) #AIS
ggsave(paste0(outDir,"\\",deply,"_AISSummary_created_",DC,".png"),pAISt)
pEVI = grid.arrange(pTide,pWind, ncol = 2,nrow = 1) #environmental data
ggsave(paste0(outDir,"\\",deply,"_ENVSummary_created_",DC,".png"),pEVI)

