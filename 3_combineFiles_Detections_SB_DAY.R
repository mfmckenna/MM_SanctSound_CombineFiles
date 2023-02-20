# combine detections with SanctSound 2_combineFiles_Abiotic.R

#only works for SB02, also plots comparisons for 2020 vs 2021

rm(list=ls())

#-----------------------------------------------------------------------------------------
library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)

#-----------------------------------------------------------------------------------------
tDir   = "E:\\RESEARCH\\SanctSound\\"
inDir  = paste0(tDir, "data2\\combineFiles2_Abiotic\\")
outDir = paste0(tDir, "data2\\combineFiles3_Detections\\")

#-----------------------------------------------------------------------------------------
sanct = "SB"
site  = "SB02"

inFile  = list.files(inDir, pattern = paste0(site,"_CombinedData_SplVesAbiotic_Day"), full.names = T) #choose.files()
iData = read.csv(inFile)
iData$Day   = as.Date(iData$Day, format = "%Y-%m-%d")
as.data.frame( colnames(iData) )
iData = iData %>% mutate_if(is.factor,as.character)
# summary(iData)


#-----------------------------------------------------------------------------------------
#Biological Detections
## atlanticcod, dolphins, bluewhale, finwhale, humpbackwhale, northatlanticrightwhale, seiwhale
#-----------------------------------------------------------------------------------------
dir2  = paste0(tDir, "data\\",sanct,"\\",site)

#ATLANTIC COD-- start and end times, need to convert to daily detections
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2, pattern = "atlanticcod", full.names=TRUE, recursive = TRUE))

#read in detection files
atlanticcod = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "atlanticcod", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "atlanticcod", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tmp  = fread(fnameF,header=F) #remove these after
  tmp$deplm = dname
  #cat(dname, "_", as.character( tmp[1,1]), ", ", as.character( tmp[1,2]) ,"\n")
  atlanticcod = rbind(atlanticcod,tmp)
  rm(fname,fnameF,tmp)
}
colnames(atlanticcod) = c( as.character(atlanticcod[1,1:3]) ,"site")
atlanticcod = atlanticcod[atlanticcod$Presence != "Presence",] #remove headers in the data frame
atlanticcod$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", atlanticcod$Begin_datetime_ISO6801)), tz = "GMT" )
atlanticcod$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", atlanticcod$End_datetime_ISO6801))  , tz = "GMT" )
atlanticcod$DUR_mins = atlanticcod$DateFe- atlanticcod$DateFs #in minutes!
atlanticcod$DayHR    = as.POSIXct( paste0(as.Date( atlanticcod$DateFs )," ", hour( atlanticcod$DateFs ),":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
atlanticcod$Day  = as.Date( atlanticcod$DateFs )

#combine with daily data- is start of the detection in specific day
iData$atlanticcod = 0
for(ii in 1:nrow(iData)){
  tdet = nrow( atlanticcod[atlanticcod$Day  == iData$Day[ii], ])
  if (tdet>0){ iData$atlanticcod[ii] =  tdet }
} 
plot(iData$atlanticcod )
idx = which(names(iData) == "atlanticcod")
names(iData)[idx] = "atlanticcodDet"

#convert to presence
for(ii in 1:nrow(iData)){
  tdet = iData$atlanticcodDet[ii]
  
  if (tdet > 0) {
    iData$atlanticcodP[ii] = 1
  } else if (tdet == 0) {
    iData$atlanticcodP[ii] = 0
  } else { 
    iData$atlanticcodP[ii] = NA
  }
}
plot(iData$atlanticcodP )

#DOLPHINS-- hourly presence/absence [0 or 1]
#-----------------------------------------------------------------------------------------
multmerge = function(path){
  filenames = list.files(path=path, pattern = "dolphins", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
} 
dolphins = ( multmerge(dir2) ) # unique(dolphins$Presence)
dolphins = as.data.frame(dolphins)
dolphins$DateFs  = as.POSIXct( gsub(".000Z", "", gsub("T", " ", dolphins$ISOStartTime)), tz = "GMT" )
dolphins$DayHR   = as.POSIXct( paste0(as.Date( dolphins$DateFs )," ", hour( dolphins$DateFs ),":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
dolphins$Day = as.Date( dolphins$DateFs )
names(dolphins)[2] = "dolphins" 
dolphins  = dolphins[!is.na(dolphins$Day),]
iData$dolphins = 0
for(ii in 1:nrow(iData)){
  tdet = nrow( dolphins[dolphins$Day == iData$Day[ii], ])
  if (tdet>0){iData$dolphins[ii] =  tdet}
} 

plot(iData$dolphins )
idx = which(names(iData) == "dolphins")
names(iData)[idx] = "dolphinsDet"

#convert to presence
for(ii in 1:nrow(iData)){
  tdet = iData$dolphinsDet[ii]
  
  if (tdet > 0) {
    iData$dolphinsP[ii] = 1
  } else if (tdet == 0) {
    iData$dolphinsP[ii] = 0
  } else { 
    iData$dolphinsP[ii] = NA
  }
}
plot(iData$dolphinsP )


#BLUE WHALE-- presence in a given day [0 or 1]
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2,pattern = "bluewhale", full.names=TRUE, recursive = TRUE))
bluewhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "bluewhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "bluewhale", full.names=TRUE, recursive = TRUE)[ff]
  dname  = sapply (strsplit( fname, "_" ),"[",3 )
  tst  =  sapply (strsplit( fname, "_" ),"[",5 )
  
  if(tst == "bluewhale"){
    cat("Processing...", fname,"\n")
    tmp  = fread(fnameF,header=F) #remove these after
    tmp$Site = dname
    bluewhale = rbind(bluewhale,tmp)
    rm(fname,fnameF,tmp)
  }
  
}
colnames(bluewhale)= c( as.character(bluewhale[1,1:3]))
bluewhale = bluewhale[bluewhale$Presence != "Presence",] #remove headers in the data frame
bluewhale$Day   = as.Date( bluewhale$ISOStartTime,tz="GMT")
names(bluewhale)[2] = "bluewhaleP" 
names(bluewhale)[3] = "Site"
bluewhale = bluewhale[!is.na(bluewhale$Day),]
bluewhale = bluewhale[!duplicated(bluewhale)]

for(ii in 1:nrow(iData)){
  tdet =  bluewhale[bluewhale$Day == iData$Day[ii], ]
  if (nrow(tdet) > 0) {
    if (tdet$bluewhaleP > 0) {
      iData$bluewhale[ii] = tdet$bluewhaleP
    }else {
      iData$bluewhale[ii] = 0
      }
      
  }else {
    iData$bluewhale[ii] = NA
  }
  
}
plot(bluewhale$bluewhaleP )
plot(iData$bluewhale )
idx = which(names(iData) == "bluewhale")
names(iData)[idx] = "bluewhaleP"

#FIN WHALE-- presence in a given day [0 or 1]
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2,pattern = "finwhale", full.names=TRUE, recursive = TRUE))
finwhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "finwhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "finwhale", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tst  =  sapply (strsplit( fname, "_" ),"[",5 )
  if(tst == "finwhale"){
    cat("Processing...", fname,"\n")
    tmp  = fread(fnameF,header=F) #remove these after
    tmp$Site = dname
    finwhale = rbind(finwhale,tmp)
    rm(fname,fnameF,tmp)
  }
}
colnames(finwhale)= c( as.character(finwhale[1,1:3]) )
finwhale = finwhale[finwhale$Presence != "Presence",] #remove headers in the data frame
finwhale = finwhale[finwhale$ISOStartTime != "",] #remove headers in the data frame
finwhale$Day   = as.Date( finwhale$ISOStartTim,tz="GMT")
names(finwhale)[2] = "finwhaleP" 
names(finwhale)[3] = "Site" 
finwhale = finwhale[!is.na(finwhale$Day),]
finwhale = finwhale[!duplicated(finwhale)]
#still duplicated entries...- between deployment, keep the presence = 1
finwhale$finwhaleP[duplicated(finwhale$ISOStartTime) ] = 1
finwhale = finwhale[!duplicated(finwhale)]

for(ii in 1:nrow(iData)){
  tdet =  finwhale[finwhale$Day == iData$Day[ii], ]
  if (nrow(tdet) > 0) {
    if (tdet$finwhaleP > 0) {
      iData$finwhale[ii] = tdet$finwhaleP
    }else {
      iData$finwhale[ii] = 0
    }
    
  }else {
    iData$finwhale[ii] = NA
  }
  
}

plot(finwhale$finwhaleP )
plot(iData$finwhale )
idx = which(names(iData) == "finwhale")
names(iData)[idx] = "finwhaleP"

#HUMPBACK WHALE-- presence in a given day [0 or 1]
#-----------------------------------------------------------------------------------------
# list.files(path=dir2,pattern = "humpbackwhale", full.names=TRUE, recursive = TRUE)
nFiles = length( list.files(path=dir2,pattern = "humpbackwhale", full.names=TRUE, recursive = TRUE))
humpbackwhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "humpbackwhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "humpbackwhale", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tst  =  sapply (strsplit( fname, "_" ),"[",5 )
  if(tst == "humpbackwhale"){
    cat("Processing...", fname,"\n")
    tmp  = fread(fnameF,header=F) #remove these after
    tmp$Site = dname
    humpbackwhale = rbind(humpbackwhale,tmp)
    rm(fname,fnameF,tmp)
  }
}
colnames(humpbackwhale)= c( as.character(humpbackwhale[1,1:3]) )
humpbackwhale = humpbackwhale[humpbackwhale$Presence != "Presence",] #remove headers in the data frame
humpbackwhale = humpbackwhale[humpbackwhale$ISOStartTime != "",] #remove headers in the data frame
humpbackwhale$Day   = as.Date( humpbackwhale$ISOStartTim,tz="GMT")
names(humpbackwhale)[2] = "humpbackwhaleP" 
names(humpbackwhale)[3] = "Site" 
humpbackwhale = humpbackwhale[!is.na(humpbackwhale$Day),]
humpbackwhale = humpbackwhale[!duplicated(humpbackwhale)]
dIdx = humpbackwhale$ISOStartTime[duplicated(humpbackwhale$ISOStartTime) ] 
for (dd in 1 :length(dIdx)) {humpbackwhale$humpbackwhaleP[ humpbackwhale$ISOStartTime == dIdx[dd]] = 1}
humpbackwhale = humpbackwhale[!duplicated(humpbackwhale)]

for(ii in 1:nrow(iData)){
  tdet =  humpbackwhale[humpbackwhale$Day == iData$Day[ii], ]
  if (nrow(tdet) > 0) {
    if (tdet$humpbackwhaleP > 0) {
      iData$humpbackwhale[ii] = tdet$humpbackwhaleP
    }else {
      iData$humpbackwhale[ii] = 0
    }
    
  }else {
    iData$humpbackwhale[ii] = NA
  }
  
}
idx = which(names(iData) == "humpbackwhale")
plot(humpbackwhale$humpbackwhaleP , main = names(iData)[idx])
plot(iData$humpbackwhale,main = names(iData)[idx] )
names(iData)[idx] = "humpbackwhaleP"

#NORTH ATLANTIC RIGHT WHALE-- presence in a given day [0 or 1]
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2,pattern = "northatlanticrightwhale", full.names=TRUE, recursive = TRUE))
northatlanticrightwhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "northatlanticrightwhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "northatlanticrightwhale", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tst  =  sapply (strsplit( fname, "_" ),"[",5 )
  if(tst == "northatlanticrightwhale"){
    cat("Processing...", fname,"\n")
    tmp  = fread(fnameF,header=F) #remove these after
    tmp$Site = dname
    northatlanticrightwhale = rbind(northatlanticrightwhale,tmp)
    rm(fname,fnameF,tmp)
  }
}
colnames(northatlanticrightwhale)= c( as.character(northatlanticrightwhale[1,1:3]) )
northatlanticrightwhale = northatlanticrightwhale[northatlanticrightwhale$Presence != "Presence",] #remove headers in the data frame
northatlanticrightwhale = northatlanticrightwhale[northatlanticrightwhale$ISOStartTime != "",] #remove headers in the data frame
northatlanticrightwhale$Day   = as.Date( northatlanticrightwhale$ISOStartTim,tz="GMT")
names(northatlanticrightwhale)[2] = "northatlanticrightwhaleP" 
names(northatlanticrightwhale)[3] = "Site" 
northatlanticrightwhale = northatlanticrightwhale[!is.na(northatlanticrightwhale$Day),]
northatlanticrightwhale = northatlanticrightwhale[!duplicated(northatlanticrightwhale)]
dIdx = northatlanticrightwhale$ISOStartTime[duplicated(northatlanticrightwhale$ISOStartTime) ] 
for (dd in 1 :length(dIdx)) {northatlanticrightwhale$northatlanticrightwhaleP[ northatlanticrightwhale$ISOStartTime == dIdx[dd]] = 1}
northatlanticrightwhale = northatlanticrightwhale[!duplicated(northatlanticrightwhale)]

for(ii in 1:nrow(iData)){
  tdet =  northatlanticrightwhale[northatlanticrightwhale$Day == iData$Day[ii], ]
  if (nrow(tdet) > 0) {
    if (tdet$northatlanticrightwhaleP > 0) {
      iData$northatlanticrightwhale[ii] = tdet$northatlanticrightwhaleP
    }else {
      iData$northatlanticrightwhale[ii] = 0
    }
    
  }else {
    iData$northatlanticrightwhale[ii] = NA
  }
  
}
idx = which(names(iData) == "northatlanticrightwhale")
plot(northatlanticrightwhale$northatlanticrightwhaleP , main = names(iData)[idx])
plot(iData$northatlanticrightwhale,main = names(iData)[idx] )
names(iData)[idx] = "northatlanticrightwhaleP"

#SEI WHALE-- presence in a given day [0 or 1]
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2,pattern = "seiwhale", full.names=TRUE, recursive = TRUE))
seiwhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "seiwhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "seiwhale", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tst  =  sapply (strsplit( fname, "_" ),"[",5 )
  if(tst == "seiwhale"){
    cat("Processing...", fname,"\n")
    tmp  = fread(fnameF,header=F) #remove these after
    tmp$Site = dname
    seiwhale = rbind(seiwhale,tmp)
    rm(fname,fnameF,tmp)
  }
}
colnames(seiwhale)= c( as.character(seiwhale[1,1:3]) )
seiwhale = seiwhale[seiwhale$Presence != "Presence",] #remove headers in the data frame
seiwhale = seiwhale[seiwhale$ISOStartTime != "",] #remove headers in the data frame
seiwhale$Day   = as.Date( seiwhale$ISOStartTim,tz="GMT")
names(seiwhale)[2] = "seiwhaleP" 
names(seiwhale)[3] = "Site" 
seiwhale = seiwhale[!is.na(seiwhale$Day),]
seiwhale = seiwhale[!duplicated(seiwhale)]
dIdx = seiwhale$ISOStartTime[duplicated(seiwhale$ISOStartTime) ] 
for (dd in 1 :length(dIdx)) {seiwhale$seiwhaleP[ seiwhale$ISOStartTime == dIdx[dd]] = 1}
seiwhale = seiwhale[!duplicated(seiwhale)]

for(ii in 1:nrow(iData)){
  tdet =  seiwhale[seiwhale$Day == iData$Day[ii], ]
  if (nrow(tdet) > 0) {
    if (tdet$seiwhaleP > 0) {
      iData$seiwhale[ii] = tdet$seiwhaleP
    }else {
      iData$seiwhale[ii] = 0
    }
    
  }else {
    iData$seiwhale[ii] = NA
  }
  
}
idx = which(names(iData) == "seiwhale")
plot(seiwhale$seiwhaleP , main = names(iData)[idx])
plot(iData$seiwhale,main = names(iData)[idx] )
names(iData)[idx] = "seiwhaleP"


#-----------------------------------------------------------------------------------------
# PLOTS OF SOURCES... used 4_combineFiles_plots_SB02. R code
#-----------------------------------------------------------------------------------------

#TILE PLOT of all sources present...
#-----------------------------------------------------------------------------------------
as.data.frame(colnames(iData))

#biological
BioAllomelt = reshape :: melt(iData, id.vars = "Day", 
                              measure.vars = c("bluewhaleP","finwhaleP","humpbackwhaleP",
                                               "northatlanticrightwhaleP","seiwhaleP", 
                                               "dolphinsP","atlanticcodP" ))
BioAllomelt$value = as.numeric(as.character(BioAllomelt$value) )
pBio = ggplot(BioAllomelt, aes(Day, variable, fill= (value))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  #labs(title = paste0(site,": summary of detections"),  fill = "") +
  xlab("") +
  ylab("")+
  geom_vline(xintercept=as.Date("2019-01-01"), linetype="dashed", 
             color = "gray", size=1)+ 
  geom_vline(xintercept=as.Date("2020-01-01"), linetype="dashed", 
             color = "gray", size=1)+ 
  geom_vline(xintercept=as.Date("2021-01-01"), linetype="dashed", 
             color = "gray", size=1)+ 
  
  theme_minimal()+
  theme(legend.position = "none")

#vessels
VessAllomelt = reshape :: melt(iData, id.vars = "Day", 
                              measure.vars = c("LOA_S_UV","LOA_M_UV","LOA_L_UV","TotalVesselDet_cnt"))
VessAllomelt$value = as.numeric(as.character(VessAllomelt$value) )
pVes = ggplot(VessAllomelt, aes(Day, variable, fill= (value))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  #labs(title = paste0(site,": summary of detections"),  fill = "") +
  xlab("") +
  ylab("")+
  geom_vline(xintercept=as.Date("2019-01-01"), linetype="dashed", 
             color = "gray", size=1)+ 
  geom_vline(xintercept=as.Date("2020-01-01"), linetype="dashed", 
             color = "gray", size=1)+ 
  geom_vline(xintercept=as.Date("2021-01-01"), linetype="dashed", 
             color = "gray", size=1)+ 
 
  theme_minimal()+
  theme(legend.position = "none")

#abiotic
as.data.frame(colnames(iData))

pwind = ggplot(iData, aes((Day), as.numeric(as.character(avgWSPDL))) ) +
  geom_line()+
  #geom_errorbar(aes(ymin=avgWSPDL-sdWSPD, ymax=avgWSPDL+sdWSPD), width=.1, alpha = .3)+
  #stat_summary(geom="ribbon", fun.ymin=avgWSPDL-sdWSPD, fun.ymax=avgWSPDL+sdWSPD, alpha=0.3) 
  geom_vline(xintercept=as.Date("2019-01-01"), linetype="dashed", 
             color = "gray", size=1)+ 
  geom_vline(xintercept=as.Date("2020-01-01"), linetype="dashed", 
             color = "gray", size=1)+ 
  geom_vline(xintercept=as.Date("2021-01-01"), linetype="dashed", 
             color = "gray", size=1) + 
  xlab("")+  ylab("Daily Wind Speed (mps) ") +
  theme_minimal()+ theme(legend.position = "none")

ptide = ggplot(iData, aes((Day), as.numeric(as.character(avgWL))) ) +
  geom_line()+
  #geom_errorbar(aes(ymin=avgWL-sdWL, ymax=avgWL+sdWL), width=.1, alpha = .3)+
  #stat_summary(geom="ribbon", fun.ymin=avgWSPDL-sdWSPD, fun.ymax=avgWSPDL+sdWSPD, alpha=0.3) 
  geom_vline(xintercept=as.Date("2019-01-01"), linetype="dashed", 
             color = "gray", size=1)+ 
  geom_vline(xintercept=as.Date("2020-01-01"), linetype="dashed", 
             color = "gray", size=1)+ 
  geom_vline(xintercept=as.Date("2021-01-01"), linetype="dashed", 
             color = "gray", size=1) + 
  xlab("")+  ylab("Tidal Change (m) ") +
  theme_minimal()+ theme(legend.position = "none")
  
#sound levels
p125 = ggplot(iData, aes((Day), OL_125 ) )+
  geom_line() +
  geom_point() +
  geom_vline(xintercept=as.Date("2019-01-01"), linetype="dashed", 
             color = "gray", size=1)+ 
  geom_vline(xintercept=as.Date("2020-01-01"), linetype="dashed", 
             color = "gray", size=1)+ 
  geom_vline(xintercept=as.Date("2021-01-01"), linetype="dashed", 
             color = "gray", size=1) + 
  xlab("")+  ylab("Median Sound Pressure Level dB re 1u Pa \n 125 Hz octave band") +
  theme_minimal()+
  theme(legend.position = "none")

pBio
pVes
pwind
p125
#copy into illustrator to make pretty

### LINE PLOT OF Proportion of sources present each day
#-----------------------------------------------------------------------------------------
iData$dolphinsP=as.numeric(as.character(iData$dolphinsP))
iData$bluewhaleP=as.numeric(as.character(iData$bluewhaleP))
iData$finwhaleP=as.numeric(as.character(iData$finwhaleP))
iData$humpbackwhaleP=as.numeric(as.character(iData$humpbackwhaleP))
iData$northatlanticrightwhaleP=as.numeric(as.character(iData$northatlanticrightwhaleP))
iData$seiwhaleP=as.numeric(as.character(iData$seiwhaleP))

iData$BioP = (iData$atlanticcodP + iData$dolphinsP + iData$bluewhaleP +iData$finwhaleP + iData$humpbackwhaleP + iData$northatlanticrightwhaleP + iData$seiwhaleP)/ 7
iData$PercentVessel_dailyP = iData$PercentVessel_daily/100

Propmelt = reshape :: melt(iData, id.vars = "Day", 
                              measure.vars = c("PercentVessel_dailyP","BioP","TimeAbove" ))
#overlapping line graph--- hard to see
#-----------------------------------------------------------------------------------------
ggplot(Propmelt, aes(Day, value, color= (variable))) + 
  geom_line(size = 2, alpha = .5)+
  theme_minimal()

#some date formatting
#-----------------------------------------------------------------------------------------
iData$Yr = year(iData$Day)
iData$jDay = yday(iData$Day)
iData$Month = month(iData$Day)
for (mm in 1:nrow(iData)){
  iData$MthLab[mm] = as.character( mthLab[mthLab$V1== iData$Month[mm],2])
}
iData$MthLab = factor( iData$MthLab, ordered = TRUE, 
                          levels = c("Jan","Feb","Mar",
                                     "Apr","May","Jun",
                                     "Jul","Aug","Sep",
                                     "Oct","Nov","Dec"))
Sys.Date()
write.csv(iData, file = paste0(outDir,site, "_CombineData_DAY_ver", Sys.Date(), ".csv") )
save(iData, file = paste0(outDir,site, "_CombineData_DAY_ver", Sys.Date()) )
Propmelt$yr = year(Propmelt$Day)
Propmelt$Jday = yday(Propmelt$Day)
Propmelt$Month = month(Propmelt$Day)
mthLab = as.data.frame (rbind( c(1,"Jan"), c(2,"Feb"), c(3,"Mar"), c(4,"Apr"), c(5,"May"), c(6,"Jun"), c(7,"Jul"), c(8,"Aug"), c(9,"Sep"), c(10,"Oct"), c(11,"Nov"), c(12,"Dec") ))
for (mm in 1:nrow(Propmelt)){
  Propmelt$MthLab[mm] = as.character( mthLab[mthLab$V1== Propmelt$Month[mm],2])
}
Propmelt$MthLab = factor( Propmelt$MthLab, ordered = TRUE, 
       levels = c("Jan","Feb","Mar",
                  "Apr","May","Jun",
                  "Jul","Aug","Sep",
                  "Oct","Nov","Dec"))
# 3 panel line graphs--- not ideas
#-----------------------------------------------------------------------------------------
pB = ggplot(iData, aes(Day, BioP)) + 
  geom_line(size = 2, alpha = .5, color = 'blue')+
  ylab("Proportion of Biological Sources Present")+ xlab("") +
  theme_minimal()
pV = ggplot(iData, aes(Day, PercentVessel_dailyP)) + 
  geom_line(size = 2, alpha = .5, color = 'red')+
  ylab("Proportion of Day Vessels Dominant")+ xlab("") +
  theme_minimal()
pW = ggplot(iData, aes(Day, TimeAbove)) + 
  ylab("Proportion of Day Wind above 10 m/s")+ xlab("") +
  geom_line(size = 2, alpha = .5, color = 'grey')+
  theme_minimal()
grid.arrange(pB,pV,pW)

#polar plot-- Daily (too much!)
#-----------------------------------------------------------------------------------------
Propmelt20 = Propmelt[Propmelt$yr == "2020",]
plot <- ggplot(Propmelt20, aes( x = Jday, y = value, fill = factor(variable))) +
  geom_bar(stat="identity")  
plot = plot + coord_polar()
plot

#polar plot-- monthly-- 2020 and 2019
#-----------------------------------------------------------------------------------------
rbind(Propmelt20, c(as.Date("2020-12-01", format = "%Y-%m-%d"), NA,NA,NA,NA,NA,NA))

Propmelt20m = aggregate( Propmelt20$value, by=list(Propmelt20$MthLab, Propmelt20$variable), mean, na.rm=T) 
tmp = aggregate( Propmelt20$value, by=list(Propmelt20$MthLab, Propmelt20$variable), sd, na.rm=T) 
Propmelt20m$sd = tmp$x
Propmelt20m = rbind(Propmelt20m, c("Dec", "TimeAbove",0,0) )
Propmelt20m = rbind(Propmelt20m, c("Dec", "BioP",0,0) )
Propmelt20m = rbind(Propmelt20m, c("Dec", "PercentVessel_dailyP",0,0) )
Propmelt20m$x = as.numeric( Propmelt20m$x)
Propmelt20m$sd = as.numeric( Propmelt20m$sd)
plot = ggplot(Propmelt20m, aes( x = as.factor(Group.1), y = x, fill = factor(Group.2))) +
  geom_bar(stat="identity"  )+
  labs(x = "Month", y = "", fill = "Soundscape Component") +
  scale_fill_manual(values=c( "#E69F00",  "#56B4E9", "gray", "darkblue"), labels = c("Vessel", "Biological", "Wind")) 
p20 = plot + coord_polar() +
  labs(  x = "",y = "", title = "Monthley Average Soundscape Components",
    caption = "orange = proportion day vessel dominant\n blue = proportion of Biological sources present\n gray = proportion of day wind above 10 m/s", 
    subtitle = "Stellwagen Bank National Marine Sanctuary-02 (2020)") +
  theme_minimal()

#polar plot-- monthly-- 2019
#-----------------------------------------------------------------------------------------
Propmelt19 = Propmelt[Propmelt$yr == "2019",]
Propmelt19m = aggregate( Propmelt19$value,    by=list(Propmelt19$MthLab,Propmelt19$variable), mean, na.rm=T)
tmp = aggregate( Propmelt19$value, by=list(Propmelt19$MthLab, Propmelt19$variable), sd, na.rm=T) 
Propmelt19m$sd = tmp$x

plot = ggplot(Propmelt19m,
               aes( x = as.factor(Group.1), y = x, fill = factor(Group.2))) +
  geom_bar(stat="identity"  ) +
  labs(x = "Month", y = "", fill = "Soundscape Component") +
  scale_fill_manual(values=c( "#E69F00",
                              "#56B4E9",
                              "gray",
                              "darkblue"), labels = c("Vessel", "Biological", "Wind")) 
p19 = plot + coord_polar() +
  labs(  x = "",y = "", title = "Monthley Average Soundscape Components",
         caption = "orange = proportion day vessel dominant\n blue = proportion of Biological sources present\n gray = proportion of day wind above 10 m/s", 
         subtitle = "Stellwagen Bank National Marine Sanctuary-02 (2020)") +
  theme_minimal()

grid.arrange(p19,p20) #not bad... but y-axis is meaning less

#another view...separate bars (with error bars)-- looks good
#-----------------------------------------------------------------------------------------
p19 = ggplot(data=Propmelt19m,aes( x = as.factor(Group.1), y = x, fill = factor(Group.2))) +
  geom_bar(stat="identity", color="black", position=position_dodge(), width=0.65, size=0.3)+
  geom_errorbar(aes(ymin=x, ymax=x+sd), position=position_dodge(.5), width=.2) +
  coord_polar(theta = "x",start=0) +
  ylim(c(0,1.1)) + 
  scale_fill_brewer(palette="YlGnBu",labels = c("Vessel", "Biological", "Wind"),name = "")+
  labs( x = "", y = "",
    title = "",
    caption = "Vessel = proportion day vessel dominant\n Biological = proportion of biological sources present\n Wind = proportion of day wind above 10 m/s", 
    subtitle = "Stellwagen Bank National Marine Sanctuary-02 \n 2019") +
  theme_light()+
  theme( legend.position = "none",axis.text.y = element_text(size = 12,colour="black"),
         axis.text.x=element_text(size = 10,colour="black"))
p20 = ggplot(data=Propmelt20m,aes( x = as.factor(Group.1), y = x, fill = factor(Group.2))) +
  geom_bar(stat="identity", color="black", position=position_dodge(),width=0.65,size=0.3)+
  geom_errorbar(aes(ymin=x, ymax=x+sd), position=position_dodge(.5), width=.2) +
  coord_polar(theta = "x",start=0) +
  ylim(c(0,1)) + 
  scale_fill_brewer(palette="YlGnBu",labels = c("Vessel", "Biological", "Wind"),name = "")+
  labs(
    x = "",
    y = "",
    title = "",
    caption = "Vessel = proportion day vessel dominant\n Biological = proportion of biological sources present\n Wind = proportion of day wind above 10 m/s", 
    subtitle = "Stellwagen Bank National Marine Sanctuary-02 \n 2020") +
  theme_light()+
  theme(  axis.text.y = element_text(size = 12,colour="black"),
         axis.text.x=element_text(size = 10,colour="black"))

grid.arrange(p19,p20, nrow = 1)
#save and make pretty in illustrator


#previous version for combining data.... not relevent to this version!!
#-----------------------------------------------------------------------------------------
# MERGE all the biologics into one matrix
#-----------------------------------------------------------------------------------------
#start with all days from input data frame
idataD$DateFs = as.Date(idataD$Day)
#bluewhale, finwhale, humpbackwhale, northatlanticrightwhale,  seiwhale
#nrows should always be same as idataD... 750, dupliates (fixed above!)
b0 = merge(idataD, bluewhale, by ="DateFs",  all.x=TRUE)
b1 = merge(b0,      finwhale, by = "DateFs", all.x=TRUE)
b1 = b1[,c(1:32,34,37)]
b2 = merge(b1, humpbackwhale, by = "DateFs", all.x=TRUE)
b2 = b2[,c(1:34,36)]
b3 = merge(b2, northatlanticrightwhale, by = "DateFs", all.x=TRUE)
b3 = b3[,c(1:35,37)]
b4 = merge(b3, seiwhale, by = "DateFs", all.x=TRUE)
b4 = b4[,c(1:36,38)]
bio = merge(b4,dolphinsD, by="DateFs", all.x=TRUE)

bio$bluewhaleP = as.numeric(as.character(bio$bluewhaleP ))
bio$finwhaleP = as.numeric(as.character(bio$finwhaleP ))
bio$humpbackwhaleP = as.numeric(as.character(bio$humpbackwhaleP ))
bio$northatlanticrightwhaleP = as.numeric(as.character(bio$northatlanticrightwhaleP ))
bio$seiwhaleP = as.numeric(as.character(bio$seiwhaleP ))
bio$DolphinHrs = as.numeric(as.character(bio$DolphinHrs ))
bio$dolphinsDetDay = as.numeric(as.character(bio$dolphinsDetDay ))
as.data.frame( colnames(bio) )

bioAll = merge(bio,atlanticcodD, by="DateFs", all.x=TRUE)
rm(numDays,udays,b0,b1,b2,b3,b4,bio,dolphins,dolphinsD,humpbackwhale,bluewhale,finwhale,seiwhale,northatlanticrightwhale,atlanticcodD,atlanticcod, dd,ff,tst)

#-----------------------------------------------------------------------------------------
# COMBINE ABIOTIC HOURLY SUMMARIES with SD values
#-----------------------------------------------------------------------------------------
udays = unique(as.Date( iData$Day) )
#iData$avgWSPD = as.numeric( as.character(iData$avgWSPD))
#iData$avgWSPD[iData$avgWSPD == 99] = NA
#hist( iData$avgWSPD )

abioD = NULL
#this loop takes a bit... not sure why-- >80 days!
for(dd in 1:length(udays)){
  tmp   = iData[ as.Date( iData$Day) == udays[dd],]
  ck = all( is.na(tmp$changeWL) )
  if(ck == TRUE){
    abioD = rbind(abioD, c(as.character(udays[dd]), nrow(tmp), 
                           mean(tmp$avgWSPD,na.rm = T),       sd(tmp$avgWSPD,na.rm = T), 
                           NA, NA ) )
  } else {
  abioD = rbind(abioD, c(as.character(udays[dd]), nrow(tmp), 
                         mean(tmp$avgWSPD,na.rm = T),       sd(tmp$avgWSPD,na.rm = T), 
                         mean(abs(tmp$changeWL),na.rm = T), max(abs(tmp$changeWL),na.rm = T) ) )
  }
}
colnames(abioD) = c("DateFs","HrsSampled","meanWSPD","sdWSPD","meanTide","maxTide")
abioD = as.data.frame(abioD)
abioD$DateFs   = as.Date(abioD$DateFs)
abioD$meanWSPD = as.numeric(as.character( abioD$meanWSPD) )
abioD$sdWSPD   = as.numeric(as.character( abioD$sdWSPD) )
abioD$meanTide = as.numeric(as.character( abioD$meanTide) )
abioD$maxTide  = as.numeric(as.character( abioD$maxTide) )

dataA = merge(bioAll,abioD, by="DateFs", all.x=TRUE)
as.data.frame(colnames(dataA))
summary(dataA)
dataA$Yr = year(dataA$DateFs)
dataA$JulianDay = yday(dataA$DateFs)
dataA$Mth = month(dataA$DateFs)
dataA$DayW= weekdays(dataA$DateFs)
summary(dataA)

#-----------------------------------------------------------------------------------------
#ADD daily information to hourly--- just repeat the measure for each hour
#-----------------------------------------------------------------------------------------
iData$Day = as.Date(iData$Day)
as.data.frame(colnames(dataA))
dataAhr = NULL
for(ii in 1:nrow(dataA)){
  tdata = iData[iData$Day == as.Date(dataA$Day[ii]),] #houly data for the day
  dataA[ii, c(23:37,41)]
  dataAhr = rbind( dataAhr, cbind(tdata, dataA[ii, c(23:37,41)]) )
}
dataAhr= as.data.frame(dataAhr)
summary(dataAhr)

#-----------------------------------------------------------------------------------------
#SAVE OUT combined matrix
#-----------------------------------------------------------------------------------------
setwd(outDir)
DC = Sys.Date()
fnameAll =     paste0(as.character( dataA$Site[1] ), "_CombinedData_SPLShipsAbioticBio_Day_",
                      as.character(min(as.Date(dataA$Day))),"_",
                      as.character(max(as.Date(dataA$Day))), "_v", DC, ".csv")  
write.csv(dataA,fnameAll) 
fnameAll2 =     paste0(as.character( dataA$Site[1] ), "_CombinedData_SPLShipsAbioticBio_Day_",
                      as.character(min(as.Date(dataA$Day))),"_",
                      as.character(max(as.Date(dataA$Day))), "_v", DC)  
save(dataA,file =fnameAll2 )
#hourly data
fnameAll =     paste0(as.character( dataAhr$Site[1] ), "_CombinedData_SPLShipsAbioticBio_HR_",
                      as.character(min(as.Date(dataA$Day))),"_",
                      as.character(max(as.Date(dataA$Day))), "_v", DC, ".csv")  
write.csv(dataAhr,fnameAll) 
nameAll2 =     paste0(as.character( dataAhr$Site[1] ), "_CombinedData_SPLShipsAbioticBio_HR_",
                     as.character(min(as.Date(dataA$Day))),"_",
                     as.character(max(as.Date(dataA$Day))), "_v", DC) 
save(dataAhr,file =nameAll2 )

#-----------------------------------------------------------------------------------------
# (optional) DATA EXPLORATION PLOTS... now that it is all combined
# Copied to 4_combineFiles_plots_SB02.R

# #-----------------------------------------------------------------------------------------
# #select days in March
# #-----------------------------------------------------------------------------------------
# tmp = dataA[dataA$JulianDay > 82 & dataA$JulianDay < 91,]
# as.data.frame(colnames(tmp))
# ggplot(tmp, aes(as.factor(JulianDay), meanWSPD, group=as.factor(Yr), color = as.factor(Yr))) +
#   geom_line() +
#   xlab("")+
#   ylab("Wind Speed (mps) ") +
#   ggtitle(("SB02: March 25-31 [2019=5.8, 2020=7.1]"))+
#   theme_minimal()
# 
# tmp %>% group_by(Yr) %>% summarize(newvar = mean(meanWSPD,na.rm = T))
# 
# #TILE PLOT of all sources present...
# BioAllomelt = reshape :: melt(tmp, id.vars = "DateFs", 
#                               measure.vars = c("bluewhaleP","finwhaleP","humpbackwhaleP",
#                                                "northatlanticrightwhaleP","seiwhaleP", 
#                                                "dolphinsDetDay","atlanticcodDetDay" ))
# BioAllomelt$value = as.numeric(as.character(BioAllomelt$value) )
# 
# ggplot(BioAllomelt, aes(DateFs, variable, fill= (value))) + 
#   geom_tile() +
#   scale_fill_gradient(low="white", high="blue") +
#   labs(title = paste0(site,": summary of sounds"),  fill = "Detections") +
#   xlab("") +
#   ylab("")
# 
# #plot of conditions
# pwind = ggplot(tmp, aes(as.factor(JulianDay), meanWSPD, group=as.factor(Yr), color = as.factor(Yr))) +
#   geom_line() +   geom_point() +
#   xlab("Julian Day")+  ylab("Wind Speed (mps) ") +
#   theme_minimal()+ theme(legend.position = "none")
# 
# ptide = ggplot(tmp, aes(as.factor(JulianDay), maxTide, group=as.factor(Yr), color = as.factor(Yr))) +
#   geom_line() +
#   geom_point() +
#   xlab("")+  ylab("Max tide change ") +
#   theme_minimal()+
#   theme(legend.position = "none")
# 
# p125 = ggplot(tmp, aes(as.factor(JulianDay), OL_250, group=as.factor(Yr), color = as.factor(Yr))) +
#   geom_line() +
#   geom_point() +
#   xlab("")+  ylab("Ol_125 ") +
#   theme_minimal()+
#   theme(legend.position = "none")
# 
# p63 = ggplot(tmp, aes(as.factor(JulianDay), OL_63, group=as.factor(Yr), color = as.factor(Yr))) +
#   geom_line() +
#   geom_point() +
#   xlab("")+  ylab("Ol_63 ") +
#   theme_minimal()+
#   theme(legend.position = "none")
# 
# BioAllomelt$JulianDay = yday(BioAllomelt$DateFs)
# BioAllomelt$Yr = year(BioAllomelt$DateFs)
# #plot of sources by year, source=x,Y=value
# pBio = ggplot(BioAllomelt, aes(variable, value, fill=as.factor(Yr)) ) +
#   geom_bar(position = "fill",stat = "identity" )+
#   xlab("")+ ylab("")+
#   scale_x_discrete(labels = c("Blue","Fin","Humpback","NARW", 
#                               "Sei","Dolphin","Cod"))+
#   scale_fill_discrete(name = "")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 50),legend.position="top" )
# uSources = unique(BioAllomelt$variable)
# BioAllomelt[BioAllomelt$variable == uSources[1],]
# BioAllomelt[BioAllomelt$variable == uSources[2],] #everyday had 1
# BioAllomelt[BioAllomelt$variable == uSources[3],] #one more day 2020 with humpbacks
# BioAllomelt[BioAllomelt$variable == uSources[4],] #2 day 2019, 7 day in 2020
# BioAllomelt[BioAllomelt$variable == uSources[5],] #one more day 2020 with sei
# BioAllomelt[BioAllomelt$variable == uSources[6],] #no data for dolphins/cod
# BioAllomelt[BioAllomelt$variable == uSources[7],] 
# 
# grid.arrange(p125,p63,pwind,ptide,pBio,ncol = 5)
# 
# #boxplots
# pwind = ggplot(tmp, aes(as.factor(Yr), meanWSPD, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("Julian Day")+  ylab("Wind Speed (mps) ") +
#   theme_minimal()+ theme(legend.position = "none")
# 
# ptide = ggplot(tmp, aes(as.factor(Yr), maxTide, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Max tide change ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# p125 = ggplot(tmp, aes(as.factor(Yr), OL_250, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Ol_125 ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# p63 = ggplot(tmp, aes(as.factor(Yr), OL_63, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Ol_63 ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# #VESSEL Metrics
# pDetVes = ggplot(tmp, aes(as.factor(Yr), PercentDay_TimeVesselDet, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("% day with vessel noise (Detector) ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# pAISa = ggplot(tmp, aes(as.factor(Yr), LOA_ALL_UV, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Unique Vessels (AIS) ") +
#   theme_minimal()+   theme(legend.position = "none")
# pAISs = ggplot(tmp, aes(as.factor(Yr), LOA_S_UV, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Small Unique Vessels (AIS) ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# pAISm = ggplot(tmp, aes(as.factor(Yr), LOA_M_UV, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Medium Unique Vessels (AIS) ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# pAISl = ggplot(tmp, aes(as.factor(Yr), LOA_L_UV, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Large Unique Vessels (AIS) ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# grid.arrange(p125,p63,pwind,ptide,pBio,ncol = 5)
# grid.arrange(pDetVes,pAISa,pAISs,pAISm,pAISl,ncol = 5)
# 
# #-----------------------------------------------------------------------------------------
# #monthly difference graphics
# #-----------------------------------------------------------------------------------------
# as.data.frame(colnames(dataA))
# #boxplot of monthly differences
# #sound level- 125Hz
# p125 = ggplot(dataA, aes(as.factor(Mth), OL_125, fill = as.factor(Yr))) +
#   geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
#   theme_minimal() +
#   xlab("") +   ylab ("") + 
#   ggtitle("Low frequency sound levels [125 Hz]")+ #median [125 Hz]
#   #coord_cartesian(ylim = c(65, 120)) +
#   scale_fill_manual(values=c("gray","#E69F00", "#56B4E9") ) +
#   theme(axis.text        = element_text(size=11),
#         panel.background = element_rect(fill = "transparent"),
#         plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
#         axis.title       = element_text(size=11,face="bold"),
#         plot.background  = element_rect(fill = "transparent", color = NA),
#         #plot.margin      = unit(c(1,2,2,2), "lines"),
#         legend.position = "none",
#         legend.title = element_blank() )
# #vessel noise 
# pDet = ggplot(dataA, aes(as.factor(Mth), PercentDay_TimeVesselDet, fill = as.factor(Yr))) +
#   geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
#   theme_minimal() +
#   xlab("") +   ylab ("") + 
#   ggtitle("% Day vessel noise")+ #median [125 Hz]
#   #coord_cartesian(ylim = c(65, 120)) +
#   scale_fill_manual(values=c("gray","#E69F00", "#56B4E9") ) +
#   theme(axis.text        = element_text(size=11),
#         panel.background = element_rect(fill = "transparent"),
#         plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
#         axis.title       = element_text(size=11,face="bold"),
#         plot.background  = element_rect(fill = "transparent", color = NA),
#         #plot.margin      = unit(c(1,2,2,2), "lines"),
#         legend.position = "none",
#         legend.title = element_blank() )
# 
# pAIS = ggplot(dataA, aes(as.factor(Mth), LOA_ALL_UV, fill = as.factor(Yr))) +
#   geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
#   theme_minimal() +
#   xlab("") +   ylab ("") + 
#   ggtitle("Unique AIS vessels")+ #median [125 Hz]
#   #coord_cartesian(ylim = c(65, 120)) +
#   scale_fill_manual(values=c("gray","#E69F00", "#56B4E9") ) +
#   theme(axis.text        = element_text(size=11),
#         panel.background = element_rect(fill = "transparent"),
#         plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
#         axis.title       = element_text(size=11,face="bold"),
#         plot.background  = element_rect(fill = "transparent", color = NA),
#         #plot.margin      = unit(c(1,2,2,2), "lines"),
#         legend.position = "none",
#         legend.title = element_blank() )
# 
# pWind = ggplot(dataA, aes(as.factor(Mth), meanWSPD, fill = as.factor(Yr))) +
#   geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
#   theme_minimal() +
#   xlab("") +   ylab ("") + 
#   ggtitle("Wind speed ")+ #median [125 Hz]
#   #coord_cartesian(ylim = c(65, 120)) +
#   scale_fill_manual(values=c("gray","#E69F00", "#56B4E9") ) +
#   theme(axis.text        = element_text(size=11),
#         panel.background = element_rect(fill = "transparent"),
#         plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
#         axis.title       = element_text(size=11,face="bold"),
#         plot.background  = element_rect(fill = "transparent", color = NA),
#         #plot.margin      = unit(c(1,2,2,2), "lines"),
#         legend.position = "bottom",
#         legend.title = element_blank() )
# #biology-- bar?
# BioAllomeltA = reshape :: melt(dataA, id.vars = "DateFs", 
#                               measure.vars = c("bluewhaleP","finwhaleP","humpbackwhaleP",
#                                                "northatlanticrightwhaleP","seiwhaleP" ))
# BioAllomeltA$value = as.numeric(as.character(BioAllomeltA$value) )
# BioAllomeltA$Yr  = year( as.Date(BioAllomeltA$DateFs ))
# BioAllomeltA$Mth = month( as.Date(BioAllomeltA$DateFs ))
# BioAllomeltA = BioAllomeltA[ BioAllomeltA$Yr > 2018,]
# 
# #values for each factor (month) to be summed up within each bar
# BioAllomeltA %>% 
#   group_by(variable, Yr, Mth) %>% 
#   summarize(summedvalues = sum(value,na.rm = T)) %>% 
#   ggplot(aes(as.factor(Mth), summedvalues, fill=as.factor(Yr)) ) +
#   geom_bar(position = "dodge", stat = "identity",width = .5)+
#   scale_fill_manual(values=c("#E69F00", "#56B4E9") )+
#   facet_wrap(~variable, ncol=1)+
#   xlab("")+ ylab("Days with calls")+
#   theme(legend.title = element_blank())
# 
# ggplot(BioAllomeltA, aes(as.factor(Mth), value, fill=as.factor(Yr)) ) +
#   geom_bar(position = "dodge", stat = "identity",width = .5)+
#   scale_fill_manual(values=c("#E69F00", "#56B4E9") )+
#   facet_wrap(~variable, ncol=1)+
#   xlab("")+ ylab("Days with calls")+
#   theme(legend.title = element_blank())
# 
# 
# grid.arrange(p125,pDet,pAIS,pWind,ncol = 1)
# #Difference MONTH COMPARISION--- 2019 vs 2020, heat map
# #-----------------------------------------------------------------------------------------
# as.data.frame(colnames(tmp))
# SPLAll1 = dataA[dataA$Yr == 2019 | dataA$Yr == 2020, ]
# SPLAll1$Bal  = rowSums ( SPLAll1[,33:37]) #sum of baleen whale calls present in given day
# SPLAll1$BalP = 0 
# SPLAll1$BalP[SPLAll1$Bal> 0] = 1
# #summarize by year, month as median values or mean
# by125 = as.data.frame ( SPLAll1 %>%
#                           group_by(Mth,Yr) %>%
#                           summarise(qs125 = quantile(OL_125, .5), nDays = n() ) )
# diff125 = NULL
# for(mm in 1:12){
#   tmp1 = by125[by125$Mth == mm,]
#   if (nrow( tmp1) == 0 ) { 
#     diff125 = rbind(diff125, c( mm, NA,NA,NA)) #no date for site/month
#   }else if (nrow( tmp1) == 1 ){ 
#     diff125 = rbind(diff125, c( mm, NA,NA,NA)) #not data for both years
#   }else if (nrow(tmp1) == 2) {
#     
#     d2019 = tmp1$qs125[tmp1$Yr == 2019]
#     d2020 = tmp1$qs125[tmp1$Yr == 2020]
#     diff125 = rbind(diff125, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
#   }
# }
# colnames(diff125) = c("Mth","Diff_125","2019_125","2020_125")
# diff125 = as.data.frame(diff125)
# diff125$Mth = ordered(diff125$Mth, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11","12"))
# 
# #OL_63
# by63= as.data.frame ( SPLAll1 %>%
#                           group_by(Mth,Yr) %>%
#                           summarise(qs63 = quantile(OL_63, .5), nDays = n() ) )
# diff63 = NULL
# for(mm in 1:12){
#   tmp1 = by63[by63$Mth == mm,]
#   if (nrow( tmp1) == 0 ) { 
#     diff63 = rbind(diff63, c( mm, NA,NA,NA)) #no date for site/month
#   }else if (nrow( tmp1) == 1 ){ 
#     diff63 = rbind(diff63, c( mm, NA,NA,NA)) #not data for both years
#   }else if (nrow(tmp1) == 2) {
#     
#     d2019 = tmp1$qs63[tmp1$Yr == 2019]
#     d2020 = tmp1$qs63[tmp1$Yr == 2020]
#     diff63 = rbind(diff63, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
#   }
# }
# colnames(diff63) = c("Mth","Diff_63","2019_63","2020_63")
# diff63 = as.data.frame(diff63)
# 
# #WIND
# byWind = as.data.frame ( SPLAll1 %>%
#                           group_by(Mth,Yr) %>%
#                           summarise(avgWSPD = mean(meanWSPD,na.rm = T), sdWSPD = sd(meanWSPD,na.rm = T), nDays = n() ) )
# 
# diffW = NULL
# for(mm in 1:12){
#   tmp1 = byWind[byWind$Mth == mm,]
#   if (nrow( tmp1) == 0 ) { 
#     diffW = rbind(diffW, c( mm, NA,NA,NA)) #no date for site/month
#   }else if (nrow( tmp1) == 1 ){ 
#     diffW = rbind(diffW, c( mm, NA,NA,NA)) #not data for both years
#   }else if (nrow(tmp1) == 2) {
#     
#     d2019 = tmp1$avgWSPD[tmp1$Yr == 2019]
#     d2020 = tmp1$avgWSPD[tmp1$Yr == 2020]
#     diffW = rbind(diffW, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
#   }
# }
# colnames(diffW) = c("Mth","Diff_WIND","2019_WIND","2020_WIND")
# diffW = as.data.frame(diffW)
# 
# #days with baleen calls present
# byBio = as.data.frame ( SPLAll1 %>%
#                         group_by(Mth,Yr) %>%
#                         summarise(daysBio = sum(BalP,na.rm = T), nDays = n() ) )
# diffB = NULL
# for(mm in 1:12){
#   tmp1 = byBio[byBio$Mth == mm,]
#   if (nrow( tmp1) == 0 ) { 
#     diffB = rbind(diffB, c( mm, NA,NA,NA)) #no date for site/month
#   }else if (nrow( tmp1) == 1 ){ 
#     diffB = rbind(diffB, c( mm, NA,NA,NA)) #not data for both years
#   }else if (nrow(tmp1) == 2) {
#     
#     d2019 = tmp1$daysBio[tmp1$Yr == 2019]
#     d2020 = tmp1$daysBio[tmp1$Yr == 2020]
#     diffB = rbind(diffB, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
#   }
# }
# colnames(diffB) = c("Mth","Diff_BioDay","2019_BioDay","2020_BioDay")
# diffB = as.data.frame(diffB)
# 
# 
# byBioC = as.data.frame ( SPLAll1 %>%
#                           group_by(Mth,Yr) %>%
#                           summarise(countBio = sum(Bal,na.rm = T), nDays = n() ) )
# diffBc = NULL
# for(mm in 1:12){
#   tmp1 = byBioC[byBioC$Mth == mm,]
#   if (nrow( tmp1) == 0 ) { 
#     diffBc = rbind(diffBc, c( mm, NA,NA,NA)) #no date for site/month
#   }else if (nrow( tmp1) == 1 ){ 
#     diffBc = rbind(diffBc, c( mm, NA,NA,NA)) #not data for both years
#   }else if (nrow(tmp1) == 2) {
#     
#     d2019 = tmp1$countBio[tmp1$Yr == 2019]
#     d2020 = tmp1$countBio[tmp1$Yr == 2020]
#     diffBc = rbind(diffBc, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
#   }
# }
# colnames(diffBc) = c("Mth","Diff_BioC","2019_BioC","2020_BioC")
# diffBc = as.data.frame(diffBc)
# 
# # combine all metrics
# #-----------------------------------------------------------------------------------------
# diffALL = cbind(diff125,diff63[,2:4],diffW[,2:4],diffB[,2:4],diffBc[,2:4] )
# diffALLmelt = reshape :: melt(diffALL, id.vars = "Mth", 
#                               measure.vars = c("Diff_125","Diff_63","Diff_WIND","Diff_BioDay","Diff_BioC" ))
# 
# #PLOT: x = month, y = metric, fill is difference
# #-----------------------------------------------------------------------------------------
# #positive is higher in 2020, negative in lower in 2020
# ggplot(diffALLmelt, aes(Mth, variable ,fill=as.numeric(as.character(value)) ) ) +
#   geom_tile()+
#   xlab("Month") + ylab("")+
#   scale_fill_gradient2(low="blue", high="orange")+
#   scale_y_discrete(labels = c("125 Hz", "63 Hz", "Wind", "Baleen whale [days]","Baleen whale [calls]"))+
#   scale_x_discrete(labels = c("Jan", "Feb", "Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"))+
#   ggtitle("Change in soundscape features (+ higher 2020, - lower 2020)")+
#   theme(legend.title = element_blank())
