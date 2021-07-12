#Integrating SanctSound data-- and some check plots

#write out csv files with full SPL integrated with other metrics
# Tide, wind, sun altitude, AIS, vessel dets, biological dets

rm(list=ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(suncalc)

#SET working directory-- currently runs per site
#-----------------------------------------------------------------------------------------
siteLoc = c(24.43313,-81.93068)
site  = 'SB'
deply = "SB01"
dir   = paste0("D:\\RESEARCH\\SanctSound\\data\\",site)
dir2  = paste0("D:\\RESEARCH\\SanctSound\\data\\",site,"\\", deply)
setwd(dir)

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
  fname = list.files(path=dir2, pattern = "_OL_1h.csv", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",4 )
  tmp  = rbind(fread(fnameF))
  tmp$deploy = dname
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
  ggtitle("Check SPL")
pSPL

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
pVes = ggplot(VESS, aes(DateFs, DUR_mins))+
  geom_point() +
  xlab("")+
  ylab("Duration Vessel Detections (mins)") +
  theme_minimal()+
  ggtitle("Check SPL")
pVes

#AIS-- daily metrics 
#-----------------------------------------------------------------------------------------
AIS = read.csv("AIS_SB_2018_10_to_2020_11.csv")
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
  xlab("Julian Day")+
  theme(legend.position="none")
grid.arrange(ps,pm,pl,pa,ncol = 2,nrow = 2,top = "Check- AIS OPHRS")

psUV = ggplot(AIS, aes(DateFJul, LOA_S_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9'),name = "") +
  ylim(c(0,50))+
  theme(legend.position=c(0.2, 0.9))
pmUV = ggplot(AIS, aes(DateFJul, LOA_M_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,50))+
  theme(legend.position="none")
plUV = ggplot(AIS, aes(DateFJul, LOA_L_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,50))+
  theme(legend.position="none")
paUV = ggplot(AIS, aes(DateFJul, LOA_ALL_UV, color = as.factor(Yr) ) ) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") + 
  scale_color_manual(values=c('#E69F00','#56B4E9')) +
  ylim(c(0,50))+
  xlab("Julian Day")+
  theme(legend.position="none")
grid.arrange(psUV,pmUV,plUV,paUV,ncol = 2,nrow = 2,top = "Check- AIS UV")

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
pTide = ggplot(TIDE, aes(DateFday, as.numeric(as.character(changeWL)) ) )+
  geom_line() +
  xlab("")+
  ylab("Hourly change in water level") +
  theme_minimal()+ggtitle("Check- Tide")
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
  theme_minimal()+ggtitle("Check- Wind")
pWind

#SUN ALTITUDE-- append to wind
#-----------------------------------------------------------------------------------------
tmp = getSunlightPosition(WSPD$DateF ,siteLoc[1],siteLoc[2])
WSPD$sunAlt = tmp$altitude

#Biological Detections-- 
#options: atlanticcod, dolphins, bluewhale, finwhale, humpbackwhale, northatlanticrightwhale,  seiwhale
#GRRR... column names are different for these outputs!!
#-----------------------------------------------------------------------------------------
#atlanticcod-- start and end times, need to convert to hourl presence!!
nFiles = length( list.files(path=dir2,pattern = "atlanticcod", full.names=TRUE, recursive = TRUE))
# list.files(path=dir2,pattern = "atlanticcod", full.names=FALSE, recursive = FALSE)
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
#unique(atlanticcod$V1)
colnames(atlanticcod)= c( as.character(atlanticcod[1,1:3]) ,"deply")
atlanticcod = atlanticcod[atlanticcod$Presence != "Presence",] #remove headers in the data frame
atlanticcod$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", atlanticcod$Begin_datetime_ISO6801)), tz = "GMT" )
atlanticcod$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", atlanticcod$End_datetime_ISO6801))  , tz = "GMT" )
atlanticcod$DUR_mins = atlanticcod$DateFe- atlanticcod$DateFs #in minutes!
atlanticcod$DayHR  = as.POSIXct( paste0(as.Date( atlanticcod$DateFs )," ", hour( atlanticcod$DateFs ),":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
atlanticcod$DateDay = as.Date( atlanticcod$DateFs )

#dolphins-- presence in a given hour, 0 or 1, summarize by day
multmerge = function(path){
  filenames = list.files(path=path, pattern = "dolphins", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
} 
#list.files(path=dir2, pattern = "dolphins", full.names=TRUE, recursive = TRUE)
dolphins  <- ( multmerge(dir2) ) # unique(dolphins$Presence)
dolphins = as.data.frame(dolphins)
dolphins$DateFs  = as.POSIXct( gsub(".000Z", "", gsub("T", " ", dolphins$ISOStartTime)), tz = "GMT" )
dolphins$DayHR   = as.POSIXct( paste0(as.Date( dolphins$DateFs )," ", hour( dolphins$DateFs ),":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
dolphins$DateDay = as.Date( dolphins$DateFs )
names(dolphins)[2] = "dolphinsP" 
dolphins  = dolphins[!is.na(dolphins$DateFs),]
#Trying to aggregate by date...just use a loop [dolphinsD = aggregate(dolphins["dolphinsP"], by=dolphins["DateDay"], sum)]
udays = unique(dolphins$DateDay)
dolphinsD = NULL
for(dd in 1:length(udays)){
  tmp = dolphins[ dolphins$DateDay == udays[dd],]
  dolphinsD = rbind(dolphinsD, c(as.character(udays[dd]),nrow(tmp),sum(tmp$dolphinsP)))
}
colnames(dolphinsD) = c("DateFs","DolphinHrs", "dolphinsPday")
dolphinsD = as.data.frame(dolphinsD)
dolphinsD$DateFs = as.Date(dolphinsD$DateFs)
dolphinsD$DolphinHrs = as.numeric( as.character(dolphinsD$DolphinHrs) )
dolphinsD$dolphinsPday = as.numeric( as.character( (dolphinsD$dolphinsPday) ) )

#bluewhale-- presence in a given day, 0 or 1 
nFiles = length( list.files(path=dir2,pattern = "bluewhale", full.names=TRUE, recursive = TRUE))
bluewhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "bluewhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "bluewhale", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tmp  = fread(fnameF,header=F) #remove these after
  tmp$deplm = dname
  bluewhale = rbind(bluewhale,tmp)
  rm(fname,fnameF,tmp)
}
colnames(bluewhale)= c( as.character(bluewhale[1,1:3]) ,"deply")
bluewhale = bluewhale[bluewhale$Presence != "Presence",] #remove headers in the data frame
bluewhale$DateFs   = as.Date( bluewhale$`ISO date`,tz="GMT")
names(bluewhale)[3] = "bluewhaleP" 
bluewhale = bluewhale[!is.na(bluewhale$DateFs),]

#finwhale-- presence in a given day, 0 or 1 
nFiles = length( list.files(path=dir2,pattern = "finwhale", full.names=TRUE, recursive = TRUE))
finwhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "finwhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "finwhale", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tmp  = fread(fnameF,header=F) #remove these after
  tmp$deplm = dname
  finwhale = rbind(finwhale,tmp)
  rm(fname,fnameF,tmp)
}
colnames(finwhale)= c( as.character(finwhale[1,1:3]) ,"deply")
finwhale = finwhale[finwhale$Presence != "Presence",] #remove headers in the data frame
finwhale = finwhale[finwhale$`ISO Date` != "",] #remove headers in the data frame
finwhale$DateFs   = as.Date( finwhale$`ISO Date`,tz="GMT")
names(finwhale)[3] = "finwhaleP" 
finwhale = finwhale[!is.na(finwhale$DateFs),]

#humpbackwhale-- presence in a given day, 0 or 1 
multmerge = function(path){
  filenames = list.files(path=path, pattern = "humpbackwhale", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
} 
humpbackwhale  <- ( multmerge(dir2) )
humpbackwhale = as.data.frame(humpbackwhale)
humpbackwhale = humpbackwhale[!is.na(humpbackwhale$Presence),]
humpbackwhale$DateFs   = as.Date( humpbackwhale$ISO_Date, tz = "GMT" )
names(humpbackwhale)[3] = "humpbackwhaleP" 
humpbackwhale = humpbackwhale[!is.na(humpbackwhale$DateFs),]

#northatlanticrightwhale-- presence in a given day, 0 or 1 
nFiles = length( list.files(path=dir2,pattern = "northatlanticrightwhale", full.names=TRUE, recursive = TRUE))
northatlanticrightwhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "northatlanticrightwhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "northatlanticrightwhale", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tmp  = fread(fnameF,header=F) #remove these after
  tmp$deplm = dname
  northatlanticrightwhale = rbind(northatlanticrightwhale,tmp)
  rm(fname,fnameF,tmp)
}
colnames(northatlanticrightwhale)= c( as.character(northatlanticrightwhale[1,1:3]) ,"deply")
northatlanticrightwhale = northatlanticrightwhale[northatlanticrightwhale$Presence != "Presence",] #remove headers in the data frame
northatlanticrightwhale$DateFs   = as.Date( northatlanticrightwhale$`ISO Date`,tz="GMT")
names(northatlanticrightwhale)[3] = "northatlanticrightwhaleP" 
northatlanticrightwhale = northatlanticrightwhale[!is.na(northatlanticrightwhale$DateFs),]

#seiwhale-- presence in a given day, 0 or 1 
nFiles = length( list.files(path=dir2,pattern = "seiwhale", full.names=TRUE, recursive = TRUE))
seiwhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "seiwhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "seiwhale", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tmp  = fread(fnameF,header=F) #remove these after
  tmp$deplm = dname
  seiwhale = rbind(seiwhale,tmp)
  rm(fname,fnameF,tmp)
}
colnames(seiwhale)= c( as.character(seiwhale[1,1:3]) ,"deply")
seiwhale = seiwhale[seiwhale$Presence != "Presence",] #remove headers in the data frame
seiwhale$DateFs   = as.Date( seiwhale$`ISO Date`,tz="GMT")
names(seiwhale)[3] = "seiwhaleP" 
seiwhale = seiwhale[!is.na(seiwhale$DateFs),]

#MERGE all the biologics into one matrix, except cod because need to modify
# atlanticcod, dolphins, bluewhale, finwhale, humpbackwhale, northatlanticrightwhale,  seiwhale
b1 = merge(bluewhale, finwhale, by = "DateFs")
b2 = merge(b1, humpbackwhale, by = "DateFs")
b3 = merge(b2, northatlanticrightwhale, by = "DateFs")
b4 = merge(b3, seiwhale, by = "DateFs")
tst = data.frame(b4$DateFs,b4$Sanctuarysite_deployment.x, b4$bluewhaleP,b4$finwhaleP,b4$humpbackwhaleP,b4$northatlanticrightwhaleP,b4$seiwhaleP)
colnames(tst) = c("DateFs","Sanctuarysite_deployment","bluewhaleP","finwhaleP","humpbackwhaleP","northatlanticrightwhaleP","seiwhaleP")
bio = merge(tst,dolphinsD, by="DateFs")
bio$bluewhaleP = as.numeric(as.character(bio$bluewhaleP ))
bio$finwhaleP = as.numeric(as.character(bio$finwhaleP ))
bio$humpbackwhaleP = as.numeric(as.character(bio$humpbackwhaleP ))
bio$northatlanticrightwhaleP = as.numeric(as.character(bio$northatlanticrightwhaleP ))
bio$seiwhaleP = as.numeric(as.character(bio$seiwhaleP ))
bio$DolphinHrs = as.numeric(as.character(bio$DolphinHrs ))
bio$dolphinsPday = as.numeric(as.character(bio$dolphinsPday ))
bio$Sanctuarysite_deployment = as.character(bio$Sanctuarysite_deployment)

#COMBINE results for days with repeated values...deployment switch days so need to combine!
udays = ( unique(bio$DateFs) )  #length( unique(bio$DateFs) ) 
bio2 = NULL
for (dd in 1: length(udays)){
  tmp1 = bio[ bio$DateFs == udays[dd],]
  if (nrow(tmp1) == 1){
    bio2 = rbind (bio2, tmp1)
  }else { 
    tmp2 =  ( colSums( tmp1[,3:9] ) )
    tmp3 =  c( as.character(tmp1[1,1]), as.character(tmp1[1,2]) )
    bio2 = rbind (bio2, c(tmp3, tmp2) ) }
}
bio2$bluewhaleP = as.numeric(as.character(bio2$bluewhaleP ))
bio2$finwhaleP = as.numeric(as.character(bio2$finwhaleP ))
bio2$humpbackwhaleP = as.numeric(as.character(bio2$humpbackwhaleP ))
bio2$northatlanticrightwhaleP = as.numeric(as.character(bio2$northatlanticrightwhaleP ))
bio2$seiwhaleP = as.numeric(as.character(bio2$seiwhaleP ))
bio2$DolphinHrs = as.numeric(as.character(bio2$DolphinHrs ))
bio2$dolphinsPday = as.numeric(as.character(bio2$dolphinsPday ))

#MATCH biological detections with all SPL measurements- NA are data not processed yet for detections
BioAll = as.data.frame( seq(from=as.Date(min(SPL$DateFday)), 
                              to=as.Date(max(SPL$DateFday)), by="day") )
colnames(BioAll) = "DateFs"
BioAllo = NULL
for (ii in 1:nrow(BioAll)){
  tst = nrow(bio2[bio2$DateFs == BioAll$DateFs[ii],])
  if (tst == 1){
    tmp = bio2[bio2$DateFs == BioAll$DateFs[ii],1:9]
    BioAllo = rbind(BioAllo, cbind(BioAll$DateFs[ii],tmp))
  } else if (tst == 0) {
    BioAllo = rbind(BioAllo,c(as.character(BioAll$DateFs[ii]),NA,NA,NA,NA,NA,NA,NA,NA,NA) )
    cat(as.character( BioAll$DateFs[ii]), ii, "\n")
  }else if (tst > 1){
    cat(as.character( BioAll$DateFs[ii]),"\n") #just a check for days with multiple enteris, should be removed in prevous step
  }
    
}
#head(BioAllo)
colnames(BioAllo)[2] = "DateFs1"
colnames(BioAllo)[1] = "DateFs"

#ADD cod to the daily presence ii = 1
BioAllo$atlanticcodP = NA
for (ii in 1:nrow(BioAllo)){
  tmp1 = nrow( atlanticcod[atlanticcod$DateDay == BioAllo$DateFs[ii],] )
  if (tmp1 > 0){ #total detection on that day
   tmp2 = atlanticcod[atlanticcod$DateDay == BioAllo$DateFs[ii],]
   BioAllo$atlanticcodP[ii] = sum(as.numeric(as.character(tmp2$Presence)))
  } else { BioAllo$atlanticcodP[ii] = 0 } #no detections on that day
}

#GRAPHIC for all presense of biological sounds
BioAllo$dolphinsPscale   =  as.character( as.numeric(as.character(BioAllo$dolphinsPday))/as.numeric(as.character(BioAllo$DolphinHrs)))#rescaled as fraction of hour because # so high
BioAllo$atlanticcodPscale = as.character((BioAllo$atlanticcodP - min(BioAllo$atlanticcodP,na.rm = T) ) / 
                                            (max(BioAllo$atlanticcodP,na.rm = T)-min(BioAllo$atlanticcodP,na.rm = T)))
BioAllomelt = reshape :: melt(BioAllo, id.vars = "DateFs", 
                             measure.vars = c("bluewhaleP","finwhaleP","humpbackwhaleP",
                                              "northatlanticrightwhaleP","seiwhaleP", 
                                              "dolphinsPscale","atlanticcodPscale" ))
BioAllomelt$value = as.numeric(as.character(BioAllomelt$value) )

p1 = ggplot(BioAllomelt, aes(DateFs, variable, fill= (value))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  scale_y_discrete(labels = c("Blue whale","Fin whale","Humpback whale","NA Right whale", 
                              "Sei whale","Dolphins \n (fraction of day)","Atlantic cod \n (scaled)"))+ 
  labs(title = paste0(deply,": Check- Identified sounds"),  fill = "Detections") +
  xlab("") +
  ylab("")
DC = Sys.Date()
ggsave(paste0(dir,"\\",deply,"_SoundSourceSummary_created_",DC,".png"),p1)

rm(b1,b2,b3,b4,bio,bio2,BioAll,tmp,tmp2,dd,dname,endR,ff,ii,nFiles,tmp1,tmp3,tst,udays,bluewhale,
   dolphinsD,finwhale,humpbackwhale,northatlanticrightwhale,seiwhale)
p1

#CHECK DATA INPUT PLOT-- too much to plot, need to break up
#grid.arrange(pSPL,pVes,   ncol = 2,nrow = 1) #acoustic data
#grid.arrange(pa,paUV,     ncol = 2,nrow = 1) #AIS
#grid.arrange(pTide,pWind, ncol = 2,nrow = 1) #environmental data

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#COMBINE: for each SPL time step, find associated metrics AIS, WSPD, TIDE, VESS using formated date
#MERGE: with daily AIS values... each SPL time step has same AIS value!!
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
print.POSIXct = function(x,...)print(format(x,"%Y-%m-%d %H:%M:%S"))

#COMBINE AIS
#-----------------------------------------------------------------------------------------
#Repeated values for each hourly SPL for a given day because only one daily value for AIS
SplAis = merge(SPL, AIS, all = FALSE, all.x = TRUE, by = "DateFday" ) 

#SOME CHECKS: values were way off for SB01_04 so checking values
#relationship between 63Hz and unique AIS ships (not on the sample time resolution)
#ggplot(SplAis,aes(LOA_ALL_UV,OL_63 ) ) +
#geom_point(aes(size = LOA_ALL_OPHRS))
#hist(AIS$LOA_ALL_UV)
#hist(SPL$OL_63)

#COMBINE WIND and TIDE
#-----------------------------------------------------------------------------------------
#matches the hourly SPL values, only gets the value for that hour (want average for the hour as well)
WSPD$Day   = as.Date(WSPD$DateF, format = "%Y-%m-%d")
WSPD$HR    = strftime(WSPD$DateF, format="%H") #hour(WSPD$DateF)
WSPD$DayHR = as.POSIXct( paste0(WSPD$Day," ", WSPD$HR,":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

WSPDavg = aggregate(WSPD$WSPD, by=list(WSPD$DayHR), mean)
colnames(WSPDavg) = c("DateF","avgWSPD")
SplAisWspd     = merge(SplAis, WSPDavg,  all = FALSE, all.x = TRUE, by = "DateF" )#mean at the top of the hour
SplAisWspd     = merge(SplAisWspd, WSPD, all = FALSE, all.x = TRUE, by = "DateF" )#single value at this 10 minute interval

#CHECK- pick one unique hour 2019-01-01 00:00:00: average for that hour, using above method, and value for top of hour (10.9666667)
ck  = mean( WSPD$WSPD[WSPD$DayHR == "2019-01-01 00:00:00"], na.rm = TRUE)
ck2 = SplAisWspd$avgWSPD[(SplAisWspd$DateF == "2019-01-01 00:00:00")]
ck3 = SplAisWspd$WSPD[(SplAisWspd$DateF == "2019-01-01 00:00:00")]
c(ck,ck2,ck3) #keeps both values

#keeps sunaltitude for the top of the hour-- not an average!

SplAisWspdTide = merge(SplAisWspd, TIDE, all = FALSE, all.x = TRUE, by = "DateF" )
cat("Check formats match: AIS-", as.character(SplAis$DateF[2]), " WSPD-", as.character(WSPD$DateF[2]), " TIDE-", as.character(TIDE$DateF[2]))

rm(ck,ck2,ck3,WSPD,WSPDavg,TIDE,AIS)

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

# COMBINE Biologics-- like AIS, baleen whales are daily values, so just repeat values for each hours
#-----------------------------------------------------------------------------------------
#Repeated values for each hourly BIOLOGICAL for a given day because only one daily value for BIO detections
colnames(BioAllo)[1] = "Day"
BioAllo$Day[1]
SplAisWspdTideVess$Day = as.Date(SplAisWspdTideVess$DateF)
SplAisWspdTideVessBio  = merge(SplAisWspdTideVess, BioAllo, all = FALSE, all.x = TRUE, by = "Day" ) 

#dolphin and cod are hourly-- remove then combine hourly back in
SplAisWspdTideVessBio = SplAisWspdTideVessBio[,1:63]
dolphins$DateF = dolphins$DayHR
SplAisWspdTideVessBio = merge(SplAisWspdTideVessBio, dolphins, all = FALSE, all.x = TRUE, by = "DateF" )

SplAisWspdTideVessBio$atlanticcodP = NA
for (ii in 1:nrow(SplAisWspdTideVessBio)){
  tmp1 = nrow( atlanticcod[atlanticcod$DayHR == SplAisWspdTideVessBio$DateF[ii],] )
  if (tmp1 > 0){ #total detection on that day
    tmp2 =     atlanticcod[atlanticcod$DayHR == SplAisWspdTideVessBio$DateF[ii],]
    SplAisWspdTideVessBio$atlanticcodP[ii] = sum(as.numeric(as.character(tmp2$Presence)))
  } else { 
    SplAisWspdTideVessBio$atlanticcodP[ii] = 0 } #no detections on that day
}


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#CLEAN UP big matrix
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#which(colnames(SplAisWspdTideVessBio) == "Day")
head( SplAisWspdTideVessBio )
dCols = data.frame(colnames(SplAisWspdTideVessBio)) 
dCols

SplAisWspdTideVessBioClean = SplAisWspdTideVessBio[,c(1:2,30,33:36,29,16,6:15,19:28,31,38,43, 48,50,52,54:55,60:63,65,69) ]
colnames(SplAisWspdTideVessBioClean)
SplAisWspdTideVessBioClean$Yr = year(SplAisWspdTideVessBioClean$DateF)
SplAisWspdTideVessBioClean$DateFJul = yday(SplAisWspdTideVessBioClean$DateF)
# add AIS together to total variables
SplAisWspdTideVessBioClean$AIS_UV_total = 
  SplAisWspdTideVessBioClean$LOA_S_UV + SplAisWspdTideVessBioClean$LOA_M_UV + SplAisWspdTideVessBioClean$LOA_L_UV +
  SplAisWspdTideVessBioClean$LOA_NA_UV
SplAisWspdTideVessBioClean$AIS_OPHRS_total = 
  SplAisWspdTideVessBioClean$LOA_S_OPHRS + SplAisWspdTideVessBioClean$LOA_M_OPHRS + SplAisWspdTideVessBioClean$LOA_L_OPHRS +
  SplAisWspdTideVessBioClean$LOA_NA_OPHRS

dCols = data.frame(colnames(SplAisWspdTideVessBioClean)) 
dCols

#THIS is hourly summary of data... note that some variables are daily so this is not ideal (but ideal for analysis!!)
HrSumALL = SplAisWspdTideVessBioClean 

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#SUMMARIZE BY DAY
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
daySum1 = NULL 
uday = unique(as.Date(HrSumALL$DateF))
dIn = HrSumALL
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
  #dtmp$totalTime > 3600
  
  #SPL-- median per octave band
  SPLm = apply( dtmp[,10:19] , 2 , quantile , probs = 0.5 , na.rm = TRUE )
  
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
  
  # BIO: already did day summary in BioAll0, so append at the end
  # OR Dolphins and Cod- summarise detections in that day and hours sampled (can I just combine with dolphinsD)
  #dolpinP, atlanticcodP,dolphinsPscale, atlanticcodPscale, Baleen whales- repeated for each hour, so just take first row 
  
  #Combine to output
  daySum1 = rbind(daySum1,c(as.character(dtmp$Day[1]), yday(uday[ii]), 
                            dtmp$LOA_S_UV[1] , dtmp$LOA_M_UV[1] , dtmp$LOA_L_UV[1] , dtmp$LOA_NA_UV[1],
                            dtmp$LOA_S_OPHRS[1] , dtmp$LOA_M_OPHRS[1] , dtmp$LOA_L_OPHRS[1] , dtmp$LOA_NA_OPHRS[1],
                            Atotal,Ahtotal,
                            VDtotal,VDperHr,VDper,
                            SPLm,WSPDm,TIDEm,
                            nrow(dtmp),depl ) )
  
  rm(dtmp,Atotal, Ahtotal, perAtotal,VDtotal,VDperHr,VDper,SPLm,WSPDm,TIDEm,depl)
}

daySum1 = as.data.frame(daySum1)
dCols = data.frame(colnames(daySum1)) 
dCols
head( daySum1 )

colnames(daySum1) = c("Day","JulDay",
                      "LOA_S_UV","LOA_M_UV","LOA_L_UV","LOA_NA_UV",
                      "LOA_S_OPHRS",'LOA_M_OPHRS','LOA_L_OPHRS','LOA_NA_OPHRS',
                      "LOA_ALL_UV", "LOA_ALL_OPHRS",
                      "VessD_TV","VessD_VD_meanPerHr","VessD_Time_PerDay",
                      "mOB_31.5","mOB_63","mOB_125","mOB_250","mOB_500","mOB_1000","mOB_2000","mOB_4000","mOB_8000","mOB_16000",
                      "WSPD_mean", "TIDE_mean", 
                      "totHRs","deployment")
daySum1$Year = year(daySum1$Day)

#combine with BioAllo$
daySum1$Day = as.Date(daySum1$Day )

#BioAllo$Day
daySumALL = merge(daySum1,BioAllo,by = "Day")

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#TRUNCATE by time period of interest
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
analysisPeriods = read.csv("D:\\RESEARCH\\SanctSound\\data\\AnalysisPeriods.csv")
tmpAP = analysisPeriods[analysisPeriods$Site == deply,]
HrTrunData  = HrSumALL[ (HrSumALL$Day >= as.Date(tmpAP$DateStart) & HrSumALL$Day  <= as.Date(tmpAP$DateEnd) ),]
DayTrunData = daySumALL[(daySumALL$Day>= as.Date(tmpAP$DateStart) & daySumALL$Day <= as.Date(tmpAP$DateEnd) ),]

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#WRITE OUT FILES
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
setwd(dir2)
DC = Sys.Date()
fnameAll =  paste0(deply, "_CombinedDataHR_",as.character(min(as.Date(HrSumALL$DateF))),"_",
                   as.character(max(as.Date(HrSumALL$DateF))), "_v", DC)  
write.csv(HrSumALL,fnameAll) 

fnameAllday =  paste0(deply, "_CombinedDataDAY_",as.character(min(as.Date(HrSumALL$DateF))),"_",
                      as.character(max(as.Date(HrSumALL$DateF))) , "_v", DC)   
write.csv(daySumALL,fnameAllday)

fnameTrun =  paste0(deply, "_", tmpAP$Analysis,"DataHR_",as.character(tmpAP$DateStart),"_",as.character(tmpAP$DateEnd) , "_v", DC)   
write.csv(HrTrunData,fnameTrun)

fnameTrun =  paste0(deply, "_", tmpAP$Analysis,"DataDay_",as.character(tmpAP$DateStart),"_",as.character(tmpAP$DateEnd) , "_v", DC)   
write.csv(DayTrunData,fnameTrun)

