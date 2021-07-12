#Integrating SanctSound data-- ome initial plots to look at Sound levels, AIS, vessel presence

# used to produce plots in RAP May 2021 presentation

rm(list=ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)

#-----------------------------------------------------------------------------------------
### SOUND LEVELS-- all sites with data April 2019 vs 2020 (missing lots of data)
#-----------------------------------------------------------------------------------------
dir     = ("E:\\RESEARCH\\SanctSound\\data\\")
filesOL = list.files (path =dir ,pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)
uSites  = unique( sapply(strsplit(basename(filesOL),"_"),"[",2) )
#filesOLU = ( unique(basename( filesOL) ))

SPL04 = NULL
for (ii in 1:length(uSites)){
  
  tFiles = filesOL[grepl(uSites[ii], filesOL, fixed = TRUE)]
  
  SPL = NULL
  for (ff in 1:length(tFiles)){
    tDat = read.csv(tFiles[ff])
   
    tDat$Site = uSites[ii]
    tDat$Deply = sapply(strsplit(basename(tFiles[ff]),"_"),"[",3)
    tDat$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tDat[,1])), tz = "GMT" )
    tDat$DateFday  = as.Date(tDat$DateF)
    
    cat(uSites[ii], "- ", tDat$Deply[1], ": ", as.character( tDat$DateF [1]),"\n" )
    
    SPL = rbind(SPL, cbind(as.character(tDat$DateF), as.character(tDat$DateFday), tDat$Site, tDat$Deply, tDat$OL_63, tDat$OL_125) )
    rm(tDat)
  }
  
  #just get values for April 
  SPL = as.data.frame(SPL)
  colnames(SPL) = c("DateTime","Day","Site","Deploy","OL_63","OL_125")
  SPL$Day  = as.Date(SPL$Day)
  SPL$DateF = as.POSIXct(SPL$DateTime,tz = "GMT")
  SPL$Mth = month(SPL$Day)
  SPL$Yr  = year(SPL$Day)
  
  SPL04 = rbind(SPL04, SPL[SPL$Mth == 4,])
  rm(SPL)
}

SPL04$OL_63  = as.numeric(as.character(SPL04$OL_63))
SPL04$OL_125 = as.numeric( as.character(SPL04$OL_125))
SPL04$DateTime2 = as.POSIXct(SPL04$DateTime ,tz = "GMT")

ggplot(SPL04, aes(as.factor(Yr), OL_125, group = as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.size=.2, outlier.alpha = .2, aes(fill=as.factor(Yr))) +
  facet_grid(~ Site) +
  theme_minimal()  +
  xlab("") +   ylab ("") + ggtitle("Hourly low frequency sound levels (125 Hz)")+ #median [125 Hz]
  ylim(c(75,120)) + labs(caption = "April") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+ 
  theme(axis.text        = element_text(size=8,angle=30),
        panel.background = element_rect(fill = "transparent"),
        plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
        axis.title       = element_text(size=5,face="bold"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        #plot.margin      = unit(c(1,2,2,2), "lines"),
        legend.position = "none") 

#-----------------------------------------------------------------------------------------
### SOUND LEVELS-- sites of interest
#-----------------------------------------------------------------------------------------

#SET working directory and input sites
#-----------------------------------------------------------------------------------------
site  = c('SB',"MB")
deply = c("SB01","SB02","SB03","MB01","MB02")
setwd(dir)
filesSB01  = list.files (path = paste0("E:\\RESEARCH\\SanctSound\\data\\",site[1],"\\", deply[1]),pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)
filesSB02  = list.files (path = paste0("E:\\RESEARCH\\SanctSound\\data\\",site[1],"\\", deply[2]), pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)
filesSB03  = list.files (path = paste0("E:\\RESEARCH\\SanctSound\\data\\",site[1],"\\", deply[3]), pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)
filesMB01  = list.files (path = paste0("E:\\RESEARCH\\SanctSound\\data\\",site[2],"\\", deply[4]), pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)
filesMB02  = list.files (path = paste0("E:\\RESEARCH\\SanctSound\\data\\",site[2],"\\", deply[5]), pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)
files = c(filesSB01,filesSB02,filesSB03,filesMB01,filesMB02)
#READ IN in data- hourly octave band levels, add site, deployment, combine
#-----------------------------------------------------------------------------------------
SPL = NULL
for (ff in 1:length(files)){
  tSit = sapply(strsplit(basename(files[ff]),"_"),"[",2)
  tDep = sapply(strsplit(basename(files[ff]),"_"),"[",3)
  tDat = read.csv(files[ff])
  tDat$Site = tSit
  tDat$Deply = tDep
  #different date formats!!
  #tDat$yyyy.mm.ddTHH.MM.SSZ
  tDat$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tDat[,1])), tz = "GMT" )
  tDat$DateFday  = as.Date(tDat$DateF)
  #CheckDate: 
  cat(tSit, "- ", tDep, ": ", as.character( tDat$DateF [1]),"\n" )
  SPL = rbind(SPL, cbind(as.character(tDat$DateF), as.character(tDat$DateFday), tDat$Site, tDat$Deply, tDat$OL_63, tDat$OL_125) )
 }
SPL = as.data.frame(SPL)
colnames(SPL) = c("DateTime","Day","Site","Deploy","OL_63","OL_125")
SPL$Day  = as.Date(SPL$Day)
SPL$DateF = as.POSIXct(SPL$DateTime,tz = "GMT")
#Format data and columns
#-----------------------------------------------------------------------------------------
unique(SPL$Site)
SPL$Mth = month(SPL$Day)
SPL$Yr = year(SPL$Day)
rm(tSit,tDep,ff,tDat)
SPL$OL_63 = as.numeric(as.character(SPL$OL_63))
SPL$OL_125 = as.numeric(as.character(SPL$OL_125))
SPL$Jul = yday(SPL$Day)
#CHECK PLOTS- ALL SITES AND DAYS??
#-----------------------------------------------------------------------------------------
ggplot(SPL, aes(Day, OL_125, color = Yr))+
  geom_point() +
  theme_minimal() +
  facet_grid(~ Site)

ggplot(SPL, aes(Yr, OL_125,group = Mth))+
  geom_boxplot() +
  theme_minimal() +
  facet_grid(~ Site)
#PLOT: Comparison of sound levels- all sites April 2019 vs 2020
#-----------------------------------------------------------------------------------------
SPLApr = SPL[SPL$Mth == 4,]
ggplot(SPLApr, aes(as.factor(Yr), OL_125, group = as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.size=.2, outlier.alpha = .2, aes(fill=as.factor(Yr))) +
  facet_grid(~ Site)+
  theme_minimal() +
  xlab("") +   ylab ("") + ggtitle("Low frequency sound levels (125 Hz)")+ #median [125 Hz]
  ylim(c(75,120)) + labs(caption = "April") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
        axis.title       = element_text(size=5,face="bold"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        plot.margin      = unit(c(1,2,2,2), "lines"),
        legend.position = "none") 
#PLOT: Comparison of sound levels- by site for each month 2019,2020,2021
#-----------------------------------------------------------------------------------------
SPL = SPL[SPL$Yr >2018,] #remove 2018 data

SPLSite = SPL[SPL$Site == "MB01",]
pMB01 = ggplot(SPLSite, aes(as.factor(Mth), OL_125, fill = as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
  theme_minimal() +
  xlab("") +   ylab ("Low frequency sound levels [125 Hz] ") + ggtitle("MB01- canyon ")+ #median [125 Hz]
  ylim(c(75,110)) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
        axis.title       = element_text(size=11,face="bold"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        #plot.margin      = unit(c(1,2,2,2), "lines"),
        legend.position = "none")   

SPLSite = SPL[SPL$Site == "MB02",]
pMB02 = ggplot(SPLSite, aes(as.factor(Mth), OL_125, fill = as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
  theme_minimal() +
  xlab("") +   ylab ("Low frequency sound levels [125 Hz] ") + ggtitle("MB02- pt. pinos ")+ #median [125 Hz]
  ylim(c(75,110)) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
        axis.title       = element_text(size=11,face="bold"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        #plot.margin      = unit(c(1,2,2,2), "lines"),
        legend.position = "none")   
#-----------------------------------------------------------------------------------------
#Monterey Bay Plot
grid.arrange(pMB01,pMB02,ncol=2)
#-----------------------------------------------------------------------------------------


SPLSite = SPL[SPL$Site == "SB01",]
pSB01 = ggplot(SPLSite, aes(as.factor(Mth), OL_125, fill = as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
  theme_minimal() +
  xlab("") +   ylab ("Low frequency sound levels [125 Hz] ") + ggtitle("SB01- in shore ")+ #median [125 Hz]
  ylim(c(75,110)) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9","gray42")) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
        axis.title       = element_text(size=11,face="bold"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        #plot.margin      = unit(c(1,2,2,2), "lines"),
        legend.position = "none")   

SPLSite = SPL[SPL$Site == "SB02",]
pSB02 = ggplot(SPLSite, aes(as.factor(Mth), OL_125, fill = as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
  theme_minimal() +
  xlab("") +   ylab ("Low frequency sound levels [125 Hz] ") + ggtitle("SB02- north ")+ #median [125 Hz]
  ylim(c(75,110)) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9","gray42")) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
        axis.title       = element_text(size=11,face="bold"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        #plot.margin      = unit(c(1,2,2,2), "lines"),
        legend.position = "none")   

SPLSite = SPL[SPL$Site == "SB03",]
pSB03 = ggplot(SPLSite, aes(as.factor(Mth), OL_125, fill = as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
  theme_minimal() +
  xlab("") +   ylab ("Low frequency sound levels [125 Hz] ") + ggtitle("SB03- south ")+ #median [125 Hz]
  ylim(c(75,110)) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9","gray42")) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
        axis.title       = element_text(size=11,face="bold"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        #plot.margin      = unit(c(1,2,2,2), "lines"),
        legend.position = "none")  
#Stellwagen Bank
#-----------------------------------------------------------------------------------------
grid.arrange(pSB01,pSB02,pSB03,ncol=3)
#-----------------------------------------------------------------------------------------
#PLOT: check SPL values
#-----------------------------------------------------------------------------------------
SPL$DateTimeF = as.POSIXct( SPL$DateTime, tz="GMT")

ggplot(SPL, aes(DateTimeF, OL_125, color = Deploy)) +
  geom_point(alpha=.2) +
  theme_minimal() +
  xlab("") +   ylab ("Low frequency sound levels [125 Hz] ") +
  ylim(c(75,120))+
  facet_wrap(~Site,nrow=5)

ggplot(SPL, aes(DateTimeF, OL_125, color = Deploy)) +
  geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
  #geom_point() +
  theme_minimal() +
  xlab("") +   ylab ("Low frequency sound levels [125 Hz] ") +
  ylim(c(75,120))+
  facet_wrap(~Site,nrow=5)

#-----------------------------------------------------------------------------------------
### AIS data and plots
#-----------------------------------------------------------------------------------------
#PLOT: Comparison of AIS vessel presence- composition
#-----------------------------------------------------------------------------------------
AISfiles = list.files(path = dir,pattern = "2020_11.csv",recursive = TRUE)
AIS=NULL
for (ff in 1:length(AISfiles)){
  tSit = sapply(strsplit(basename(AISfiles[ff]),"_"),"[",2)
  tDat = read.csv(AISfiles[ff])
  tDat$Sanct = tSit
  AIS = rbind(AIS, tDat )
}
AIS = as.data.frame(AIS)
as.data.frame(colnames(AIS))
unique(AIS$LOC_ID)
AIS$DATE = as.Date(AIS$DATE,format = "%m/%d/%Y")
AIS$Yr  = year(AIS$DATE)
AIS$Mth = month(AIS$DATE)
AIS$Jul = yday(AIS$DATE)
AIS$YrM = paste0(AIS$Yr,AIS$Mth)
AIS2 = AIS[AIS$Yr > 2018,]
#-----------------------------------------------------------------------------------------
VesSite = AIS2[AIS2$LOC_ID == "MB02",]
unique(VesSite$Yr)
as.data.frame(colnames(VesSite))

Vessm    = reshape :: melt(VesSite, id.vars = ("YrM"),
                           measure.vars = c("LOA_S_UV","LOA_M_UV", "LOA_L_UV"))
Vessm$Yr  = substr(Vessm$YrM,start = 1,stop=4)
Vessm$Mth = as.numeric( substr(Vessm$YrM,start = 5,stop=6) )

ggplot(Vessm, aes(as.factor(Mth), value, fill = variable)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  xlab("") +   ylab ("Total Unique Vessels ") + ggtitle("MB02- pt. pinos ")+ #median [125 Hz]
  facet_wrap(~Yr,ncol=2)+
  scale_fill_discrete(name = "Vessel Size", labels = c("Small", "Medium", "Large")) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_blank())
#how are the vales for the bars generated- sum of all values, so doing what expected
aggregate(Vessm$value, by=list(Vessm$Yr,Vessm$Mth), sum)
#-----------------------------------------------------------------------------------------
VesSite = AIS2[AIS2$LOC_ID == "MB01",]
unique(VesSite$Yr)
as.data.frame(colnames(VesSite))
Vessm    = reshape :: melt(VesSite, id.vars = ("YrM"),
                           measure.vars = c("LOA_S_UV","LOA_M_UV", "LOA_L_UV"))
Vessm$Yr  = substr(Vessm$YrM,start = 1,stop=4)
Vessm$Mth = as.numeric( substr(Vessm$YrM,start = 5,stop=6) )

ggplot(Vessm, aes(as.factor(Mth), value, fill = variable)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  xlab("") +   ylab ("Total Unique Vessels ") + ggtitle("MB01-canyon ")+ #median [125 Hz]
  facet_wrap(~Yr,ncol = 2)+
  scale_fill_discrete(name = "Vessel Size", labels = c("Small", "Medium", "Large")) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_blank())
aggregate(Vessm$value, by=list(Vessm$Yr,Vessm$Mth), sum)
#-----------------------------------------------------------------------------------------
VesSite = AIS2[AIS2$LOC_ID == "SB01",]
unique(VesSite$Yr)
as.data.frame(colnames(VesSite))
Vessm    = reshape :: melt(VesSite, id.vars = ("YrM"),
                           measure.vars = c("LOA_S_UV","LOA_M_UV", "LOA_L_UV"))
Vessm$Yr  = substr(Vessm$YrM,start = 1,stop=4)
Vessm$Mth = as.numeric( substr(Vessm$YrM,start = 5,stop=6) )

ggplot(Vessm, aes(as.factor(Mth), value, fill = variable)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  xlab("") +   ylab ("Total Unique Vessels ") + ggtitle("SB01-in shore ")+ #median [125 Hz]
  facet_wrap(~Yr,ncol = 2)+
  scale_fill_discrete(name = "Vessel Size", labels = c("Small", "Medium", "Large")) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_blank())
aggregate(Vessm$value, by=list(Vessm$Yr,Vessm$Mth), sum)
#-----------------------------------------------------------------------------------------
VesSite = AIS2[AIS2$LOC_ID == "SB02",]
unique(VesSite$Yr)
as.data.frame(colnames(VesSite))
Vessm    = reshape :: melt(VesSite, id.vars = ("YrM"),
                           measure.vars = c("LOA_S_UV","LOA_M_UV", "LOA_L_UV"))
Vessm$Yr  = substr(Vessm$YrM,start = 1,stop=4)
Vessm$Mth = as.numeric( substr(Vessm$YrM,start = 5,stop=6) )

ggplot(Vessm, aes(as.factor(Mth), value, fill = variable)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  xlab("") +   ylab ("Total Unique Vessels ") + ggtitle("SB02-north ")+ #median [125 Hz]
  facet_wrap(~Yr,ncol = 2)+
  scale_fill_discrete(name = "Vessel Size", labels = c("Small", "Medium", "Large")) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_blank())
aggregate(Vessm$value, by=list(Vessm$Yr,Vessm$Mth), sum)
#-----------------------------------------------------------------------------------------
VesSite = AIS2[AIS2$LOC_ID == "SB03",]
unique(VesSite$Yr)
as.data.frame(colnames(VesSite))
Vessm    = reshape :: melt(VesSite, id.vars = ("YrM"),
                           measure.vars = c("LOA_S_UV","LOA_M_UV", "LOA_L_UV"))
Vessm$Yr  = substr(Vessm$YrM,start = 1,stop=4)
Vessm$Mth = as.numeric( substr(Vessm$YrM,start = 5,stop=6) )

ggplot(Vessm, aes(as.factor(Mth), value, fill = variable)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  xlab("") +   ylab ("Total Unique Vessels ") + ggtitle("SB03-south ")+ #median [125 Hz]
  facet_wrap(~Yr,ncol = 2)+
  scale_fill_discrete(name = "Vessel Size", labels = c("Small", "Medium", "Large")) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_blank())
aggregate(Vessm$value, by=list(Vessm$Yr,Vessm$Mth), sum)