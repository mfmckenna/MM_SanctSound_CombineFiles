# plot sancturary sound vessel data,  AIS results, and SPL data
rm(list=ls())

#-----------------------------------------------------------------------------------------
# MAIN functions-- by site
#-----------------------------------------------------------------------------------------
# reads in output from: process_VesselDetectionsAIS.R


# SITE DETAILS
#-----------------------------------------------------------------------------------------
dirSite   = "E:\\RESEARCH\\SanctSound\\data\\SB"
sanctuary = "SB"
sites     = c("01","02","03","04","05") #list sites
dir1      = "E:\\RESEARCH\\SanctSound\\data2\\combineFileEffort_SplAisVesDet\\"
outDir    = "E:\\RESEARCH\\SanctSound\\data2\\plots_COVID\\"
DC = Sys.Date()

#range of dates for output graphics
eDatePlot = '2020-12-31'
sDatePlot = '2018-11-01'

#true if you want to ouptut hourly and daily csv files per site
flagCSV = TRUE

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
# FIND AND READ IN 1) SPL, AIS, Vessel detections
# from process_VesselDetectionsAIS.R
# process daily files only
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

dataInDay$Day   = as.Date(dataInDay$Day)
dataInDay$Year   = year(dataInDay$Day)
dataInDay$Month   = month(dataInDay$Day)
dataInDay2 = dataInDay
dataInDay2[,5:ncol(dataInDay)] = lapply(dataInDay[,5:ncol(dataInDay)], function(x) as.numeric(as.character(x)))
dataInDay = dataInDay2
rm(tmp,x,dataInDay2)

#-----------------------------------------------------------------------------------------
# PLOT OF SPL, VESSEL METRIC, AIS METRIC by site
# A comparison between January-June 2019 and 2020 in the 
# number of vessels tracked at the surface, 
# the number of vessels heard on the hydrophone, and noise levels at low frequencies at a SanctSound listening station in Stellwagen Bank National Marine Sanctuary.
#-----------------------------------------------------------------------------------------
usite = unique( dataInDay$Site ) # 
ii = 1
siteDay = dataInDay[dataInDay$Site == usite[ii],]
#truncate data January-June
siteDay = siteDay[siteDay$Month <=6 ,]
siteDay$Year = as.factor(siteDay$Year)

#noise levels at low frequencies at a SanctSound listening station in Stellwagen Bank National Marine Sanctuary.
pSPL125 = ggplot(siteDay, aes( y=OL_125, x= Year, group= (Year) ) ) +  
  geom_boxplot(outlier.colour="white", outlier.shape=8, outlier.size=.5,fill=c("#E69F00", "#56B4E9"), color="black") +
  xlab("") +   ylab ("") + 
  #ggtitle(paste0( usite[ii], ": Low frequency sound levels (125 Hz)")) + #median [125 Hz]
  ylim(c(85,110)) + 
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        plot.caption = element_text(color = "black", face = "italic",hjust = 0),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(1,2,2,2), "lines")  ) 

# the number of vessels heard on the hydrophone,
pDet1 = ggplot(siteDay, aes(y=totalVessels, x=(Year), group=(Year))) + 
  geom_boxplot(outlier.colour="white", outlier.shape=8,outlier.size=.5,fill=c("#E69F00", "#56B4E9"), color="black") +
  xlab("") +  ylab("") + 
  #ggtitle("% of day with close-by vessel noise")+
  ylim(c(0,25)) +
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA), 
        plot.margin = unit(c(1,2,2,2), "lines") )  

# number of vessels tracked at the surface, 
pAISb = ggplot(siteDay, aes(y= LOA_ALL_UV, x=(Year), group=(Year)) ) + 
  geom_boxplot(outlier.colour="white", outlier.shape=8, outlier.size=2,fill=c("#E69F00", "#56B4E9"), color="black") +
  xlab("") +  ylab ("") + 
  # ggtitle("Unique vessels tracking close to recorder")+
  ylim(c(0,25))+
  labs(caption = "January-June")+
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(1,2,2,2), "lines")  ) 

grid.arrange(pAISb, pDet1, pSPL125, ncol = 3, nrow = 1)         
ggplotly(pAISb)
ggplotly(pDet1)
ggplotly(pSPL125)
# number of vessels tracked at the surface, 
# the number of vessels heard on the hydrophone, and 
# noise levels at low frequencies at a SanctSound listening station in Stellwagen Bank National Marine Sanctuary.


#NOT USED...

pSPL63 = ggplot(siteDay, aes( y=OL_63, x= Year, group= (Year) ) ) +  
  geom_boxplot(outlier.colour="gray", outlier.shape=8, outlier.size=2,fill=c("#E69F00", "#56B4E9"), color="black") +
  xlab("") +   ylab ("") + 
  #ggtitle("Low frequency sound levels (63 Hz)") + #median [125 Hz]
  ylim(c(85,100)) + labs(caption = "January-June")+
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        plot.caption = element_text(color = "black", face = "italic",hjust = 0),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(1,2,2,2), "lines")  ) 
pAISa = ggplot(siteDay, aes(y= LOA_ALL_OPHRS, x=(Year), group=(Year)) ) + 
  geom_boxplot(outlier.colour="gray", outlier.shape=8, outlier.size=2,fill=c("#E69F00", "#56B4E9"), color="black") +
  xlab("") +  ylab ("") + 
  #ggtitle("Transit hours of vessels close to recorder")+
  ylim(c(0,100))+
  theme(axis.text = element_text(size=14),
        panel.background = element_rect(fill = "transparent"),
        axis.title=element_text(size=5,face="bold"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(1,2,2,2), "lines")  )