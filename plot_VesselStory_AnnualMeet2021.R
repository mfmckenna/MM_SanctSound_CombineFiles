# recreate plots from process_VesselDetectionsAIS.R

rm(list=ls())
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
eDatePlot = '2019-12-31'
sDatePlot = '2019-01-01'
ra = 7 #days for running average
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
#-----------------------------------------------------------------------------------------
# MULTIPLE SITE PLOT-- for 2021 annual meeting
#-----------------------------------------------------------------------------------------
dirData      = paste0("E:\\RESEARCH\\SanctSound\\data2\\combineFileEffort_SplAisVesDet\\")
nFiles   = length( list.files(path=dirData,pattern = "VesselDetectionAISDay_", full.names=TRUE, recursive = TRUE))
inFiles  =       ( list.files(path=dirData,pattern = "VesselDetectionAISDay_", full.names=TRUE, recursive = TRUE))
x = strsplit(basename( inFiles ),"_")
sanctsAIS = unique(sapply( x, "[", 1 ))

#combine files
multmerge = function(path){
  filenames = list.files(path=path, pattern = "VesselDetectionAISDay_", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
dataIn <- multmerge(dirData)
# head( read.csv(inFiles[4]) ) different heading for one of the sites...
unique ( dataIn$Site )

#re-format columns
dataIn$Day   = as.Date (dataIn$Day)
dataIn$deply = as.factor(dataIn$deply)
dataIn2 = dataIn[dataIn$Site == "GR01" | dataIn$Site == "MB01" | dataIn$Site == "SB02" ,] 
dataCk  = dataIn[dataIn$Site == "OC01" ,] 
unique( dataCk$deply )

#select specific sites
head(dataIn2)
#plot-- durations
p1 =ggplot(dataIn2, aes(Day,PerDay, color = Site) ) +
  geom_point(alpha = .2)+   geom_line(aes(y=rollmean(PerDay, ra, na.pad=TRUE)),size=1) +
  xlab("")+     ylab("") + theme_minimal()+
  ggtitle("Detector: % day dominated by vessel noise")+
  scale_colour_manual(name = "Site", values=c( "#009E73", "#BB9D00", "#F8766D") ) +
  ylim(c(0, round(max(dataIn2$PerDay, na.rm = TRUE)) ) ) +
  scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
  theme(axis.text.x = element_text(colour="black",size=12,angle = 90), 
        axis.text.y = element_text(size=12,colour="black"),
        legend.position=c(0.9, 0.9), 
        legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
p2 =ggplot(dataIn2, aes(Day,LOA_ALL_OPHRS, color = Site) ) +
  geom_point(alpha = .2)+   geom_line(aes(y=rollmean(LOA_ALL_OPHRS, ra, na.pad=TRUE)),size=1) +
  xlab("")+     ylab("") + theme_minimal()+
  ggtitle("AIS: daily operational hours")+
  scale_colour_manual(name = "Site", values=c( "#009E73", "#BB9D00", "#F8766D") ) +
  ylim(c(0, round(max(dataIn2$LOA_ALL_OPHRS, na.rm = TRUE)) ) ) +
  scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
  theme(axis.text.x = element_text(colour="black",size=12,angle = 90), 
        axis.text.y = element_text(size=12,colour="black"),
        legend.position=c(0.9, 0.9), 
        legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
#plot-- counts
p3 =ggplot(dataIn2, aes(Day,LOA_ALL_UV, color = Site) ) +
  geom_point(alpha = .2)+   geom_line(aes(y=rollmean(LOA_ALL_UV, ra, na.pad=TRUE)),size=1) +
  xlab("")+     ylab("") + theme_minimal()+
  ggtitle("AIS: unique vessels") +
  scale_colour_manual(name = "Site", values=c( "#009E73", "#BB9D00", "#F8766D") ) +
  ylim(c(0, round(max(dataIn2$LOA_ALL_UV, na.rm = TRUE)) ) ) +
  scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
  theme(axis.text.x = element_text(colour="black",size=14,angle = 90), 
        axis.text.y = element_text(size=14,colour="black"),
        legend.position=c(0.9, 0.9), 
        legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 

p4 = ggplot(dataIn2, aes(Day,totalVessels, color = Site) ) +
  geom_point(alpha = .2)+   geom_line(aes(y=rollmean(totalVessels, ra, na.pad=TRUE)),size=1) +
  xlab("")+     ylab("") + theme_minimal()+
  scale_colour_manual(name = "Site", values=c( "#009E73", "#BB9D00", "#F8766D") ) +
  ggtitle("Detector: count vessel niose dominate periods")+
  ylim(c(0, round(max(dataIn2$totalVessels, na.rm = TRUE)) ) ) +
  scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
  theme(axis.text.x = element_text(colour="black",size=14,angle = 90), 
        axis.text.y = element_text(size=14,colour="black"),
        legend.position=c(0.9, 0.9), 
        legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
ggplotly(p1)
ggplotly(p2)
ggplotly(p3)
ggplotly(p4)


#-----------------------------------------------------------------------------------------
# SINGLE SITE PLOT
#-----------------------------------------------------------------------------------------
dataIn = "E:\\RESEARCH\\SanctSound\\data2\\combineFileEffort_SplAisVesDet\\SB02_VesselDetectionAISDay_2018-11-12to2020-06-12_v2021-02-16.csv"
cDatat = read.csv( dataIn )


cDatat$Day = as.Date (cDatat$Day)
cDatat$deply = as.factor(cDatat$deply)

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
