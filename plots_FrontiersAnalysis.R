#Make plots for Frontiers paper

rm(list=ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(plyr)

#SET working directory--
#-----------------------------------------------------------------------------------------
mytheme <- theme(panel.spacing=unit(2,"lines"),legend.position="right",axis.text.x = element_text(colour="black",size=14,angle = 90), 
                 axis.text.y=element_text(size=14,colour="black"),axis.title.x=element_text(size=16,colour="black"), 
                 axis.title.y=element_text(size=16,colour="black"), axis.line = element_line(colour="black",size=1), 
                 panel.background=element_rect(fill= "transparent",colour =NA),
                 legend.background= element_rect(fill="transparent",colour=NA),
                 axis.line.x = element_line(colour="black"),axis.line.y=element_line(colour="black"))


#-----------------------------------------------------------------------------------------
# SPL comparison across sites-- precentiles, adapted from rd_ncFiles.R
#-----------------------------------------------------------------------------------------

#load combined files
#-----------------------------------------------------------------------------------------
dirSPL   = paste0("E:\\RESEARCH\\SanctSound\\data2\\combineFiles")
nFiles   = length( list.files(path=dirSPL, pattern = "FrontiersDataHR_2", full.names=TRUE, recursive = FALSE))
FilesSPL =       list.files(path=dirSPL,pattern  = "FrontiersDataHR_2", full.names=TRUE, recursive = FALSE)

dataSPL = NULL
dataSPLp = NULL
for(ss in 1:(nFiles)){
  site = sapply( strsplit(basename(FilesSPL[ss]),"_") , `[`, 1)
  tmp = read.csv(FilesSPL[ss])
  #checks....
  #cat(site, ":", ncol(tmp), c(colnames(tmp)), "\n" ) #different length because detections or no detections
  #colnames(dataSPL) 
  #colnames( tmp )
  #BB column has a different name- ugh!!
  if (site == "OC01") { colnames(tmp)[18] = "BB_20.24000" }
  if (site == "SB01") { 
    tmp = tmp[, c(1:7, 9:33)]
    colnames(tmp)[18] = "BB_20.24000" }
  #all data
  dataSPL = rbind(dataSPL, tmp[,1:32])
  
  #percentiles
  g =  c(as.numeric(grep("^OL", colnames(tmp) )),18)
  probst = c(.95,.75,.5,.25,.05)
  tmpP = apply( tmp[,g], 2 , quantile , probs = probst, na.rm = TRUE )
  tmpP = as.data.frame(tmpP)
  tmpP$site =site
  tmpP$quart = rownames(tmpP)
  colnames(tmpP)[1:11] = c("31.5","63","125","250","500","1000","2000","4000","8000","16000","BB")
  dataSPLm = reshape2 :: melt(tmpP, id.vars = "quart", measure.vars = colnames(tmpP)[1:11])
  dataSPLm$site = site
  dataSPLp = rbind(dataSPLp, dataSPLm)
}

#dataSPLp = dataSPLp[dataSPLp$site!="OC02",]
dataSPLp = dataSPLp[dataSPLp$site!="FK04",]
dataSPLp$variable2 = as.numeric( as.character(dataSPLp$variable))
dataSPLp$quart2 = as.numeric(gsub("%","",dataSPLp$quart))

#remove lower OB, decrease values by 4 dB
#-----------------------------------------------------------------------------------------
dataSPLp = dataSPLp[dataSPLp$variable != "31.5",]
dataSPLp = dataSPLp[dataSPLp$variable != "63",]
dataSPLp$value2 = dataSPLp$value -4

#remove BB and add as separate box
#-----------------------------------------------------------------------------------------
dataSPLpOL = dataSPLp[!is.na(dataSPLp$variable2),]
dataSPLpBB = dataSPLp[is.na(dataSPLp$variable2),]

#PLOTS -----------------------------------------------------------------------------------------

# V2
ggplot(dataSPLp, aes(as.factor(variable), value2, color = as.factor(quart2), group = as.factor((quart2))) ) +
  geom_point(size=2)+
  geom_line(size=.5)+
  scale_color_discrete(name = "Percentiles")+
  facet_wrap(~site,ncol=3)+
  theme( strip.text.x = element_text( size = 12, color = "black", face = "bold.italic") )+
  ylim(c(70,130))+
  mytheme +
  xlab( "Frequency [Hz]") +   ylab( expression(paste("Octave band sound pressure levels dB re: 1", mu, "Pa"))) + 
  ggtitle("")

ggplot(dataSPLpBB, aes(site, value2, color = as.factor(quart2) ) )+
  scale_color_discrete(name = "Percentiles")+
  geom_point(size=2) +
  mytheme +
  ylim(c(70,130))+
  xlab( "Site") +   ylab( expression(paste("Broad band sound pressure levels dB re: 1", mu, "Pa"))) + 
  ggtitle("")
rm(tmp,tmpP,g,ss)

#V3: separate by site, so I can combine with BB
ssA = c("HI01", "FK02", "CI01", "SB01","OC01","OC02","GR01", "MB01", "MB02")

ss = ssA[1]
dataSPLp1 = dataSPLp[dataSPLp$site == ss, ]
dataSPLp1 = dataSPLp1[dataSPLp1$variable != "BB", ]
p1 = ggplot(dataSPLp1, aes(as.factor(variable), value2, color = as.factor(quart2) , group = as.factor(quart2)  )) +
  geom_point(size=2) +
  geom_line( size=.5) +
  scale_color_grey(name = "Percentiles",start=0.8, end=0.2) +
  theme(strip.text.x = element_text( size = 12, color = "black", face = "bold.italic") )+
  ylim(c(65,120))+
  mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) +
  xlab( "" ) +  ylab( "" ) +   ggtitle(ss)

ss =  ssA[2]
dataSPLp1 = dataSPLp[dataSPLp$site == ss, ]
dataSPLp1 = dataSPLp1[dataSPLp1$variable != "BB", ]
p2 = ggplot(dataSPLp1, aes(as.factor(variable), value2, color = as.factor(quart2) , group = as.factor(quart2)) ) +
  geom_point(size=2) +   geom_line( size=.5) +
  scale_color_grey(start=0.8, end=0.2)  +
  theme( strip.text.x = element_text( size = 12, color = "black", face = "bold.italic") ) +
  ylim(c(65,120))+   mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) +
  xlab( "" ) +  ylab( "" ) +   ggtitle(ss)

ss =  ssA[3]
dataSPLp1 = dataSPLp[dataSPLp$site == ss, ]
dataSPLp1 = dataSPLp1[dataSPLp1$variable != "BB", ]
p3 = ggplot(dataSPLp1, aes(as.factor(variable), value2, color = as.factor(quart2) , group = as.factor(quart2)) ) +
  geom_point(size=2) +   geom_line( size=.5) +
  scale_color_grey(start=0.8, end=0.2)  +
  theme( strip.text.x = element_text( size = 12, color = "black", face = "bold.italic") ) +
  ylim(c(65,120))+   mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) +
  xlab( "" ) +  ylab( "" ) +   ggtitle(ss)

ss =  ssA[4]
dataSPLp1 = dataSPLp[dataSPLp$site == ss, ]
dataSPLp1 = dataSPLp1[dataSPLp1$variable != "BB", ]
p4 = ggplot(dataSPLp1, aes(as.factor(variable), value2, color = as.factor(quart2) , group = as.factor(quart2)) ) +
  geom_point(size=2) +   geom_line( size=.5) +
  scale_color_grey(start=0.8, end=0.2)  +
  theme( strip.text.x = element_text( size = 12, color = "black", face = "bold.italic") ) +
  ylim(c(65,120))+   mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) +
  xlab( "" ) +  ylab( "" ) +   ggtitle(ss)

ss =  ssA[5]
dataSPLp1 = dataSPLp[dataSPLp$site == ss, ]
dataSPLp1 = dataSPLp1[dataSPLp1$variable != "BB", ]
p5 = ggplot(dataSPLp1, aes(as.factor(variable), value2, color = as.factor(quart2) , group = as.factor(quart2)) ) +
  geom_point(size=2) +   geom_line( size=.5) +
  scale_color_grey(start=0.8, end=0.2)  +
  theme( strip.text.x = element_text( size = 12, color = "black", face = "bold.italic") ) +
  ylim(c(65,120))+   mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) +
  xlab( "" ) +  ylab( "" ) +   ggtitle(ss)

ss =  ssA[6]
dataSPLp1 = dataSPLp[dataSPLp$site == ss, ]
dataSPLp1 = dataSPLp1[dataSPLp1$variable != "BB", ]
p6 = ggplot(dataSPLp1, aes(as.factor(variable), value2, color = as.factor(quart2) , group = as.factor(quart2)) ) +
  geom_point(size=2) +   geom_line( size=.5) +
  scale_color_grey(start=0.8, end=0.2)  +
  theme( strip.text.x = element_text( size = 12, color = "black", face = "bold.italic") ) +
  ylim(c(65,120))+   mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) +
  xlab( "" ) +  ylab( "" ) +   ggtitle(ss)

ss =  ssA[7]
dataSPLp1 = dataSPLp[dataSPLp$site == ss, ]
dataSPLp1 = dataSPLp1[dataSPLp1$variable != "BB", ]
p7 = ggplot(dataSPLp1, aes(as.factor(variable), value2, color = as.factor(quart2) , group = as.factor(quart2)) ) +
  geom_point(size=2) +   geom_line( size=.5) +
  scale_color_grey(start=0.8, end=0.2)  +
  theme( strip.text.x = element_text( size = 12, color = "black", face = "bold.italic") ) +
  ylim(c(65,120))+   mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) +
  xlab( "" ) +  ylab( "" ) +   ggtitle(ss)

ss =  ssA[8]
dataSPLp1 = dataSPLp[dataSPLp$site == ss, ]
dataSPLp1 = dataSPLp1[dataSPLp1$variable != "BB", ]
p8 = ggplot(dataSPLp1, aes(as.factor(variable), value2, color = as.factor(quart2) , group = as.factor(quart2)) ) +
  geom_point(size=2) +   geom_line( size=.5) +
  scale_color_grey(start=0.8, end=0.2)  +
  theme( strip.text.x = element_text( size = 12, color = "black", face = "bold.italic") ) +
  ylim(c(65,120))+   mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) +
  xlab( "" ) +  ylab( "" ) +   ggtitle(ss)

ss =  ssA[9]
dataSPLp1 = dataSPLp[dataSPLp$site == ss, ]
dataSPLp1 = dataSPLp1[dataSPLp1$variable != "BB", ]
p9 = ggplot(dataSPLp1, aes(as.factor(variable), value2, color = as.factor(quart2) , group = as.factor(quart2)) ) +
  geom_point(size=2) +   geom_line( size=.5) +
  scale_color_grey(start=0.8, end=0.2)  +
  theme( strip.text.x = element_text( size = 12, color = "black", face = "bold.italic") ) +
  ylim(c(65,120))+   mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) +
  xlab( "" ) +  ylab( "" ) +   ggtitle(ss)

#BB as box plots
dataSPL = dataSPL[dataSPL$Site!="FK04",]
dataSPL$Site2 = factor(dataSPL$Site ,levels=c("HI01", "FK02", "CI01", "SB01","OC01","OC02","GR01", "MB01", "MB02"))
dataSPL$YR = year( dataSPL$Day )
pBB = ggplot(dataSPL, aes(x= (as.character(YR)), y = BB_20.24000, fill = Site2) ) +
  geom_boxplot(outlier.size = .2, outlier.color = "gray" ) +
  mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) +
  xlab( "") +   ylab( "") + 
  ggtitle("")
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,nrow = 3, ncol = 3)
pBB

#BB new data: 125-16000 Hz to match OL graphics
dirBB = "E:\\RESEARCH\\SanctSound\\data\\SPL_updatedBB"
nFiles = length( list.files(dirBB))
analysisPeriods = read.csv("E:\\RESEARCH\\SanctSound\\data\\AnalysisPeriods.csv")
BBv2 = NULL
for (ii in 1:nFiles ){
  fname = list.files(dirBB,full.names = F)[ii]
  tmp = read.csv(list.files(dirBB,full.names = T)[ii])
  
  #format, add lables, turncate,  append
  tmp$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" )
  tmp$Day   = as.Date(tmp$DateF)
  tmp$Site  = sapply (strsplit( fname, "_" ),"[",2 )
  tmp$Deply = sapply (strsplit( fname, "_" ),"[",3 )
  colnames(tmp)[2] = "BB_88-22720" 
  tmpAP = as.data.frame( analysisPeriods[analysisPeriods$Site == tmp$Site[1],] )
  tmp2  = tmp[ (tmp$Day >= as.Date(as.character( tmpAP$DateStart),format = "%m/%d/%Y" ) & tmp$Day  <= as.Date(as.character( tmpAP$DateEnd),format = "%m/%d/%Y" )  ),]
  BBv2 = rbind(BBv2, tmp2[2:6])
}
unique(BBv2$Site)

BBv2$Site2 = factor(BBv2$Site ,levels=c("HI01", "FK02", "CI01", "SB01","OC01","OC02","GR01", "MB01", "MB02"))
BBv2$YR = year( BBv2$Day )
pBB2 = ggplot(BBv2, aes(x= (as.character(YR)), y = `BB_88-22720`, fill = Site2) ) +
  geom_boxplot(outlier.size = .2, outlier.color = "gray" ) +
  mytheme +
  #coord_cartesian(ylim = c(70, 130)) +
  theme(legend.position = 'none', axis.text.x = element_blank() ) +
  xlab( "") +   ylab( "") + 
  ggtitle("")
# coord_cartesian(ylim = c(70, 120)) 
pBB
pBB2

#for web story...
BBv2w = BBv2[BBv2$Site =="SB01" | BBv2$Site =="OC01" | BBv2$Site =="GR01", ]
BBv2w$Site2 = factor(BBv2w$Site ,levels=c("SB01","OC01","GR01"))
BBv2w$YR = year( BBv2w$Day )
pBB2web = ggplot(BBv2w, aes(x= (as.character(YR)), y = `BB_88-22720`, fill = Site2 ) ) +
  geom_boxplot(outlier.size = .2, outlier.color = "gray" ) +
  mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) +
  xlab( "") +   ylab( expression(paste("Sound Levels dB re: 1", mu, "Pa") ) ) + 
  ggtitle("") +
  annotate(geom="text", x=1, y=125, label="Olympic Coast",
           color="black",fontface="bold") +
  annotate(geom="text", x=.75, y=125, label="Stellwagon",
         color="black",fontface="bold") +
  annotate(geom="text", x=1.25, y=125, label="Gray's Reef",
           color="black",fontface="bold")

pBB2web


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#SBO1- bio does not contribute to SPLs in ships dominate SPL levels, adapted from combineFiles_SantSound_saveOut_SB.R
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
dirSPLdets ="E:\\RESEARCH\\SanctSound\\data2\\combineFiles_dets"
FilesSB = list.files(path=dirSPLdets,pattern  = "SB01_FrontiersDataDay", full.names=TRUE, recursive = FALSE)
DayTrunData = read.csv(FilesSB)

#make columns numeric
as.data.frame(colnames( DayTrunData))
g =   as.numeric(grep("P", colnames(DayTrunData) ) ) #columns with presence metrics
DayTrunData$bluewhaleP  = as.numeric( DayTrunData$bluewhaleP )
DayTrunData$finwhaleP   = as.numeric( DayTrunData$finwhaleP )
DayTrunData$humpbackwhaleP = as.numeric( DayTrunData$humpbackwhaleP )
DayTrunData$northatlanticrightwhaleP = as.numeric( DayTrunData$northatlanticrightwhaleP )
DayTrunData$seiwhaleP = as.numeric( DayTrunData$seiwhaleP )
DayTrunData$atlanticcodP = as.numeric( DayTrunData$atlanticcodP )

#summarize bioloical
DayTrunData$BioTotal = rowSums(DayTrunData[,34:38]) + DayTrunData[,41]
#check sources present:
DayTrunData[DayTrunData$BioTotal==3,]
#combine just 125, AIS, WSPD, BIO
DayTrunDataT = ( DayTrunData[c(2, 19,12,13,27,44,16)] )
as.data.frame(colnames( DayTrunDataT))
#rescale so values between 0-1 for AIS, WSPD, BIO
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
DayTrunDataT$LOA_ALL_UVs   =  range01(DayTrunDataT$LOA_ALL_UV) 
rgA = c( min( DayTrunDataT$LOA_ALL_UV ) ,max( DayTrunDataT$LOA_ALL_UV ) )
DayTrunDataT$LOA_ALL_OPHRSs   =  range01(DayTrunDataT$LOA_ALL_OPHRS) 
DayTrunDataT$BioTotals   =  range01(DayTrunDataT$BioTotal) 
rgB = c( min( DayTrunDataT$BioTotal ) ,max( DayTrunDataT$BioTotal ) )
DayTrunDataT$WSPD_means  =  range01(DayTrunDataT$WSPD_mean) 
rgW = round( c( min( DayTrunDataT$WSPD_mean ) ,max( DayTrunDataT$WSPD_mean ) ))
DayTrunDataT$VessD_Time_PerDays  =  range01(DayTrunDataT$ VessD_Time_PerDay) 
#melt so value for each sound level
DayTrunDataTm = reshape :: melt(DayTrunDataT, id.vars = "mOB_125", 
                                measure.vars = c("LOA_ALL_UVs","BioTotals","WSPD_means") )

#tile plot as tile for each componenet... not easy to interpret...
ggplot(DayTrunDataTm, aes(as.factor(round(mOB_125,digits=2)), variable, fill= (value)) ) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="black") +
  theme(axis.text.x = element_text(angle = 90))+
  labs(fill = "scaled value") +
  xlab(expression (paste ("daily median Sound Pressure Levels dB re: 1", mu, "Pa (125 Hz octave band)"))) +
  ylab("")+
  annotate(geom = "text",x=27,y=3,label = "a",color="red" )+
  annotate(geom = "text",x=30,y=3,label = "b",color="red" )+
  annotate(geom = "text",x=31,y=3,label = "c",color="red" )+
  annotate(geom = "text",x=9,y=3,label = "d",color="red" )+
  scale_y_discrete(labels = c( paste("AIS", "\n", "# vessels", "\n ", rgA[1],"-", rgA[2]),
                               paste("Biological", "\n", "presence", "\n ", rgB[1],"-", rgB[2] ),
                               paste("Wind", "\n", "Speed", "\n ", rgW[1],"-", rgW[2] ) ) )

mytheme <- theme(panel.spacing=unit(2,"lines"),legend.position="right",axis.text.x = element_text(colour="black",size=14), 
                 axis.text.y=element_text(size=14,colour="black"),axis.title.x=element_text(size=16,colour="black"), 
                 axis.title.y=element_text(size=16,colour="black"), axis.line = element_line(colour="black",size=1), 
                 panel.background=element_rect(fill= "transparent",colour =NA),
                 legend.background= element_rect(fill="transparent",colour=NA),
                 axis.line.x = element_line(colour="black"),axis.line.y=element_line(colour="black"))
#line plots
ggplot(DayTrunDataTm,  aes(value, (round(mOB_125,digits=1)), color= (variable)) ) + 
  geom_point(alpha = .4) +
  geom_smooth(method = "lm", se=F) +
  ylab("Sound source presence (scaled values)")+
  labs(color = "Sound source") +
  scale_color_hue(labels = c(paste("AIS vessels (range = ", rgA[1],"-", rgA[2],")"), 
                             paste("Biological  (range = ", rgB[1],"-", rgB[2],")"),
                             paste("Wind speed [m/s] (range = ", rgW[1],"-", rgW[2] ,")")) ) +
  xlab(expression (paste("Daily median band sound pressure levels dB re: 1", mu, "Pa (125 Hz octave band)")))+
  mytheme+
  theme(legend.position = c(0.2, 0.8), legend.title = element_text(colour="black", size=10,  face="bold"))
                                                                   
#lines look suspicious without error or rsquared
##Include rsquared values- NO not significant                                                                
inMod = DayTrunDataTm[ DayTrunDataTm$variable == "LOA_ALL_UVs",]
lm.out = lm(mOB_125 ~ value, inMod)
summary(lm.out)$adj.r.squared

inMod = DayTrunDataTm[ DayTrunDataTm$variable == "BioTotals",]
lm.out =lm(mOB_125 ~ value, inMod)
summary(lm.out)$adj.r.squared

inMod = DayTrunDataTm[ DayTrunDataTm$variable == "WSPD_means",]
lm.out =lm(mOB_125 ~ value, inMod)
summary(lm.out)$adj.r.squared

#all variables 
lm.out = lm(mOB_125 ~ LOA_ALL_UVs + BioTotals + WSPD_means, data = DayTrunDataT)
summary(lm.out)

#Historgrams as histograms- NO
ggplot( DayTrunDataTm, aes(x=(round(mOB_125,digits=1)), fill=value)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  facet_wrap(~variable)

#Bars
ggplot( DayTrunDataTm, aes( y=value,x=mOB_125 ) ) +
  geom_bar( stat="identity") +
  facet_wrap(~variable)

#Points
ggplot( DayTrunDataTm, aes( y=value,x=mOB_125 ) ) +
  geom_point() +
  facet_wrap(~variable) +
  ylab("Relative presence of source") +
  xlab("Sound level") +
  mytheme

#use unscaled values- FINAL plot SB01
DayTrunDataT$mOB_1252 = DayTrunDataT$mOB_125 -4
p1 = ggplot( DayTrunDataT, aes( x=LOA_ALL_UV,y=mOB_1252 ) ) +
  geom_smooth(method = lm, colour="black",size=0.5, fill = "light gray", alpha = 0.2) +
  geom_point() +
  ylab(expression (paste ("Sound Pressure Levels dB re: 1 ", mu, "Pa")) ) +
  xlab("AIS vessels") +
  mytheme
p2 = ggplot( DayTrunDataT, aes( x=WSPD_mean,y=mOB_1252 ) ) +
  geom_smooth(method = lm, colour="black",size=0.5, fill = "light gray", alpha = 0.2) +
  geom_point() +
  ylab("") +
  xlab("Windspeed [m/s]") +
  mytheme
p3 = ggplot( DayTrunDataT, aes( x=BioTotal,y=mOB_1252 ) ) +
  geom_smooth(method = lm, colour="black",size=0.5, fill = "light gray", alpha = 0.2) +
  geom_point() +
  ylab("") +
  xlab("Biologic sources") +
  mytheme
library(gridExtra)
grid.arrange(p3,p1,p2,nrow=1)
dev.copy2pdf(file="E:\\RESEARCH\\SanctSound\\analysis\\frontiers\\graphs\\test.pdf",out.type="cairo", width=15, height=5)

ggsave(filename = "E:\\RESEARCH\\SanctSound\\analysis\\frontiers\\graphstest.pdf",pOut, dpi=300,device = cairo_eps)
cairo_ps(filename = "E:\\RESEARCH\\SanctSound\\analysis\\frontiers\\graphstest.eps",
         width = 7, height = 7, pointsize = 12,
         fallback_resolution = 300)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#MB comparison- 
# PURPOSE: small spatial scales big differences, adapted from combineFiles_exampleMB01.R
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
setwd("E:\\RESEARCH\\SanctSound\\data2\\combineFiles")
dateInHrMB01 = read.csv("MB01_FrontiersDataHR_2019-03-01_2019-03-31_v2021-01-07.csv")
dateInHrMB02 = read.csv("MB02_FrontiersDataHR_2019-03-01_2019-03-31_v2021-01-07.csv")

#data input modifications
dataIn = rbind(dateInHrMB01,dateInHrMB02)
dataIn$OL_500
dataIn$DayHr
dataIn$Site
hist( dataIn$avgWSPD ) 
dataIn$totalTime
library(scales)
#-----------------------------------------------------------------------------------------
#PLOT 1A: daily difference in sound levels (box plot),  AIS all as bars, hr regional windspeed
dataIn = dataIn[as.Date( dataIn$Day) <= as.Date("2019-03-28"), ]
dataIn102m = reshape::melt(dataIn, id.vars = c("Day","Site"), measure.vars = c("OL_500"))

#SPL  axis.text.x = element_blank(),
pSPL = ggplot(data=dataIn102m) +
  geom_boxplot(aes(x=(Day), y=value, fill=Site))+
  ylab ("") +   xlab("")+
  scale_fill_grey() +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



dataIn102mA = reshape::melt(dataIn, id.vars = c("Day","Site"), measure.vars = c("LOA_ALL_UV"))
pAIS = ggplot(data=dataIn102mA, aes(x=as.Date(Day), y=value, fill=Site)) +
  geom_bar(stat = "identity", position=position_dodge(),width=0.5) +
  ylab ("") +   xlab("")+
  scale_fill_grey() +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank() )
        
dataInW = dataIn[ dataIn$Site == "MB02", ]
daWSPD = as.data.frame( aggregate(dataInW["avgWSPD"],by=dataInW["Day"],function(x) mean(x,na.rm=T)) )
dataInW$DayHrF = as.POSIXct(dataInW$DayHr, format = "%Y-%m-%d %H:%M:%S")
dataInW = dataInW[as.Date( dataInW$Day) <= as.Date("2019-03-28"), ]
pWind = ggplot(data=dataInW, aes(x=DayHrF, y=avgWSPD,group=1) ) +
  geom_line(color="grey",size = 1)+
  scale_x_datetime(limits = lims, date_breaks = "1 day", labels = date_format("%m/%d")) + 
  ylab("") +   xlab("") + scale_y_continuous(position = "left") +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1) ) 

grid.arrange(pSPL,pAIS, pWind,nrow=3) #not aligned... export as separate files
pSPL
pAIS
pWind


weekdays(as.Date(daWSPD$Day))


#PLOT 1: SPL vs Date- similar b/c same regional windspeed at both sites?
#-----------------------------------------------------------------------------------------
ggplot(dataIn,aes(as.Date(dataIn$DayHr), OL_500, color = Site) )+
  geom_point()

#PLOT2: SPL vs wind by site-- not that helpful
pWind = ggplot(dataIn, aes( y = OL_500, x = avgWSPD) )+ #, color = Site
  geom_point(alpha = .5)+
  geom_smooth(method="lm")+
  xlab(expression (paste ("Sound Pressure Levels dB re: 1", mu, "Pa (125 Hz octave band)") ))
  xlab("average houly wind speed (m/s)")+
  facet_wrap(~Site) 
pWind         

#SPL vs vessel detection
dataIn2 = dataIn[dataIn$totalTime > 0,]
dataIn$OL_5002 = dataIn$OL_500 -4
pVess = ggplot(dataIn2, aes(y = OL_500, x = totalTime/(60)/(60) ) )+
  geom_smooth (method = lm, colour="black", size=0.5)+
  geom_point()+
  xlab("Prop of hour with vessel noise")+
  ylab(expression (paste ("Sound Pressure Levels dB re: 1", mu, " Pa (500 Hz)") )) +
  facet_wrap(~Site) +
  mytheme
pVess
inMod = dataIn2[ dataIn2$Site == "MB01",]
lm.out = lm(OL_500 ~ totalTime, inMod)
summary(lm.out)$adj.r.squared
lm.out = lm(OL_500 ~ totalTime + WSPD_means, data = inMod)
summary(lm.out)


inMod = dataIn2[ dataIn2$Site == "MB02",]
lm.out = lm(OL_500 ~ totalTime, inMod)
summary(lm.out)$adj.r.squared
lm.out = lm(OL_500 ~ totalTime + WSPD_means, data = inMod)
summary(lm.out)


#histograms of SPL and vessel detection-- does not show difference
ggplot( dataIn2, aes(x=OL_500, fill=Site)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  mytheme
ggplot( dataIn2, aes(x=totalTime/(60)/(60), fill=Site)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  mytheme


grid.arrange(pWind,pVess,ncol=1,nrow =2)
ddply(dataIn, .(Site), summarize, xm=median(OL_500))

#paired box plots: compare MB01 v MB02 at wind speeds less than 10 kts/5 mps
dataIn10  = dataIn[dataIn$avgWSPD <= 5 ,]
dataIn102 = dataIn10[!is.na(dataIn10$Day),]
dataIn102m = reshape::melt(dataIn102, id.vars = c("Day","Site"), measure.vars = c("OL_500"))
dataIn102m$DayF = as.Date(dataIn102m$Day)
ggplot(data=dataIn102m) +
  geom_boxplot(aes(x=(Day), y=value, fill=Site))+
  ylab (expression(paste("Band sound pressure level dB re 1", mu, Pa, " (500 Hz OB)") ))+
  xlab("")+
  mytheme 
  #scale_x_date(date_breaks = "1 day", labels=date_format("%b-%d") )+
  #theme( axis.text.x = element_text(angle=45, hjust = 1))  

#paired box plots: compare MB01 v MB02 with windspeed as line-- not great
dataIn102m = reshape::melt(dataIn, id.vars = c("Day","Site"), measure.vars = c("OL_500"))
pSPL = ggplot(data=dataIn102m) +
  geom_boxplot(aes(x=(Day), y=value, fill=Site))+
  ylab ("")+
  xlab("")+
  mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) 
  
dataInW = dataIn[ dataIn$Site == "MB01", ]
daWSPD = as.data.frame( aggregate(dataInW["avgWSPD"],by=dataInW["Day"],function(x) mean(x,na.rm=T)) )
pWind = ggplot(data=daWSPD, aes(x=Day, y=avgWSPD,group=1) ) +
  #geom_boxplot(data=dataIn, aes(x=Day, y=avgWSPD), notch=TRUE, outlier.size=.5, fill="gray") +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="black", size=2)+
  ylab("")+
  xlab("")+
  mytheme +
  theme(legend.position = 'none', axis.text.x = element_blank() ) 

grid.arrange(pSPL,pWind,nrow=2)
weekdays(as.Date(daWSPD$Day))
  

