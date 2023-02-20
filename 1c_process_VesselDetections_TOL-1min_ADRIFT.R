# process ADRIFT vessel detection data ####

# modified 1b-- wanted sound levels only during vessel detections "noise added" 
# using TOLs at 1 minute resolution

rm(list=ls())

# LOAD Libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(scales)
library(BBmisc)
library(zoo)

# SETUP parameters ####
site = "ADRIFT017"
output  = NULL
tDir   = paste0( "F:\\RESEARCH\\ADRIFT\\data\\", site, "\\")
DC = Sys.Date()

# READ IN-- vessel detection ####
nFilesVD  = length( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVDF = ( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD = basename(inFilesVDF)
sant = sapply(strsplit(inFilesVD[1], "_"), "[[", 2)
depl = sapply(strsplit(inFilesVD, "_"), "[[", 3)
VD=NULL

for (ii in 1:(nFilesVD)) {
  tmp = read.csv(inFilesVDF[ii])
  # names(tmp)
  tmp$Sant = sant
  tmp$Dep  = depl[ii]
  colnames(tmp) = c("ISOStartTime","ISOEndTime","StartTime","EndTime", "Label","Sant","Depl" )
  
  VD = rbind(VD,tmp)
}
VD$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD$ISOStartTime)), tz = "GMT" )
VD$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD$ISOEndTime)), tz = "GMT" )
VD$Mth = month(VD$Start )
VD$Yr  = year(VD$Start )
VD$Dur = as.numeric(as.character( difftime(VD$End, VD$Start, units = "secs" )) )
VD$Dep  = as.numeric(as.character(VD$Dep))
VD$DepC = c(0,diff(VD$Dep))
indx = which(VD$DepC == 1) # find transitions in deployments
(rm(tmp))

# ADD time stamps between detections ####
VDall = NULL
for (ii in 1:(nrow(VD) -1 ) )  {
  
  if (VD$DepC [ii+1] == 0) {
    tpL = "ambient2"
    tpS = VD$End [ii] + 1
    tpE = VD$Start [ii+1] - 1
    tpD = as.numeric( difftime(tpE, tpS, units = "secs") )
    
    #recombine and build new matrix
    r1 =  cbind.data.frame( VD$Sant[ii], VD$Dep[ii], as.POSIXct(VD$Start[ii]), as.POSIXct(VD$End[ii]), VD$Label[ii], (VD$Dur[ii]) ) 
    colnames(r1)= c("Sanctuary", "Deployment", "Start", "End","Label", "DurS")
    
    r2 =  cbind.data.frame( VD$Sant[ii], VD$Dep[ii], tpS, tpE, tpL, tpD) 
    colnames(r2)= c("Sanctuary", "Deployment", "Start", "End","Label", "DurS")
    
    VDall =  rbind(VDall , rbind.data.frame(r1,r2) )
    
  } else {
    r1 =  cbind.data.frame( VD$Sant[ii], VD$Dep[ii], as.POSIXct(VD$Start[ii]), as.POSIXct(VD$End[ii]), VD$Label[ii], (VD$Dur[ii])) 
    colnames(r1)= c("Sanctuary", "Deployment", "Start", "End","Label", "DurS")
    VDall =  rbind(VDall , r1) 
    
  }
  
}

VDall$Mth = month(VDall$Start)
VDall$Yr = year(VDall$Start)
VDall$Hr = hour(VDall$Start)

VDallt = VDall[VDall$DurS < (3600*6),]
rm(VDallt, r1, r2, VD)
VDall$startDay = as.Date(VDall$Start)

# READ IN-- TOLs ####
nFilesPSD   = length( list.files(path=tDir, pattern = "_2min", full.names=TRUE, recursive = TRUE))
inFilesPSDF = ( list.files(path=tDir, pattern = "TOL_mean_2min", full.names=TRUE, recursive = TRUE))
inFilesPSD  = basename(inFilesPSDF)
#combine all TOLs together
TOLmin = NULL
for (ii in 1:(length(inFilesPSDF)) )  {
  tmpPSD = read.csv(inFilesPSDF[ii]) #this takes a while to load...
  TOLmin = rbind( TOLmin, tmpPSD)
}
TOLmin$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", TOLmin$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" ) 

# ADD TOL TO DETECION PERIODS ####
output = NULL
# For each period- find corresponding sound levels and take the median-- takes a bit
for (ii in 1:nrow(VDall)){
  tmp = as.data.frame( TOLmin[ TOLmin$DateF >= VDall$Start [ii] & TOLmin$DateF <= VDall$End [ii] ,] )
 colnames(tmp)
  
  tmpMax =  ( apply( tmp[, c(2:24)], 2, max) )
  tmpQuant = as.data.frame( apply(tmp[,c(2:24)],2,quantile) )
  tmpMed = tmpQuant[3,]
  
  tmpo = cbind(tmpMax[1], tmpMed[1])
  colnames(tmpo) = c("TOL_125 max", "TOL_125 median")
  tmpo = cbind(VDall[ii,], tmpo)
  
  output = rbind(output,tmpo)
}
ouputSave = output 

(rm(tmp, tmpMed,tmpo, tmpQuant))

as.data.frame(colnames(output))
min( as.Date(output$Start ) )
max( as.Date(output$Start ) )
difftime(  max( (output$Start ) ), min(output$Start ) )

# PLOTS ####
## METRIC: Median Sound Levels during different categories of acoustic conditions ####
ggplot(output, aes(x=Label, y=`TOL_125 median`, fill = as.factor(Deployment))  )+
  geom_boxplot() +
  labs(title = "Median sound levels in different periods (125 Hz TOL)" )+
   xlab("")+  ylab("")+   theme_minimal()+   theme(axis.text.x=element_text(angle=30,hjust=1)) 

## METRIC: Timeline of acoustic conditions ####
library("viridis")  
ggplot(output, aes(x=Start, xend=End, y=Label, yend=Label, color=`TOL_125 median`)) +
  geom_segment()+
  theme_bw()+ #use ggplot theme with black gridlines and white background
  geom_segment(size=8) +
  xlab("")+  ylab("")+ 
  scale_color_gradientn(colours = viridis(10))+
   guides(fill=guide_legend(title="Participation")) #increase line width of segments in the chart

## METRIC: Pie chart of time comparison for categories ####
#Total samples
sumBaseline = c( sum(output$Label == "ambient"),  
                 sum(output$Label == "ambient2"),
                 sum(output$Label == "ship")) 
#Total time
sumTIMEBaseline = c( sum (output$DurS[output$Label == "ambient"] ),  
                     sum (output$DurS[output$Label == "ambient2"]),
                     sum (output$DurS[output$Label == "ship"] ) )
dtTime = data.table(X = c(1), Y = c(1), Uships = c(33), Names = c(sant), 
                    types = rbind(sumTIMEBaseline ) )
dtTime = as.data.frame(dtTime)
colnames(dtTime) = c("X", "Y", "UniqueShips", "Period",  "FalseVessel", "Ambient", "Vessel" )

library(scatterpie)
ggplot() +
  geom_scatterpie( aes(x=X, y=Y, r=UniqueShips, group=Period), data=dtTime,
                   cols=c( "FalseVessel", "Ambient", "Vessel") )  + 
  coord_equal()+
  geom_text(data = dt, aes(x=X+20, y=Y-34,label = paste0("Total days = ", floor( sum(output$DurS)/(60*60*24) )) ), size = 5)  +
  ylab("") + xlab("") + ggtitle("Durations of Acoustic Categories") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## METRIC: ECDF of sound levels during different categories of acoustic conditions ####
ggplot(output, aes(`TOL_125 max`,  color = Label ) ) + 
  stat_ecdf(geom = "step") +
  xlab("")+  ylab("")+
  # labs(title = "Sound levels (125 Hz TOL)",
  #      caption = paste0("FALSE = ",   sum (output$DurS[output$Label == "ambient"]/3600) ,"\n",
  #                       "NO = ",   sum (output$DurS[output$Label == "ambient2"]/3600) , "\n",
  #                       "YES = ",  sum (output$DurS[output$Label == "ship"]/3600) ) ) +
  theme_minimal()


## METRIC: Noise Above ####
outputAB = output[ output$Label != "ship",] # (nrow(outputVD))
outputVD = output[ output$Label == "ship",] # (nrow(outputAB))

output$SNR = NA
output$SNRmax = NA
output$SNRtime = NA
for (ii in 1:nrow(outputVD)) {
  #find the closest (in time) ambient sample
  idx = which.min(abs(outputVD$Start[ii] - outputAB$Start))
  signalMed = outputVD$`TOL_125 median`[ii]
  noiseMed  = outputAB$`TOL_125 median`[idx]
  outputVD$SNR[ii]   = signalMed - noiseMed # signal - noise
  outputVD$SNRtime[ii]= as.numeric( difftime(outputAB$Start[idx], outputVD$Start[ii], units = "mins") )
  
  signalMed = outputVD$`TOL_125 max`[ii]
  noiseMed  = outputAB$`TOL_125 max`[idx]
  outputVD$SNRmax[ii]= signalMed - noiseMed # signal - noise
}

outputVD = outputVD[outputVD$SNRtime < 24*60, ] # only samples when ambient is < 24 hours away
ggplot(outputVD, aes(SNRmax,  color = Label ) ) + 
  stat_ecdf(geom = "step")+
  xlab("")+  ylab("") +
  theme_minimal()

## METRIC: Spectral comparisons ####
idxC = which( output$Label == "ship" )[1] # just look at first one
output[idxC,]
tmp = TOLmin[ TOLmin$DateF >= output$Start [idxC] & TOLmin$DateF <= output$End [idxC] , ] 
tmpM = reshape2 :: melt(tmp, id.vars = "yyyy.mm.ddTHH.MM.SSZ", measure.vars =  colnames(tmp)[2:14]) 
ggplot(tmpM, aes(variable, value))+
  geom_boxplot() +
  xlab("") + ylab("")+
  ylim(c(75,130)) +
  ggtitle(paste0("Samples: ",  nrow(tmp)," minutes" ) ) +
  theme_minimal()

outputTOL = NULL
for (ii in 1:nrow(VDall)){
  tmp = as.data.frame( TOLmin[ TOLmin$DateF >= VDall$Start [ii] & TOLmin$DateF <= VDall$End [ii] ,] )
  colnames(tmp)
  
  tmpMax =  ( apply( tmp[, c(2:24)], 2, max) )
  tmpQuant = as.data.frame( apply(tmp[,c(2:24)],2,quantile) )
  tmpMed = tmpQuant[3,]
  
  tmpo = cbind(tmpMax[1], tmpMed[1])
  colnames(tmpo) = c("TOL_125 max", "TOL_125 median")
  tmpo = cbind(VDall[ii,], tmpo)
  
  outputTOL = rbind(outputTOL,tmpo)
}
ouputSave = output 
