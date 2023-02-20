# process sanctuary sound vessel detection data and AIS results

#modified 1b-- wanted sound levels only during vessel detections "noise added" 
# using TOLs at 1 minute resolution

#tested for OC02 speed reduction
rm(list=ls())

#-----------------------------------------------------------------------------------------
# MAIN functions-- by site processing
#-----------------------------------------------------------------------------------------
# Reads in both vessel detection 

#outputs 
# a per site .csv file: E:\RESEARCH\SanctSound\data2\combineFiles1_SPLShips
# summary table for average of all data per site (copied here):
# https://docs.google.com/spreadsheets/d/1dw9Vy0dXKW3Q6Ter3t19cmLf_5EJWbmL9nqJsCrNXQE/edit#gid=888829761

#-----------------------------------------------------------------------------------------
# TO ADD (future versions)
#-----------------------------------------------------------------------------------------

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

#-----------------------------------------------------------------------------------------
# SETUP parameters
#-----------------------------------------------------------------------------------------
ra = 7 #days for running average
range01 = function(x){(x-min(x))/(max(x)-min(x))} #normalize between 0-1
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
yearflag = 2019
site = "OC02"

# range of dates for output graphics
eDatePlot = '2020-12-31'
sDatePlot = '2018-11-01'

# some analysis and output flags 
flagCSV  = TRUE  #true if you want to output hourly and daily csv files per site
flagPLT  = FALSE  #true if you want to output summary plots of data
flagYear = TRUE   #true writes out a csv of yearly data to be copied into google docs

# OUTPUT details 
#!!!(FUTURE: create site loop here) !!!
# cat("Processing site...", sitesVD[ss], ":",ss, "of", length(sitesVD),"\n")
output  = NULL
tDir   = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site, "\\")
outDir = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site, "\\")
DC = Sys.Date()

slowStart     = as.Date("2021-06-01")
slowEnd       = as.Date("2021-10-31")
slowStartTrim = as.Date("2021-07-01")
slowEndTrim   = as.Date("2021-08-01")
baseStart     = as.Date("2019-07-12")
baseEnd       = as.Date("2019-08-01")

#-----------------------------------------------------------------------------------------
# READ IN-- vessel detection ####
#-----------------------------------------------------------------------------------------
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
  colnames(tmp) = c("ISOStartTime","ISOEndTime","Label","Sant","Depl" )
  
  VD = rbind(VD,tmp)
}
VD$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD$ISOStartTime)), tz = "GMT" )
VD$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD$ISOEndTime)), tz = "GMT" )
VD$Mth = month(VD$Start )
VD$Yr  = year(VD$Start )
VD$Dur = as.numeric(as.character( difftime(VD$End, VD$Start, units = "secs" )) )

VD$Dep  = as.numeric(as.character(VD$Dep))
VD$DepC = c(0,diff(VD$Dep))
indx = which(VD$DepC == 1) #find transitions in deployments

#vessel detections greater than 12 hour!!!
VD[VD$Dur > 3600*12,]
VD$DurH = VD$Dur/3600 

# VESSEL DETECTION DURATIONS- summary
#-----------------------------------------------------------------------------------------
VDmean = aggregate(VD$Dur,    by=list(VD$Mth,VD$Yr), mean, na.rm=T) 
VDsd =  aggregate(VD$Dur,    by=list(VD$Mth,VD$Yr), sd, na.rm=T) 
VDagg = cbind(VDmean, VDsd$x)
ggplot(VD, aes(x=as.factor(Mth), y=(DurH), fill = as.factor(Yr), color = as.factor(Yr) )  )+
  geom_boxplot() +
  theme_minimal()
colnames(VD)

# ADD time stamps between detections... faster way??
#-----------------------------------------------------------------------------------------
VDall = NULL
for (ii in 1:(nrow(VD) -1 ) )  {
  
  if (VD$DepC [ii+1] == 0) {
    tpL = "ambient"
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


# IS there a difference in durations of vessel vs non vessel periods (all data)
#-----------------------------------------------------------------------------------------
VDallt = VDall[VDall$DurS < (3600*6),]
ggplot(VDallt, aes(x=DurS, color = Label )  )+
  geom_boxplot()+
  ggtitle ("Is there a difference in durations of vessel vs non vessel periods?")+
  theme_minimal()

# vessel durations are very similar at this site- with longer durations of non-vessel periods
# how does this breakdown by slowdowns?
VDall$Mth = month(VDall$Start)
VDall$Yr = year(VDall$Start)
VDall$Hr = hour(VDall$Start)

#interpretation-- means not ideal
sum1 = aggregate(VDall$DurS,    by=list(VDall$Mth, VDall$Yr, VDall$Label), mean, na.rm=T) 
# expect longer duration of vessel noise in slower period- but maybe not bc lower source level/ shorter detection range
sum12019 = sum1[ sum1$Group.2 == "2019",]

VDallt = VDall[VDall$DurS < (3600*6),]
ggplot(VDallt, aes(x=as.factor(Mth), y=(DurS)/3600, fill = as.factor(Yr), color = as.factor(Yr) )  )+
  geom_boxplot() +
  labs(x="",y = "Duration-hours")+
  theme_minimal()
summary(VDall)


# Label vessel detections as in or out of slow down
#-----------------------------------------------------------------------------------------
VDall$startDay = as.Date(VDall$Start)
min(VDall$startDay)

VDall$Period [VDall$startDay > slowStart & VDall$startDay < slowEnd] = "slowdown"
VDall$Period [VDall$startDay > baseStart & VDall$startDay < baseEnd] = "baseline"

#remove all NA data
VDall2 = VDall[!is.na(VDall$Period), ]

#durations of vessel detections for different periods-- durations were longer during the slowdown!
VDall2 = VDall2[(VDall2$Label)== "ship", ] 
ggplot(VDall2, aes(x=as.factor(Mth), y=(DurS)/3600, fill = as.factor(Period), color = as.factor(Period) )  )+
  geom_boxplot() +
  labs(x="",y = "Duration-hours")+
  theme_minimal()
ggplot(VDall2, aes((DurS)/3600,  color = Period ) ) + stat_ecdf(geom = "point") + 
  theme_minimal()

# Truncate to just July...
VDall2July = VDall2[VDall2$Mth == 7,]
# did the vessel noise detection differ in duration?
VDall2JulyVD = VDall2July[VDall2July$Label == "ship",]
mean( VDall2JulyVD$DurS[VDall2JulyVD$Period == "baseline"] ) /60
sd( VDall2JulyVD$DurS[VDall2JulyVD$Period == "baseline"] ) /60

mean( VDall2JulyVD$DurS[VDall2JulyVD$Period == "slowdown"] )/ 60
sd( VDall2JulyVD$DurS[VDall2JulyVD$Period == "slowdown"] )/ 60
# FINDING: durations were longer during the slowdown!

# did  non-vessel noise detections differ in duration ?
VDall2 = VDall[!is.na(VDall$Period), ]
VDall2 = VDall2[(VDall2$Label)== "ambient", ]
VDall2July = VDall2[VDall2$Mth == 7,]

VDall2JulyVD = VDall2July[VDall2July$Label == "ambient",]
mean( VDall2JulyVD$DurS[VDall2JulyVD$Period == "baseline"] ) /60
sd( VDall2JulyVD$DurS[VDall2JulyVD$Period == "baseline"] ) /60
mean( VDall2JulyVD$DurS[VDall2JulyVD$Period == "slowdown"] )/ 60
sd( VDall2JulyVD$DurS[VDall2JulyVD$Period == "slowdown"] )/ 60
# FINDING: durations were longer during the slowdown- not sure why??

#-----------------------------------------------------------------------------------------
# READ IN-- TOLs ####
#-----------------------------------------------------------------------------------------
nFilesPSD   = length( list.files(path=tDir, pattern = "_1min", full.names=TRUE, recursive = TRUE))
inFilesPSDF = ( list.files(path=tDir, pattern = "mean_1min", full.names=TRUE, recursive = TRUE))
inFilesPSD  = basename(inFilesPSDF)
inFilesPSD

#combine all TOLs together
TOLmin = NULL
for (ii in 1:(length(inFilesPSDF)) )  {
  tmpPSD = read.csv(inFilesPSDF[ii]) #this takes a while to load...
  TOLmin = rbind( TOLmin, tmpPSD)
}

TOLmin$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", TOLmin$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" ) 


#-----------------------------------------------------------------------------------------
#  ADD TOL TO DETECION PERIODS ####
#-----------------------------------------------------------------------------------------
# For each period- find corresponding sound levels and take the median-- takes a bit
VDall3 = VDall[!is.na(VDall$Period), ]
VDall3July = VDall3[VDall3$Mth == 7,]
output = NULL
for (ii in 1:nrow(VDall3July)){
  tmp = TOLmin[ TOLmin$DateF >= VDall3July$Start [ii] & TOLmin$DateF <= VDall3July$End [ii] ,] 
  
  tmpMax =  ( apply(tmp[,2:31],2,max) )
  tmpQuant = as.data.frame( apply(tmp[,2:31],2,quantile) )
  tmpMed = tmpQuant[3,]
  
  tmpo = cbind(tmpMax[8], tmpMed[8])
  colnames(tmpo) = c("TOL_125 max", "TOL_125 median")
  tmpo = cbind(VDall3July[ii,], tmpo)
  
  output = rbind(output,tmpo)
}
ouputSave = output 

#-----------------------------------------------------------------------------------------
# ADD AIS data vessel detections and ECHO type labels ####
#-----------------------------------------------------------------------------------------
AIStran   = read.csv("F:\\RESEARCH\\SanctSound\\data2\\OC02\\smp_transit_data.csv") #AIS transit data
AIStranOC = AIStran[ AIStran$loc_id == "OC02",]
AIStranOC$Start = as.POSIXct( gsub("[+]00", "", AIStranOC$start_time_utc), tz = "GMT" ) 
AIStranOC$End   = as.POSIXct( gsub("[+]00", "", AIStranOC$end_time_utc), tz = "GMT" ) 
unique(AIStranOC$type)

AISparticipants = read.csv("G:\\My Drive\\ActiveProjects\\ECHO\\2021 Jul Swiftsure Bank slowdown results.csv") #AIS transit data July 2021 only
# but can use this to get types of vessels.... and filter by time later
unique(AISparticipants$ECHO.Vessel.Class)
AIStranOC$type_echo = NA
AIStranOC$type_jasco = NA
AIStranOC$slowdown = NA
AIStranOC$type_MT = NA
AIStranOC$type_MT = NA
AIStranOC$dateTransit = NA

# combine AIS data tables ####
#-----------------------------------------------------------------------------------------
for (ss in 1:nrow(AIStranOC)) {
  
  tmp =  AISparticipants[ AISparticipants$Vessel.MMSI == AIStran$mmsi[ss], ]
  
  if (nrow(tmp) > 0){
    
    #check date matches
    
    tmp$Date = as.POSIXct(substr( tmp$Local.pacific.time.to.enter.Swiftsure.Bank.slowdown.zone, start = 5, stop = 15 ) , 
                          tz = "America/Los_Angeles",
                          format = ("%d/%b/%Y") )
    
    #if ( as.Date (AIStranOC$Start[ss] ) == tmp$Date ){
    AIStranOC$type_echo[ss]  = tmp$ECHO.Vessel.Class
    AIStranOC$type_jasco[ss] = tmp$JASCO.Vessel.Class
    AIStranOC$slowdown[ss]   = tmp$Did.the.vessel.meet.slowdown.target.
    AIStranOC$type_MT[ss]    = tmp$Marine.Traffic.Vessel.Class
    AIStranOC$dateTransit[ss] = tmp$Local.pacific.time.to.enter.Swiftsure.Bank.slowdown.zone
    cat(ss,"\n")
    
    #}
  }
  
}

# add type of vessels to the vessel detection periods ####
#-----------------------------------------------------------------------------------------
# reset: output = ouputSave
output$AIS = 0
output$BulkCarrier = 0
output$Container = 0
output$Tanker = 0
output$CarCarrier = 0
output$Other = 0
output$Passenger = 0
output$Unk = 0
output$slowdownY = 0
output$slowdownN = 0
output$slowdownUNK = 0 
output$mDist = NA

for (ii in 1: nrow(output) ){
  
  #get AIS vessel transits that occurred in this vessel detection period-- can be multiple given duration of some of detection periods
  tmp = AIStranOC[AIStranOC$Start >= output$Start[ii] & AIStranOC$Start <= output$End[ii],] 
  
  if (  nrow(tmp) > 0 ){
    
    output$AIS[ii]          = nrow(tmp) #total AIS vessels
    output$BulkCarrier[ii]  = length( which(tmp$type_echo == "Bulk Carrier") )
    output$Container[ii]    = length( which(tmp$type_echo == "Container") )
    output$Tanker[ii]       = length( which(tmp$type_echo == "Tanker") )
    output$CarCarrier[ii]   = length( which(tmp$type_echo == "Car Carrier") )
    output$Passenger[ii]    = length( which(tmp$type_echo == "Passenger") )
    output$Other            = length( which(tmp$type_echo == "Other") )
    output$Unk[ii]          = nrow(tmp) -  (output$BulkCarrier[ii] +  output$Container[ii] + output$Tanker[ii] + 
                                              output$CarCarrier[ii] + output$Passenger[ii] + output$Other[ii] )
    output$slowdownY[ii] = length( which(tmp$slowdown == "YES") )
    output$slowdownN[ii] = length( which(tmp$slowdown == "NO") )
    output$slowdownUNK[ii] = sum(is.na(tmp$slowdown) )
    
    output$mDist[ii] = min(tmp$dist_nm)
  }
}

# Assign labels to time periods ####
#-----------------------------------------------------------------------------------------
output$Category[output$AIS > 0]         = "A. AIS vessel (unk)" # has an AIS vessel

output$Category[output$AIS > 0 & output$Tanker > 0 | output$BulkCarrier > 0 & output$Container == 0 & output$CarCarrier == 0 & output$Passenger  == 0]  = "B. 11 knt (bulk,tanker)"
length( which (output$Category == "B. 11 knt (bulk,tanker)") )

output$Category[output$AIS > 0 & output$Container  > 0 | output$CarCarrier > 0 | output$Passenger  > 0 & output$BulkCarrier == 0 & output$Tanker  == 0] = "C. 14.5 knt (cruise,container,vehicle)"
length( which (output$Category == "C. 14.5 knt (cruise,container,vehicle)") )

output$Category[output$AIS == 0 & output$Label == "ship"]    = "D. Non-AIS vessel"
length( which (output$Category == "D. Non-AIS vessel") )

output$Category[output$AIS == 0 & output$Label == "ambient"] = "E. No vessel"
output$Category[output$AIS > 0  & output$Label == "ambient"] ="E. No vessel" #no vessel det, but AIS-- checked and pretty far away so consider non-vessel
length( which (output$Category == "E. No vessel") )

length( which (output$Category == "A. AIS vessel (unk)") ) 

# output$Category[output$AIS > 0  & output$Label == "ambient"] = "F. No nearby AIS" #no vessel det, but AIS
# length( which (output$Category == "F. No nearby AIS") ) 

unique(output$Category)

# METRIC: distribution of sound levels in a specific event ####
#-----------------------------------------------------------------------------------------
# need to go back and grab SPL data for a given time
idxC = which( output$Category == "C. 14.5 knt (cruise,container,vehicle)" )[8] # just look at first one
output$AIS[idxC]
output[idxC,]

tmp = TOLmin[ TOLmin$DateF >= output$Start [idxC] & TOLmin$DateF <= output$End [idxC] ,] 
tmpM = reshape2 :: melt(tmp, id.vars = "yyyy.mm.ddTHH.MM.SSZ", measure.vars =  colnames(tmp)[2:14]) 
p1 = ggplot(tmpM, aes(variable, value))+
  geom_boxplot() +
  xlab("") + ylab("")+
  ylim(c(75,130)) +
  ggtitle(paste0("AIS vessel (1-min SPLs, n=", nrow(tmp)," minutes)" ) ) +
  theme_minimal()
# Ambient distribution
idxA = which( output$Category == "E. No vessel" )[1]
tmp = TOLmin[ TOLmin$DateF >= output$Start [idxA] & TOLmin$DateF <= output$End [idxA] ,] 
tmpM = reshape2 :: melt(tmp, id.vars = "yyyy.mm.ddTHH.MM.SSZ", measure.vars =  colnames(tmp)[2:14]) 
p2 = ggplot(tmpM, aes(variable, value))+
  geom_boxplot() +
  xlab("") + ylab("")+
  ylim(c(75,130)) +
  ggtitle(paste0("No vessel detection (1-min SPLs, n=", nrow(tmp)," minutes)" ) ) +
  theme_minimal()
# Ambient distribution
idxA = which( output$Category == "D. Non-AIS vessel" )[3]
tmp = TOLmin[ TOLmin$DateF >= output$Start [idxA] & TOLmin$DateF <= output$End [idxA] ,] 
tmpM = reshape2 :: melt(tmp, id.vars = "yyyy.mm.ddTHH.MM.SSZ", measure.vars =  colnames(tmp)[2:14]) 
p3 = ggplot(tmpM, aes(variable, value))+
  geom_boxplot() +
  xlab("") + ylab("")+
  ylim(c(75,130)) +
  ggtitle(paste0("Non-AIS vessel detection (1-min SPLs, n=", nrow(tmp)," minutes)" ) ) +
  theme_minimal()

grid.arrange(p1,p3,p2)


# PLOTS COMPARING BASELINE ###
#-----------------------------------------------------------------------------------------
as.data.frame(colnames(output))
outputSD = output[output$Period == "slowdown", ]

# which ships actually slowed down?

outputSD

# PLOTS COMPARING SLOWDOWN AND BASELINE ###
#-----------------------------------------------------------------------------------------
as.data.frame(colnames(output))
output$DurHR = output$DurS/3600

# (1) DURATION DIFFERENCE (not very helpful)
ggplot(output, aes(x=Category, y=DurHR, fill = Period )  )+
  geom_boxplot() +
  labs(title = "Durations of different periods",
       caption = paste0("A = ", sum(output$Category == "A. AIS vessel (unk)") ,"\n",
                        "B = ", sum(output$Category == "B. 11 knt (bulk,tanker)"),"\n",
                        "C = ", sum(output$Category == "C. 14.5 knt (cruise,container,vehicle)"),"\n",
                        "D = ", sum(output$Category == "D. Non-AIS vessel"),"\n",
                        "E = ", sum(output$Category == "E. No vessel"),"\n",
                        "F = ", sum(output$Category == "F. No nearby AIS"),"\n") ) +
  xlab("")+  ylab("Hours")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=30,hjust=1)) 

# (2) Noise Difference
ggplot(output, aes(`TOL_125 max`,  color = Period ) ) + 
  stat_ecdf(geom = "step") + 
  #ggtitle (paste0("Comparison of noise levels of different periods  (n=", nrow(output),")" )) +
  facet_wrap(~Category)+
  labs(title = paste0("Comparision of noise levels of different periods  (n=", nrow(output),")" ),
       caption = paste0("A = ", sum(output$Category == "A. AIS vessel (unk)") ,"\n",
                        "B = ", sum(output$Category == "B. 11 knt (bulk,tanker)"),"\n",
                        "C = ", sum(output$Category == "C. 14.5 knt (cruise,container,vehicle)"),"\n",
                        "D = ", sum(output$Category == "D. Non-AIS vessel"),"\n",
                        "E = ", sum(output$Category == "E. No vessel"),"\n",
                        "F = ", sum(output$Category == "F. No nearby AIS"),"\n") )+
  ylab("")+  xlab("max sound level dB (125 Hz 1/3 octave band)")+
  theme_minimal()

# (3) Pie chart of time comparison for categories
outputBaseline = output[ output$Period == "baseline", ]
sumBaseline = c( sum(outputBaseline$Category == "A. AIS vessel (unk)"),  sum(outputBaseline$Category == "B. 11 knt (bulk,tanker)"),
                 sum(outputBaseline$Category == "C. 14.5 knt (cruise,container,vehicle)"),  sum(outputBaseline$Category == "D. Non-AIS vessel"),
                 sum(outputBaseline$Category == "E. No vessel"), sum(outputBaseline$Category == "F. No nearby AIS")) 
sumTIMEBaseline = c( sum (outputBaseline$DurHR[outputBaseline$Category == "A. AIS vessel (unk)"] ),  
                     sum (outputBaseline$DurHR[outputBaseline$Category == "B. 11 knt (bulk,tanker)"]),
                     sum (outputBaseline$DurHR[outputBaseline$Category == "C. 14.5 knt (cruise,container,vehicle)"]),  
                     sum (outputBaseline$DurHR[outputBaseline$Category == "D. Non-AIS vessel"]) ,
                     sum (outputBaseline$DurHR[outputBaseline$Category == "E. No vessel"]), 
                     sum (outputBaseline$DurHR[outputBaseline$Category == "F. No nearby AIS"] ) ) 

outputBaseline = output[ output$Period == "slowdown", ]
sumSlowdonw = c( sum(outputBaseline$Category == "A. AIS vessel (unk)"),  sum(outputBaseline$Category == "B. 11 knt (bulk,tanker)"),
                 sum(outputBaseline$Category == "C. 14.5 knt (cruise,container,vehicle)"),  sum(outputBaseline$Category == "D. Non-AIS vessel"),
                 sum(outputBaseline$Category == "E. No vessel"), sum(outputBaseline$Category == "F. No nearby AIS")) 
sumTIMESlowdown =  c( sum (outputBaseline$DurHR[outputBaseline$Category == "A. AIS vessel (unk)"] ),  
                      sum (outputBaseline$DurHR[outputBaseline$Category == "B. 11 knt (bulk,tanker)"]),
                      sum (outputBaseline$DurHR[outputBaseline$Category == "C. 14.5 knt (cruise,container,vehicle)"]),  
                      sum (outputBaseline$DurHR[outputBaseline$Category == "D. Non-AIS vessel"]) ,
                      sum (outputBaseline$DurHR[outputBaseline$Category == "E. No vessel"]), 
                      sum (outputBaseline$DurHR[outputBaseline$Category == "F. No nearby AIS"] ) ) 

dt = data.table(X = c(1,70), Y = c(1,1), Uships = c(33, 31), Names = c("Baseline (2019)","Slowdown (2021)"), 
                types = rbind(sumBaseline, sumSlowdonw ) )
dt = as.data.frame(dt)
colnames(dt) = c("X", "Y", "UniqueShips", "Period",  "AIS (unk)", "11knt", "14knt",  "Non-AIS", "No-Vessel", "No Nearby AIS" )

dtTime = data.table(X = c(1,70), Y = c(1,1), Uships = c(33, 31), Names = c("Baseline (2019)","Slowdown (2021)"), 
                    types = rbind(sumTIMEBaseline, sumTIMESlowdown ) )
dtTime = as.data.frame(dtTime)
colnames(dtTime) =c("X", "Y", "UniqueShips", "Period",  "AIS (unk)", "11knt", "14knt",  "Non-AIS", "No-Vessel", "No Nearby AIS" )
# dtTime$Total = sum(dtTime$Cargo, dtTime$Tanker, dtTime$Military, dtTime$Other )
library(scatterpie)
ggplot() +
  geom_scatterpie( aes(x=X, y=Y, r=UniqueShips, group=Period), data=dt,
                   cols=c( "AIS (unk)", "11knt", "14knt",  "Non-AIS", "No-Vessel", "No Nearby AIS") )  + coord_equal()+
  geom_text(data = dt, aes(x=X, y=Y-1,label = Period ), size = 5)  +
  ylab("") + xlab("") + ggtitle("Comparison of Acoustic Categories") +
  #geom_text(data = dtTime, aes(y = Y*20, x = X, label = paste0(formatC(Y*100, digits = 3), "%")), nudge_y = 0.07, nudge_x = -0.25, size = 2) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggplot() +
  geom_scatterpie( aes(x=X, y=Y, r=UniqueShips, group=Period), data=dtTime,
                   cols=c("Cargo", "Tanker", "Military",  "Other", "Non-AIS", "Ambient") )  + coord_equal()+
  geom_text(data = dt, aes(x=X, y=Y-1,label = Period ), size = 5)  +
  ylab("")+xlab("")+ ggtitle("Comparison of Total time in Acoustic Categories") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# (4) NOISE ABOVE
# get difference from ambient closest sample
outputVD = output[ output$Category != "F. Ambient",] # (nrow(outputVD))
outputAB = output[ output$Category == "F. Ambient",] # (nrow(outputAB))
outputVD$SNR = NA
outputVD$SNRmax = NA
outputVD$SNRtime = NA
for (ii in 1:nrow(outputVD)) {
  #find the closest (in time) ambient sample
  idx = which.min(abs(outputVD$Start[ii]-outputAB$Start))
  signalMed = outputVD$`TOL_125 median`[ii]
  noiseMed  = outputAB$`TOL_125 median`[idx]
  outputVD$SNR[ii]   = signalMed - noiseMed # signal - noise
  outputVD$SNRtime[ii]= as.numeric( difftime(outputVD$Start[ii], outputAB$Start[idx], units = "mins") )
  
  signalMed = outputVD$`TOL_125 max`[ii]
  noiseMed  = outputAB$`TOL_125 max`[idx]
  outputVD$SNRmax[ii]= signalMed - noiseMed # signal - noise
}

outputVDall$DurHR = outputVDall$DurS/3600
# look at the SNR relative to number of vessels detected...
ggplot(outputVD, aes(x = Cargo, y = SNRmax,color =mDist))+
  geom_point() # more vessels the higher the SNR!
outputVD = outputVD[outputVD$SNRtime < 24*60, ] # only samples when ambient is < 24 hours away
unique( outputVD$Category )
outputVD = outputVD[outputVD$Category != "E. Non-AIS vessels",]
mB = median( outputVD$SNRmax[outputVD$Period =="baseline"], na.rm = T )
sd( outputVD$SNRmax[outputVD$Period =="baseline"], na.rm = T )
sB = nrow( outputVD[outputVD$Period =="baseline", ] )

mS = median( outputVD$SNRmax[outputVD$Period =="slowdown"], na.rm = T )
sd( outputVD$SNRmax[outputVD$Period =="slowdown"], na.rm = T )
sS = nrow( outputVD[outputVD$Period =="slowdown", ] )

outputVD2 = output[ output$Category == "E. Non-AIS vessels",] #(nrow(outputVD2))
outputVD2$SNR = NA
outputVD2$SNRmax = NA
outputVD2$SNRtime = NA
for (ii in 1:nrow(outputVD2)) {
  #find the closest (in time) ambient sample
  idx = which.min(abs(outputVD2$Start[ii]-outputAB$Start))
  signalMed = outputVD2$`TOL_125 median`[ii]
  noiseMed  = outputVD2$`TOL_125 median`[idx]
  outputVD2$SNR[ii]   = signalMed - noiseMed # signal - noise
  outputVD2$SNRtime[ii]= as.numeric( difftime(outputVD2$Start[ii], outputAB$Start[idx], units = "mins") )
  
  signalMed = outputVD2$`TOL_125 max`[ii]
  noiseMed  = outputVD2$`TOL_125 max`[idx]
  outputVD2$SNRmax[ii]= signalMed - noiseMed # signal - noise
}

outputVDall = rbind(outputVD2,outputVD)
unique( outputVDall$Category )

#boxplot---------------------------------------------------
ggplot(outputVDall, aes(x=Category, y=SNRmax, fill = Period )  )+
  geom_boxplot() +
  labs(title = "Difference in sound level when vessel present",
       caption =  paste0("AIS vessel detection comparision: \n",
                         "Baseline: ", round(mB, digits = 1), " dB (n=", sB, ")\n",
                         "Slowdown: ", round(mS, digits = 1), " dB (n=" ,sS, ")\n") ) +
  xlab("")+  ylab("difference in dB (125 Hz 1/3 octave band)")+
  theme_minimal()

#ecdf---------------------------------------------------
ggplot(outputVDall, aes(SNRmax,  color = Period ) ) + 
  stat_ecdf(geom = "step") + 
  ggtitle (paste0("Noise Above Metric  (n=", nrow(outputVDall),")" )) +
  facet_wrap(~Category)+
  ylab("")+  xlab("difference in dB (125 Hz 1/3 octave band)")+
  theme_minimal()

## STOP HERE !!!!!
# Previous versions!!! WARNING might not work
#-----------------------------------------------------------------------------------------
# Compare slowdown AIS vessel periods to baseline vessel periods
#-----------------------------------------------------------------------------------------
#remove all non AIS samples
outputShip = output[output$Label == "ship",]      #removes ambient
outputShipAIS = outputShip[ outputShip$AIS > 0, ] #removes detections without AIS

#Comparison of just AIS vessel between periods-- 
ggplot(outputShipAIS, aes(`TOL_125 max`,  color = Period ) ) + 
  stat_ecdf(geom = "step") + 
  ggtitle (paste0("Comparision of vessel detections with AIS vessels (n=", nrow(outputShipAIS),")" )) +
  theme_minimal()

#difference from non-ship periods-- remove samples with 0 detections and
#select non-vessel detection periods
outputNShip = output[output$Label == "ambient",]
#remove any non-vessle detection periods with AIS... weird these exist!
outputNShipAIS = outputNShip[ outputNShip$AIS == 0, ]
nrow(outputNShipAIS) # interesting many no ship periods with AIS within 10 km!!

ggplot(outputNShipAIS, aes(`TOL_125 max`,  color = Period ) ) + 
  stat_ecdf(geom = "step") + 
  ggtitle (paste0("Comparision of non-AIS vessel (n=", nrow(outputNShipAIS),")" )) +
  theme_minimal()


# just remove ambient samples with AIS detections to plot all three categories
# COPY THISE PLOTS TO PPT https://docs.google.com/presentation/d/1SJd4pKsQn46464IH-L5fOiqt5-WUT-11MCg4V0IRdeU/edit#slide=id.g13e85015171_0_63

idx = which( output$Label == "ambient" & output$AIS > 0) #558!!
output3 = output[-idx, ]
unique(output3$Category)

ggplot(output3, aes(x=Category, y=`TOL_125 median`, fill = as.factor(Period) )  )+
  geom_boxplot() +
  ggtitle (paste0("Comparision of sound levels (125 TOL) (n=", nrow(output3),")" )) +
  labs(x="",y = "median Sound Level (125 Hz TOL)")+
  theme_minimal()

ggplot(output3, aes(x=Category, y=`TOL_125 max`, fill = as.factor(Period) )  )+
  geom_boxplot() +
  ggtitle (paste0("Comparision of sound levels (125 TOL) (n=", nrow(output3),")" )) +
  labs(x="",y = "max Sound Level (125 Hz TOL)")+
  theme_minimal()

ggplot(output3, aes(x=Category, y=DurS/3600, fill = as.factor(Period) )  )+
  geom_boxplot() +
  ggtitle (paste0("Comparision of duration of events (125 TOL) (n=", nrow(output3),")" )) +
  labs(x="",y = "Duration of event (Hours)")+
  theme_minimal()


#-----------------------------------------------------------------------------------------
# Comparisons-- AIS vessel vs non-vessel (after removing ambient samples with AIS )
#-----------------------------------------------------------------------------------------
output2 = rbind(outputShipAIS, outputNShipAIS)
nrow(output)- nrow(output2)

#combine Label and period into same variable to plot with Labelc
output2$Condition = as.factor(  paste0( output2$Period,"-", output2$Label ) )

#is the difference from ambient less?
ggplot(output2, aes(`TOL_125 median`, fill = Condition, color = Condition )  )+
  stat_ecdf(geom = "step")+
  scale_color_manual(values=c("gray", "black","pink","red"))+
  labs(x="median Sound Level (125 Hz TOL)",y = "Exceedance Probability")+
  theme_minimal()
#is the difference from ambient less?
ggplot(output2, aes(`TOL_125 max`, fill = Condition, color = Condition )  )+
  stat_ecdf(geom = "step")+
  scale_color_manual(values=c("gray", "black","pink","red"))+
  labs(x="max Sound Level (125 Hz TOL)",y = "Exceedance Probability")+
  theme_minimal()

#difference in SNR- greater in slowdown because lower ambient
ggplot(output2, aes(SNR, fill = Condition, color = Condition )  )+
  stat_ecdf(geom = "step")+
  scale_color_manual(values=c("gray", "black","pink","red"))+
  labs(x="SNR (125 Hz TOL)",y = "Exceedance Probability")+
  ggtitle (paste0("Comparision of SNR (125 TOL) (n=", nrow(output3),")" )) +
  theme_minimal()

#difference in SNR- same because baseline had lower ambient louder ships/ slowdown had higher ambient and quieter ships
ggplot(output2, aes(SNRmax, fill = Condition, color = Condition )  )+
  stat_ecdf(geom = "step")+
  scale_color_manual(values=c("gray", "black","pink","red"))+
  labs(x="SNR (125 Hz TOL)",y = "Exceedance Probability")+
  ggtitle (paste0("Comparision of SNRmax (125 TOL) (n=", nrow(output3),")" )) +
  theme_minimal()

# Time sampled in each category-- add to above graphs
sum(output2$DurS[ output2$Condition == "baseline-ambient"])/3600
sum(output2$DurS[ output2$Condition == "baseline-ship"])/3600
sum(output2$DurS[ output2$Condition == "slowdown-ambient"])/3600
sum(output2$DurS[ output2$Condition == "slowdown-ship"])/3600

# SHIFT IN CONDIION

#difference from non-vessel periods
NA_baseline = median(output2$`TOL_125 median`[ output2$Condition == "baseline-ship"], na.rm = T )-
  median(output2$`TOL_125 median`[ output2$Condition == "baseline-ambient"], na.rm = T )

NAmax_baseline = median(output2$`TOL_125 max`[ output2$Condition == "baseline-ship"], na.rm = T )-
  median(output2$`TOL_125 max`[ output2$Condition == "baseline-ambient"], na.rm = T )

NA_slowdown = median(output2$`TOL_125 median`[ output2$Condition == "slowdown-ship"], na.rm = T )-
  median(output2$`TOL_125 median`[ output2$Condition == "slowdown-ambient"], na.rm = T )

NAmax_slowdown = median(output2$`TOL_125 max`[ output2$Condition == "slowdown-ship"], na.rm = T )-
  median(output2$`TOL_125 max`[ output2$Condition == "slowdown-ambient"], na.rm = T )

diff_periods = median(output2$`TOL_125 median`[ output2$Condition == "baseline-ship"], na.rm = T )-
  median(output2$`TOL_125 median`[ output2$Condition == "slowdown-ship"], na.rm = T )

# Average duration category
mean(output2$DurS[ output2$Condition == "baseline-ambient"])/3600

durVes_baseline     = mean(output2$DurS[ output2$Condition == "baseline-ship"])/3600
sddurVes_baseline   = (sd(output2$DurS[ output2$Condition == "baseline-ship"])/3600) / nrow(output2)
SamplesVes_baseline = (length(output2$DurS[ output2$Condition == "baseline-ship"]) ) 
TimesVes_baseline   = (sum(output2$DurS[ output2$Condition == "baseline-ship"]) ) /3600
PerTimeVessel_baseline = TimesVes_baseline/ (TimesVes_baseline + ((sum(output2$DurS[ output2$Condition == "baseline-ambient"]) ) /3600)) *100

mean(output2$DurS[ output2$Condition == "slowdown-ambient"])/3600
durVes_Slowdown     = mean(output2$DurS[ output2$Condition == "slowdown-ship"])/3600
sddurVes_Slowdown   = (sd(output2$DurS[ output2$Condition == "slowdown-ship"])/3600)  / nrow(output2)
SamplesVes_Slowdown = (length(output2$DurS[ output2$Condition == "slowdown-ship"]) ) 
TimesVes_Slowdown   = (sum(output2$DurS[ output2$Condition == "slowdown-ship"]) ) /3600
PerTimeVessel_Slowdown = TimesVes_Slowdown/ (TimesVes_Slowdown + ((sum(output2$DurS[ output2$Condition == "slowdown-ambient"]) ) /3600)) *100


#duration of ambient: 0.2 hrs vs 0.4 hrs (longer in 2021-- interesting)
#duration of VD: 0.7 vs 0.7 (no change in duration of VD)

cmplt = as.data.frame( rbind(    c("Baseline-2019", NA_baseline, NAmax_baseline, durVes_baseline, sddurVes_baseline, TimesVes_baseline ), 
                                 c("Slowdown-2021", NA_slowdown, NAmax_slowdown, durVes_Slowdown, sddurVes_Slowdown, TimesVes_Slowdown )) )
colnames(cmplt) = c("Period","NoiseAbove","NoiseAboveMax",  "Duration", "Dursd","DaySampled")

cmplt$NoiseAbove = as.numeric(as.character(cmplt$NoiseAbove ))
cmplt$NoiseAboveMax = as.numeric(as.character(cmplt$NoiseAboveMax ))
cmplt$Duration = as.numeric(as.character(cmplt$Duration ))
cmplt$Dursd = as.numeric(as.character(cmplt$Dursd ))
cmplt$DaySampled = as.numeric(as.character(cmplt$DaySampled ))

ggplot(cmplt, aes(x = NoiseAbove, y = Duration, color = Period, size = DaySampled)) +
  geom_point() + 
  #geom_text(label = floor(cmplt$DaySampled), nudge_y = 0.25, check_overlap = T, label.size = 0.35) +
  geom_pointrange(aes( ymin= Duration - Dursd, ymax= Duration+Dursd ))+
  geom_label(label = paste0("Baseline (2019) ", as.character( round(cmplt$DaySampled[1])) , " hrs") , x = 6, y = .7,color = "gray")+
  geom_label(label = paste0("Slowdown (2021) ", as.character( round(cmplt$DaySampled[2])) , " hrs")  , x = 7.5, y = .8,color = "orange")+
  ylim(c(.5,1))+
  xlim(c(0,10))+
  xlab("Noise above non-vessel (median 125 TOL) ")+
  ylab("Duration of AIS vessel detections (hours) ")+  
  scale_color_manual(values=c('#999999','#E69F00'))+
  theme_minimal() +
  theme(legend.position="none")+ 
  ggtitle("Comparision of vessel noise conditions \n Median Noise Above & Duration")+
  theme(text = element_text(size = 16))

ggplot(cmplt, aes(x = NoiseAboveMax, y = Duration, color = Period, size = DaySampled)) +
  geom_point() + 
  #geom_text(label = floor(cmplt$DaySampled), nudge_y = 0.25, check_overlap = T, label.size = 0.35) +
  geom_pointrange(aes( ymin= Duration - Dursd, ymax= Duration+Dursd ))+
  geom_label(label = paste0("Baseline (2019) ", as.character( round(cmplt$DaySampled[1])) , " hrs") ,  x = 11, y = .7,color = "gray")+
  geom_label(label = paste0("Slowdown (2021) ", as.character( round(cmplt$DaySampled[2])) , " hrs")  , x = 11, y = .8,color = "orange")+
  ylim(c(.5,1))+
  xlim(c(0,15))+
  xlab("Noise above non-vessel (max 125 TOL) ")+
  ylab("Duration of AIS vessel detections (hours) ")+  
  scale_color_manual(values=c('#999999','#E69F00'))+
  theme_minimal() +
  theme(legend.position="none")+ 
  ggtitle("Comparision of vessel noise conditions \n Max Noise Above & Duration")+ 
  theme(text = element_text(size = 16))
#-----------------------------------------------------------------------------------------
# pie graphs of difference in ship traffic
#-----------------------------------------------------------------------------------------
dt = data.table(X = c(1,70), Y = c(1,1), Uships = c(33, 31), Names = c("Baseline (2019)","Slowdown (2021)"), ships = rbind(c(10, 11, 78), c(7,10,80))) 
dt = as.data.frame(dt)
colnames(dt) = c("X", "Y", "UniqueShips", "Period", "Small", "Medium", "Large" )

library(scatterpie)
ggplot() +
  geom_scatterpie( aes(x=X, y=Y, r=UniqueShips, group=Period), data=dt,
                   cols=c("Small", "Medium", "Large") )  + coord_equal()+
  geom_text(data = dt, aes(x=X, y=Y-1,  label = Period ), size = 3)  +
  ylab("")+xlab("")+
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



geom_map(map=world, aes(map_id=region), fill="gray90", color="black") +
  geom_point(data = siteLoc, aes(x=lon, y=lat), color = "red", size = 1) +
  geom_text(data = siteLoc, aes(x=lon, y=lat+.5,  label = Names ), size = 3)  +
  coord_quickmap()+
  xlim(-170,-150) + ylim(18, 30) +
  theme_minimal()
p

p2 = p +  
  
  p2