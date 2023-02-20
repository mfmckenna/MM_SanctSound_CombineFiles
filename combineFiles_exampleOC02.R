#-----------------------------------------------------------------------------------------
# READS IN-- AIS data 
#-----------------------------------------------------------------------------------------
# summar of AIS traffic in defined time periods to make sure metrics area compariable... how to offset for different in

rm(list=ls())
library(data.table)
tDir = "F:\\RESEARCH\\SanctSound\\"

dirAIS      = paste0(tDir,"data")
Fpattern = "_2018_10_to_2020_11.csv"  # "_2018_10_to_2021_04.csv"

#newer data
dirAIS      = paste0(tDir,"data2\\")
Fpattern = "_2021_05_to_2021_09.csv"  # "_2018_10_to_2021_04.csv"


nFilesAIS   = length( list.files(path=dirAIS,pattern = Fpattern, full.names=TRUE, recursive = TRUE))
inFilesAIS  =       ( list.files(path=dirAIS,pattern = Fpattern, full.names=TRUE, recursive = TRUE))
x = strsplit(inFilesAIS,"_")
sanctsAIS = unique(sapply( x, "[", 2 ))
#combine AIS files
multmerge = function(path){
  filenames = list.files(path=path, pattern = Fpattern, full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
AIS <- multmerge(dirAIS)
sitesAIS = unique(AIS$LOC_ID)
cat(length(sitesAIS), " number of sites with AIS data", "\n")
rm(x)

#re-name sites
AIS$LOC_ID[AIS$LOC_ID=="PM08"] = "PHRB"
sitesAIS = unique(AIS$LOC_ID)
AIS$UV = (AIS$LOA_S_UV+AIS$LOA_M_UV+AIS$LOA_L_UV+AIS$LOA_NA_UV)

#get site and timeperiod of interest
AISt = AIS[AIS$LOC_ID=="OC02", ]

AISt$Day = as.Date(AISt$DATE,format = "%m/%d/%Y") #AIS is daily resolution
AISt$Mth = month(AISt$Day )
AISt$YR = year(AISt$Day )

AISt2 =AISt[AISt$Mth ==7, ]
AISt2 =AISt2[AISt2$YR=="2019", ]

mean(AISt2$UV)
sd(AISt2$UV)

mean( AISt2$LOA_S_UV/ AISt2$UV) *100 
mean( AISt2$LOA_M_UV/ AISt2$UV) *100 
mean( AISt2$LOA_L_UV/ AISt2$UV) *100
