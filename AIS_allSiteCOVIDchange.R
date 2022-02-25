#CHANGE IN NEARBY VESSEL USE
#-----------------------------------------------------------------------------------------
tDir = "E:\\RESEARCH\\SanctSound\\"
dirAIS      = paste0(tDir,"data")
Fpattern = "_2018_10_to_2020_11.csv" # "_2018_10_to_2021_04.csv"
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
AIS$DAY = as.Date(AIS$DATE,format = "%m/%d/%Y") 
AIS$MTH = month(AIS$DAY )
AIS$YR = year(AIS$DAY )

AIS = AIS[ AIS$YR >2018, ]

AIS$ALL_UV = AIS$LOA_S_UV+AIS$LOA_M_UV+AIS$LOA_L_UV+AIS$LOA_NA_UV
AIS$ALL_HR = AIS$LOA_S_OPHRS+AIS$LOA_M_OPHRS+AIS$LOA_L_OPHRS+AIS$LOA_NA_OPHRS

tmpHRm = as.data.frame( aggregate(AIS$ALL_HR,    by=list(AIS$MTH,AIS$LOC_ID,AIS$YR), mean, na.rm=T)  )
tmpUVm = as.data.frame( aggregate(AIS$ALL_UV,    by=list(AIS$MTH,AIS$LOC_ID,AIS$YR), mean, na.rm=T)  )

tmpHRs = as.data.frame( aggregate(AIS$ALL_HR,    by=list(AIS$MTH,AIS$LOC_ID,AIS$YR), sum, na.rm=T)  )
tmpUVs = as.data.frame( aggregate(AIS$ALL_UV,    by=list(AIS$MTH,AIS$LOC_ID,AIS$YR), sum, na.rm=T)  )

#smean operational hours
ggplot(tmpHRm, aes(as.factor(Group.1), Group.2, fill=as.numeric(as.character(x )) ) ) +
  geom_tile()+
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred")+
  theme( legend.title = element_blank() ) +
 facet_grid(~Group.3)

ggplot(tmpUVm, aes(as.factor(Group.1), Group.2, fill=as.numeric(as.character(x )) ) ) +
  geom_tile()+
  scale_fill_gradient2(low = "white",  high = "purple")+
  theme( legend.title = element_blank() ) +
  facet_grid(~Group.3)

#CHANGE IN MONTHLY MEAN OPERATIONAL HOURS
HRM = as.data.frame( spread(tmpHRm, Group.3, x))    
HRM$Diff = HRM$`2020`- HRM$`2019`
ggplot(HRM, aes(as.factor(Group.1), Group.2, fill=as.numeric(as.character(Diff )) ) ) +
  geom_tile()+
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred")+
  theme( legend.title = element_blank() ) 

UVM = as.data.frame( spread(tmpUVm, Group.3, x))    
UVM$Diff = UVM$`2020`- UVM$`2019`
ggplot(UVM, aes(as.factor(Group.1), Group.2, fill=as.numeric(as.character(Diff )) ) ) +
  geom_tile()+
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred")+
   xlab("")+ ylab("")+
  theme_minimal() +
  theme( legend.title = element_blank() )
