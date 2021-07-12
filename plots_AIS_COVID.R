# PLOT AIS data for all Sanctuary Sound Sites to see difference in 2019-2020

rm(list=ls())
dir   = paste0("D:\\RESEARCH\\SanctSound\\data")
nFiles = length( list.files(path=dir,pattern = "_2018_10_to_2020_11.csv", full.names=TRUE, recursive = TRUE))
inFiles = ( list.files(path=dir,pattern = "_2018_10_to_2020_11.csv", full.names=TRUE, recursive = TRUE))
outDir = "D:\\RESEARCH\\SanctSound\\data\\AISsummary_COVID"
DC = Sys.Date()
for (ii in 2: nFiles) {#sanctuaries
  
  AISa = read.csv(inFiles[ii])
  usite = unique(AISa$LOC_ID)
  
  for (ss in 1:length(usite)) {#sites
    AIS = AISa[AISa$LOC_ID == usite[ss],]
    AIS$LOA_ALL_UV = AIS$LOA_S_UV + AIS$LOA_M_UV + AIS$LOA_L_UV + AIS$LOA_NA_UV 
    AIS$LOA_ALL_OPHRS = AIS$LOA_S_OPHRS + AIS$LOA_M_OPHRS + AIS$LOA_L_OPHRS + AIS$LOA_NA_OPHRS 
    AIS$DateFday     = as.Date(AIS$DATE,format = "%m/%d/%Y")#AIS is daily resolution
    AIS$DateFJul     = yday(AIS$DateFday)
    AIS$Yr = year(AIS$DateFday)
    
    AIS = AIS[AIS$Yr > 2018,]
    #OPHRS plots-- scale by max
    mx = max(AIS$LOA_ALL_OPHRS)
    pAISs = ggplot(AIS, aes(DateFJul, LOA_S_OPHRS, color = as.factor(Yr) ) ) +
      geom_point(alpha = .2) +
      geom_smooth(method = "loess") +
      scale_color_manual(values=c('#E69F00','#56B4E9'),name = "") +
      ylim(c(0,mx))+
      xlab("Julian Day")+
      theme(legend.position=c(0.2, 0.9),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)))
    pAISm = ggplot(AIS, aes(DateFJul, LOA_M_OPHRS, color = as.factor(Yr) ) ) +
      geom_point(alpha = .2) +
      geom_smooth(method = "loess") +
      scale_color_manual(values=c('#E69F00','#56B4E9')) +
      ylim(c(0,mx))+
      xlab("Julian Day")+
      theme(legend.position="none")
    pAISl = ggplot(AIS, aes(DateFJul, LOA_L_OPHRS, color = as.factor(Yr) ) ) +
      geom_point(alpha = .2) +
      geom_smooth(method = "loess") +
      scale_color_manual(values=c('#E69F00','#56B4E9')) +
      ylim(c(0,mx))+
      xlab("Julian Day")+
      theme(legend.position="none")
    pAISa= ggplot(AIS, aes(DateFJul, LOA_ALL_OPHRS, color = as.factor(Yr) ) ) +
      geom_point(alpha = .2) +
      geom_smooth(method = "loess") + 
      scale_color_manual(values=c('#E69F00','#56B4E9')) +
      ylim(c(0,mx))+
      xlab("Julian Day")+
      theme(legend.position="none")
    pAIS_HRgrid = grid.arrange(pAISs,pAISm,pAISl,pAISa,ncol = 2,nrow = 2, top = paste0("AIS OPHRS (",usite[ss], ")"))
    ggsave(paste0(outDir,"\\",usite[ss],"_AIS_OPHRSsummary_created_",DC,".png"),pAIS_HRgrid)
    
    mx = max(AIS$LOA_ALL_UV)
    pAISsUV = ggplot(AIS, aes(DateFJul, LOA_S_UV, color = as.factor(Yr) ) ) +
      geom_point(alpha = .2) +
      geom_smooth(method = "loess") +
      scale_color_manual(values=c('#E69F00','#56B4E9'),name = "") +
      ylim(c(0,mx))+
      xlab("Julian Day")+
      theme(legend.position=c(0.2, 0.9))
    pAISmUV = ggplot(AIS, aes(DateFJul, LOA_M_UV, color = as.factor(Yr) ) ) +
      geom_point(alpha = .2) +
      geom_smooth(method = "loess") +
      scale_color_manual(values=c('#E69F00','#56B4E9')) +
      ylim(c(0,mx))+
      xlab("Julian Day")+
      theme(legend.position="none")
    pAISlUV = ggplot(AIS, aes(DateFJul, LOA_L_UV, color = as.factor(Yr) ) ) +
      geom_point(alpha = .2) +
      geom_smooth(method = "loess") +
      scale_color_manual(values=c('#E69F00','#56B4E9')) +
      ylim(c(0,mx))+
      xlab("Julian Day")+
      theme(legend.position="none")
    pAISaUV = ggplot(AIS, aes(DateFJul, LOA_ALL_UV, color = as.factor(Yr) ) ) +
      geom_point(alpha = .2) +
      geom_smooth(method = "loess") + 
      scale_color_manual(values=c('#E69F00','#56B4E9')) +
      ylim(c(0,mx))+
      xlab("Julian Day")+
      theme(legend.position="none")
    pAIS_UVgrid= grid.arrange(pAISsUV,pAISmUV,pAISlUV,pAISaUV,ncol = 2,nrow = 2,top = paste0("Check- AIS UV (",usite[ss], ")") )
    ggsave(paste0(outDir,"\\",usite[ss],"_AIS_UVsummary_created_",DC,".png"),pAIS_UVgrid)
  }
} 