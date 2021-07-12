# Log of where I am saving different file types...and how to get a current list of files

# AUDIO data
#-------------------------------
# E:\RESEARCH\SanctSound\data\AUDIO AND on HARDDrive from SIO


# SPL data-- within each sanctuary
#-------------------------------
# GENERAL: E:\RESEARCH\SanctSound\data\sanctuary\site\deployment
dir   = ("E:\\RESEARCH\\SanctSound\\data\\")
basename( ( list.files(path=dir, pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)) )
length(unique(basename( ( list.files(path=dir, pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)))))
length((basename( ( list.files(path=dir, pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)))))

# AIS data
#-------------------------------
#GENERAL:  "E:\RESEARCH\SanctSound\data\sanctuary\ -- AIS data for sanctuary
#detectionsB = c("bluewhale","finwhale","humpbackwhale")  
#detectionsO = c("Explosions") 
basename( ( list.files(path=dir, pattern = "AIS", full.names=TRUE, recursive = TRUE)) )
 

# Detection data
#-------------------------------
# GENERAL: ("E:\RESEARCH\SanctSound\data\sanctuary\site\)
#download to site/deployment files
#vessel only directory
#basename( ( list.files(path=dir, pattern = "hips", full.names=TRUE, recursive = TRUE)) )
#COPY files here: "E:\\RESEARCH\\SanctSound\\data2\\SanctSound_VesselDetection_DataProducts"
basename( ( list.files(path="E:\\RESEARCH\\SanctSound\\data2\\SanctSound_VesselDetection_DataProducts", pattern = "hips", full.names=TRUE, recursive = TRUE)) )

# Abiotic data
#-------------------------------
# dirAB = paste("E:\RESEARCH\SanctSound\data\sanctuary\

# Other
#-------------------------------

# Processed files
#-------------------------------

# combined vessel/AIS
#-------------------------------


#-------------------------------
## Processing steps....
#-------------------------------
#1) Combine SPL, AIS,Vessel detection, !effort control!--- output for each site-- (process_VesselDetectionsAIS.R)
#2) Combine SPL, AIS, Abiotic       --- output for each site-- (combineFiles_SantSound_saveOut_noDet.R)
#3) Combine above with site specific detections --- (combineFiles_VesselDetectionsAISDets_SITE.R, did this for some sites previously so used code to create)
#4) Graphics using these outputs