#reading in a looking at specific SancSound files
library(ncdf4)
library(lubridate)
library(reshape)
library(ggplot2)
library(tidyverse)
library(gridExtra)

rm(list=ls())

#.nc files
dirSPL   = list.dirs( "F:\\SanctSound_DataProducts\\SB\\SB01_04")
list.files(dirSPL)
SPL_OL = list.files(dirSPL, pattern = "_OL" ,full.names = TRUE)
r = nc_open(SPL_OL[1]) 
fname = gsub(".nc", "", basename( SPL_OL[1] ))
siteDep = substr(fname, start = 9 , stop = 15)
site =    substr(fname, start = 9 , stop = 12)
deploy =  substr(fname, start = 14 , stop = 15)
cat("Processing : ", siteDep, "\n")
# read in data
OTB = r$dim$frequency$vals
spl <- as.data.frame (ncvar_get(r, "sound_pressure_levels")) #rows = frequency, columns = time
tm  <- ncvar_get(r, "timeStamp")
tmF = as.POSIXct ( gsub(":000Z","", gsub("T", " ", tm)),tz="GMT" )
cdata = rbind(as.character(tmF),spl)
rownames(cdata) = c("dtime", as.character(OTB))
cdata = as.data.frame( t(cdata) )
cdata$date = as.Date(cdata$dtime)
