# read in SPLs for SanctSound Project

library(ncdf4)
library(lubridate)
library(reshape)
library(ggplot2)
library(tidyverse)
library(gridExtra)

rm(list=ls())

dirSPL   = setwd( "E:\\RESEARCH\\SanctSound\\") #dirSPL = dirs[ grepl("data", dirs) ]
list.files(dirSPL)

ncFiles= list.files(dirSPL, pattern = "ships.nc" ,full.names = TRUE)

#test a file
r = nc_open(ncFiles[1]) 
r$var$time_stamp$longname

tm  <- ncvar_get(r, "time_stamp")
tmF = as.POSIXct ( gsub(":000Z","", gsub("T", " ", tm)), tz="GMT" )

tb  <- ncvar_get(r, "time_bounds")/1000
tb/1000
options(scipen =999)
r$var$time_bounds$units

#rows are stare/end, columns 
nrow(tb)
ncol(tb)
as.POSIXct(tb[1,1], origin = "1970-01-01", tz = "GMT")  #"50921-04-26 19:03:20 GMT"
#remove last three zeros
tedit = substr( as.character(tb[1,1]), 1, nchar(as.character(tb[1,1]))-3 ) 
as.POSIXct(as.numeric(tedit), origin = "1970-01-01", tz = "GMT")  #"2018-12-14 02:26:35 GMT"


info = NULL
infoT = NULL
quants <- c(.01,0.25,0.50,0.75,0.99)
perSum = NULL
for (ff in 1: length(SPL_OL) ) { # ff = 1
  #1) meta data-- details
  r = nc_open(SPL_OL[ff]) 
  fname = gsub(".nc", "", basename( SPL_OL[ff] ))
  siteDep = substr(fname, start = 9 , stop = 15)
  site =    substr(fname, start = 9 , stop = 12)
  deploy =  substr(fname, start = 14 , stop = 15)
  cat("Processing : ", siteDep, "\n")
  #r$is_GMT
  #tmpT = r$var$timeStamp$dim[[1]]$vals
  #x = as.POSIXct(tmpT,origin = "1970-01-01",tz = "GMT")
  
  #2) read in data
  OTB = r$dim$frequency$vals
  spl <- as.data.frame (ncvar_get(r, "sound_pressure_levels")) #rows = frequency, columns = time
  tm  <- ncvar_get(r, "timeStamp")
  #format data tm[1]
  tmF = as.POSIXct ( gsub(":000Z","", gsub("T", " ", tm)),tz="GMT" )
  cdata = rbind(as.character(tmF),spl)
  rownames(cdata) = c("dtime", as.character(OTB))
  cdata = as.data.frame( t(cdata) )
  cdata$date = as.Date(cdata$dtime)
  
  #4) truncate data to time period of interest- between March 1 and May 1, 30 days (is this possible??)
  # and calculate percentiles on truncated data and save to new matrix
  #condition for each deployment...
  ###sites to start at the beginning of deployment
  if (siteDep == "CI04_02") {
    dwindow =  c( min(cdata$date), min(cdata$date) + 30) #start at beginning + 30 days
    tst = cdata[cdata$date >= dwindow[1] & cdata$date <= dwindow[2],]
    ndys = length( unique(tst$date) )
    inD =  tst[, 2:11]
    indx <- sapply(inD, is.factor)
    inD[indx] <- lapply(inD[indx], function(x) as.numeric(as.character(x)))
    outD = as.data.frame( apply( inD, 2 , quantile, probs = quants ))
    outD = cbind(outD,quants,siteDep)
    
    #spectrogram, tile plot
    tst2 = reshape2 :: melt(tst, id.var = "dtime", measure.vars = as.character(OTB) )
    tst2$dtime2 = as.POSIXct(tst2$dtime ,tz = "GMT")
    plot1 = ggplot(tst2, aes(dtime2, variable, fill= as.numeric(value))) + 
      geom_tile() +
      scale_fill_gradient(low="white", high="black",limits=c(70,120) ) +
      #scale_y_discrete(labels = ulabs) +
      labs(fill = "Sound Pressure Levels", title = siteDep) +
      xlab("") +
      ylab("Frequency [Hz]")
   
    #cumulative dynamic range, line plot (per frequency use inD, columns)
    cumV = NULL
    fqID = colnames(inD)
    for (vv in 1:length(fqID)){
      x = inD[,fqID[vv]] #should I un-dB?
      n <- seq_along(x)      ## cumulative sample size
      m <- cumsum(x) / n     ## cumulative mean check: inD[1,vv]+inD[2,vv]
      m2<- cumsum(x * x) / n ## cumulative squared mean plot(m2)
      v <- (m2 - m * m) * (n / (n - 1)) ## cumulative variance
      s <- sqrt(v)                      ## cumulative standard deviation
      cumV = rbind(cumV, as.data.frame(cbind(fqID[vv],x,n,m,m2,v,s)))
      rm(x,n,m,m2,v,s)
    }
    cumV = as.data.frame(cumV)
    unique(cumV$V1)
    plot3 = ggplot(cumV, aes(as.numeric(as.character(n)), as.numeric(as.character(s)), 
                             color = as.factor(V1), group = (V1)) )+
      geom_line()+
      scale_color_discrete(name = "OTB")+
      labs(y = "cumulative variance",x ="hourly SPL", title=siteDep, fill = "OTB")

    #outputs
    pv =  grid.arrange(plot1, plot3, ncol=1, nrow =2)
    #outputs
    perSum = rbind(perSum, melt(outD, id.vars = c("quants","siteDep"), measure.vars = as.character(OTB) ))
    infoT  = rbind(infoT,  c( fname, ndys, as.character(dwindow ) ) )
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_hSPLcumVar.png"), pv, dpi=300)
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_cumVar.png"), plot3, dpi=300)
    
    #clean up
    rm(tst,ndys,inD,outD)
  }
  if (siteDep == "CI01_02") {
    dwindow =  c( min(cdata$date), min(cdata$date) + 30) #start at beginning + 30 days
    tst = cdata[cdata$date >= dwindow[1] & cdata$date <= dwindow[2],]
    ndys = length( unique(tst$date) )
    inD =  tst[, 2:11]
    indx <- sapply(inD, is.factor)
    inD[indx] <- lapply(inD[indx], function(x) as.numeric(as.character(x)))
    outD = as.data.frame( apply( inD, 2 , quantile, probs = quants ))
    outD = cbind(outD,quants,siteDep)
    
    #spectrogram, tile plot
    tst2 = reshape2 :: melt(tst, id.var = "dtime", measure.vars = as.character(OTB) )
    tst2$dtime2 = as.POSIXct(tst2$dtime ,tz = "GMT")
    plot1 = ggplot(tst2, aes(dtime2, variable, fill= as.numeric(value))) + 
      geom_tile() +
      scale_fill_gradient(low="white", high="black",limits=c(70,120) ) +
      #scale_y_discrete(labels = ulabs) +
      labs(fill = "Sound Pressure Levels", title = siteDep) +
      xlab("") +
      ylab("Frequency [Hz]") 
    
    #mean-median, tile plot
    medD = NULL
    for (mm in 1:ncol(inD)){ # mm = 1  
      medD = cbind(medD, outD[3,mm] - inD[,mm])  # median - SPL
    }
    medD = as.data.frame( cbind(as.character(tst$dtime), medD) )
    colnames(medD) = c("dtime", as.character(OTB))
    medDm = reshape2 :: melt(medD, id.var = "dtime", measure.vars = as.character(OTB) )
    medDm$dtime2 = as.POSIXct(medDm$dtime ,tz = "GMT")
    plot2 = ggplot(medDm, aes(dtime2, variable, fill= as.numeric(value))) + 
      geom_tile() +
      scale_fill_gradient(low="white", high="black")  +
      #scale_y_discrete(labels = ulabs) +
      labs(fill = "Median-SPL", title = "") +
      xlab("") +
      ylab("Frequency [Hz]")
    
    #cumulative dynamic range, line plot (per frequency use inD, columns)
    cumV = NULL
    fqID = colnames(inD)
    for (vv in 1:length(fqID)){
      x = inD[,fqID[vv]] #should I un-dB?
      n <- seq_along(x)      ## cumulative sample size
      m <- cumsum(x) / n     ## cumulative mean check: inD[1,vv]+inD[2,vv]
      m2<- cumsum(x * x) / n ## cumulative squared mean plot(m2)
      v <- (m2 - m * m) * (n / (n - 1)) ## cumulative variance
      s <- sqrt(v)                      ## cumulative standard deviation
      cumV = rbind(cumV, as.data.frame(cbind(fqID[vv],x,n,m,m2,v,s)))
      rm(x,n,m,m2,v,s)
    }
    cumV = as.data.frame(cumV)
    unique(cumV$V1)
    plot3 = ggplot(cumV, aes(as.numeric(as.character(n)), as.numeric(as.character(s)), 
                             color = as.factor(V1), group = (V1)) )+
      geom_line()+
      scale_color_discrete(name = "OTB")+
      labs(y = "cumulative variance",x ="hourly SPL", title=siteDep, fill = "OTB")
    #ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_cumVar.png"), plot3, dpi=300)
    
    #outputs
    pv =  grid.arrange(plot1, plot3, ncol=1, nrow =2)
    #outputs
    perSum = rbind(perSum, melt(outD, id.vars = c("quants","siteDep"), measure.vars = as.character(OTB) ))
    infoT  = rbind(infoT,  c( fname, ndys, as.character(dwindow ) ) )
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_hSPLcumVar.png"), pv, dpi=300)
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_cumVar.png"), plot3, dpi=300)    
    #clean up
    rm(tst,ndys,inD,outD)
  }
  
  if (siteDep == "OC02_01") { 
    dwindow =  c( min(cdata$date), min(cdata$date) + 30) #start at beginning + 30 days
    tst = cdata[cdata$date >= dwindow[1] & cdata$date <= dwindow[2],]
    ndys = length( unique(tst$date) )
    inD =  tst[, 2:11]
    indx <- sapply(inD, is.factor)
    inD[indx] <- lapply(inD[indx], function(x) as.numeric(as.character(x)))
    outD = as.data.frame( apply( inD, 2 , quantile, probs = quants ))
    outD = cbind(outD,quants,siteDep)
    
    #spectrogram, tile plot
    tst2 = reshape2 :: melt(tst, id.var = "dtime", measure.vars = as.character(OTB) )
    tst2$dtime2 = as.POSIXct(tst2$dtime ,tz = "GMT")
    plot1 = ggplot(tst2, aes(dtime2, variable, fill= as.numeric(value))) + 
      geom_tile() +
      scale_fill_gradient(low="white", high="black",limits=c(70,120) ) +
      #scale_y_discrete(labels = ulabs) +
      labs(fill = "Sound Pressure Levels", title = siteDep) +
      xlab("") +
      ylab("Frequency [Hz]")
    
    #cumulative dynamic range, line plot (per frequency use inD, columns)
    cumV = NULL
    fqID = colnames(inD)
    for (vv in 1:length(fqID)){
      x = inD[,fqID[vv]] #should I un-dB?
      n <- seq_along(x)      ## cumulative sample size
      m <- cumsum(x) / n     ## cumulative mean check: inD[1,vv]+inD[2,vv]
      m2<- cumsum(x * x) / n ## cumulative squared mean plot(m2)
      v <- (m2 - m * m) * (n / (n - 1)) ## cumulative variance
      s <- sqrt(v)                      ## cumulative standard deviation
      cumV = rbind(cumV, as.data.frame(cbind(fqID[vv],x,n,m,m2,v,s)))
      rm(x,n,m,m2,v,s)
    }
    cumV = as.data.frame(cumV)
    unique(cumV$V1)
    plot3 = ggplot(cumV, aes(as.numeric(as.character(n)), as.numeric(as.character(s)), 
                             color = as.factor(V1), group = (V1)) )+
      geom_line()+
      scale_color_discrete(name = "OTB")+
      labs(y = "cumulative variance",x ="hourly SPL", title=siteDep, fill = "OTB")
    
    #outputs
    pv =  grid.arrange(plot1, plot3, ncol=1, nrow =2)
    #outputs
    perSum = rbind(perSum, melt(outD, id.vars = c("quants","siteDep"), measure.vars = as.character(OTB) ))
    infoT  = rbind(infoT,  c( fname, ndys, as.character(dwindow ) ) )
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_hSPLcumVar.png"), pv, dpi=300)
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_cumVar.png"), plot3, dpi=300)    
    #clean up
    rm(tst,ndys,inD,outD)
  }
  if (siteDep == "OC01_01") { 
    dwindow =  c( min(cdata$date), min(cdata$date) + 30) #start at beginning + 30 days
    tst = cdata[cdata$date >= dwindow[1] & cdata$date <= dwindow[2],]
    ndys = length( unique(tst$date) )
    inD =  tst[, 2:11]
    indx <- sapply(inD, is.factor)
    inD[indx] <- lapply(inD[indx], function(x) as.numeric(as.character(x)))
    outD = as.data.frame( apply( inD, 2 , quantile, probs = quants ))
    outD = cbind(outD,quants,siteDep)
    
    #spectrogram, tile plot
    tst2 = reshape2 :: melt(tst, id.var = "dtime", measure.vars = as.character(OTB) )
    tst2$dtime2 = as.POSIXct(tst2$dtime ,tz = "GMT")
    plot1 = ggplot(tst2, aes(dtime2, variable, fill= as.numeric(value))) + 
      geom_tile() +
      scale_fill_gradient(low="white", high="black",limits=c(70,120) ) +
      #scale_y_discrete(labels = ulabs) +
      labs(fill = "Sound Pressure Levels", title = siteDep) +
      xlab("") +
      ylab("Frequency [Hz]")
    
    #cumulative dynamic range, line plot (per frequency use inD, columns)
    cumV = NULL
    fqID = colnames(inD)
    for (vv in 1:length(fqID)){
      x = inD[,fqID[vv]] #should I un-dB?
      n <- seq_along(x)      ## cumulative sample size
      m <- cumsum(x) / n     ## cumulative mean check: inD[1,vv]+inD[2,vv]
      m2<- cumsum(x * x) / n ## cumulative squared mean plot(m2)
      v <- (m2 - m * m) * (n / (n - 1)) ## cumulative variance
      s <- sqrt(v)                      ## cumulative standard deviation
      cumV = rbind(cumV, as.data.frame(cbind(fqID[vv],x,n,m,m2,v,s)))
      rm(x,n,m,m2,v,s)
    }
    cumV = as.data.frame(cumV)
    unique(cumV$V1)
    plot3 = ggplot(cumV, aes(as.numeric(as.character(n)), as.numeric(as.character(s)), 
                             color = as.factor(V1), group = (V1)) )+
      geom_line()+
      scale_color_discrete(name = "OTB")+
      labs(y = "cumulative variance",x ="hourly SPL", title=siteDep, fill = "OTB")
    
    #outputs
    pv =  grid.arrange(plot1, plot3, ncol=1, nrow =2)
    #outputs
    perSum = rbind(perSum, melt(outD, id.vars = c("quants","siteDep"), measure.vars = as.character(OTB) ))
    infoT  = rbind(infoT,  c( fname, ndys, as.character(dwindow ) ) )
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_hSPLcumVar.png"), pv, dpi=300)
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_cumVar.png"), plot3, dpi=300)    
    #clean up
    rm(tst,ndys,inD,outD)
  }
  ###sites to start on March 01
  if (siteDep == "FK04_01") {
    dwindow =  c( as.Date("2019-03-01",format="%Y-%m-%d"), as.Date("2019-03-01",format="%Y-%m-%d") + 30) #start at beginning + 30 days
    tst = cdata[cdata$date >= dwindow[1] & cdata$date <= dwindow[2],]
    ndys = length( unique(tst$date) )
    inD =  tst[, 2:11]
    indx <- sapply(inD, is.factor)
    inD[indx] <- lapply(inD[indx], function(x) as.numeric(as.character(x)))
    outD = as.data.frame( apply( inD, 2 , quantile, probs = quants ))
    outD = cbind(outD,quants,siteDep)
    
    #spectrogram, tile plot
    tst2 = reshape2 :: melt(tst, id.var = "dtime", measure.vars = as.character(OTB) )
    tst2$dtime2 = as.POSIXct(tst2$dtime ,tz = "GMT")
    plot1 = ggplot(tst2, aes(dtime2, variable, fill= as.numeric(value))) + 
      geom_tile() +
      scale_fill_gradient(low="white", high="black",limits=c(70,120) ) +
      #scale_y_discrete(labels = ulabs) +
      labs(fill = "Sound Pressure Levels", title = siteDep) +
      xlab("") +
      ylab("Frequency [Hz]")
    
    #cumulative dynamic range, line plot (per frequency use inD, columns)
    cumV = NULL
    fqID = colnames(inD)
    for (vv in 1:length(fqID)){
      x = inD[,fqID[vv]] #should I un-dB?
      n <- seq_along(x)      ## cumulative sample size
      m <- cumsum(x) / n     ## cumulative mean check: inD[1,vv]+inD[2,vv]
      m2<- cumsum(x * x) / n ## cumulative squared mean plot(m2)
      v <- (m2 - m * m) * (n / (n - 1)) ## cumulative variance
      s <- sqrt(v)                      ## cumulative standard deviation
      cumV = rbind(cumV, as.data.frame(cbind(fqID[vv],x,n,m,m2,v,s)))
      rm(x,n,m,m2,v,s)
    }
    cumV = as.data.frame(cumV)
    unique(cumV$V1)
    plot3 = ggplot(cumV, aes(as.numeric(as.character(n)), as.numeric(as.character(s)), 
                             color = as.factor(V1), group = (V1)) )+
      geom_line()+
      scale_color_discrete(name = "OTB")+
      labs(y = "cumulative variance",x ="hourly SPL", title=siteDep, fill = "OTB")
    
    #outputs
    pv =  grid.arrange(plot1, plot3, ncol=1, nrow =2)
    #outputs
    perSum = rbind(perSum, melt(outD, id.vars = c("quants","siteDep"), measure.vars = as.character(OTB) ))
    infoT  = rbind(infoT,  c( fname, ndys, as.character(dwindow ) ) )
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_hSPLcumVar.png"), pv, dpi=300)
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_cumVar.png"), plot3, dpi=300)    
    #clean up
    rm(tst,ndys,inD,outD)
  }
  if (siteDep == "GR01_01") {
    dwindow =  c( as.Date("2019-03-01",format="%Y-%m-%d"), as.Date("2019-03-01",format="%Y-%m-%d") + 30) #start at beginning + 30 days
    tst = cdata[cdata$date >= dwindow[1] & cdata$date <= dwindow[2],]
    ndys = length( unique(tst$date) )
    inD =  tst[, 2:11]
    indx <- sapply(inD, is.factor)
    inD[indx] <- lapply(inD[indx], function(x) as.numeric(as.character(x)))
    outD = as.data.frame( apply( inD, 2 , quantile, probs = quants ))
    outD = cbind(outD,quants,siteDep)
    
    #spectrogram, tile plot
    tst2 = reshape2 :: melt(tst, id.var = "dtime", measure.vars = as.character(OTB) )
    tst2$dtime2 = as.POSIXct(tst2$dtime ,tz = "GMT")
    plot1 = ggplot(tst2, aes(dtime2, variable, fill= as.numeric(value))) + 
      geom_tile() +
      scale_fill_gradient(low="white", high="black",limits=c(70,120) ) +
      #scale_y_discrete(labels = ulabs) +
      labs(fill = "Sound Pressure Levels", title = siteDep) +
      xlab("") +
      ylab("Frequency [Hz]")
    
    #cumulative dynamic range, line plot (per frequency use inD, columns)
    cumV = NULL
    fqID = colnames(inD)
    for (vv in 1:length(fqID)){
      x = inD[,fqID[vv]] #should I un-dB?
      n <- seq_along(x)      ## cumulative sample size
      m <- cumsum(x) / n     ## cumulative mean check: inD[1,vv]+inD[2,vv]
      m2<- cumsum(x * x) / n ## cumulative squared mean plot(m2)
      v <- (m2 - m * m) * (n / (n - 1)) ## cumulative variance
      s <- sqrt(v)                      ## cumulative standard deviation
      cumV = rbind(cumV, as.data.frame(cbind(fqID[vv],x,n,m,m2,v,s)))
      rm(x,n,m,m2,v,s)
    }
    cumV = as.data.frame(cumV)
    unique(cumV$V1)
    plot3 = ggplot(cumV, aes(as.numeric(as.character(n)), as.numeric(as.character(s)), 
                             color = as.factor(V1), group = (V1)) )+
      geom_line()+
      scale_color_discrete(name = "OTB")+
      labs(y = "cumulative variance",x ="hourly SPL", title=siteDep, fill = "OTB")
    
    #outputs
    pv =  grid.arrange(plot1, plot3, ncol=1, nrow =2)
    #outputs
    perSum = rbind(perSum, melt(outD, id.vars = c("quants","siteDep"), measure.vars = as.character(OTB) ))
    infoT  = rbind(infoT,  c( fname, ndys, as.character(dwindow ) ) )
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_hSPLcumVar.png"), pv, dpi=300)
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_cumVar.png"), plot3, dpi=300)    
    #clean up
    rm(tst,ndys,inD,outD)
  }
  ###sites to start on March 01-- enough data?
  if (siteDep == "HI01_01") { #!!! not enough days, start sooner?
    dwindow =  c( as.Date("2019-03-01",format="%Y-%m-%d"), as.Date("2019-03-01",format="%Y-%m-%d") + 30) #start at beginning + 30 days
    tst = cdata[cdata$date >= dwindow[1] & cdata$date <= dwindow[2],]
    ndys = length( unique(tst$date) )
    inD =  tst[, 2:11]
    indx <- sapply(inD, is.factor)
    inD[indx] <- lapply(inD[indx], function(x) as.numeric(as.character(x)))
    outD = as.data.frame( apply( inD, 2 , quantile, probs = quants ))
    outD = cbind(outD,quants,siteDep)
    
    #spectrogram, tile plot
    tst2 = reshape2 :: melt(tst, id.var = "dtime", measure.vars = as.character(OTB) )
    tst2$dtime2 = as.POSIXct(tst2$dtime ,tz = "GMT")
    plot1 = ggplot(tst2, aes(dtime2, variable, fill= as.numeric(value))) + 
      geom_tile() +
      scale_fill_gradient(low="white", high="black",limits=c(70,120) ) +
      #scale_y_discrete(labels = ulabs) +
      labs(fill = "Sound Pressure Levels", title = siteDep) +
      xlab("") +
      ylab("Frequency [Hz]")
    
    #cumulative dynamic range, line plot (per frequency use inD, columns)
    cumV = NULL
    fqID = colnames(inD)
    for (vv in 1:length(fqID)){
      x = inD[,fqID[vv]] #should I un-dB?
      n <- seq_along(x)      ## cumulative sample size
      m <- cumsum(x) / n     ## cumulative mean check: inD[1,vv]+inD[2,vv]
      m2<- cumsum(x * x) / n ## cumulative squared mean plot(m2)
      v <- (m2 - m * m) * (n / (n - 1)) ## cumulative variance
      s <- sqrt(v)                      ## cumulative standard deviation
      cumV = rbind(cumV, as.data.frame(cbind(fqID[vv],x,n,m,m2,v,s)))
      rm(x,n,m,m2,v,s)
    }
    cumV = as.data.frame(cumV)
    unique(cumV$V1)
    plot3 = ggplot(cumV, aes(as.numeric(as.character(n)), as.numeric(as.character(s)), 
                             color = as.factor(V1), group = (V1)) )+
      geom_line()+
      scale_color_discrete(name = "OTB")+
      labs(y = "cumulative variance",x ="hourly SPL", title=siteDep, fill = "OTB")
    
    #outputs
    pv =  grid.arrange(plot1, plot3, ncol=1, nrow =2)
    #outputs
    perSum = rbind(perSum, melt(outD, id.vars = c("quants","siteDep"), measure.vars = as.character(OTB) ))
    infoT  = rbind(infoT,  c( fname, ndys, as.character(dwindow ) ) )
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_hSPLcumVar.png"), pv, dpi=300)
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_cumVar.png"), plot3, dpi=300)    
    #clean up
    rm(tst,ndys,inD,outD)
  }
  if (siteDep == "MB02_01") { 
    dwindow =  c( as.Date("2019-03-01",format="%Y-%m-%d"), as.Date("2019-03-01",format="%Y-%m-%d") + 30) #start at beginning + 30 days
    tst = cdata[cdata$date >= dwindow[1] & cdata$date <= dwindow[2],]
    ndys = length( unique(tst$date) )
    inD =  tst[, 2:11]
    indx <- sapply(inD, is.factor)
    inD[indx] <- lapply(inD[indx], function(x) as.numeric(as.character(x)))
    outD = as.data.frame( apply( inD, 2 , quantile, probs = quants ))
    outD = cbind(outD,quants,siteDep)
    
    #spectrogram, tile plot
    tst2 = reshape2 :: melt(tst, id.var = "dtime", measure.vars = as.character(OTB) )
    tst2$dtime2 = as.POSIXct(tst2$dtime ,tz = "GMT")
    plot1 = ggplot(tst2, aes(dtime2, variable, fill= as.numeric(value))) + 
      geom_tile() +
      scale_fill_gradient(low="white", high="black",limits=c(70,120) ) +
      #scale_y_discrete(labels = ulabs) +
      labs(fill = "Sound Pressure Levels", title = siteDep) +
      xlab("") +
      ylab("Frequency [Hz]")
    
    #cumulative dynamic range, line plot (per frequency use inD, columns)
    cumV = NULL
    fqID = colnames(inD)
    for (vv in 1:length(fqID)){
      x = inD[,fqID[vv]] #should I un-dB?
      n <- seq_along(x)      ## cumulative sample size
      m <- cumsum(x) / n     ## cumulative mean check: inD[1,vv]+inD[2,vv]
      m2<- cumsum(x * x) / n ## cumulative squared mean plot(m2)
      v <- (m2 - m * m) * (n / (n - 1)) ## cumulative variance
      s <- sqrt(v)                      ## cumulative standard deviation
      cumV = rbind(cumV, as.data.frame(cbind(fqID[vv],x,n,m,m2,v,s)))
      rm(x,n,m,m2,v,s)
    }
    cumV = as.data.frame(cumV)
    unique(cumV$V1)
    plot3 = ggplot(cumV, aes(as.numeric(as.character(n)), as.numeric(as.character(s)), 
                             color = as.factor(V1), group = (V1)) )+
      geom_line()+
      scale_color_discrete(name = "OTB")+
      labs(y = "cumulative variance",x ="hourly SPL", title=siteDep, fill = "OTB")
    
    #outputs
    pv =  grid.arrange(plot1, plot3, ncol=1, nrow =2)
    #outputs
    perSum = rbind(perSum, melt(outD, id.vars = c("quants","siteDep"), measure.vars = as.character(OTB) ))
    infoT  = rbind(infoT,  c( fname, ndys, as.character(dwindow ) ) )
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_hSPLcumVar.png"), pv, dpi=300)
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_cumVar.png"), plot3, dpi=300)   
    #clean up
    rm(tst,ndys,inD,outD)
  }
  if (siteDep == "SB01_02") { 
    dwindow =  c( as.Date("2019-03-01",format="%Y-%m-%d"), as.Date("2019-03-01",format="%Y-%m-%d") + 30) #start at beginning + 30 days
    tst  = cdata[cdata$date >= dwindow[1] & cdata$date <= dwindow[2],]
    ndys = length( unique(tst$date) )
    inD  =  tst[, 3:12] #frequency is different for this deployent? 16 Hz included- does not start at 31.5
    indx <- sapply(inD, is.factor)
    inD[indx] <- lapply(inD[indx], function(x) as.numeric(as.character(x)))
    outD = as.data.frame( apply( inD, 2 , quantile, probs = quants ))
    outD = cbind(outD,quants,siteDep)
    
    #spectrogram, tile plot
    tst2 = reshape2 :: melt(tst, id.var = "dtime", measure.vars = as.character(OTB) )
    tst2$dtime2 = as.POSIXct(tst2$dtime ,tz = "GMT")
    plot1 = ggplot(tst2, aes(dtime2, variable, fill= as.numeric(value))) + 
      geom_tile() +
      scale_fill_gradient(low="white", high="black",limits=c(70,120) ) +
      #scale_y_discrete(labels = ulabs) +
      labs(fill = "Sound Pressure Levels", title = siteDep) +
      xlab("") +
      ylab("Frequency [Hz]")
    
    #cumulative dynamic range, line plot (per frequency use inD, columns)
    cumV = NULL
    fqID = colnames(inD)
    for (vv in 1:length(fqID)){
      x = inD[,fqID[vv]] #should I un-dB?
      n <- seq_along(x)      ## cumulative sample size
      m <- cumsum(x) / n     ## cumulative mean check: inD[1,vv]+inD[2,vv]
      m2<- cumsum(x * x) / n ## cumulative squared mean plot(m2)
      v <- (m2 - m * m) * (n / (n - 1)) ## cumulative variance
      s <- sqrt(v)                      ## cumulative standard deviation
      cumV = rbind(cumV, as.data.frame(cbind(fqID[vv],x,n,m,m2,v,s)))
      rm(x,n,m,m2,v,s)
    }
    cumV = as.data.frame(cumV)
    unique(cumV$V1)
    plot3 = ggplot(cumV, aes(as.numeric(as.character(n)), as.numeric(as.character(s)), 
                             color = as.factor(V1), group = (V1)) )+
      geom_line()+
      scale_color_discrete(name = "OTB")+
      labs(y = "cumulative variance",x ="hourly SPL", title=siteDep, fill = "OTB")
    
    #outputs
    OTB = as.character(OTB)[2:11] #need to turncate because this site has measurement at 16 Hz!?
    pv =  grid.arrange(plot1, plot3, ncol=1, nrow =2)
    perSum = rbind(perSum, melt(outD, id.vars = c("quants","siteDep"), measure.vars = (OTB) ))
    infoT  = rbind(infoT,  c( fname, ndys, as.character(dwindow ) ) )
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_hSPLcumVar.png"), pv, dpi=300)
    ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, "_OTB_cumVar.png"), plot3, dpi=300)   
    #clean up
    rm(tst,ndys,inD,outD)
  }
  
  #not processing... MB02_02 or GR01_02
  
  #3) simple tile plot all data for deployment: x = time, y = frequency, fill = SPL (save out with title of deployment)
  #combine for plotting- rows = time, columns = frequency
  tst = reshape2 :: melt(cdata, id.vars = "dtime" , measure.vars = as.character(OTB) )
  tst$dtime2 = as.POSIXct(tst$dtime ,tz = "GMT")
  p1 = ggplot(tst, aes(dtime2, variable, fill= as.numeric(value))) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="black",limits=c(70,120) ) +
    #scale_y_discrete(labels = ulabs) +
    labs(fill = "Sound Pressure Levels", title = fname) +
    xlab("") +
    ylab("Frequency [Hz]") 
  ggsave(filename = paste0("D:\\RESEARCH\\SanctSound\\plots\\", fname, ".png"), p1, dpi=300)
  
  
  #5) details for gantt chart
  info = rbind(info,  c( fname, as.character(min(tmF)), as.character(max(tmF)), length(unique(as.Date(tmF)) )) )
  
  rm(p1,tst,cdata,tmF,OTB,fname)
}

#---------------------------------------------------
# GANTT chart for all deployments
#---------------------------------------------------
#https://www.molecularecologist.com/2019/01/simple-gantt-charts-in-r-with-ggplot2-and-the-tidyverse/
head(info)
colnames(info) = c("site","start","end","days")
info = as.data.frame(info)
gantt = reshape2 :: melt(info, id.vars =  "site", measure.vars = c("start","end") )

site2 = substr(gantt$site, start = 9 , stop = 15)
site3 =  substr(gantt$site, start = 9 , stop = 12)
deploy = substr(gantt$site, start = 14 , stop = 15)
gantt$date = as.Date(gantt$value)
gantt = cbind(gantt,site2,site3,deploy)

# Data window of analysis
D2plotS = c(as.Date("2019-03-01",format="%Y-%m-%d"),as.Date("2019-05-01",format="%Y-%m-%d"))
D2plotS = cbind(D2plotS, c(0,0))
colnames(D2plotS) = c("dateS","value")
D2plotC = as.data.frame(D2plotS)

ggplot(gantt, aes(date, site3,color = deploy)) +
  geom_line(size=10)+
  labs(x = "2019", y = "", title = "Deployment period")+
  geom_vline(xintercept = D2plotC$dateS[1],linetype="dashed", 
             color = "black", size=.3) +
  geom_vline(xintercept = D2plotC$dateS[2],linetype="dashed", 
             color = "black", size=.3) 

# GANTT chart for all deployments, only period of analysis
head(infoT)
colnames(infoT) = c("site","days", "start","end")
infoT = as.data.frame(infoT)
gantt = reshape2 :: melt(infoT, id.vars =  "site", measure.vars = c("start","end") )
site2 = substr(gantt$site, start = 9 , stop = 15)
site3 =  substr(gantt$site, start = 9 , stop = 12)
deploy = substr(gantt$site, start = 14 , stop = 15)
gantt$date = as.Date(gantt$value)
gantt = cbind(gantt,site2,site3,deploy)

ggplot(gantt, aes(date, site3, color = deploy)) +
  geom_line(size=10)+
  labs(x = "2019", y = "",title = "Analysis period")+
  geom_vline(xintercept = D2plotC$dateS[1],linetype="dashed", 
             color = "black", size=.3) +
  geom_vline(xintercept = D2plotC$dateS[2],linetype="dashed", 
             color = "black", size=.3) 

#---------------------------------------------------
# PLOT OF Percentiles from SPLS across octave bands
#---------------------------------------------------
head(perSum)
perSum$siteDep
ggplot(perSum, aes(variable, value, color = as.factor(quants), group = (quants)) ) +
  geom_line(size=2)+
  #geom_point(size=1)+
  scale_color_discrete(name = "Percentiles")+
  facet_grid(~as.factor(siteDep))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Frequency", y = "Sound Pressure Level dB re: 1uPa",title = "")

