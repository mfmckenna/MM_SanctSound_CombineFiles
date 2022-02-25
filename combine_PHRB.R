library(data.table)

#PM08_12
#------------------------------------------------------------------------------------------
#OL
dirPHRB     = "E:\\RESEARCH\\SanctSound\\data\\PM\\PM08\\PM08_12"
Fpattern = "_OL_1h_Part"
list.files(path=dirPHRB, pattern = Fpattern, full.names=TRUE, recursive = TRUE)
multmerge = function(path){
  filenames = list.files(path=path, pattern = Fpattern, full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
all_OL = multmerge(dirPHRB)
fname =  "E:\\RESEARCH\\SanctSound\\data\\PM\\PM08\\PM08_12\\SanctSound_PM08_12_OL_1h.csv"
write.csv(all_OL,fname,row.names=FALSE) 
#BB
dirPHRB     = "E:\\RESEARCH\\SanctSound\\data\\PM\\PM08\\PM08_12"
Fpattern = "_BB_1h_Part"
multmerge = function(path){
  filenames = list.files(path=path, pattern = Fpattern, full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
all_BB = multmerge(dirPHRB)
fname =  "E:\\RESEARCH\\SanctSound\\data\\PM\\PM08\\PM08_12\\SanctSound_PM08_12_BB_1h.csv"
write.csv(all_BB,fname,row.names=FALSE) 
#TOL
dirPHRB     = "E:\\RESEARCH\\SanctSound\\data\\PM\\PM08\\PM08_12"
Fpattern = "_TOL_1h_Part"
multmerge = function(path){
  filenames = list.files(path=path, pattern = Fpattern, full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
all_BB = multmerge(dirPHRB)
fname =  "E:\\RESEARCH\\SanctSound\\data\\PM\\PM08\\PM08_12\\SanctSound_PM08_12_TOL_1h.csv"
write.csv(all_BB,fname,row.names=FALSE) 

