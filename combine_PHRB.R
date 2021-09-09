

#also stitch together files
dirPHRB     = "E:\\RESEARCH\\SanctSound\\data\\PH\\PHRB\\PHRB_12"
Fpattern = "_OL_1h_Part"
multmerge = function(path){
  filenames = list.files(path=path, pattern = Fpattern, full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
all_OL = multmerge(dirPHRB)
fname =  "E:\\RESEARCH\\SanctSound\\data\\PH\\PHRB\\PHRB_12\\SanctSound_PHRB_12_OL_1h.csv"
write.csv(all_OL,fname,row.names=FALSE) 


#also stitch together files
dirPHRB     = "E:\\RESEARCH\\SanctSound\\data\\PH\\PHRB\\PHRB_12"
Fpattern = "_BB_1h_Part"
multmerge = function(path){
  filenames = list.files(path=path, pattern = Fpattern, full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
all_BB = multmerge(dirPHRB)
fname =  "E:\\RESEARCH\\SanctSound\\data\\PH\\PHRB\\PHRB_12\\SanctSound_PHRB_12_BB_1h.csv"
write.csv(all_BB,fname,row.names=FALSE) 