# Listening range analysis-- CI

OUT = readxl::read_xlsx("C:\\Users\\mckenna6\\Google Drive\\ActiveProjects\\SANCTSOUND\\LR_125Hz\\CI_125Hz_LR_SNR_0dB.xlsx")

usite = unique(OUT$Site)
OUTsum = NULL
for (ss in 1:length(usite)){
  tmp = OUT[OUT$Site == usite[ss],]
  #summary(tmp)
  avg = mean(tmp$`Listening Range, km`) 
  std = sd(tmp$`Listening Range, km` )
  
  zer = sum(tmp$`Listening Range, km` == 0 )/nrow(tmp)
  greater10km = sum(tmp$`Listening Range, km` >10000 )/nrow(tmp)
  
  OUTsum = rbind(OUTsum, c(usite[ss],round(avg), round(std), round(zer,2), round(greater10km, 2) ) )
}
colnames( OUTsum ) = c("Site","avgRange_km","sdRange_km", "Percent0","Percent>10km")
OUTsum = as.data.frame( OUTsum )
OUTsum
