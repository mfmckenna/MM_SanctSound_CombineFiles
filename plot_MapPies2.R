library(scatterpie)

# PIES ON MAP FOR SONAR DATA IN HAWAII
#https://cran.r-project.org/web/packages/scatterpie/vignettes/scatterpie.html


#all site lat/longs
siteLoc = read.csv("E:\\RESEARCH\\SanctSound\\data\\SiteLocations.csv")
siteLoc = siteLoc[,1:3]
siteLoc = siteLoc[c(13,15:18,26:27,30),]

#siteLoc$sonarY = c(0.7,7,8.5,7.5)
#siteLoc$dys    = c(135/365,185/365,164/365,67/365)
#siteLoc$sonarN = c(100-0.7,100-7, 100-8.5, 100-7.5)

siteLoc$dys2    = c(3240/24, 4440/24,3912/24, 624/24 ,5127/24, 792/24, 2931/24 ,3075/24) #bubble size as days analyzed
siteLoc$Names  = c("Olowalu","Oahu","Kauai","Penguin Bank","Hawaii Island","Middle Bank","French Frigate Shoals","Gardner Pinnacles")
siteLoc$sonarY = c(0.7, 7, 8.5, 0, 0, 7.5, 0, 0) # percent of days with sonar
siteLoc$sonarN = 100 - siteLoc$sonarY

#siteLoc$sonarY = c(1.3,14.0006,22.3,0,0,8.72,0,0)
#siteLoc$sonarN = siteLoc$hrs - siteLoc$sonarY 
#siteLoc$sonarYp = siteLoc$sonarY/siteLoc$hrs *100
#siteLoc$sonarNp = siteLoc$sonarN/siteLoc$hrs *100

ggplot() + geom_scatterpie(aes(x=lon, y=lat), data=siteLoc, cols=c("sonarY","sonarN" ))  + coord_equal()


world <- map_data('world')
p = ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="gray90", color="black") +
  geom_point(data = siteLoc, aes(x=lon, y=lat), color = "red", size = 1) +
  geom_text(data = siteLoc, aes(x=lon, y=lat+.5,  label = Names ), size = 3)  +
  coord_quickmap()+
  xlim(-170,-150) + ylim(18, 30) +
  theme_minimal()
p

p2 = p +  geom_scatterpie(aes(x=lon, y=lat-1, r=dys2/365), data=siteLoc, cols=c("sonarY","sonarN" ))
  
  p2

#p2 + geom_scatterpie_legend(siteLoc$dys, x=-170, y=20)

