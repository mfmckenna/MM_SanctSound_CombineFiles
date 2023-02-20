library(scatterpie)

# PIES ON MAP FOR SONAR DATA IN HAWAII
#https://cran.r-project.org/web/packages/scatterpie/vignettes/scatterpie.html


#all site lat/longs
siteLoc = read.csv("E:\\RESEARCH\\SanctSound\\data\\SiteLocations.csv")
siteLoc = siteLoc[,1:3]
siteLoc = siteLoc[c(13,15,16,26),]

siteLoc$sonarY = c(0.7,7, 8.5,7.5)
siteLoc$dys    = c(135/365,185/365,164/365,67/365)
siteLoc$sonarN = c(100-0.7,100-7, 100-8.5, 100-7.5)

ggplot() + geom_scatterpie(aes(x=lon, y=lat), data=siteLoc, cols=c("sonarY","sonarN" ))  + coord_equal()

P = ggplot() + geom_scatterpie(aes(x=x, y=y) , data=cData, cols=c("Small","Medium","Large" )) + coord_equal() +
  geom_text(data = cData, aes(x=x, y=y, label = SiteCnt ), vjust = 0, nudge_y = 0.5, size = 2)  
P #copied to illustrator to put this graphics together-- because so much overlap in sites on this graphic... really only works for within sanctuary comparisons


world <- map_data('world')
p = ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="gray90", color="black") +
  geom_point(data = siteLoc, aes(x=lon, y=lat), color = "red", size = 1) +
  geom_text(data = siteLoc, aes(x=lon+1.1, y=lat+1.1,  label = Site ), size = 3)  +
  coord_quickmap()+
  xlim(-170,-150) + ylim(18, 30) +
  theme_minimal()
p #copied to illustrator to put this graphics together-- because so much overlap in sites on this graphic... really only works for within sanctuary comparisons

p2 = p +  geom_scatterpie(aes(x=lon+.5, y=lat+.5, r = dys), data=siteLoc, cols=c("sonarY","sonarN" ))
  
  p2

#p2 + geom_scatterpie_legend(siteLoc$dys, x=-170, y=20)

