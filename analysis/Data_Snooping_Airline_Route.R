source("https://raw.githubusercontent.com/UKGANG/IST-687/master/report/Project_Code.R", echo=FALSE)

installLibrary("ggmap")
installLibrary("sqldf")

airlineRoute <- rawData %>% 
  select(c("Geom.Depature.Airport.Latitude"
           , "Geom.Depature.Airport.Longitude"
           , "Geom.Arrival.Airport.Latitude"
           , "Geom.Arrival.Airport.Longitude")) %>% 
  distinct()
tmpAirlineRoute <- airlineRoute
colnames(tmpAirlineRoute) <- c("dLat", "dLong", "aLat", "aLong")
sqldf("SELECT DISTINCT dLat, dLong, aLat, aLong FROM tmpAirlineRoute")

# 1928
v <- 1:(1928/2)

tmp <- data.frame(x=c(1,1,1), y=c(1,1,1),xend=2:4, yend=2:4)
ggplot(tmp) + 
  geom_curve(aes(x = x, y = y
                 , xend = xend, yend = yend
                 , colour = "curve"), curvature = -0.2)

for (i in 1:1928) {
  ggplot(airlineRoute) + 
    geom_curve(aes(x = Geom.Depature.Airport.Latitude[i], y = Geom.Depature.Airport.Longitude[i]
                   , xend = Geom.Arrival.Airport.Latitude[i], yend = Geom.Arrival.Airport.Longitude[i]
                   , colour = "curve"), curvature = -0.2)
}
ggplot(airlineRoute) + 
  geom_curve(aes(x = Geom.Depature.Airport.Latitude, y = Geom.Depature.Airport.Longitude
                 , xend = Geom.Arrival.Airport.Latitude, yend = Geom.Arrival.Airport.Longitude
                 , colour = "curve"), curvature = -0.2) +
  geom_polygon(data = map_data("world"), aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

distinct()


