source("https://raw.githubusercontent.com/UKGANG/IST-687/master/report/Project_Code.R", echo=FALSE)

installLibrary("ggmap")

airlineRoute <- rawData %>% 
  select(c("Geom.Depature.Airport.Latitude"
           , "Geom.Depature.Airport.Longitude"
           , "Geom.Arrival.Airport.Latitude"
           , "Geom.Arrival.Airport.Longitude")) %>% 
  distinct()

ggplot(airlineRoute) + 
  geom_curve(aes(x = Geom.Depature.Airport.Latitude, y = Geom.Depature.Airport.Longitude
                 , xend = Geom.Arrival.Airport.Latitude, yend = Geom.Arrival.Airport.Longitude
                 , colour = "curve"), curvature = -0.2) +
  geom_polygon(data = map_data("world"), aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

distinct()


