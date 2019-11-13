source("https://raw.githubusercontent.com/UKGANG/IST-687/master/analysis/visualization/Map_Render.R")

flightRouteRender <- function() {
  airlineRoute <- rawData %>% 
    select(c(
      "Geom.Arrival.Airport.Longitude"
      , "Geom.Arrival.Airport.Latitude"
      , "Geom.Arrival.State"
      , "Geom.Arrival.State.Abbr"
      , "Geom.Arrival.City.Label"
      , "Geom.Departure.Airport.Longitude"
      , "Geom.Departure.Airport.Latitude"
      , "Geom.Departure.State"
      , "Geom.Departure.State.Abbr"
      , "Geom.Departure.City.Label"
    )) %>% 
    group_by_all() %>% 
    add_tally() %>% 
    distinct()
  return (geom_curve(data = airlineRoute, aes(x = Geom.Departure.Airport.Longitude, xend = Geom.Arrival.Airport.Longitude
                                              ,y = Geom.Departure.Airport.Latitude,  yend = Geom.Arrival.Airport.Latitude)
                     ,alpha=0.25, size=0.1, color = "white", inherit.aes = FALSE))
}


renderFlightInformation(flightRouteRender)
