source("https://raw.githubusercontent.com/UKGANG/IST-687/master/analysis/visualization/Map_Render.R")

flightRouteRender <- function() {
  return (geom_curve(data = airlineRoute, aes(x = Geom.Departure.Airport.Longitude, xend = Geom.Arrival.Airport.Longitude
                                              ,y = Geom.Departure.Airport.Latitude,  yend = Geom.Arrival.Airport.Latitude)
                     ,alpha=0.25, size=0.1, color = "white", inherit.aes = FALSE))
}


renderFlightInformation(flightRouteRender)
