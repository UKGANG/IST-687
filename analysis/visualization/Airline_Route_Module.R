flightRouteRender <- function(dataFilter) {
  map.render.alpha.default <- 0.39
  flightRouteRender.alpha <- ifelse(exists("map.render.alpha"), map.render.alpha, map.render.alpha.default)
  airlineRoute <- rawData %>% 
    dataFilter() %>%
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
                     , alpha=flightRouteRender.alpha, size=0.1, color = "white", inherit.aes = FALSE))
}
