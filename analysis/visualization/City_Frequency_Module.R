cityFrequencyRender <- function(dataFilter) {
  map.render.size.default <- 2
  size <- ifelse(exists("map.render.size"), map.render.size, map.render.size.default)
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
    ))
  
  departureAirports <- airlineRoute %>% 
    select(c(
      "Geom.Departure.Airport.Longitude"
      , "Geom.Departure.Airport.Latitude"
      , "Geom.Departure.State"
      , "Geom.Departure.State.Abbr"
      , "Geom.Departure.City.Label"
    )) %>% 
    setNames(c(
      "Geom.Airport.Longitude"
      , "Geom.Airport.Latitude"
      , "Geom.State"
      , "Geom.State.Abbr"
      , "Geom.City.Label"
    ))

    
  arrivalAirports <- airlineRoute %>% 
    select(c(
      "Geom.Arrival.Airport.Longitude"
      , "Geom.Arrival.Airport.Latitude"
      , "Geom.Arrival.State"
      , "Geom.Arrival.State.Abbr"
      , "Geom.Arrival.City.Label"
    )) %>% 
    setNames(c(
      "Geom.Airport.Longitude"
      , "Geom.Airport.Latitude"
      , "Geom.State"
      , "Geom.State.Abbr"
      , "Geom.City.Label"
    ))

  airports <- rbind(departureAirports, arrivalAirports) %>% 
    group_by_at(vars(Geom.State, Geom.State.Abbr, Geom.City.Label)) %>% 
    summarise(Geom.Airport.Longitude = mean(Geom.Airport.Longitude)
              ,Geom.Airport.Latitude = mean(Geom.Airport.Latitude)) %>% 
    add_tally() %>% 
    as.data.frame() %>% 
    arrange(desc(n))

  flightCnt <- rbind(departureAirports, arrivalAirports) %>% 
    group_by_at(vars(Geom.State.Abbr)) %>% 
    summarise(Geom.Airport.Longitude = mean(Geom.Airport.Longitude)
              ,Geom.Airport.Latitude = mean(Geom.Airport.Latitude)
              , n = n()) %>% 
    mutate(q = percent_rank(n)) 

  return (geom_point(data=flightCnt,shape=21, inherit.aes = F, color = "gray", fill="white"
                     , size=exp(flightCnt$q * size) * size
                     , aes(x = Geom.Airport.Longitude, y = Geom.Airport.Latitude, alpha = q)))
}
