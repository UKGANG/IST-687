echo = F
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/Data_Cleaner.R")
renderFlightInformation <- function(layerRenders) {
  installLibrary("tidyverse")
  installLibrary("ggplot")

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
  
  pic <- map_data("world") %>% 
    filter(`region` != "Antarctica") %>% 
    ggplot(aes(long, lat, group = group)) + 
    geom_polygon(fill="gray17", color = "white", size=0.15) 
  funcs <- layerRenders
  if (!is.na(funcs) & !is.vector(funcs)) {
    funcs = c(funcs)
  }
  for (func in funcs) {
    pic <- pic + func()
  }
  pic <- pic+geom_curve(data = airlineRoute
               , aes(x = Geom.Departure.Airport.Longitude, xend = Geom.Arrival.Airport.Longitude,
                     y = Geom.Departure.Airport.Latitude,  yend = Geom.Arrival.Airport.Latitude), 
               alpha=0.25, size=0.1, color = "white", inherit.aes = FALSE) 
  pic <- pic + scale_size_manual(values = c(0.05, 0.01) ) + 
    theme_void() + 
    theme(plot.background=element_rect(fill="gray12"), legend.position="none") + 
    coord_sf(xlim=c(-180,-65), ylim=c(10,72))
  
  # Plot
  pic
}
