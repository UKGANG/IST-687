echo = F
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/Data_Cleaner.R")
renderFlightInformation <- function(layerRenders=NA) {
  installLibrary("tidyverse")
  installLibrary("ggplot2")

  map.render.zoom.x.default <- c(-180,-65)
  map.render.zoom.y.default <- c(10,72)
  renderFlightInformation.zoom.x <- map.render.zoom.x.default
  renderFlightInformation.zoom.y <- map.render.zoom.y.default
  if(exists("map.render.zoom.x")) {
    renderFlightInformation.zoom.x <- map.render.zoom.x
  }
  if(exists("map.render.zoom.y")) {
    renderFlightInformation.zoom.y <- map.render.zoom.y
  }
  pic <- map_data("world") %>% 
    filter(`region` != "Antarctica") %>% 
    ggplot(aes(long, lat, group = group)) + 
    geom_polygon(fill="gray17", color = "white", size=0.15) + 
    geom_polygon(data = map_data("state"), fill="gray17", color = "white", size=0.15) 
  funcs <- layerRenders
  if (!is.na(funcs)) {
    if (!is.vector(funcs)) {
      funcs = c(funcs)
    }
    for (func in funcs) {
      pic <- pic + func()
    }
  }
  pic <- pic + scale_size_manual(values = c(0.05, 0.01) ) + 
    theme_void() + 
    theme(plot.background=element_rect(fill="gray12"), legend.position="none") + 
    coord_sf(xlim=renderFlightInformation.zoom.x, ylim=renderFlightInformation.zoom.y)
  
  # Plot
  pic

}
