echo = F
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/Data_Cleaner.R")
renderFlightInformation <- function(layerRenders) {
  installLibrary("tidyverse")
  installLibrary("ggplot")

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
  pic <- pic + scale_size_manual(values = c(0.05, 0.01) ) + 
    theme_void() + 
    theme(plot.background=element_rect(fill="gray12"), legend.position="none") + 
    coord_sf(xlim=c(-180,-65), ylim=c(10,72))
  
  # Plot
  pic
}
