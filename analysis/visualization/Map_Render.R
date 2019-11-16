source("https://raw.githubusercontent.com/UKGANG/IST-687/master/analysis/visualization/Base_Map_Render.R")
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/analysis/visualization/Airline_Route_Module.R")
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/analysis/visualization/City_Frequency_Module.R")

map.render.zoom.x=c(-160,-65)
map.render.zoom.y=c(14,64)
map.render.alpha = 0.19
map.render.size = 1.7 

renderFlightInformation(c(flightRouteRender, cityFrequencyRender))

renderFlightInformation(c(flightRouteRender, cityFrequencyRender))