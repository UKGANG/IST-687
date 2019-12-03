source("https://raw.githubusercontent.com/UKGANG/IST-687/master/analysis/visualization/Base_Map_Render.R")
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/analysis/visualization/Airline_Route_Module.R")
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/analysis/visualization/City_Frequency_Module.R")

# Map render configuration
map.render.zoom.x=c(-160,-65)
map.render.zoom.y=c(14,64)
map.render.alpha = 0.39
map.render.size = 1.7 

airlineCnt <- rawData %>% 
  add_count(Flight.Ticket.Partner.Code) %>% 
  select(Flight.Ticket.Partner.Name, Flight.Ticket.Partner.Code ,n) %>% 
  distinct() %>% 
  arrange(desc(n));
airlineCnt;

airlineCnt %>% 
  select(Flight.Ticket.Partner.Code)

partnerFilter <- function(data) {
    return (filter(data, Flight.Ticket.Partner.Code %in% c(""
                                                           # , "WN" 
                                                           # , "DL" 
                                                           # , "OO" 
                                                           # , "EV" 
                                                           # , "OU" 
                                                            , "US" 
                                                           # , "AA" 
                                                           # , "MQ" 
                                                           # , "B6" 
                                                           # , "AS" 
                                                           # , "FL" 
                                                           # , "F9" 
                                                           # , "VX" 
                                                           # , "HA" 
                                                           )));
}

renderFlightInformation(
  partnerFilter, 
  layerRenders =c(flightRouteRender, cityFrequencyRender))
