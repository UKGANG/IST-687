echo=F
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/dummy_builder.R")

createIQRBox <- function(data, x, y) {
  x = data[,which(colnames(rawData) == x)];
  y = data[,which(colnames(rawData) == y)];
  ggplot() +
    aes(x=x, y=y) +
    geom_boxplot() +
    scale_y_continuous(labels=scales::comma)
}

createIQRBox(rawData, "Flight.Ticket.Partner.Code", "Person.Age")
createIQRBox(rawData, "Flight.Ticket.Partner.Code", "Person.Price.Sensitivity")
createIQRBox(rawData, "Flight.Ticket.Partner.Code", "Person.First.Flight.Year")
# Outlier
createIQRBox(rawData, "Flight.Ticket.Partner.Code", "Person.Flights.Per.Year")
# Outlier
createIQRBox(rawData, "Flight.Ticket.Partner.Code", "Person.Total.Freq.Flyer.Accts")
# Outlier Null
createIQRBox(rawData, "Flight.Ticket.Partner.Code", "Flight.Departure.Delay.Minute")
# Outlier Null
createIQRBox(rawData, "Flight.Ticket.Partner.Code", "Flight.Arrival.Delay.Minute")
# Outlier Null
createIQRBox(rawData, "Flight.Ticket.Partner.Code", "Flight.Time.Minutes")
# Outlier
createIQRBox(rawData, "Flight.Ticket.Partner.Code", "Flight.Distance.Cnt")
# Outlier
createIQRBox(rawData, "Flight.Ticket.Partner.Code", "Flight.Airport.Shopping.Amount")
# Outlier
createIQRBox(rawData, "Flight.Ticket.Partner.Code", "Flight.Airport.Food.Amount")