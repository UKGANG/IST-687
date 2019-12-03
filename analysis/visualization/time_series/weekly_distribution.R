echo = F
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/dummy_builder.R")

ggplot(rawData) + 
  aes(x=weekDay) + 
  geom_histogram(binwidth = 1, color="white",alpha=0.5, position="identity")

