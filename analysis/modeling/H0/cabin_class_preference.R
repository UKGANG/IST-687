echo = F
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/dummy_builder.R")
# trainList <- createDataPartition(y=goodDiamonds$cut,p=.67,list=FALSE)
installLibrary("lsr")

cabinClassFilter <- function(data, type) {
  return (filter(rawData, Flight.Cabin.Class == type))
}
eType <- cabinClassFilter(rawData, "Eco")
ebType <- cabinClassFilter(rawData, "Eco Plus")
bType <- cabinClassFilter(rawData, "Business")

# H0 Travel preferences on distance are the same *
# H1 Travel perferences on distance are different

eStat <- eType$Flight.Distance.Cnt
ebStat <- ebType$Flight.Distance.Cnt
bStat <- bType$Flight.Distance.Cnt

t.test(eStat,ebStat)
t.test(eStat,bStat)
t.test(ebStat,bStat)

# H0 Travel preferences on price sensitiveness are the same 
# H1 Travel perferences on price sensitiveness are different * 

eStat <- eType$Person.Price.Sensitivity
ebStat <- ebType$Person.Price.Sensitivity
bStat <- bType$Person.Price.Sensitivity

t.test(eStat,ebStat)
t.test(eStat,bStat)
t.test(ebStat,bStat)

# H0 Travel preferences on loyalty are the same 
# H1 Travel perferences on loyalty are different * 

eStat <- eType$Person.Loyalty
ebStat <- ebType$Person.Loyalty
bStat <- bType$Person.Loyalty

t.test(eStat,ebStat)
t.test(eStat,bStat)
t.test(ebStat,bStat)

# H0 Travel preferences on shopping amount are the same *
# H1 Travel perferences on shopping amount are different 

eStat <- eType$Flight.Airport.Shopping.Amount
ebStat <- ebType$Flight.Airport.Shopping.Amount
bStat <- bType$Flight.Airport.Shopping.Amount

t.test(eStat,ebStat)
t.test(eStat,bStat)
t.test(ebStat,bStat)

# H0 Travel preferences on food spending are the same 
# H1 Travel perferences on food spending are different *

eStat <- eType$Flight.Airport.Food.Amount
ebStat <- ebType$Flight.Airport.Food.Amount
bStat <- bType$Flight.Airport.Food.Amount

t.test(eStat,ebStat)
t.test(eStat,bStat)
t.test(ebStat,bStat)
