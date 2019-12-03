echo = F
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/dummy_builder.R")
# trainList <- createDataPartition(y=goodDiamonds$cut,p=.67,list=FALSE)
installLibrary("lsr")

memberClassFilter <- function(data, type) {
  return (filter(rawData, Flight.Airline.Membership.Class == type))
}
sType <- memberClassFilter(rawData, "Silver")
bType <- memberClassFilter(rawData, "Blue")
gType <- memberClassFilter(rawData, "Gold")
pType <- memberClassFilter(rawData, "Platinum")

# H0 Travel preferences on distance are the same *
# H1 Travel perferences on distance are different

sStat <- sType$Flight.Distance.Cnt
bStat <- bType$Flight.Distance.Cnt
gStat <- gType$Flight.Distance.Cnt
pStat <- pType$Flight.Distance.Cnt

t.test(bStat,sStat)
t.test(bStat,gStat)
t.test(bStat,pStat)

t.test(sStat,gStat)
t.test(sStat,pStat)
t.test(gStat,pStat)

# H0 Travel preferences on price sensitiveness are the same 
# H1 Travel perferences on price sensitiveness are different * 

sStat <- sType$Person.Price.Sensitivity
bStat <- bType$Person.Price.Sensitivity
gStat <- gType$Person.Price.Sensitivity
pStat <- pType$Person.Price.Sensitivity

# Blue group is significant different with others
t.test(bStat,sStat)
t.test(bStat,gStat)
t.test(bStat,pStat)

t.test(sStat,gStat)
t.test(sStat,pStat)
t.test(gStat,pStat)

# H0 Travel preferences on loyalty are the same 
# H1 Travel perferences on loyalty are different * 

sStat <- sType$Person.Loyalty
bStat <- bType$Person.Loyalty
gStat <- gType$Person.Loyalty
pStat <- pType$Person.Loyalty

t.test(bStat,sStat)
t.test(bStat,gStat)
t.test(bStat,pStat)

t.test(sStat,gStat)
t.test(sStat,pStat)
t.test(gStat,pStat)

# H0 Travel preferences on shopping amount are the same *
# H1 Travel perferences on shopping amount are different 

sStat <- sType$Flight.Airport.Shopping.Amount
bStat <- bType$Flight.Airport.Shopping.Amount
gStat <- gType$Flight.Airport.Shopping.Amount
pStat <- pType$Flight.Airport.Shopping.Amount

t.test(bStat,sStat)
t.test(bStat,gStat)
t.test(bStat,pStat)

t.test(sStat,gStat)
t.test(sStat,pStat)
t.test(gStat,pStat)

# H0 Travel preferences on food spending are the same 
# H1 Travel perferences on food spending are different *

sStat <- sType$Flight.Airport.Food.Amount
bStat <- bType$Flight.Airport.Food.Amount
gStat <- gType$Flight.Airport.Food.Amount
pStat <- pType$Flight.Airport.Food.Amount

t.test(bStat,sStat)
t.test(bStat,gStat)
t.test(bStat,pStat)

t.test(sStat,gStat)
t.test(sStat,pStat)
t.test(gStat,pStat)