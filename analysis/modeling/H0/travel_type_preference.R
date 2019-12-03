echo = F
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/dummy_builder.R")
# trainList <- createDataPartition(y=goodDiamonds$cut,p=.67,list=FALSE)
installLibrary("lsr")
travelTypeFilter <- function(data, type) {
  return (filter(rawData, Flight.Travel.Type == type))
}
bType <- travelTypeFilter(rawData, "Business travel")
mType <- travelTypeFilter(rawData, "Mileage tickets")
pType <- travelTypeFilter(rawData, "Personal Travel")

# H0 Travel preferences on distance are the same *
# H1 Travel perferences on distance are different
ggplot(rawData) + 
  aes(x=Flight.Distance.Cnt) + 
  geom_histogram(binwidth = 50, color="white", alpha=0.5, position="identity") + 
  facet_grid(Flight.Travel.Type~., margins = FALSE, scales = "free_y") + 
  scale_y_continuous() +
  geom_vline(data=bType, aes(xintercept=quantile(bType$Flight.Distance.Cnt, 0.05), group=Flight.Travel.Type), linetype="dotted", color="blue") +
  geom_vline(data=mType, aes(xintercept=quantile(mType$Flight.Distance.Cnt, 0.05)), linetype="dotted", color="orange") +
  geom_vline(data=pType, aes(xintercept=quantile(pType$Flight.Distance.Cnt, 0.05)), linetype="dotted", color="red") +
  geom_vline(data=bType, aes(xintercept=quantile(bType$Flight.Distance.Cnt, 0.5)), color="blue") +
  geom_vline(data=mType, aes(xintercept=quantile(mType$Flight.Distance.Cnt, 0.5)), color="orange") +
  geom_vline(data=pType, aes(xintercept=quantile(pType$Flight.Distance.Cnt, 0.5)), color="red") +
  geom_vline(data=bType, aes(xintercept=quantile(bType$Flight.Distance.Cnt, 0.95)), linetype="dotted", color="blue") +
  geom_vline(data=mType, aes(xintercept=quantile(mType$Flight.Distance.Cnt, 0.95)), linetype="dotted", color="orange") +
  geom_vline(data=pType, aes(xintercept=quantile(pType$Flight.Distance.Cnt, 0.95)), linetype="dotted", color="red") 

bStat <- bType$Flight.Distance.Cnt
mStat <- mType$Flight.Distance.Cnt
pStat <- pType$Flight.Distance.Cnt

t.test(bStat,mStat)
t.test(bStat,pStat)
t.test(mStat,pStat)

# H0 Travel preferences on price sensitiveness are the same 
# H1 Travel perferences on price sensitiveness are different * 

bStat <- bType$Person.Price.Sensitivity
mStat <- mType$Person.Price.Sensitivity
pStat <- pType$Person.Price.Sensitivity

t.test(bStat,mStat)
t.test(bStat,pStat)
t.test(mStat,pStat)

# H0 Travel preferences on loyalty are the same 
# H1 Travel perferences on loyalty are different * 

bStat <- bType$Person.Loyalty
mStat <- mType$Person.Loyalty
pStat <- pType$Person.Loyalty

t.test(bStat,mStat)
t.test(bStat,pStat)
t.test(mStat,pStat)

# H0 Travel preferences on shopping amount are the same *
# H1 Travel perferences on shopping amount are different 

bStat <- bType$Flight.Airport.Shopping.Amount
mStat <- mType$Flight.Airport.Shopping.Amount
pStat <- pType$Flight.Airport.Shopping.Amount

t.test(bStat,mStat)
t.test(bStat,pStat)
t.test(mStat,pStat)

# H0 Travel preferences on food spending are the same 
# H1 Travel perferences on food spending are different *

bStat <- bType$Flight.Airport.Food.Amount
mStat <- mType$Flight.Airport.Food.Amount
pStat <- pType$Flight.Airport.Food.Amount

t.test(bStat,mStat)
t.test(bStat,pStat)
t.test(mStat,pStat)
