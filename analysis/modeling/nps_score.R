echo=F
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/Data_Cleaner.R")

partnerDf <- rawData %>%
  mutate(
    detractor = ifelse(Recommend.Likelihood < 7, 1, 0)
    , passive = ifelse(Recommend.Likelihood == 7 | Recommend.Likelihood == 8, 1, 0)
    , promoter = ifelse(Recommend.Likelihood > 8, 1, 0)) %>%
  group_by(Flight.Ticket.Partner.Code) %>%
  add_count() %>% 
  summarise(detractor = sum(detractor)/mean(n)
            , passive = sum(passive)/mean(n)
            , promoter = sum(promoter)/mean(n)
            , n = mean(n)
            ) %>%
  mutate(length = n
         ,frequency = n / sum(n))
partnerNameMap <- unique(select(rawData, c("Flight.Ticket.Partner.Name", "Flight.Ticket.Partner.Code")))
partnerDf <- partnerDf %>% 
  merge(partnerNameMap, by="Flight.Ticket.Partner.Code") %>% 
  select(Flight.Ticket.Partner.Code
         , Flight.Ticket.Partner.Name
         , length 
         , frequency
         , detractor
         , passive
         , promoter
         ) %>% 
  mutate(
    overall_nps = promoter - detractor
    , weighted_nps = promoter - 1.5 * detractor) %>%
  arrange(desc(length))

View(partnerDf)