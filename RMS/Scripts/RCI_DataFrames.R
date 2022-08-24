#Load in prackages
library(dplyr)
load(file = 'RMSws.RData')

#QSRs=====Insight 1------------------------------------------------------------------------------------------------------------------------#

#Extract percentages of more QSR visits by generation

QSR_Visits <- Data %>% 
  group_by(Generation) %>%
  count(QSR) %>%
  mutate(percent = paste0(round((n/sum(n))*100),'%')) %>%
  filter(QSR == 'more') %>%
  select(Generation,QSR,percent)


#Drive Thru visits=====Insight 4-----------------------------------------------------------------------------------------------------------#

#Extract percentages of less value visits by generation

Restaurant_Value <- Data %>% 
  group_by(Generation) %>%
  count(Value) %>%
  mutate(percent = paste0(round((n/sum(n))*100),'%')) %>%
  filter(Value == 'less') %>%
  select(Generation,Value,percent)


#Variables to blame for less value=====Insight 6--------------------------------------------------------------------------------------------#

#Extract percentages of variables to blame for lower value by generation

Value_Loss <- Data %>%
  count(name = 'Total',Generation) 
Value_Loss <- Data %>%
  group_by(Generation) %>%
  summarise(variety = sum(variety), 
            price = sum(price),
            shrinkflation = sum(shrinkflation),
            service = sum(service),
            quality = sum(quality)) %>%
  select(variety,price,shrinkflation,service,quality) %>%
  cbind(Value_Loss,.) 
Value_Loss <- Value_Loss %>%
  transmute(variety = paste0(round((variety/Total)*100),'%'),
            price = paste0(round((price/Total)*100),'%'),
            shrinkflation = paste0(round((shrinkflation/Total)*100),'%'),
            service = paste0(round((service/Total)*100),'%'),
            quality = paste0(round((quality/Total)*100),'%')) %>%
  cbind(Value_Loss$Generation,.)
colnames(Value_Loss)[1] <- 'Generation'


#Spending increase at restaurants=====Insight 7----------------------------------------------------------------------------------------------#

#Extract percentages between more or less spending at restaurants by generation

Restaurant_Spending_Generation <- Data %>%
  select(Generation,Restaurant_Spending) %>%
  group_by(Generation) %>%
  count(Restaurant_Spending) %>%
  mutate(percent = paste0(round(n/sum(n)*100),'%')) %>%
  select(Generation,Restaurant_Spending,percent) %>%
  filter(Restaurant_Spending == 'more' | Restaurant_Spending == 'less')


#Family vs single household more or less spending=====Insight 10-----------------------------------------------------------------------------#

#Extract percentages between more or less spending at restaurants by household

Household_Spending <- Data %>%
  mutate(Family = Children > 0) %>%
  group_by(Family) %>%
  count(Household_Spending) %>%
  mutate(percent = paste0(round(n/sum(n)*100),'%')) %>%
  filter(Household_Spending == 'more' | Household_Spending == 'less') %>%
  select(Family,Household_Spending,percent)
Household_Spending$Family <- gsub(T,'Family',Household_Spending$Family)
Household_Spending$Family <- gsub(F,'Single',Household_Spending$Family) 


#Family vs single households using delivery once a week=====Insight 11-----------------------------------------------------------------------#

#Extract percentages of at least one delivery per week by household

Household_Delivery <- Data %>%
  mutate(Family = Children > 0) %>%
  group_by(Family) %>%
  count(Delivery) %>%
  mutate(percent = paste0(round(n/sum(n)*100),'%')) %>%
  filter(Delivery == 1) %>%
  select(Family,percent)
Household_Delivery$Family <- gsub(T,'Family',Household_Delivery$Family)
Household_Delivery$Family <- gsub(F,'Single',Household_Delivery$Family) 
