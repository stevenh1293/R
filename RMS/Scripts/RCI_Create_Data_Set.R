#Load in prackages
library(dplyr)

#Gender Percentage
Gender <- c(sample('Female',round(769*.46),replace = T),
            sample('Male',round(769*.54),replace = T))

#Genreation Percentage
Generation <- c(sample('Gen Z',round(769*.15),replace = T),
                sample('Millenials',round(769*.28),replace = T),
                sample('Gen X',round(769*.27),replace = T),
                sample('Boomers',round(769*.30),replace = T))

#Employment Percentage
Employment_Status <- c(sample('Employed_for_wages',round(769*.481),replace = T),
                       sample('Employed_for_wages',round(769*.21),replace = T),
                       sample('Employed_for_wages',round(769*.06),replace = T),
                       sample('Employed_for_wages',round(769*.06),replace = T),
                       sample('Employed_for_wages',round(769*.07),replace = T),
                       sample('Employed_for_wages',round(769*.06),replace = T),
                       sample('Employed_for_wages',round(769*.04),replace = T),
                       sample('Employed_for_wages',round(769*.02),replace = T))

#Region Percentage
Region <- c(sample('West',round(769*.21),replace = T),
                sample('Midwest',round(769*.18),replace = T),
                sample('Northeast',round(769*.19),replace = T),
                sample('South',round(769*.421),replace = T))

#Children Under 16 Percentage
Children <- c(as.numeric(sample('3',round(769*.07),replace = T)),
              as.numeric(sample('2',round(769*.12),replace = T)),
              as.numeric(sample('1',round(769*.16),replace = T)),
              as.numeric(sample('0',round(769*.65),replace = T)))

#Household Income Percentage
Income <- c(sample(50000,round(769*.53),replace = T),
            sample(50001:100000,round(769*.28),replace = T),
            sample(100001,round(769*.15),replace = T),
            sample(NA,round(769*.04),replace = T))

#Geographic Area Percentage
Geographic_Area <- c(sample('Rural',round(769*.26),replace = T),
                    sample('Suburban',round(769*.44),replace = T),
                    sample('Urban',round(769*.30),replace = T))

#Create data frame
Data <- as.data.frame(cbind(Gender,Generation,Employment_Status,Region,Children,Income,Geographic_Area))
rm(Gender,Generation,Employment_Status,Region,Children,Income,Geographic_Area)


#QSRs=====Insight 1-----------------------------------------------------------------------------------------------------#

#Millenials
mil_index <- which(Data$Generation == 'Millenials')
mil_index_len <- round(length(mil_index)*.31)
mil_more_index <- mil_index[1:mil_index_len]

mil_new_index <- mil_index[-1:-mil_index_len]
mil_new_index_len <- round(length(mil_new_index)*.5)
mil_less_index <- mil_new_index[1:mil_new_index_len]

mil_no_change_index <- mil_new_index[(1+mil_new_index_len):length(mil_new_index)]

mil_index
mil_more_index
mil_less_index
mil_no_change_index

#Gen Z

gen_z_index <- which(Data$Generation == 'Gen Z')
gen_z_index_len <- round(length(gen_z_index)*.29)
gen_z_more_index <- gen_z_index[1:gen_z_index_len]

gen_z_new_index <- gen_z_index[-1:-gen_z_index_len]
gen_z_new_index_len <- round(length(gen_z_new_index)*.5)
gen_z_less_index <- gen_z_new_index[1:gen_z_new_index_len]

gen_z_no_change_index <- gen_z_new_index[(1+gen_z_new_index_len):length(gen_z_new_index)]

gen_z_index
gen_z_more_index
gen_z_less_index
gen_z_no_change_index

#Gen X

gen_x_index <- which(Data$Generation == 'Gen X')
gen_x_index_len <- round(length(gen_x_index)*.2)
gen_x_more_index <- gen_x_index[1:gen_x_index_len]

gen_x_new_index <- gen_x_index[-1:-gen_x_index_len]
gen_x_new_index_len <- round(length(gen_x_new_index)*.5)
gen_x_less_index <- gen_x_new_index[1:gen_x_new_index_len]

gen_x_no_change_index <- gen_x_new_index[(1+gen_x_new_index_len):length(gen_x_new_index)]

gen_x_index
gen_x_more_index
gen_x_less_index
gen_x_no_change_index

#Boomers
boomers_index <- which(Data$Generation == 'Boomers')
boomers_index_len <- round(length(boomers_index)*.08)
boomers_more_index <- boomers_index[1:boomers_index_len]

boomers_new_index <- boomers_index[-1:-boomers_index_len]
boomers_new_index_len <- round(length(boomers_new_index)*.5)
boomers_less_index <- boomers_new_index[1:boomers_new_index_len]

boomers_no_change_index <- boomers_new_index[(1+boomers_new_index_len):length(boomers_new_index)]

boomers_index
boomers_more_index
boomers_less_index
boomers_no_change_index

#Create column
Data$QSR[1:769] <- NA
Data$QSR[boomers_more_index] <- 'more'
Data$QSR[boomers_less_index] <- 'less'
Data$QSR[boomers_no_change_index] <- 'no change'

Data$QSR[gen_x_more_index] <- 'more'
Data$QSR[gen_x_less_index] <- 'less'
Data$QSR[gen_x_no_change_index] <- 'no change'

Data$QSR[gen_z_more_index] <- 'more'
Data$QSR[gen_z_less_index] <- 'less'
Data$QSR[gen_z_no_change_index] <- 'no change'

Data$QSR[mil_more_index] <- 'more'
Data$QSR[mil_less_index] <- 'less'
Data$QSR[mil_no_change_index] <- 'no change'


#Drive Thru visits=====Insight 4-----------------------------------------------------------------------------------------------------------#

#Millenials
mil_index <- which(Data$Generation == 'Millenials')
mil_index_len <- round(length(mil_index)*.31)
mil_more_index <- mil_index[1:mil_index_len]

mil_new_index <- mil_index[-1:-mil_index_len]
mil_new_index_len <- round(length(mil_new_index)*.5)
mil_less_index <- mil_new_index[1:mil_new_index_len]

mil_no_change_index <- mil_new_index[(1+mil_new_index_len):length(mil_new_index)]

mil_index
mil_more_index
mil_less_index
mil_no_change_index

#Gen Z

gen_z_index <- which(Data$Generation == 'Gen Z')
gen_z_index_len <- round(length(gen_z_index)*.29)
gen_z_more_index <- gen_z_index[1:gen_z_index_len]

gen_z_new_index <- gen_z_index[-1:-gen_z_index_len]
gen_z_new_index_len <- round(length(gen_z_new_index)*.5)
gen_z_less_index <- gen_z_new_index[1:gen_z_new_index_len]

gen_z_no_change_index <- gen_z_new_index[(1+gen_z_new_index_len):length(gen_z_new_index)]

gen_z_index
gen_z_more_index
gen_z_less_index
gen_z_no_change_index

#Gen X

gen_x_index <- which(Data$Generation == 'Gen X')
gen_x_index_len <- round(length(gen_x_index)*.20)
gen_x_more_index <- gen_x_index[1:gen_x_index_len]

gen_x_new_index <- gen_x_index[-1:-gen_x_index_len]
gen_x_new_index_len <- round(length(gen_x_new_index)*.5)
gen_x_less_index <- gen_x_new_index[1:gen_x_new_index_len]

gen_x_no_change_index <- gen_x_new_index[(1+gen_x_new_index_len):length(gen_x_new_index)]

gen_x_index
gen_x_more_index
gen_x_less_index
gen_x_no_change_index

#Boomers
boomers_index <- which(Data$Generation == 'Boomers')
boomers_index_len <- round(length(boomers_index)*.08)
boomers_more_index <- boomers_index[1:boomers_index_len]

boomers_new_index <- boomers_index[-1:-boomers_index_len]
boomers_new_index_len <- round(length(boomers_new_index)*.5)
boomers_less_index <- boomers_new_index[1:boomers_new_index_len]

boomers_no_change_index <- boomers_new_index[(1+boomers_new_index_len):length(boomers_new_index)]

boomers_index
boomers_more_index
boomers_less_index
boomers_no_change_index


#Create column
Data$Drive_Thru[1:769] <- NA
Data$Drive_Thru[boomers_more_index] <- 'more'
Data$Drive_Thru[boomers_less_index] <- 'less'
Data$Drive_Thru[boomers_no_change_index] <- 'no change'

Data$Drive_Thru[gen_x_more_index] <- 'more'
Data$Drive_Thru[gen_x_less_index] <- 'less'
Data$Drive_Thru[gen_x_no_change_index] <- 'no change'

Data$Drive_Thru[gen_z_more_index] <- 'more'
Data$Drive_Thru[gen_z_less_index] <- 'less'
Data$Drive_Thru[gen_z_no_change_index] <- 'no change'

Data$Drive_Thru[mil_more_index] <- 'more'
Data$Drive_Thru[mil_less_index] <- 'less'
Data$Drive_Thru[mil_no_change_index] <- 'no change'


#Value from restaurants=====Insight 5-----------------------------------------------------------------------------------------------------------#

#Millenials
mil_index <- which(Data$Generation == 'Millenials')
mil_less_index_len <- round(length(mil_index)*.26)
mil_less_index <- mil_index[1:mil_less_index_len]

mil_new_index <- mil_index[-1:-mil_index_len]
mil_new_index_len <- round(length(mil_new_index)*.5)
mil_more_index <- mil_new_index[1:mil_new_index_len]

mil_no_change_index <- mil_new_index[(1+mil_new_index_len):length(mil_new_index)]

#Gen Z
gen_z_index <- which(Data$Generation == 'Gen Z')
gen_z_less_index_len <- round(length(gen_z_index)*.3)
gen_z_less_index <- gen_z_index[1:gen_z_index_len]

gen_z_new_index <- gen_z_index[-1:-gen_z_index_len]
gen_z_new_index_len <- round(length(gen_z_new_index)*.5)
gen_z_more_index <- gen_z_new_index[1:gen_z_new_index_len]

gen_z_no_change_index <- gen_z_new_index[(1+gen_z_new_index_len):length(gen_z_new_index)]

#Gen X
gen_x_index <- which(Data$Generation == 'Gen X')
gen_x_less_index_len <- round(length(gen_x_index)*.38)
gen_x_less_index <- gen_x_index[1:gen_x_less_index_len]

gen_x_new_index <- gen_x_index[-1:-gen_x_index_len]
gen_x_new_index_len <- round(length(gen_x_new_index)*.5)
gen_x_more_index <- gen_x_new_index[1:gen_x_new_index_len]

gen_x_no_change_index <- gen_x_new_index[(1+gen_x_new_index_len):length(gen_x_new_index)]

#Boomers
boomers_index <- which(Data$Generation == 'Boomers')
boomers_less_index_len <- round(length(boomers_index)*.48)
boomers_less_index <- boomers_index[1:boomers_less_index_len]

boomers_new_index <- boomers_index[-1:-boomers_index_len]
boomers_new_index_len <- round(length(boomers_new_index)*.5)
boomers_more_index <- boomers_new_index[1:boomers_new_index_len]

boomers_no_change_index <- boomers_new_index[(1+boomers_new_index_len):length(boomers_new_index)]


#Create column
Data$Value[1:769] <- NA
Data$Value[boomers_more_index] <- 'more'
Data$Value[boomers_less_index] <- 'less'
Data$Value[boomers_no_change_index] <- 'no change'

Data$Value[gen_x_more_index] <- 'more'
Data$Value[gen_x_less_index] <- 'less'
Data$Value[gen_x_no_change_index] <- 'no change'

Data$Value[gen_z_more_index] <- 'more'
Data$Value[gen_z_less_index] <- 'less'
Data$Value[gen_z_no_change_index] <- 'no change'

Data$Value[mil_more_index] <- 'more'
Data$Value[mil_less_index] <- 'less'
Data$Value[mil_no_change_index] <- 'no change'


#Variables to blame for less value=====Insight 6------------------------------------------------------------------------------------

#Millenials
mil_index <- which(Data$Generation == 'Millenials')

mil_variety_index_len <- round(length(mil_index)*.05)
mil_variety_index_yes <- sample(mil_index,mil_variety_index_len)
mil_variety_index_no <- mil_index[-which(mil_index %in% mil_variety_index_yes)]

mil_price_index_len <- round(length(mil_index)*.68)
mil_price_index_yes <- sample(mil_index,mil_price_index_len)
mil_price_index_no <- mil_index[-which(mil_index %in% mil_price_index_yes)]

mil_shrinkflation_index_len <- round(length(mil_index)*.27)
mil_shrinkflation_index_yes <- sample(mil_index,mil_shrinkflation_index_len)
mil_shrinkflation_index_no <- mil_index[-which(mil_index %in% mil_shrinkflation_index_yes)]

mil_service_index_len <- round(length(mil_index)*.17)
mil_service_index_yes <- sample(mil_index,mil_service_index_len)
mil_service_index_no <- mil_index[-which(mil_index %in% mil_service_index_yes)]

mil_quality_index_len <- round(length(mil_index)*.17)
mil_quality_index_yes <- sample(mil_index,mil_quality_index_len)
mil_quality_index_no <- mil_index[-which(mil_index %in% mil_quality_index_yes)]

#Gen Z
gen_z_index <- which(Data$Generation == 'Gen Z')

gen_z_variety_index_len <- round(length(gen_z_index)*.14)
gen_z_variety_index_yes <- sample(gen_z_index,gen_z_variety_index_len)
gen_z_variety_index_no <- gen_z_index[-which(gen_z_index %in% gen_z_variety_index_yes)]

gen_z_price_index_len <- round(length(gen_z_index)*.66)
gen_z_price_index_yes <- sample(gen_z_index,gen_z_price_index_len)
gen_z_price_index_no <- gen_z_index[-which(gen_z_index %in% gen_z_price_index_yes)]

gen_z_shrinkflation_index_len <- round(length(gen_z_index)*.28)
gen_z_shrinkflation_index_yes <- sample(gen_z_index,gen_z_shrinkflation_index_len)
gen_z_shrinkflation_index_no <- gen_z_index[-which(gen_z_index %in% gen_z_shrinkflation_index_yes)]

gen_z_service_index_len <- round(length(gen_z_index)*.21)
gen_z_service_index_yes <- sample(gen_z_index,gen_z_service_index_len)
gen_z_service_index_no <- gen_z_index[-which(gen_z_index %in% gen_z_service_index_yes)]

gen_z_quality_index_len <- round(length(gen_z_index)*.1)
gen_z_quality_index_yes <- sample(gen_z_index,gen_z_quality_index_len)
gen_z_quality_index_no <- gen_z_index[-which(gen_z_index %in% gen_z_quality_index_yes)]

#Gen X
gen_x_index <- which(Data$Generation == 'Gen X')

gen_x_variety_index_len <- round(length(gen_x_index)*.11)
gen_x_variety_index_yes <- sample(gen_x_index,gen_x_variety_index_len)
gen_x_variety_index_no <- gen_x_index[-which(gen_x_index %in% gen_x_variety_index_yes)]

gen_x_price_index_len <- round(length(gen_x_index)*.78)
gen_x_price_index_yes <- sample(gen_x_index,gen_x_price_index_len)
gen_x_price_index_no <- gen_x_index[-which(gen_x_index %in% gen_x_price_index_yes)]

gen_x_shrinkflation_index_len <- round(length(gen_x_index)*.53)
gen_x_shrinkflation_index_yes <- sample(gen_x_index,gen_x_shrinkflation_index_len)
gen_x_shrinkflation_index_no <- gen_x_index[-which(gen_x_index %in% gen_x_shrinkflation_index_yes)]

gen_x_service_index_len <- round(length(gen_x_index)*.09)
gen_x_service_index_yes <- sample(gen_x_index,gen_x_service_index_len)
gen_x_service_index_no <- gen_x_index[-which(gen_x_index %in% gen_x_service_index_yes)]

gen_x_quality_index_len <- round(length(gen_x_index)*.08)
gen_x_quality_index_yes <- sample(gen_x_index,gen_x_quality_index_len)
gen_x_quality_index_no <- gen_x_index[-which(gen_x_index %in% gen_x_quality_index_yes)]

#Boomers
boomers_index <- which(Data$Generation == 'Boomers')

boomers_variety_index_len <- round(length(boomers_index)*.04)
boomers_variety_index_yes <- sample(boomers_index,boomers_variety_index_len)
boomers_variety_index_no <- boomers_index[-which(boomers_index %in% boomers_variety_index_yes)]

boomers_price_index_len <- round(length(boomers_index)*.75)
boomers_price_index_yes <- sample(boomers_index,boomers_price_index_len)
boomers_price_index_no <- boomers_index[-which(boomers_index %in% boomers_price_index_yes)]

boomers_shrinkflation_index_len <- round(length(boomers_index)*.52)
boomers_shrinkflation_index_yes <- sample(boomers_index,boomers_shrinkflation_index_len)
boomers_shrinkflation_index_no <- boomers_index[-which(boomers_index %in% boomers_shrinkflation_index_yes)]

boomers_service_index_len <- round(length(boomers_index)*.12)
boomers_service_index_yes <- sample(boomers_index,boomers_service_index_len)
boomers_service_index_no <- boomers_index[-which(boomers_index %in% boomers_service_index_yes)]

boomers_quality_index_len <- round(length(boomers_index)*.1)
boomers_quality_index_yes <- sample(boomers_index,boomers_quality_index_len)
boomers_quality_index_no <- boomers_index[-which(boomers_index %in% boomers_quality_index_yes)]

#Create columns for less value variables

Data$variety[1:769] <- NA
Data$variety[mil_variety_index_yes] <- 1
Data$variety[mil_variety_index_no] <- 0

Data$variety[gen_z_variety_index_yes] <- 1
Data$variety[gen_z_variety_index_no] <- 0

Data$variety[gen_x_variety_index_yes] <- 1
Data$variety[gen_x_variety_index_no] <- 0

Data$variety[boomers_variety_index_yes] <- 1
Data$variety[boomers_variety_index_no] <- 0

Data$price[1:769] <- NA
Data$price[mil_price_index_yes] <- 1
Data$price[mil_price_index_no] <- 0

Data$price[gen_z_price_index_yes] <- 1
Data$price[gen_z_price_index_no] <- 0

Data$price[gen_x_price_index_yes] <- 1
Data$price[gen_x_price_index_no] <- 0

Data$price[boomers_price_index_yes] <- 1
Data$price[boomers_price_index_no] <- 0

Data$shrinkflation[1:769] <- NA
Data$shrinkflation[mil_shrinkflation_index_yes] <- 1
Data$shrinkflation[mil_shrinkflation_index_no] <- 0

Data$shrinkflation[gen_z_shrinkflation_index_yes] <- 1
Data$shrinkflation[gen_z_shrinkflation_index_no] <- 0

Data$shrinkflation[gen_x_shrinkflation_index_yes] <- 1
Data$shrinkflation[gen_x_shrinkflation_index_no] <- 0

Data$shrinkflation[boomers_shrinkflation_index_yes] <- 1
Data$shrinkflation[boomers_shrinkflation_index_no] <- 0

Data$service[1:769] <- NA
Data$service[mil_service_index_yes] <- 1
Data$service[mil_service_index_no] <- 0

Data$service[gen_z_service_index_yes] <- 1
Data$service[gen_z_service_index_no] <- 0

Data$service[gen_x_service_index_yes] <- 1
Data$service[gen_x_service_index_no] <- 0

Data$service[boomers_service_index_yes] <- 1
Data$service[boomers_service_index_no] <- 0

Data$quality[1:769] <- NA
Data$quality[mil_quality_index_yes] <- 1
Data$quality[mil_quality_index_no] <- 0

Data$quality[gen_z_quality_index_yes] <- 1
Data$quality[gen_z_quality_index_no] <- 0

Data$quality[gen_x_quality_index_yes] <- 1
Data$quality[gen_x_quality_index_no] <- 0

Data$quality[boomers_quality_index_yes] <- 1
Data$quality[boomers_quality_index_no] <- 0


#Spending increase at restaurants=====Insight 7---------------------------------------------------------------------------------------------------#

#Millenials
mil_index <- which(Data$Generation == 'Millenials')
mil_more_index_len <- round(length(mil_index)*.39)
mil_more_index <- mil_index[1:mil_more_index_len]

mil_less_index_len <- round(length(mil_index)*.24)
mil_less_index <- mil_index[(1+mil_more_index_len):sum(mil_more_index_len,mil_less_index_len)]

mil_no_change_index_len <- round(length(mil_index)*.37)
mil_no_change_index <- mil_index[(1+mil_more_index_len+mil_less_index_len):sum(mil_more_index_len,mil_less_index_len,mil_no_change_index_len)-1]

#Gen Z
gen_z_index <- which(Data$Generation == 'Gen Z')
gen_z_more_index_len <- round(length(gen_z_index)*.35)
gen_z_more_index <- gen_z_index[1:gen_z_more_index_len]

gen_z_less_index_len <- round(length(gen_z_index)*.2)
gen_z_less_index <- gen_z_index[(1+gen_z_more_index_len):sum(gen_z_more_index_len,gen_z_less_index_len)]

gen_z_no_change_index_len <- round(length(gen_z_index)*.45)
gen_z_no_change_index <- gen_z_index[(1+gen_z_more_index_len+gen_z_less_index_len):sum(gen_z_more_index_len,gen_z_less_index_len,gen_z_no_change_index_len)]

#Gen X
gen_x_index <- which(Data$Generation == 'Gen X')
gen_x_more_index_len <- round(length(gen_x_index)*.29)
gen_x_more_index <- gen_x_index[1:gen_x_more_index_len]

gen_x_less_index_len <- round(length(gen_x_index)*.36)
gen_x_less_index <- gen_x_index[(1+gen_x_more_index_len):sum(gen_x_more_index_len,gen_x_less_index_len)]

gen_x_no_change_index_len <- round(length(gen_x_index)*.35)
gen_x_no_change_index <- gen_x_index[(1+gen_x_more_index_len+gen_x_less_index_len):sum(gen_x_more_index_len,gen_x_less_index_len,gen_x_no_change_index_len)]

#Boomers
boomers_index <- which(Data$Generation == 'Boomers')
boomers_more_index_len <- round(length(boomers_index)*.23)
boomers_more_index <- boomers_index[1:boomers_more_index_len]

boomers_less_index_len <- round(length(boomers_index)*.48)
boomers_less_index <- boomers_index[(1+boomers_more_index_len):sum(boomers_more_index_len,boomers_less_index_len)]

boomers_no_change_index_len <- round(length(boomers_index)*.29)
boomers_no_change_index <- boomers_index[(1+boomers_more_index_len+boomers_less_index_len):sum(boomers_more_index_len,boomers_less_index_len,boomers_no_change_index_len)]


#Create columns for more or less spending by generation
Data$Restaurant_Spending[1:769] <- NA
Data$Restaurant_Spending[boomers_more_index] <- 'more'
Data$Restaurant_Spending[boomers_less_index] <- 'less'
Data$Restaurant_Spending[boomers_no_change_index] <- 'no change'

Data$Restaurant_Spending[gen_x_more_index] <- 'more'
Data$Restaurant_Spending[gen_x_less_index] <- 'less'
Data$Restaurant_Spending[gen_x_no_change_index] <- 'no change'

Data$Restaurant_Spending[gen_z_more_index] <- 'more'
Data$Restaurant_Spending[gen_z_less_index] <- 'less'
Data$Restaurant_Spending[gen_z_no_change_index] <- 'no change'

Data$Restaurant_Spending[mil_more_index] <- 'more'
Data$Restaurant_Spending[mil_less_index] <- 'less'
Data$Restaurant_Spending[mil_no_change_index] <- 'no change'


#Family vs single household more or less spending=====Insight 10-------------------------------------------------------------------------------

#family more or less spending
family_index <- which(Data$Children > 0)
family_more_index_len <- round(length(family_index)*.41)
family_more_index <- family_index[1:family_more_index_len]

family_less_index_len <- round(length(family_index)*.27)
family_less_index <- family_index[(1+family_more_index_len):sum(family_more_index_len,family_less_index_len)]

family_no_change_index_len <- round(length(family_index)*.32)
family_no_change_index <- family_index[(1+family_more_index_len+family_less_index_len):sum(family_more_index_len,family_less_index_len,family_no_change_index_len)]

#single more or less spending
single_index <- which(Data$Children == 0)
single_more_index_len <- round(length(single_index)*.26)
single_more_index <- single_index[1:single_more_index_len]

single_less_index_len <- round(length(single_index)*.38)
single_less_index <- single_index[(1+single_more_index_len):sum(single_more_index_len,single_less_index_len)]

single_no_change_index_len <- round(length(single_index)*.36)
single_no_change_index <- single_index[(1+single_more_index_len+single_less_index_len):sum(single_more_index_len,single_less_index_len,single_no_change_index_len)]

#Create columns
Data$Household_Spending[1:769] <- NA
Data$Household_Spending[family_more_index] <- 'more'
Data$Household_Spending[family_less_index] <- 'less'
Data$Household_Spending[family_no_change_index] <- 'no change'

Data$Household_Spending[single_more_index] <- 'more'
Data$Household_Spending[single_less_index] <- 'less'
Data$Household_Spending[single_no_change_index] <- 'no change'


#Family vs single households using delivery once a week=====Insight 11------------------------------------------------------------------------------------

#family more or less spending
family_index <- which(Data$Children > 0)
family_more_index_len <- round(length(family_index)*.78)
family_more_index <- family_index[1:family_more_index_len]

family_less_index_len <- round(length(family_index)*.22)
family_less_index <- family_index[(1+family_more_index_len):sum(family_more_index_len,family_less_index_len)]

#single more or less spending
single_index <- which(Data$Children == 0)
single_more_index_len <- round(length(single_index)*.51)
single_more_index <- single_index[1:single_more_index_len]

single_less_index_len <- round(length(single_index)*.49)
single_less_index <- single_index[(1+single_more_index_len):sum(single_more_index_len,single_less_index_len)]

#Create columns
Data$Delivery[1:769] <- NA
Data$Delivery[family_more_index] <- 1
Data$Delivery[family_less_index] <- 0

Data$Delivery[single_more_index] <- 1
Data$Delivery[single_less_index] <- 0

#Remove all temp objects from GlobalEnv
to.remove <- ls()
to.remove <- to.remove[-which(to.remove == 'Data')]
rm(list=to.remove)
rm(to.remove)

