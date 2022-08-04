#Load in necessary packages 
library(dplyr)


#Set working directory to home directory of project and save as vector
if (getwd() != "/home/steven/R/JSON Flatten"){
  setwd('/home/steven/R/JSON Flatten')
}
dir <- getwd()


#Load in each month's CSV and view the total hashtag frequency per month
for (i in list.files(paste0(dir,'/CSV_Final_Months/'))){
  data <- read_csv(paste0(dir,'/CSV_Final_Months/',i))
  assign(substr(i,1,7),aggregate(data.frame(count = data$hashtag), list(value = data$hashtag), length) %>%
    arrange(.,desc(count)))
  rm(data)
}
  

