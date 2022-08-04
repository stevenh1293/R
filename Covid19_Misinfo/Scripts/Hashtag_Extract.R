#Load in necessary packages 
library(tidyverse)
library(stringr)
library(readr)
library(xlsx)


#Set working directory to home directory of project and save as vector
if (getwd() != "/home/steven/R/JSON Flatten"){
  setwd('/home/steven/R/JSON Flatten')
}
dir <- getwd()


#List of misinformation hashtags to be searched for
Misinfo_tags_Final <- tolower(c(
                        '#Scamdemic',
                        '#Plandemic',
                        '#Scaredemic',
                        '#NoCovidVaccine',
                        '#FilmYourHospital',
                        '#5GCoronavirus',
                        '#FireFauci',
                        '#ExposeBillGates',
                        '#hoaxvirus',
                        '#covid19hoax',
                        '#dontwearamask',
                        '#id2020',
                        '#covid1984',
                        '#maskoff',
                        '#Arrestbillgates',
                        '#Babaramdev',
                        '#Chinesebioterrorism',
                        '#Coronahoax',
                        '#Coronil',
                        '#Covidhoax',
                        '#Cv1984',
                        '#Drfraudfauci',
                        '#Emptyhospitals',
                        '#Endcovidscamnow',
                        '#Faucifraud',
                        '#Faucithefraud',
                        '#Hcqworks',
                        '#Hydroxychloroquineworks',
                        '#Limengyan',
                        '#Plandemicdocumentary',
                        '#Ramdev',
                        '#Arrestfauci',
                        '#Casedemic'
))


#For loop to extract data from CSV files (100 at a time) and extract indicated hashtags
#   uses about 28GB of RAM at most, averages 26GB
for (x in list.files(paste0(dir,'/CSV_Unzip'))){
  regex <- "([[<#]]+)([[:alnum:]]+)"
  AlreadyExtracted <- which(x == substr(list.files(paste0(dir,'/CSV_Final_Months/')),1,7))
  if (is_empty(AlreadyExtracted)==F){
    print(paste0("Already extracted data in: ",x))
  }
  if (is_empty(AlreadyExtracted)==T){
    print(paste0('Extracting data in: ',x))
    subdir <- list.files(paste0(dir,'/CSV_Unzip/',x))
    for (i in subdir){
      setwd(paste0(dir,'/CSV_Unzip/',x,'/',i))
      df_100 <- list.files(paste0(dir,'/CSV_Unzip/',x,'/',i), pattern = '*.csv') %>%
        map_df(~read_csv(.,show_col_types = F))
      TempDF <- df_100 %>%
        mutate(hashtag = str_extract_all(as.character(text), regex)) %>%
        unnest() %>%
        ungroup()
      rm(df_100)
      
      TempDF$hashtag <- tolower(TempDF$hashtag)
      misinfo_index <- NULL
      for (y in seq(1:length(Misinfo_tags_Final))){
        misinfo_index <- c(which(TempDF$hashtag == Misinfo_tags_Final[y]),misinfo_index)
      }
      keepcols <- c(1,2,3,7,17,27)
      TempDF <- TempDF[misinfo_index,keepcols]
      
      if (i == subdir[1]){
        write_csv(TempDF, 
                  paste0('/home/steven/R/JSON Flatten/CSV_Final_Months/',x,'.csv'), 
                  col_names = T, 
                  append = T)
        rm(TempDF,misinfo_index)
      }
      if (i > subdir[1]){
        write_csv(TempDF, 
                  paste0('/home/steven/R/JSON Flatten/CSV_Final_Months/',x,'.csv'), 
                  col_names = F, 
                  append = T)
        rm(TempDF,misinfo_index)
      }
    }
    rm(y,keepcols,i,subdir)
  }
  rm(AlreadyExtracted,regex,x)
}


#Clean CSV files to move dates to correct month
for (i in seq(1:length(list.files(paste0(dir,'/CSV_Final_Months'))))){
  setwd(paste0(dir,'/CSV_Final_Months'))
  x <- months[i]
  data <- read_csv(list.files(paste0(dir,'/CSV_Final_Months'))[i])
  print(paste0('Searching in: ',list.files(paste0(dir,'/CSV_Final_Months'))[i],' for dates with month: ',x))
  x_index <- which(substr(data$created_at,6,7) == x)
  if (i==10 & is_empty(x_index)==F){
    add_to_next_month <- data[x_index,]
    data <- data[-x_index,]
    write_csv(data,  paste0(paste0(dir,'/CSV_Final_Months/'),list.files(paste0(dir,'/CSV_Final_Months'))[i]))
    write_csv(add_to_next_month, (paste0(dir,'/CSV_Final_Months_2021/',x,'-2021.csv')))
  }
  if(i != 10 & is_empty(x_index)==F){
    add_to_next_month <- data[x_index,]
    data <- data[-x_index,]
    write_csv(data, paste0(paste0(dir,'/CSV_Final_Months/'),list.files(paste0(dir,'/CSV_Final_Months'))[i]))
    data <- read_csv(paste0(dir,'/CSV_Final_Months/',x,'-2020.csv'))
    data <- rbind(data,add_to_next_month)
    write_csv(data, (paste0(dir,'/CSV_Final_Months/',x,'-2020.csv')))
  }
  if(is_empty(x_index)==T){
    print(paste0(list.files(paste0(dir,'/CSV_Final_Months'))[i],' already extract months for ',x))
  }
  rm(data,x,i,x_index,add_to_next_month)
}


#Compile all CSV files into full year for 2020
for (i in list.files(paste0(dir,'/CSV_Final_Months'))){
  if (i == list.files(paste0(dir,'/CSV_Final_Months'))[1]){
    Full_Year_2020 <- NULL
  }
  data <- read_csv(paste0(dir,'/CSV_Final_Months/',i))
  Full_Year_2020 <- rbind(Full_Year_2020,data)
  if(i == list.files(paste0(dir,'/CSV_Final_Months/'))[length(list.files(paste0(dir,'/CSV_Final_Months/')))]){
    Full_Year_2020 <- arrange(Full_Year_2020, created_at)
    Full_Year_2020$id_str <- as.character(Full_Year_2020$id_str)
    Full_Year_2020$user_id_str <- as.character(Full_Year_2020$user_id_str)
    Full_Year_2020$retweeted_status_id_str <- as.character(Full_Year_2020$retweeted_status_id_str)
    write_csv(Full_Year_2020, paste0(dir,'/CSV_Full_Year_2020/Full_Year_2020.csv'))
  }
  rm(data,Full_Year_2020)
}

