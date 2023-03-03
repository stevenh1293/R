library(data.table)
library(dplyr)
setwd('C:/Users/steve/OneDrive/USF/Spring 2023/R Programming/Week 8')

Student_assignment_6 <- read.table('Assignment 6 Dataset.txt', header = T,sep = ',')
Gender_Average <- Student_assignment_6 %>%
  group_by(Sex) %>%
  reframe(Grade.Average = mean(Grade))
write.table(Gender_Average, 'Gender_Avg.csv',sep = ',')

Names_With_i <- Student_assignment_6 %>%
  filter(Name %like% 'i')
write.table(Names_With_i,'Students_With_i_in_name.csv',sep = ',')
