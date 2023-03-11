library(ggplot2)

#Read in data
prof.df <- read.csv('salaries.csv')

#Scatter plot
ggplot(prof.df, aes(yrs.service,salary, color = rank)) +
  geom_point() +
  labs(
    title = 'Salary vs Years of Service by Title',
    x = 'Years of Serice',
    y = 'Annual Salary',
    color = 'Title'
  ) 

#Boxplots
ggplot(prof.df,aes(sex,salary))+
  geom_boxplot() +
  labs(
    title = 'Salary vs Sex',
    x = 'Sex',
    y = 'Annual Salary')

#Histogram
ggplot(prof.df,aes(sex,fill = rank)) +
  geom_histogram(stat='count') +
  labs(
    title = 'Number of Instructors based on Sex by Title',
    x = 'Sex',
    y = 'Number of Instructors',
    color = 'Title'
  ) 


