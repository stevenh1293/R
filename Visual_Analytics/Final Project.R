#--------------------------------------------------------Load Packages--------------------------------------------------#
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(ggthemes)
library(kableExtra)
library(usmap)

#-----------------------------------------------------Set working directory--------------------------------------------#

dir <- getwd()
if (dir != "/home/steven/R/Visual Analytics"){
  setwd("/home/steven/R/Visual Analytics")
  print(getwd())
}

#-------------------------------------------------------Get CSV files---------------------------------------------------#

csvdir <- "/home/steven/R/Visual Analytics/CSVs"
list.files(csvdir)
csvfile <- "DataScientist.csv" 
data <- read.csv(paste(csvdir,csvfile,sep = "/"))


#------------------------------------------------------Clean data---------------------------------------------------------#
#Remove uneeded columns (X, index, Job.Description, Headquarters, Revenue, Competitors, Easy.Apply)
cols <- c('X', 'index', 'Job.Description', 'Headquarters', 'Revenue', 'Competitors', 'Easy.Apply')
rm_col_index <- which(colnames(data) %in% cols)
data <- data[,-rm_col_index]

#Clean up column names, I prefer an underscore to a period but this is just preference
colnames(data) <- gsub('\\.','_', colnames(data))

#We will have to manually review the job titles to figure out a selection of base level data scientist jobs to analyze
Data_Scientist_Roles <- unique(data$Job_Title[grep('^Data Scientist',data$Job_Title)])
#   Note that I am starting with Data Scientist at the beginning here because most of the titles are very messy, and I dont have time to exam all of them for this project.

#We see a lot of data scientist roles with higher classification, we want to mostly keep base level data scienteist roles.
DS_DF <- data[grepl('^Data Scientist',data$Job_Title) 
               & !grepl('2',data$Job_Title)
               & !grepl('3',data$Job_Title)
               & !grepl('II',data$Job_Title)
               & !grepl('III',data$Job_Title)
               & !grepl('Sr.',data$Job_Title)
               & !grepl('Senior',data$Job_Title)
               & !grepl('Lead',data$Job_Title)
               & !grepl('Manager',data$Job_Title)
               & !grepl('Intern',data$Job_Title)
               & !grepl('mid',data$Job_Title),]

#Now we can rename all this to Data Scientist to group them together for the analysis. 
DS_DF$Job_Title[1:length(DS_DF$Job_Title)] <- 'Data Scientist'

#Now we will do the same for data analyst
Data_Analyst_Roles <- unique(data$Job_Title[grep('Data Analyst',data$Job_Title)])
DA_DF <- data[grepl('Data Analyst',data$Job_Title) 
              & !grepl('2',data$Job_Title)
              & !grepl('3',data$Job_Title)
              & !grepl('II',data$Job_Title)
              & !grepl('III',data$Job_Title)
              & !grepl('Sr.',data$Job_Title)
              & !grepl('Senior',data$Job_Title)
              & !grepl('Lead',data$Job_Title)
              & !grepl('Manager',data$Job_Title)
              & !grepl('Intern',data$Job_Title)
              & !grepl('mid',data$Job_Title)
              & !grepl('Junior',data$Job_Title, ignore.case = T)
              & !grepl('IV',data$Job_Title)
              & !grepl('V',data$Job_Title)
              & !grepl('Master',data$Job_Title),]

#Now we can rename all this to Data Analyst to group them together for the analysis. 
DA_DF$Job_Title[1:length(DA_DF$Job_Title)] <- 'Data Analyst'


#Now that we have a good size data set to work with for both job categories, we will combine them into one data frame
Data_Proffesions_DF <- rbind(DA_DF,DS_DF)

#Instead of using a salary range, we will transform this into two rows of a Min and a Max range per job position. First I'll remove any per hour salary
data <- data[-grep('per hour', data$Salary_Estimate, ignore.case = T),]

Data_Proffesions_DF$Salary_Estimate <- gsub('\\$','',Data_Proffesions_DF$Salary_Estimate)
Data_Proffesions_DF$Salary_Estimate <- gsub('(Glassdoor est.)','',Data_Proffesions_DF$Salary_Estimate)
Data_Proffesions_DF$Salary_Estimate <- gsub('\\()','',Data_Proffesions_DF$Salary_Estimate)
Data_Proffesions_DF$Salary_Estimate <- gsub('K','',Data_Proffesions_DF$Salary_Estimate)
Data_Proffesions_DF <- cbind(Data_Proffesions_DF, data.frame(t(matrix(
  unlist(strsplit(as.vector(Data_Proffesions_DF$Salary_Estimate), split = "-")), 
  ncol = length(Data_Proffesions_DF$Salary_Estimate), nrow = 2))))
names(Data_Proffesions_DF)[11:12] <- c('Min_Salary', 'Max_Salary')
Data_Proffesions_DF <- Data_Proffesions_DF[,-2]
Data_Proffesions_DF <- Data_Proffesions_DF %>% relocate(c(Min_Salary,Max_Salary), .before = Location)

#Now we can change the salary to a full numeric range
Data_Proffesions_DF$Min_Salary <- as.numeric(Data_Proffesions_DF$Min_Salary)
Data_Proffesions_DF$Max_Salary <- as.numeric(Data_Proffesions_DF$Max_Salary)
Data_Proffesions_DF[,4:5] <- Data_Proffesions_DF[,4:5] * 1000

#In the company name, for some reason the rating is in the name, we will have to remove this
Length_Index <- nchar(Data_Proffesions_DF$Company_Name[1:length(Data_Proffesions_DF$Company_Name)])
Data_Proffesions_DF$Company_Name <- substr(Data_Proffesions_DF$Company_Name,start = 1, stop = Length_Index - 3)

#Now we will remove all rows containing any -1 values as they will not be useful in the analysis
rownames(Data_Proffesions_DF) <- NULL
rm_index <- which(Data_Proffesions_DF$Rating == -1)
Data_Proffesions_DF <- Data_Proffesions_DF[-rm_index,]

rownames(Data_Proffesions_DF) <- NULL
rm_index <- which(Data_Proffesions_DF$Founded == -1)
Data_Proffesions_DF <- Data_Proffesions_DF[-rm_index,]

rownames(Data_Proffesions_DF) <- NULL
rm_index <- which(Data_Proffesions_DF$Industry == -1)
Data_Proffesions_DF <- Data_Proffesions_DF[-rm_index,]
rownames(Data_Proffesions_DF) <- NULL

#Some salary info only contains one number, so this will be used as the min and max
NA_Index <- which(is.na(Data_Proffesions_DF$Max_Salary) == T)
Data_Proffesions_DF$Max_Salary[NA_Index] <- Data_Proffesions_DF$Min_Salary[NA_Index]

#I found that some salaries start at 12K, which does not seem realistic compared to the Max being over 100K, so these will be removed to not mess up any averages.
Data_Proffesions_DF <- Data_Proffesions_DF[-which(Data_Proffesions_DF$Min_Salary < 30000),]
rownames(Data_Proffesions_DF) <- NULL

#Next we will split city and state into two columns.
Data_Proffesions_DF <- cbind(Data_Proffesions_DF, data.frame(t(matrix(
  unlist(strsplit(as.vector(Data_Proffesions_DF$Location), split = ", ")), 
  ncol = length(Data_Proffesions_DF$Location), nrow = 2))))
names(Data_Proffesions_DF)[12:13] <- c('City', 'State')
Data_Proffesions_DF <- Data_Proffesions_DF[,-6]
Data_Proffesions_DF <- Data_Proffesions_DF %>% relocate(c(City,State), .before = Size)

#Change the size column to employees and keep only numbers
Data_Proffesions_DF <- rename(Data_Proffesions_DF, Employees = Size)
Data_Proffesions_DF$Employees <- gsub(' to ','-',Data_Proffesions_DF$Employees)
Data_Proffesions_DF$Employees <- gsub(' employees','',Data_Proffesions_DF$Employees)



#Get an average Min and Max salary vs company employee size by job title
Row_Order <- c("1-50","51-200","201-500","501-1000","1001-5000","5001-10000","10000+","Unknown")
DP_Minimum_Average_Salary <- matrix(ncol = 3)
colnames(DP_Minimum_Average_Salary) <- (c('Job_Title','Employees','Average'))
for (i in seq(1:length(Row_Order))){
  row_index <- which(Data_Proffesions_DF$Employees == Row_Order[i] & Data_Proffesions_DF$Job_Title == 'Data Analyst')
  DP_Minimum_Average_Salary <- rbind(DP_Minimum_Average_Salary,matrix(c('Data Analyst',Row_Order[i], round(mean(Data_Proffesions_DF$Min_Salary[row_index]))),ncol = 3))
  row_index <- which(Data_Proffesions_DF$Employees == Row_Order[i] & Data_Proffesions_DF$Job_Title == 'Data Scientist')
  DP_Minimum_Average_Salary <- rbind(DP_Minimum_Average_Salary,matrix(c('Data Scientist',Row_Order[i], round(mean(Data_Proffesions_DF$Min_Salary[row_index]))),ncol = 3))
}

DP_Minimum_Average_Salary <- DP_Minimum_Average_Salary[-1,]
DP_Minimum_Average_Salary <- DP_Minimum_Average_Salary[-15:-16,]
DP_Minimum_Average_Salary <- as.data.frame(DP_Minimum_Average_Salary)
DP_Minimum_Average_Salary$Average <- as.numeric(DP_Minimum_Average_Salary$Average)

Row_Order <- c("1-50","51-200","201-500","501-1000","1001-5000","5001-10000","10000+","Unknown")
DP_Maximum_Average_Salary <- matrix(ncol = 3)
colnames(DP_Maximum_Average_Salary) <- c('Job_Title','Employees','Average')
for (i in seq(1:length(Row_Order))){
  row_index <- which(Data_Proffesions_DF$Employees == Row_Order[i] & Data_Proffesions_DF$Job_Title == 'Data Analyst')
  DP_Maximum_Average_Salary <- rbind(DP_Maximum_Average_Salary,matrix(c('Data Analyst',Row_Order[i], round(mean(Data_Proffesions_DF$Max_Salary[row_index]))),ncol = 3))
  row_index <- which(Data_Proffesions_DF$Employees == Row_Order[i] & Data_Proffesions_DF$Job_Title == 'Data Scientist')
  DP_Maximum_Average_Salary <- rbind(DP_Maximum_Average_Salary,matrix(c('Data Scientist',Row_Order[i], round(mean(Data_Proffesions_DF$Max_Salary[row_index]))),ncol = 3))
}

DP_Maximum_Average_Salary <- DP_Maximum_Average_Salary[-1,]
DP_Maximum_Average_Salary <- as.data.frame(DP_Maximum_Average_Salary)
DP_Maximum_Average_Salary <- DP_Maximum_Average_Salary[-15:-16,]
DP_Maximum_Average_Salary$Average <- as.numeric(DP_Maximum_Average_Salary$Average)

#Next we will look at min and max salaries vs types of ownership by job title
#After looking through the types of ownership, some are too small to be considered for a sample size. We will only take samples from types which have sample size over 30
Type_Ownership_Totals <- data.frame(Data_Proffesions_DF %>% group_by(Type_of_ownership) %>% tally())
Type_Ownership_Over_30 <- Type_Ownership_Totals$Type_of_ownership[which(Type_Ownership_Totals$n > 30)]
Type_Ownership_Maximum_Average_Salary <- matrix(ncol = 3)
for (i in seq(1:length(Type_Ownership_Over_30))){
  row_index <- which(Data_Proffesions_DF$Type_of_ownership == Type_Ownership_Over_30[i] & Data_Proffesions_DF$Job_Title == 'Data Analyst')
  Type_Ownership_Maximum_Average_Salary <- rbind(Type_Ownership_Maximum_Average_Salary,matrix(c('Data Analyst',Type_Ownership_Over_30[i], round(mean(Data_Proffesions_DF$Max_Salary[row_index]))),ncol = 3))
  row_index <- which(Data_Proffesions_DF$Type_of_ownership == Type_Ownership_Over_30[i] & Data_Proffesions_DF$Job_Title == 'Data Scientist')
  Type_Ownership_Maximum_Average_Salary <- rbind(Type_Ownership_Maximum_Average_Salary,matrix(c('Data Scientist',Type_Ownership_Over_30[i], round(mean(Data_Proffesions_DF$Max_Salary[row_index]))),ncol = 3))
}

Type_Ownership_Maximum_Average_Salary <- Type_Ownership_Maximum_Average_Salary[-1,]
colnames(Type_Ownership_Maximum_Average_Salary) <- c('Job_Title','TypeOwn','Average')
Type_Ownership_Maximum_Average_Salary <- as.data.frame(Type_Ownership_Maximum_Average_Salary)
Type_Ownership_Maximum_Average_Salary$Average <- as.numeric(Type_Ownership_Maximum_Average_Salary$Average)

Type_Ownership_Minimum_Average_Salary <- matrix(ncol = 3)
for (i in seq(1:length(Type_Ownership_Over_30))){
  row_index <- which(Data_Proffesions_DF$Type_of_ownership == Type_Ownership_Over_30[i] & Data_Proffesions_DF$Job_Title == 'Data Analyst')
  Type_Ownership_Minimum_Average_Salary <- rbind(Type_Ownership_Minimum_Average_Salary,matrix(c('Data Analyst',Type_Ownership_Over_30[i], round(mean(Data_Proffesions_DF$Min_Salary[row_index]))),ncol = 3))
  row_index <- which(Data_Proffesions_DF$Type_of_ownership == Type_Ownership_Over_30[i] & Data_Proffesions_DF$Job_Title == 'Data Scientist')
  Type_Ownership_Minimum_Average_Salary <- rbind(Type_Ownership_Minimum_Average_Salary,matrix(c('Data Scientist',Type_Ownership_Over_30[i], round(mean(Data_Proffesions_DF$Min_Salary[row_index]))),ncol = 3))
}

Type_Ownership_Minimum_Average_Salary <- Type_Ownership_Minimum_Average_Salary[-1,]
colnames(Type_Ownership_Minimum_Average_Salary) <- c('Job_Title','TypeOwn','Average')
Type_Ownership_Minimum_Average_Salary <- as.data.frame(Type_Ownership_Minimum_Average_Salary)
Type_Ownership_Minimum_Average_Salary$Average <- as.numeric(Type_Ownership_Minimum_Average_Salary$Average)

#Last, we will find out which state has the highest min and max salary by job title
State_Totals <- data.frame(Data_Proffesions_DF %>% group_by(State) %>% tally())
States_Over_5 <- State_Totals$State[which(State_Totals$n > 5)]
State_Minimum_Average_Salary <- matrix(ncol = 3)
for (i in seq(1:length(States_Over_5))){
  row_index <- which(Data_Proffesions_DF$State == States_Over_5[i] & Data_Proffesions_DF$Job_Title == 'Data Analyst')
  State_Minimum_Average_Salary <- rbind(State_Minimum_Average_Salary,matrix(c('Data Analyst',States_Over_5[i], round(mean(Data_Proffesions_DF$Min_Salary[row_index]))),ncol = 3))
  row_index <- which(Data_Proffesions_DF$State == States_Over_5[i] & Data_Proffesions_DF$Job_Title == 'Data Scientist')
  State_Minimum_Average_Salary <- rbind(State_Minimum_Average_Salary,matrix(c('Data Scientist',States_Over_5[i], round(mean(Data_Proffesions_DF$Min_Salary[row_index]))),ncol = 3))
}

State_Minimum_Average_Salary <- State_Minimum_Average_Salary[-1,]
colnames(State_Minimum_Average_Salary) <- c('Job_Title','State','Average')
State_Minimum_Average_Salary <- as.data.frame(State_Minimum_Average_Salary)
State_Minimum_Average_Salary$Average <- as.numeric(State_Minimum_Average_Salary$Average)

State_Maximum_Average_Salary <- matrix(ncol = 3)
for (i in seq(1:length(States_Over_5))){
  row_index <- which(Data_Proffesions_DF$State == States_Over_5[i] & Data_Proffesions_DF$Job_Title == 'Data Analyst')
  State_Maximum_Average_Salary <- rbind(State_Maximum_Average_Salary,matrix(c('Data Analyst',States_Over_5[i], round(mean(Data_Proffesions_DF$Max_Salary[row_index]))),ncol = 3))
  row_index <- which(Data_Proffesions_DF$State == States_Over_5[i] & Data_Proffesions_DF$Job_Title == 'Data Scientist')
  State_Maximum_Average_Salary <- rbind(State_Maximum_Average_Salary,matrix(c('Data Scientist',States_Over_5[i], round(mean(Data_Proffesions_DF$Max_Salary[row_index]))),ncol = 3))
}

State_Maximum_Average_Salary <- State_Maximum_Average_Salary[-1,]
colnames(State_Maximum_Average_Salary) <- c('Job_Title','State','Average')
State_Maximum_Average_Salary <- as.data.frame(State_Maximum_Average_Salary)
State_Maximum_Average_Salary$Average <- as.numeric(State_Maximum_Average_Salary$Average)

#---------------------------------------------------------------Data Visualizing------------------------------------------------------------#

#---Min and Max Salary vs Company Rating by Job Title-----#
g1 <- ggplot(Data_Proffesions_DF, aes(Rating, Min_Salary, color = Job_Title)) +
  geom_point() +
  scale_y_continuous(n.breaks = 10) +
  theme_clean() +
  labs(x = 'Company Rating', y = 'Minimum Salary', color = 'Job Title') +
  ggtitle('Minimum Salary vs Company Rating by Job Title') +
  theme(plot.title = element_text(hjust = 0.5, vjust = .25, size = 20),
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(.5, 'cm'),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16))
ggMarginal(g1 + theme(legend.position = "left"), type = "density", groupFill = T)

g2 <- ggplot(Data_Proffesions_DF, aes(Rating, Max_Salary, color = Job_Title)) +
  geom_point() +
  scale_y_continuous(n.breaks = 10) +
  theme_clean() +
  labs(x = 'Company Rating', y = 'Maximum Salary', color = 'Job Title') +
  ggtitle('Maximum Salary vs Company Rating by Job Title') +
  theme(plot.title = element_text(hjust = 0.5, vjust = .25, size = 20),
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(.5, 'cm'),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16))
ggMarginal(g2 + theme(legend.position = "left"), type = "density", groupFill = T)

#---Min and Max Salary Distribution by Job Title-----#
g3 <- ggplot(Data_Proffesions_DF, aes(Job_Title, Min_Salary, color = Job_Title)) +
  geom_violin() +
  geom_point() +
  scale_y_continuous(n.breaks = 10) +
  theme_clean() +
  labs(x = 'Job Title', y = 'Minimum Salary', color = 'Job Title') +
  ggtitle('Minimum Salary Distribution by Job Title') +
  theme(plot.title = element_text(hjust = 0.5, vjust = .25, size = 20),
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(.5, 'cm'),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16))
plot(g3)

g4 <- ggplot(Data_Proffesions_DF, aes(Job_Title, Max_Salary, color = Job_Title)) +
  geom_violin() +
  geom_point() +
  scale_y_continuous(n.breaks = 10) +
  theme_clean() +
  labs(x = 'Job Title', y = 'Maximum Salary', color = 'Job Title') +
  ggtitle('Maximum Salary Distribution by Job Title') +
  theme(plot.title = element_text(hjust = 0.5, vjust = .25, size = 20),
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(.5, 'cm'),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16))
plot(g4)

#-------Average Minimum and Maximum Salary by Company Size-----------#
g5 <- ggplot(DP_Minimum_Average_Salary, aes(Employees, Average, fill = Job_Title)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  scale_y_continuous(breaks = seq(0,120000,20000),limits = c(0,120000),expand = c(0,0))+
  scale_x_discrete(labels = Row_Order) +
  theme_calc() +
  labs(x = 'Company Size (Employee Range)', y = 'Minimum Average Salary', fill = "Job Title") +
  ggtitle('Minimum Average Salary vs Company Size by Job Title') +
  theme(plot.title = element_text(hjust = 0.5, vjust = .25, size = 20),
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(.5, 'cm'),
        legend.title = element_text(size=16),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.ticks.length=unit(.25, "cm"))
plot(g5)

g6 <- ggplot(DP_Maximum_Average_Salary, aes(Employees, Average, fill = Job_Title)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  scale_y_continuous(breaks = seq(0,180000,30000),limits = c(0,180000),expand = c(0,0))+
  scale_x_discrete(labels = Row_Order) +
  theme_calc() +
  labs(x = 'Company Size (Employee Range)', y = 'Maximum Average Salary', fill = "Job Title") +
  ggtitle('Maximum Average Salary vs Company Size by Job Title') +
  theme(plot.title = element_text(hjust = 0.5, vjust = .25, size = 20),
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(.5, 'cm'),
        legend.title = element_text(size=16),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.ticks.length=unit(.25, "cm"))
plot(g6)

#-----Average min and max salary vs type of ownership by job title-------------#


g7 <- ggplot(Type_Ownership_Minimum_Average_Salary, aes(TypeOwn, Average, fill = Job_Title)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  scale_y_continuous(breaks = seq(0,120000,30000),limits = c(0,120000),expand = c(0,0))+
  theme_calc() +
  labs(x = 'Type of Ownership', y = 'Minimum Average Salary', fill = "Job Title") +
  ggtitle('Minimum Average Salary vs Type of Ownership by Job Title') +
  theme(plot.title = element_text(hjust = 0.5, vjust = .25, size = 20),
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(.5, 'cm'),
        legend.title = element_text(size=16),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.ticks.length=unit(.25, "cm"))
plot(g7)

g8 <- ggplot(Type_Ownership_Maximum_Average_Salary, aes(TypeOwn, Average, fill = Job_Title)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  scale_y_continuous(breaks = seq(0,180000,30000),limits = c(0,180000),expand = c(0,0))+
  theme_calc() +
  labs(x = 'Type of Ownership', y = 'Maximum Average Salary', fill = "Job Title") +
  ggtitle('Maximum Average Salary vs Type of Ownership by Job Title') +
  theme(plot.title = element_text(hjust = 0.5, vjust = .25, size = 20),
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(.5, 'cm'),
        legend.title = element_text(size=16),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.ticks.length=unit(.25, "cm"))
plot(g8)

#------Average min and max salary vs state by job title-------#
DAMSDF <- Data_Proffesions_DF[Data_Proffesions_DF$Job_Title == "Data Analyst",]
DAMSDF <- data.frame(state=DAMSDF$State,val=DAMSDF$Min_Salary)
DAMSDF <- as.data.frame(round(tapply(DAMSDF$val, DAMSDF$state, mean)))
colnames(DAMSDF) <- 'val'
DAMSDF$state <- rownames(DAMSDF)
rownames(DAMSDF) <- NULL

plot_usmap(data = DAMSDF, values = 'val', color = 'black') + 
  scale_fill_continuous(low = 'white', high = 'dark blue',
                        name = "Minimum Salary Average",
                        breaks = seq(0,120000,20000),
                        labels = seq(0,120000,20000),
                        limits = c(0,120000)) +
  labs(title = "Data Analyst Minimum Salary Average") +
  theme(plot.title = element_text(hjust = 0.5, vjust = .25, size = 25),
        legend.key.size = unit(.1, 'cm'),
        legend.key.height = unit(1.75, 'cm'),
        legend.key.width = unit(1.25, 'cm'),
        legend.title = element_text(size=16),
        legend.text = element_text(size=12),
        legend.position = 'right',
        legend.direction = 'vertical')

DAMXDF <- Data_Proffesions_DF[Data_Proffesions_DF$Job_Title == "Data Analyst",]
DAMXDF <- data.frame(state=DAMXDF$State,val=DAMXDF$Max_Salary)
DAMXDF <- as.data.frame(round(tapply(DAMXDF$val, DAMXDF$state, mean)))
colnames(DAMXDF) <- 'val'
DAMXDF$state <- rownames(DAMXDF)
rownames(DAMXDF) <- NULL

plot_usmap(data = DAMXDF, values = 'val', color = 'black') + 
  scale_fill_continuous(low = 'white', high = 'dark blue',
                        name = "Maximum Salary Average",
                        breaks = seq(0,180000,20000),
                        labels = seq(0,180000,20000),
                        limits = c(0,180000)) +
  labs(title = "Data Analyst Maximum Salary Average") +
  theme(plot.title = element_text(hjust = 0.5, vjust = .25, size = 25),
        legend.key.size = unit(.1, 'cm'),
        legend.key.height = unit(1.75, 'cm'),
        legend.key.width = unit(1.25, 'cm'),
        legend.title = element_text(size=16),
        legend.text = element_text(size=12),
        legend.position = 'right',
        legend.direction = 'vertical')

DSMXDF <- Data_Proffesions_DF[Data_Proffesions_DF$Job_Title == "Data Scientist",]
DSMXDF <- data.frame(state=DSMXDF$State,val=DSMXDF$Max_Salary)
DSMXDF <- as.data.frame(round(tapply(DSMXDF$val, DSMXDF$state, mean)))
colnames(DSMXDF) <- 'val'
DSMXDF$state <- rownames(DSMXDF)
rownames(DSMXDF) <- NULL

plot_usmap(data = DSMXDF, values = 'val', color = 'black') + 
  scale_fill_continuous(low = 'white', high = 'dark blue',
                        name = "Maximum Salary Average",
                        breaks = seq(0,180000,20000),
                        labels = seq(0,180000,20000),
                        limits = c(0,180000)) +
  labs(title = "Data Scientist Maximum Salary Average") +
  theme(plot.title = element_text(hjust = 0.5, vjust = .25, size = 25),
        legend.key.size = unit(.1, 'cm'),
        legend.key.height = unit(1.75, 'cm'),
        legend.key.width = unit(1.25, 'cm'),
        legend.title = element_text(size=16),
        legend.text = element_text(size=12),
        legend.position = 'right',
        legend.direction = 'vertical')

DSMSDF <- Data_Proffesions_DF[Data_Proffesions_DF$Job_Title == "Data Scientist",]
DSMSDF <- data.frame(state=DSMSDF$State,val=DSMSDF$Min_Salary)
DSMSDF <- as.data.frame(round(tapply(DSMSDF$val, DSMSDF$state, mean)))
colnames(DSMSDF) <- 'val'
DSMSDF$state <- rownames(DSMSDF)
rownames(DSMSDF) <- NULL

plot_usmap(data = DSMSDF, values = 'val', color = 'black') + 
  scale_fill_continuous(low = 'white', high = 'dark blue',
                        name = "Minimum Salary Average",
                        breaks = seq(0,140000,20000),
                        labels = seq(0,140000,20000),
                        limits = c(0,140000)) +
  labs(title = "Data Scientist Minimum Salary Average") +
  theme(plot.title = element_text(hjust = 0.5, vjust = .25, size = 25),
        legend.key.size = unit(.1, 'cm'),
        legend.key.height = unit(1.75, 'cm'),
        legend.key.width = unit(1.25, 'cm'),
        legend.title = element_text(size=16),
        legend.text = element_text(size=12),
        legend.position = 'right',
        legend.direction = 'vertical')

#--------------------------------------------------Tables of findings------------------------------------------------------------------#
DS_Jobs_DF <- Data_Proffesions_DF[which(Data_Proffesions_DF$Job_Title == 'Data Scientist' 
                                        & Data_Proffesions_DF$Rating > 3.5
                                        & Data_Proffesions_DF$Employees == '51-200'
                                        #& c(Data_Proffesions_DF$Type_of_ownership == 'Company - Public' | Data_Proffesions_DF$Type_of_ownership == 'Subsidiary or Business Segment')
                                        & Data_Proffesions_DF$State == 'CA'),]

DA_Jobs_DF <- Data_Proffesions_DF[which(Data_Proffesions_DF$Job_Title == 'Data Analyst' 
                                        & Data_Proffesions_DF$Rating > 3.0
                                        & Data_Proffesions_DF$Employees == '1-50'
                                        & c(Data_Proffesions_DF$Type_of_ownership == 'Company - Public' | Data_Proffesions_DF$Type_of_ownership == 'Company - Private')
                                        & c(Data_Proffesions_DF$State == 'CA' | Data_Proffesions_DF$State == 'NY')),]

colnames(DS_Jobs_DF) <- gsub('_',' ',colnames(DS_Jobs_DF))
colnames(DA_Jobs_DF) <- gsub('_',' ',colnames(DA_Jobs_DF))

DS_Jobs_DF %>%
      kbl() %>%
      kable_styling()

DA_Jobs_DF %>%
  kbl() %>%
  kable_styling()
