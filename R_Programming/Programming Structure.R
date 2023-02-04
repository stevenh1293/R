library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

Freq <- c(.6,.3,.4,.4,.2,.6,.3,.4,.9,.2)
bloodp <- c(103,87,32,42,59,109,78,205,135,176)
first <- c("bad","bad","bad","bad","good","good","good","good",NA,"bad")
second <- c("low","low","high","high","low","low","high","high","high","high")
finaldecision <- c("low","high","low","high","low","high","low","high","high","high")

data <- data.frame(Freq,bloodp,first,second,finaldecision)

data <- data %>% 
  mutate_all(funs(str_replace(.,"good","0"))) %>%
  mutate_all(funs(str_replace(.,"low","0"))) %>%
  mutate_all(funs(str_replace(.,"bad","1"))) %>%
  mutate_all(funs(str_replace(.,"high","1"))) %>%
  mutate(across(where(is.character), as.numeric))


boxplot(data$Freq, xlab = 'Frequency')
boxplot(data$bloodp, xlab = 'Blood Pressure')

data.norm <- as.data.frame(scale(data[c(1,2)],center = F))
boxplot(data.norm[c(1,2)], beside = T)


data.melt <- melt(data[c(3:5)])
ggplot(na.exclude(data.melt), aes(variable, fill = as.factor(value), groups = as.factor(value))) +
  geom_bar(position = "dodge") +
  ggtitle("Count of Results") +
  guides(fill = guide_legend(title = "Results")) +
  scale_fill_discrete(type = c('#87d46e','#f74f31'),
                      labels = c("Good","Bad")) +
  theme(title = element_text(hjust = .5),
        axis.title.x = element_blank(),
        legend.text = element_text())

mean(data$finaldecision)













