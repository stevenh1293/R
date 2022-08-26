library(ggplot2)
library(dplyr)

Data %>% 
  group_by(Generation) %>%
  count(QSR) %>%
  mutate(percent = (n/sum(n))) %>%
  filter(QSR == 'more') %>%
  select(Generation,percent) %>%
  
  ggplot(aes(Generation,percent)) +
  geom_col(fill = 'blue') +
  geom_text(aes(x=Generation,y=percent-.03,label = paste0(round(percent*100),'%')), color = 'white', size=10, fontface = 'bold') +
  coord_flip() + 
  theme_classic() +
  ggtitle('QSR Visits by Generation') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 25, face = 'bold',hjust = -.08),
        axis.line = element_blank()) + 
  scale_y_continuous(limits = c(0,.5))

#Remove plot
#dev.off()
