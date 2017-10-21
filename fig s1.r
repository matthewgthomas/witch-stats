##
## wealth rank by zhu/not
##
# source("data functions/reproductive success - load data.r")
source("init.r")
library(tidyverse)

# summarise
wealths = hh %>% 
  group_by(WealthRank, zhubo1) %>% 
  dplyr::summarise(count=n())

# calc proportions
wealths$prop = wealths$count / ifelse(wealths$zhubo1==1, sum(subset(wealths, zhubo1==1)$count), sum(subset(wealths, zhubo1==0)$count))

ggplot(wealths, aes(x=factor(WealthRank), y=prop, fill=factor(zhubo1))) + 
  # geom_bar(aes(fill=factor(zhubo1)), position="dodge", binwidth=1, stat = "identity") +
  geom_bar(aes(fill=factor(zhubo1)), position="dodge", stat = "identity") +
  
  # scale_x_discrete(labels=c("Poorest", "", "", "", "", "Richest")) +
  
  scale_fill_manual(values = wes_palette("Chevalier")) +
  xlab("Wealth rank") +
  ylab("Proportion of households") +
  
  theme_bw() +
  #eliminates baground, gridlines, and chart border
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
    # ,axis.text.x = element_blank()
    # ,axis.text.y = element_blank()
  ) + theme(axis.line = element_line(color = 'black')) +
  # hide legend
  theme(legend.position="none") 

# ggsave(filename="witches - household wealth.pdf", width=150, height=100, units="mm")
ggsave(filename="witches - household wealth.png", width=150, height=100, units="mm")
