rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 

library(ggplot2)
library(tidyverse)



icvs_1992 <- read.csv("data/khokhobaia_gugushvili_fig_2_icvs_trends.csv")

icvs_1992  <- icvs_1992 %>%
              dplyr::mutate(percentage_2 = as.numeric(percentage),
                            year_2 = as.numeric(year))  


tgp_icvs_1992 <- ggplot(icvs_1992, aes(year_2, percentage_2, color = type)) +
                 geom_line()+
          geom_point()+
          theme_minimal(base_family="Sylfaen")+
          theme(axis.text.x=element_text(angle = 90, hjust=0.9, size=10, colour="black"),
                legend.position = "bottom",
                axis.line = element_line(size = 0.2, colour = "black"))+
          scale_x_continuous(breaks=c(1992, 1996, 2010, 2011, 2012, 2013, 2020))+
          scale_y_continuous(breaks=c(0, 2.5, 5, 7.5, 10, 12.5, 15))+
          labs(y = "Percent", 
               x = "Year")+
          scale_color_manual(name = "Victimisation Period",
                             labels = c("Last year", "5 year"),
                             values = c("darkred", "steelblue"))+
    labs(title = "",
         subtitle ="",
         caption = "Source:ICVS Report",
         color="")


#Save the ggplot
ggsave("visualization/tgp_icvs_1992.png", 
       plot = tgp_icvs_1992,
       units = "mm",
       width = 300,
       height = 175) 
  