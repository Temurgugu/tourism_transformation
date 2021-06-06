rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 

library(tidyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggthemes)
library(dplyr)
library(scales)
library(grid)



tg_tourism_trips <- read.csv("data/tourism_trips_1995.csv")

tg_tourism <- tg_tourism %>%
  dplyr::filter(#type != "International Traveller Trips_!", 
                #type != "Same Day Trips",
                #type != "International Visitor Trips",
                Year < 2020)

tgp_trips <- ggplot2::ggplot(tg_tourism_trips, aes(Year, number, group = type, color = type))+
                      geom_line(size = 0.7)+
                      geom_point(size = 1.5, color = "black")+
                      theme_minimal(base_family="Sylfaen")+
  theme(axis.title.x = element_text(colour="black", size=12, hjust=0.5),
        axis.title.y = element_text(colour="black", size=12, hjust=0.5),
        axis.text.x=element_text(angle = 90, hjust=0.5, size=12, colour="black"),
        axis.text.y=element_text(angle = 0, hjust=0.5, size=12, colour="black"),
        plot.caption = element_text(size=10, colour="black", hjust=0),
        legend.position = "bottom",
        legend.text=element_text(colour="black", size=11),
        axis.line = element_line(size = 0.2, colour = "black"))+
  labs(caption = "Source: World Bank, Georgian National Tourism Administration, Ministry of Internal Affairs of Georgia",
       x = "",
       y = "Trips")+
  scale_color_discrete(name = "",
                       labels = c("International Traveller Trips",
                                  "International Visitor Trips",
                                  "Same Day Trips",
                                  "Tourist (Overnight) Trips"))+
  scale_x_continuous(breaks=seq(1995, 2020, 1))+
  scale_y_continuous(breaks=seq(0, 10000000, 2000000), labels = scales::comma)


#Save the ggplot
ggsave("visualization/tgp_trips.png", 
       plot = tgp_trips,
       units = "mm",
       width = 300,
       height = 175) 


