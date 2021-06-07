rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 

library(tidyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(hrbrthemes)
library(viridis)
library(plotly)
library(lubridate)
library(facetscales)

event_names <- c(`i` = "",
                 `ii` = "",
                 `iii`= "", 
                 `iv` = "")
scales_y <- list(
  `i` = scale_y_continuous(limits = c(4000, 25000), breaks = seq(4000, 250000, 6000)),
  `ii` = scale_y_continuous(limits = c(2000, 150000), breaks = seq(2000, 150000, 40000)),
  `iii` = scale_y_continuous(limits = c(30000, 200000), breaks = seq(30000, 200000, 50000)),
  `iv` = scale_y_continuous(limits = c(10000, 280000), breaks = seq(10000, 280000, 40000))
                )


tg_russia_trips <- read_excel("data/salukvadze_fig_1_russia_trips.xlsx")


tgp_russia_trips <- ggplot(tg_russia_trips, aes(month(month_year), int_travelers_trip, color=as.factor(year))) +
                    geom_point()+      
                    geom_line()+
                    theme(axis.title.x = element_text(colour="black", size=14, hjust=0.5),
                          axis.title.y = element_text(colour="black", size=14, hjust=0.5),
                          axis.text.x=element_text(angle = 30,  hjust=0.5, size=14, colour="black"),
                          axis.text.y=element_text(angle = 20,  hjust=0.5, size=14, colour="black"),
                          legend.position = "right",
                          plot.caption = element_text(size=16, colour="black", hjust=0),
                          plot.title=element_text(colour="black", size=20),
                          strip.text.y = element_text( size = 14, color = "black", face = "bold.italic"),
                          strip.background = element_rect(color="white", fill="white", size=0.5, linetype="solid"),
                          legend.title=element_text(size=14), 
                          legend.text=element_text(size=14))+
                    labs(title = "",
                         subtitle ="",
                         caption = "Source: Ministry of Internal Affairs of Georgia",
                         color="")+
                    xlab("Month")+
                    ylab("Number of visitors")+
                    facet_grid_sc(row=vars(period), 
                                  scales = list(y = scales_y),
                                  labeller = labeller(time=as_labeller(event_names)),)+
                    scale_x_continuous(breaks=seq(01, 12, 1))+
                    scale_color_manual(values=c('#E42411','#9F188F',"#2AE411", '#BE7A36','#EEEA0A',
                                                '#9F9D18','#189F9D','#1A704E','#701A4A','#B56330',
                                                '#646325','#BBB914','#32BB14','#0A41EE'))

 
#Save the ggplot
ggsave("visualization/tgp_russia_trips.png", 
       plot = tgp_russia_trips,
       units = "mm",
       width = 300,
       height = 175) 


