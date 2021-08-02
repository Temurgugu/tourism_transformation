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
library(data.table)


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


tg_russia_trips <-  readr::read_csv("data/salukvadze_fig_1_russia_trips.csv") 


tg_russia_trips_filter <- tg_russia_trips %>%
                   dplyr::filter(year != 2010, year != 2011,
                                 year != 2016, year != 2017)  %>%
                   mutate(month = fct_relevel(month, "January", "February", "March",
                                                     "April", "May", "June", "July", "August",
                                                     "September", "October", "November", "December"))

tg_russia_trips_wider <-  tg_russia_trips %>% 
  dplyr::select(year,int_travelers_trip, month) %>% 
  tidyr::pivot_wider (names_from = year, values_from = int_travelers_trip) %>% 
setnames(old = c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"), 
        new = c("y2007", "y2008", "y2009", "y2010", "y2011", "y2012", "y2013", "y2014", "y2015", "y2016", "y2017", "y2018", "y2019", "y2020")) %>% 
  mutate(Year_2008 = y2008 - y2007,
         Year_2009 = y2009 - y2008,
         Year_2010 = y2010 - y2009,
         Year_2011 = y2011 - y2010,
         Year_2012 = y2012 - y2011,
         Year_2013 = y2013 - y2012,
         Year_2014 = y2014 - y2013,
         Year_2015 = y2015 - y2014,
         Year_2016 = y2016 - y2015,
         Year_2017 = y2017 - y2016,
         Year_2018 = y2018 - y2017,
         Year_2019 = y2019 - y2018,
         Year_2020 = y2020 - y2019) %>% 
  dplyr::select(month, Year_2008 : Year_2020) %>% 
  tidyr::pivot_longer(!month, names_to = "index_year", values_to = "Trips") %>% 
  mutate(month = fct_relevel(month, "January", "February", "March",
                             "April", "May", "June", "July", "August",
                             "September", "October", "November", "December")) %>% 
  dplyr::filter(index_year != "Year_2010", index_year != "Year_2011", index_year != "Year_2012",
                index_year != "Year_2017", index_year != "Year_2020")
 

tgp_salukvadze_fig_1_russia_trips <- ggplot(tg_russia_trips_filter, aes(month, int_travelers_trip, color=as.factor(year), group=as.factor(year))) +
                    geom_point()+      
                    geom_line()+
                    theme(axis.title.x = element_text(colour="black", size=14, hjust=0.5),
                          axis.title.y = element_text(colour="black", size=14, hjust=0.5),
                          axis.text.x=element_text(angle = 90,  hjust=0.5, size=14, colour="black"),
                          axis.text.y=element_text(angle = 0,  hjust=0.5, size=14, colour="black"),
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
                         color="Number of visitors")+
                    xlab("Month")+
                    ylab("")+
                    facet_grid_sc(row=vars(period), 
                                  scales = list(y = scales_y),
                                  labeller = labeller(time=as_labeller(event_names)),)+
                    scale_color_manual(values=c('#E42411','#9F188F',"#2AE411", '#BE7A36','#EEEA0A',
                                                '#9F9D18','#189F9D','#1A704E','#701A4A','#B56330',
                                                '#646325','#BBB914','#32BB14','#0A41EE'))

 
#Save the ggplot
ggsave("visualization/salukvadze_fig_1_russia_trips.png", 
       plot = tgp_salukvadze_fig_1_russia_trips,
       units = "mm",
       width = 300,
       height = 175) 




#Export subdatase 
write.csv(tg_russia_trips_wider,"data/salukvadze_fig_1_russia_trips_difference.csv", row.names = FALSE)


salukvadze_fig_1_russia_trips_difference <- ggplot(tg_russia_trips_wider, aes(month, Trips, fill = Trips))+
  geom_col()+      
  theme(axis.title.x = element_text(colour="black", size=10, hjust=0.5),
        axis.title.y = element_text(colour="black", size=10, hjust=0.5),
        axis.text.x=element_text(angle = 90,  hjust=0.5, size=10, colour="black"),
        axis.text.y=element_text(angle = 0,  hjust=0.5, size=10, colour="black"),
        legend.position = "right",
        plot.caption = element_text(size=10, colour="black", hjust=0),
        plot.title=element_text(colour="black", size=10),
        strip.text.y = element_text(size = 10, color = "black", face = "bold.italic"),
        strip.background = element_rect(color="white", fill="white", size=0.5, linetype="solid"),
        legend.title=element_text(size=10), 
        legend.text=element_text(size=10))+
  labs(title = "",
       subtitle ="",
       caption = "Source: Ministry of Internal Affairs of Georgia",
       color="")+
  xlab("Month")+
  ylab("The amount of difference")+
  scale_fill_viridis_c()+
  facet_wrap(vars(index_year), scales = "free")
            


#Save the ggplot
ggsave("visualization/salukvadze_fig_1_russia_trips_difference.png", 
       plot = salukvadze_fig_1_russia_trips_difference,
       units = "mm",
       width = 300,
       height = 200) 



