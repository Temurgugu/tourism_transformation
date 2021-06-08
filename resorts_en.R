rm(list=ls())    # clear the workspace
    
#=== Temur Gugushvii === 
# Author https://temurgugushvili.ge/
# Data source:
    
#libraries
    
library(ggplot2)
library(tidyverse)
library(ggfittext)
library(patchwork)
    
# import data
    
tg_tourism_1939 <- readr::read_csv("data/khokhobaia_gugushvili_fig_1_tourism_1939.csv",
                                      col_types = cols(n = "c",
                                                       year = "n",
                                                       establishments_ge = "c",
                                                       establishments_en = "c",
                                                       accommodation_unit = "n",
                                                       bed_thousand = "n",
                                                       carrying_capacity = "n"))
    
#Prepare data for visualization (filter data)
     
tg_tourism <- tg_tourism_1939 %>% 
                     dplyr::select(year:bed_thousand) %>% 
                     dplyr::filter(establishments_en != "Tourism/resort establishments") 


#Build the visualization 
    
tg_tourism_diagram <- ggplot2::ggplot(tg_tourism, aes(as.factor(year), accommodation_unit, fill = establishments_ge)) +
                               geom_col(position = "dodge") +
                               theme_minimal(base_family="Sylfaen")+
                               theme(axis.title.x = element_text(colour="black", size=7, hjust=0.5),
                                     axis.title.y = element_text(colour="black", size=7, hjust=0.5),
                                     axis.text.x=element_text(angle = 90, hjust=0.5, size=7, colour="black"),
                                     axis.text.y=element_text(angle = 0, hjust=0.5, size=7, colour="black"),
                                     plot.caption = element_text(size=3, colour="black", hjust=0),
                                     plot.title=element_text(colour="black", size=10),
                                     legend.text = element_text(angle = 0, hjust=0, size=6, colour="black"),
                                     legend.title = element_text(colour="black", size=7, face="bold"),
                                     legend.key.width=unit(7, "mm"),
                                     legend.key.height=unit(7, "mm"),
                                     panel.grid.major = element_line(size = 0.2),
                                     axis.line = element_line(size = 0.2, colour = "black"),
                                     legend.position = "none")+
                               labs(title = "Resort and Tourism Establishments (Georgia)",
                                    subtitle ="",
                                    caption = "",
                                    x = "Year",
                                    y = "Accommodation Unit")+
      
                               scale_fill_manual(name = "Accommodation types",
                                                 labels = c("Holiday house", "Boarding housen", "Sanatorium", "Turbaza"),
                                                 values = c("#f5aa42", "#f5d742", "#e6f542", "#96f542"))+
                               scale_y_continuous(breaks=seq(0, 120, 20), limits = c(0, 120))

#Save the ggplot
ggsave("visualization/tg_tourism_diagram.png", 
       plot = tg_tourism_diagram,
       units = "mm",
       width = 100,
       height = 75) 
    

#Build the visualization 

tg_resort_diagram_bed <- ggplot2::ggplot(tg_tourism, aes(as.factor(year), bed_thousand, fill = establishments_ge)) +
                                  geom_col(position = "dodge") +
                                  theme_minimal(base_family="Sylfaen")+
                                  theme(axis.title.x = element_text(colour="black", size=7, hjust=0.5),
                                        axis.title.y = element_text(colour="black", size=7, hjust=0.5),
                                        axis.text.x=element_text(angle = 90, hjust=0.5, size=7, colour="black"),
                                        axis.text.y=element_text(angle = 0, hjust=0.5, size=7, colour="black"),
                                        plot.caption = element_text(size=3, colour="black", hjust=0),
                                        plot.title=element_text(colour="black", size=5),
                                        legend.text = element_text(angle = 0, hjust=0, size=6, colour="black"),
                                        legend.title = element_text(colour="black", size=7, face="bold"),
                                        legend.key.width=unit(6, "mm"),
                                        legend.key.height=unit(6, "mm"),
                                        panel.grid.major = element_line(size = 0.05),
                                        axis.line = element_line(size = 0.2, colour = "black"),
                                        legend.position = "right")+
                                  labs(title = "",
                                       subtitle ="",
                                       caption = "",
                                       x = "Year",
                                       y = "Number of Beds (thousands)")+
                                  scale_fill_manual(name = "Accommodation types",
                                                    labels = c("Holiday house", "Boarding housen", "Sanatorium", "Turbaza"),
                                                    values = c("#f5aa42", "#f5d742", "#e6f542", "#96f542"))+
                                  scale_y_continuous(breaks=seq(0, 20, 2), limits = c(0, 20))

#Save the ggplot
ggsave("visualization/tg_resort_diagram_bed.png", 
       plot = tg_resort_diagram_bed,
       units = "mm",
       width = 100,
       height = 75) 


tg_resorts_all <- tg_tourism_1939 %>% 
                  dplyr::select(n:bed_thousand) %>% 
                  dplyr::filter(year != 1965,
                                year != 1969,
                                year != 1945)  %>% 
                  dplyr::mutate(year=ifelse(n=="5","1966-1967",
                                     ifelse(n=="10","1966-1967",
                                     ifelse(n=="15","1966-1967",
                                     ifelse(n=="19","1966-1967", year)))))


tourism_all_bed <- ggplot2::ggplot(tg_resorts_all, aes(as.factor(year), bed_thousand, fill = establishments_ge)) +
                            geom_col() +
                            theme_minimal(base_family="Sylfaen")+
  
                            theme(axis.title.x = element_text(colour="black", size=7, hjust=0.5),
                                  axis.title.y = element_text(colour="black", size=7, hjust=0.5),
                                  axis.text.x=element_text(angle = 90, hjust=0.5, size=7, colour="black"),
                                  axis.text.y=element_text(angle = 0, hjust=0.5, size=7, colour="black"),
                                  plot.caption = element_text(size=3, colour="black", hjust=0),
                                  plot.title=element_text(colour="black", size=5),
                                  legend.text = element_text(angle = 0, hjust=0, size=6, colour="black"),
                                  legend.title = element_text(colour="black", size=7, face="bold"),
                                  legend.key.width=unit(6, "mm"),
                                  legend.key.height=unit(6, "mm"),
                                  panel.grid.major = element_line(size = 0.05),
                                  axis.line = element_line(size = 0.2, colour = "black"),
                                  legend.position = "right")+
                            labs(title = "",
                                 subtitle ="",
                                 caption = "",
                                 x = "Year",
                                 y = "Number of Beds (thousands)")+
                            scale_y_continuous(breaks=seq(0, 155, 20), limits = c(0, 155))+
                            scale_fill_manual(name = "Accommodation types",
                                              labels = c("Holiday house", "Boarding housen", "Sanatorium", "Turbaza", "Tourism/resort establishments"),
                                              values = c("#00e6e6", "#ff9900", "#ace600", "#03fccf", "#fcd303"))


#Save the ggplot
ggsave("visualization/tourism_all_bed.png", 
       plot = tourism_all_bed,
       units = "mm",
       width = 100,
       height = 75)     





tourism_all <- ggplot2::ggplot(tg_resorts_all, aes(as.factor(year), accommodation_unit, fill = establishments_ge)) +
                        geom_col() +
                        theme_minimal(base_family="Sylfaen")+
                        theme(axis.title.x = element_text(colour="black", size=7, hjust=0.5),
                              axis.title.y = element_text(colour="black", size=7, hjust=0.5),
                              axis.text.x=element_text(angle = 90, hjust=0.5, size=7, colour="black"),
                              axis.text.y=element_text(angle = 0, hjust=0.5, size=7, colour="black"),
                              plot.caption = element_text(size=7, colour="black", hjust=0),
                              plot.title=element_text(colour="black", size=7),
                              legend.text = element_text(angle = 0, hjust=0, size=6, colour="black"),
                              legend.title = element_text(colour="black", size=7, face="bold"),
                              legend.key.width=unit(3, "mm"),
                              legend.key.height=unit(3, "mm"),
                              panel.grid.major = element_line(size = 0.05),
                              axis.line = element_line(size = 0.2, colour = "black"),
                              legend.position = "none")+
                        labs(title = "",
                             subtitle ="",
                             caption = "Source: Kobakhidze E. (1971). SSR Resort and Tourism of Georgia. Metsniereba\n             Shubladze, V. (2004). Georgia and Tourism. Gump",
                             x = "Year",
                             y = "Accommodation Unit")+
                        scale_y_continuous(breaks=seq(0, 630, 60), limits = c(0, 630))+
                        scale_fill_manual(name = "Accommodation types",
                                          labels = c("Holiday house", "Boarding housen", "Sanatorium", "Turbaza", "Tourism/resort establishments"),
                                          values = c("#00e6e6", "#ff9900", "#ace600", "#03fccf", "#fcd303"))



#Save the ggplot
ggsave("visualization/tourism_all.png", 
       plot = tourism_all,
       units = "mm",
       width = 100,
       height = 75)     


#grouped all charts 

tourism_grouped <- (tg_tourism_diagram + tg_resort_diagram_bed) / (tourism_all + tourism_all_bed)



#Save the ggplot
ggsave("visualization/tourism_grouped.png", 
       plot = tourism_grouped,
       units = "mm",
       width = 250,
       height = 175)   