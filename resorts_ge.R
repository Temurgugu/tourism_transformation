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
    
tg_resorts <- readr::read_csv("data/tourism_1939_.csv",
                                      col_types = cols(year = "n",
                                                       establishments_ge = "c",
                                                       establishments_en = "c",
                                                       number = "n",
                                                       bed_thousand = "n",
                                                       carrying_capacity = "n"))
    
#Prepare data for visualization (filter data)
     
tg_tourism <- tg_resorts %>% 
                     dplyr::select(year:bed_thousand) %>% 
                     dplyr::filter(
                                   establishments_ge != "ტურისტული/საკურორტო დაწესებულება") 
#Build the visualization 
    
tg_tourism_diagram <- ggplot2::ggplot(tg_tourism, aes(as.factor(year), number, fill = establishments_ge)) +
                         geom_col(position = "dodge") +
                         geom_text(aes(label=number), position = position_dodge(width = 0.9), hjust = 0, size = 2)+
                         theme_minimal(base_family="Sylfaen")+
      
                         theme(axis.title.x = element_text(colour="black", size=4, hjust=0.5),
                               axis.title.y = element_text(colour="black", size=4, hjust=0.5),
                               axis.text.x=element_text(angle = 90, hjust=0.5, size=4, colour="black"),
                               axis.text.y=element_text(angle = 0, hjust=0.5, size=4, colour="black"),
                               plot.caption = element_text(size=3, colour="black", hjust=0),
                               plot.title=element_text(colour="black", size=5),
                               legend.text = element_text(angle = 0, hjust=0, size=4, colour="black"),
                               legend.title = element_text(colour="black", size=4, face="bold"),
                               legend.key.width=unit(3, "mm"),
                               legend.key.height=unit(3, "mm"),
                               panel.grid.major = element_line(size = 0.05),
                               axis.line = element_line(size = 0.2, colour = "black"),
                               legend.position = "bottom")+
      
                         labs(title = "Resort Establishments (Georgia)\nსაკურორტო დაწესებულებები (საქართველო)",
                              subtitle ="",
                              caption = "Source: Kobakhidze E. (1971). SSR Resort and Tourism of Georgia. Metsniereba\nწყარო: კობახიძე ე. (1971) საქართველოს სსრ საკურორტო მეურნეობა და ტურიზმი. მეცნიერება",
                              x = "Year\nწელი",
                              y = "Establishments\nდაწესებულებები")+
      
                         scale_fill_manual(name = "Resort establishments types\nსაკურორტო დაწესებულებების ტიპები",
                                          labels = c("Holiday house\nდასასვენებელი სახლი", "Boarding housen\nპანსიონატი", "Sanatorium\nსანატორიუმი", "Turbaza\nტურისტული ბაზა"),
                                          values = c("#00e6e6", "#ff9900", "#ace600"))+
                         scale_y_continuous(breaks=seq(0, 150, 30), limits = c(0, 150))

#Save the ggplot
ggsave("visualization/tg_resort_diagram.png", 
           plot = tg_resort_diagram,
           units = "mm",
           width = 100,
           height = 75) 
    

#Build the visualization 

tg_resort_diagram_bed <- ggplot2::ggplot(tg_tourism, aes(as.factor(year), bed_thousand, fill = establishments_ge)) +
  geom_col() +
  geom_text(aes(label=bed_thousand), position=position_stack(0.5), size = 2)+
  theme_minimal(base_family="Sylfaen")+
  
  theme(axis.title.x = element_text(colour="black", size=4, hjust=0.5),
        axis.title.y = element_text(colour="black", size=4, hjust=0.5),
        axis.text.x=element_text(angle = 90, hjust=0.5, size=4, colour="black"),
        axis.text.y=element_text(angle = 0, hjust=0.5, size=4, colour="black"),
        plot.caption = element_text(size=3, colour="black", hjust=0),
        plot.title=element_text(colour="black", size=5),
        legend.text = element_text(angle = 0, hjust=0, size=4, colour="black"),
        legend.title = element_text(colour="black", size=4, face="bold"),
        legend.key.width=unit(3, "mm"),
        legend.key.height=unit(3, "mm"),
        panel.grid.major = element_line(size = 0.05),
        axis.line = element_line(size = 0.2, colour = "black"))+
  
  labs(title = "Tourism and Resort Establishments (Georgia)\nტურისტული და საკურორტო დაწესებულებები (საქართველო)",
       subtitle ="",
       caption = "Source: Kobakhidze E. (1971). SSR Resort and Tourism of Georgia. Metsniereba\nწყარო: კობახიძე ე. (1971) საქართველოს სსრ საკურორტო მეურნეობა და ტურიზმი. მეცნიერება",
       x = "Year\nწელი",
       y = "Number of Beds\nსაწოლების რააოდენობა")+
  
  scale_fill_manual(name = "Resort establishments types\nსაკურორტო დაწესებულებების ტიპები",
                    labels = c("Holiday house\nდასასვენებელი სახლი", "Boarding housen\nპანსიონატი", "Sanatorium\nსანატორიუმი", "Turbaza\nტურისტული ბაზა"),
                    values = c("#00e6e6", "#ff9900", "#ace600"))+
  scale_y_continuous(breaks=seq(0, 35, 5), limits = c(0, 35))

#Save the ggplot
ggsave("visualization/tg_resort_diagram_bed.png", 
       plot = tg_resort_diagram_bed,
       units = "mm",
       width = 100,
       height = 75) 


#Prepare data for visualization (filter data)
    
    
tg_resorts_tourism <- tg_resorts %>% 
                        dplyr::select(year:bed_thousand) %>% 
                        dplyr::filter(establishments_ge == "ტურბაზა (ტურისტული ბაზა)")
    
#Build the visualization 
    
tg_tourism_diagram <- ggplot2::ggplot(tg_resorts_tourism, aes(as.factor(year), number)) +
                         geom_col(tg_resorts_tourism, aes(as.factor(year), number), fill = "#ff6699") +
                         geom_text(aes(label = number), size = 1.8, position = position_stack(vjust = .5), 
                                   color = "#00334d",)+
                         theme_minimal(base_family="Sylfaen")+
                         theme(axis.title.x = element_text(colour="black", size=4, hjust=0.5),
                               axis.title.y = element_text(colour="black", size=4, hjust=0.5),
                               axis.text.x=element_text(angle = 90, hjust=0.5, size=4, colour="black"),
                               axis.text.y=element_text(angle = 0, hjust=0.5, size=4, colour="black"),
                               plot.caption = element_text(size=3, colour="black", hjust=0),
                               plot.title=element_text(colour="black", size=5),
                               legend.text = element_text(angle = 0, hjust=0, size=4, colour="black"),
                               legend.title = element_text(colour="black", size=4, face="bold"),
                               legend.key.width=unit(3, "mm"),
                               legend.key.height=unit(3, "mm"),
                               panel.grid.major = element_line(size = 0.05),
                               axis.line = element_line(size = 0.2, colour = "black"))+
                         labs(title = "Resort Establishments (Georgia)\nტურისტული ბაზები (საქართველო)",
                            subtitle ="",
                            caption = "Source: Kobakhidze E. (1971). SSR Resort and Tourism of Georgia. Metsniereba\nწყარო: კობახიძე ე. (1971) საქართველოს სსრ საკურორტო მეურნეობა და ტურიზმი. მეცნიერება",
                            x = "Year\nწელი",
                            y = "Turbaza\nტურისტული ბაზა")+
                         scale_y_continuous(breaks=seq(0, 40, 10), limits = c(0, 40))

#Save the ggplot
ggsave("visualization/tg_tourism_diagram.png", 
           plot = tg_tourism_diagram,
           units = "mm",
           width = 100,
           height = 75)     




tg_tourism_diagram_bed <- ggplot2::ggplot(tg_resorts_tourism, aes(as.factor(year), bed_thousand)) +
  geom_col(fill = "#ff6699") +
  geom_text(aes(label = bed_thousand), size = 1.8, position = position_stack(vjust = .5), 
            color = "#00334d",)+
  theme_minimal(base_family="Sylfaen")+
  theme(axis.title.x = element_text(colour="black", size=4, hjust=0.5),
        axis.title.y = element_text(colour="black", size=4, hjust=0.5),
        axis.text.x=element_text(angle = 90, hjust=0.5, size=4, colour="black"),
        axis.text.y=element_text(angle = 0, hjust=0.5, size=4, colour="black"),
        plot.caption = element_text(size=3, colour="black", hjust=0),
        plot.title=element_text(colour="black", size=5),
        legend.text = element_text(angle = 0, hjust=0, size=4, colour="black"),
        legend.title = element_text(colour="black", size=4, face="bold"),
        legend.key.width=unit(3, "mm"),
        legend.key.height=unit(3, "mm"),
        panel.grid.major = element_line(size = 0.05),
        axis.line = element_line(size = 0.2, colour = "black"))+
  labs(title = "Resort Establishments (Georgia)\nტურისტული ბაზები (საქართველო)",
       subtitle ="",
       caption = "Source: Kobakhidze E. (1971). SSR Resort and Tourism of Georgia. Metsniereba\nწყარო: კობახიძე ე. (1971) საქართველოს სსრ საკურორტო მეურნეობა და ტურიზმი. მეცნიერება",
       x = "Year\nწელი",
       y = "Turbaza\nტურისტული ბაზა")+
  scale_y_continuous(breaks=seq(0, 10, 2), limits = c(0, 10))







tg_resorts_all <- tg_resorts %>% 
  dplyr::select(year:bed_thousand) %>% 
  dplyr::filter(year != 1965,
                year != 1969,
                year != 1945)
  




resorts_all_bed <- ggplot2::ggplot(tg_resorts_all, aes(as.factor(year), bed_thousand, fill = establishments_ge)) +
                         geom_col() +
                         geom_text(aes(label=bed_thousand), position=position_stack(0.5), size = 1.8)+
                         theme_minimal(base_family="Sylfaen")+
  
                         theme(axis.title.x = element_text(colour="black", size=4, hjust=0.5),
                               axis.title.y = element_text(colour="black", size=4, hjust=0.5),
                               axis.text.x=element_text(angle = 90, hjust=0.5, size=4, colour="black"),
                               axis.text.y=element_text(angle = 0, hjust=0.5, size=4, colour="black"),
                               plot.caption = element_text(size=3, colour="black", hjust=0),
                               plot.title=element_text(colour="black", size=5),
                               legend.text = element_text(angle = 0, hjust=0, size=4, colour="black"),
                               legend.title = element_text(colour="black", size=4, face="bold"),
                               legend.key.width=unit(3, "mm"),
                               legend.key.height=unit(3, "mm"),
                               panel.grid.major = element_line(size = 0.05),
                               axis.line = element_line(size = 0.2, colour = "black"))+
  
  labs(title = "Resort Establishments (Georgia)\nსაკურორტო დაწესებულებები (საქართველო)",
       subtitle ="",
       caption = "Source: Kobakhidze E. (1971). SSR Resort and Tourism of Georgia. Metsniereba\nწყარო: კობახიძე ე. (1971) საქართველოს სსრ საკურორტო მეურნეობა და ტურიზმი. მეცნიერება",
       x = "Year\nწელი",
       y = "Number of Beds\nსაწოლების რააოდენობა")+
  scale_y_continuous(breaks=seq(0, 155, 20), limits = c(0, 155))+
  
  scale_fill_manual(name = "Resort establishments types\nსაკურორტო დაწესებულებების ტიპები",
                    labels = c("Holiday house\nდასასვენებელი სახლი", "Boarding housen\nპანსიონატი", "Sanatorium\nსანატორიუმი", "Turbaza\nტურისტული ბაზა", "Tourism/resort establishments\nტურისტული/საკურორტო დაწესებულება"),
                    values = c("#00e6e6", "#ff9900", "#ace600", "#03fccf", "#fcd303"))


#Save the ggplot
ggsave("visualization/tg_resorts_all_bed.png", 
       plot = tg_resorts_all_bed,
       units = "mm",
       width = 100,
       height = 75)     





resorts_all <- ggplot2::ggplot(tg_resorts_all, aes(as.factor(year), number, fill = establishments_ge)) +
  geom_col() +
  geom_text(aes(label=number), position=position_stack(0.5), size = 1.8)+
  theme_minimal(base_family="Sylfaen")+
  
  theme(axis.title.x = element_text(colour="black", size=4, hjust=0.5),
        axis.title.y = element_text(colour="black", size=4, hjust=0.5),
        axis.text.x=element_text(angle = 90, hjust=0.5, size=4, colour="black"),
        axis.text.y=element_text(angle = 0, hjust=0.5, size=4, colour="black"),
        plot.caption = element_text(size=3, colour="black", hjust=0),
        plot.title=element_text(colour="black", size=5),
        legend.text = element_text(angle = 0, hjust=0, size=4, colour="black"),
        legend.title = element_text(colour="black", size=4, face="bold"),
        legend.key.width=unit(3, "mm"),
        legend.key.height=unit(3, "mm"),
        panel.grid.major = element_line(size = 0.05),
        axis.line = element_line(size = 0.2, colour = "black"))+
  
  labs(title = "Resort Establishments (Georgia)\nსაკურორტო დაწესებულებები (საქართველო)",
       subtitle ="",
       caption = "Source: Kobakhidze E. (1971). SSR Resort and Tourism of Georgia. Metsniereba\nწყარო: კობახიძე ე. (1971) საქართველოს სსრ საკურორტო მეურნეობა და ტურიზმი. მეცნიერება",
       x = "Year\nწელი",
       y = "Number of Beds\nსაწოლების რააოდენობა")+
  scale_y_continuous(breaks=seq(0, 630, 60), limits = c(0, 630))+
  
  scale_fill_manual(name = "Resort establishments types\nსაკურორტო დაწესებულებების ტიპები",
                    labels = c("Holiday house\nდასასვენებელი სახლი", "Boarding housen\nპანსიონატი", "Sanatorium\nსანატორიუმი", "Turbaza\nტურისტული ბაზა", "Tourism/resort establishments\nტურისტული/საკურორტო დაწესებულება"),
                    values = c("#00e6e6", "#ff9900", "#ace600", "#03fccf", "#fcd303"))


#Save the ggplot
ggsave("visualization/resorts_all.png", 
       plot = resorts_all,
       units = "mm",
       width = 100,
       height = 75)     



tg <- (tg_resort_diagram + tg_resort_diagram_bed) / (tg_tourism_diagram+tg_tourism_diagram_bed) / (resorts_all + resorts_all_bed)


ggsave("visualization/tg.png", 
       plot = tg,
       units = "mm",
       width = 250,
       height = 175)   