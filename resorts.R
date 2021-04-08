    rm(list=ls())    # clear the workspace
    
    #=== Temur Gugushvii === 
    # Author https://temurgugushvili.ge/
    # Data source:
    
    #libraries
    
    library(ggplot2)
    library(tidyverse)
    
    # import data
    
    tg_resorts <- readr::read_csv("data/tourism_1939_.csv",
                                      col_types = cols(year = "n",
                                                       establishments_ge = "c",
                                                       establishments_en = "c",
                                                       number = "n",
                                                       bed_thousand = "n",
                                                       carrying_capacity = "n"))
    
    #Prepare data for visualization (filter data)
     
    tg_resorts <- tg_resorts %>% 
                  dplyr::select(year:bed_thousand) %>% 
                  dplyr::filter(establishments_ge != "ტურბაზა (ტურისტული ბაზა)",
                                establishments_ge != "ტურისტული/საკურორტო დაწესებულება")
    
    #Build the visualization 
    
    tg_resort_diagram <- ggplot2::ggplot(tg_resorts, aes(year, number)) +
      geom_col(aes(fill = establishments_ge)) +
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
           y = "Establishments\nდაწესებულებები")+
      scale_fill_manual(name = "Resort establishments types\nსაკურორტო დაწესებულებების ტიპები",
                        labels = c("Sanatorium\nსანატორიუმი","Holiday house\nდასასვენებელი სახლი", "Boarding housen\nპანსიონატი"),
                        values = c("#00e6e6", "#ff9900", "#ace600"))+
      scale_y_continuous(breaks=seq(0, 150, 30), limits = c(0, 150))+
      scale_x_continuous(breaks=c(1939, 1945, 1950, 1960, 1967))
    
    #Save the ggplot
    ggsave("visualization/tg_resort_diagram.png", 
           plot = tg_resort_diagram,
           units = "mm",
           width = 100,
           height = 75) 
