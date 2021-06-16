rm(list=ls())    # clear the workspace

############ sankey_analysis ###########

#install.packages("D3Network")

library(readxl)
library(tidyverse)
library(networkD3)
library(d3Network) 

tourismorgprofile <- read.csv("data/tourismorgprofile.csv")


tourismorgprofile <- tourismorgprofile %>%
  mutate(Dimensions = ifelse(Activities=="B&B service","Escapism",
                             ifelse(Activities=="Demonstration farm to the table concept","Entertaitment",
                                    ifelse(Activities=="Demonstration of activities","Aesthetic",  
                                           ifelse(Activities=="Direct sales of local products", "Direct sales", 
                                                  ifelse(Activities=="Educaiton camping", "Escapism",
                                                         ifelse(Activities=="Excursions", "Escapism",
                                                                ifelse(Activities=="Folk and dance", "Entertaitment",    
                                                                       ifelse(Activities=="Food & wine tasting", "Aesthetic",
                                                                              ifelse(Activities=="Master classes in art & crafts", "Education",
                                                                                     ifelse(Activities=="Organizing events", "Entertaitment", 
                                                                                            ifelse(Activities=="Participation in agriculture", "Entertaitment",  
                                                                                                   ifelse(Activities=="Ranch", "Escapism",
                                                                                                          ifelse(Activities=="Outdoor recreation", "Escapism",
                                                                                                                 ifelse(Activities=="Telling stories", "Entertaitment",
                                                                                                                        ifelse(Activities=="Culinary offers","Aesthetic", Activities))))))))))))))))


ProfileActivities  <-  tourismorgprofile  %>% 
                       select(EnterpriseName, Profile, Activities) %>% 
                       distinct(EnterpriseName, Profile, Activities) %>% 
                       group_by(Profile, Activities) %>% 
                       summarise(value = n()) %>% 
                       rename(Source = Profile,
                              Target = Activities)


ActivitiesDimensions <- tourismorgprofile  %>% 
                        select(EnterpriseName, Activities, Dimensions) %>%
                        distinct(EnterpriseName, Activities, Dimensions) %>%
                        group_by(Activities, Dimensions) %>% 
                        summarise(value = n()) %>% 
                        rename(Source = Activities,
                               Target = Dimensions)  %>% 
                        filter(Target != "Direct sales")



links <- bind_rows(list(ProfileActivities, ActivitiesDimensions)) %>%
         mutate(group = ifelse(Source =="Agri-tourism, culinary, gastronomy",
                               "Agri-tourism, culinary, gastronomy",Source))

nodes <- data.frame(name=c(as.character(links$Source), 
                           as.character(links$Target)) %>% unique()) %>%
                    mutate(group = ifelse(name =="Agri-tourism, culinary, 
                                          gastronomy","Agri-tourism, culinary, gastronomy",name))

links$IDsource <- match(links$Source, nodes$name)-1 
links$IDtarget <- match(links$Target, nodes$name)-1


orgprofile <-  sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource",
              Target = "IDtarget", Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 12, 
              units = "chain", nodePadding = 15, 
              fontFamily = "sans-serif", iterations = 0,
              sinksRight = FALSE, 
              LinkGroup = "group", NodeGroup = "group") 


saveNetwork(orgprofile, "visualization/orgprofile.html", selfcontained = TRUE)





