rm(list=ls())    # clear the workspace

############ sankey_analysis ###########

#install.packages("D3Network")

library(readxl)
library(tidyverse)
library(networkD3)
library(webshot)
library(d3Network) 
library(rbokeh)


tourismorgprofile <- read.csv("data/khartishvili_fig_2_org_profile.csv")


tourismorgprofile <- tourismorgprofile %>%
  mutate(Dimensions = ifelse(Activities=="B&B service","Escapism",
                             ifelse(Activities=="Demonstration farm-to-table concept","Entertainment",
                                    ifelse(Activities=="Demonstration of activities","Aesthetic",  
                                           ifelse(Activities=="Direct sales of local products", "Direct sales", 
                                                  ifelse(Activities=="Educaiton camping", "Escapism",
                                                         ifelse(Activities=="Excursions", "Escapism",
                                                                ifelse(Activities=="Folk dance", "Entertainment",    
                                                                       ifelse(Activities=="Food & wine tasting", "Aesthetic",
                                                                              ifelse(Activities=="Master classes in art & crafts", "Education",
                                                                                     ifelse(Activities=="Organizing events", "Entertainment", 
                                                                                            ifelse(Activities=="Participation in agriculture", "Entertainment",  
                                                                                                   ifelse(Activities=="Ranch", "Escapism",
                                                                                                          ifelse(Activities=="Outdoor recreation", "Escapism",
                                                                                                                 ifelse(Activities=="Telling stories", "Entertainment",
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
         mutate(group = ifelse(Source =="Agritourism, culinary, gastronomy",
                               "Agritourism, culinary, gastronomy",Source))

nodes <- data.frame(name=c(as.character(links$Source), 
                           as.character(links$Target)) %>% unique()) %>%
                    mutate(group = ifelse(name =="Agritourism, culinary, 
                                          gastronomy","Agritourism, culinary, gastronomy",name))

links$IDsource <- match(links$Source, nodes$name)-1 
links$IDtarget <- match(links$Target, nodes$name)-1


khartishvili_fig_2_org_profile <-  sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource",
              Target = "IDtarget", Value = "value", NodeID = "name",
              fontSize = 18, nodeWidth = 12, 
              units = "chain", nodePadding = 15, 
              fontFamily = "sans-serif", iterations = 0,
              sinksRight = FALSE, 
              LinkGroup = "group", NodeGroup = "group") 


saveNetwork(khartishvili_fig_2_org_profile, "visualization/khartishvili_fig_2_org_profile.html", selfcontained = TRUE)

webshot("visualization/khartishvili_fig_2_org_profile.html", "visualization/khartishvili_fig_2_org_profile.png",
        vwidth = 1592,
        vheight = 744)

