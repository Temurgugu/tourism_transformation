rm(list=ls())    # clear the workspace

############ sankey_analysis ###########

#install.packages("D3Network")

library(readxl)
library(networkD3)
library(d3Network) 

links <- read.csv ("data/links_1.csv")
nodes <- read.csv ("data/nodes_1.csv")  


#nodes <- data.frame(
#  name=c(as.character(links$source), 
#         as.character(links$target)) %>% unique()
#)



links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1


# Give a color for each group:
#my_color <- 'd3.scaleOrdinal() .domain(["a", "b", "c"]) .range(["orange", "3366CC", "CC6699"])'

sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource",
              Target = "IDtarget", Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 12, 
              units = "chain", nodePadding = 15, 
              fontFamily = "sans-serif", iterations = 0,
              sinksRight = FALSE, 
              LinkGroup = "group", NodeGroup = "group") 








