library(dplyr)
library(reshape2)

setwd("C:/Users/Jevon/Desktop/DevGPS/Data")

Nodes <- read.table("MSA/NAICS.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
colnames(Nodes) <- c("id", "description")
Nodes$category <- substr(Nodes$id, 0, 1)

Distance <- read.csv("Avg_Dist.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)

normalize <- function(x){ 
     a <- min(x) 
     b <- max(x) 
     (x - a)/(b - a) 
}

NDist <- normalize(Distance)

Edges <- melt(as.matrix(NDist), value.name = "weight", varnames = c("from", "to"))
Edges <- Edges[!(Edges$from==Edges$to),]
Edges$weight <- 1-Edges$weight

# load traded

traded <- read.csv("traded.csv", header = T, sep = ",", check.names = F)
Edges <- filter(Edges, from %in% traded$ID)
Edges <- filter(Edges, to %in% traded$ID)



Edges <- Edges %>% group_by(from) %>% arrange(desc(weight)) %>% filter(row_number() %in% c(2:11)) %>% ungroup() %>% arrange(from)
Edges$weight <- Edges$weight * 100 - 90
Edges$weight <- ifelse(Edges$weight < 0, 0, Edges$weight)
Edges$weight <- round(Edges$weight*2)
