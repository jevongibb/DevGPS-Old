## This script can be run after finishing Calc2 (converting from RA to Binary)

## Proximity uses definition by Hausmann: The likelihood of concurrence given separate incidence

# prepare matrix for Proximity calculation
Calc2_matrix <- spread(Calc2, Industry, Binary, fill=0)
row.names(Calc2_matrix) <- Calc2_matrix$Region
Calc2_matrix$Region <- NULL
Calc2_matrix <- as.matrix(Calc2_matrix)

# calculate number of markets for all combinations with market multiplication
combs_matrix <- t(Calc2_matrix)%*%Calc2_matrix

# convert matrix to data.frame (long format)
combs_matrix <- as.data.frame(combs_matrix)
combs_matrix$industry1 <- rownames(combs_matrix)
combs <- gather(combs_matrix, industry2, comb, -industry1)

# remove duplicated records
combs <- combs[combs$industry1 < combs$industry2, ]

# calcuate max number of markets and proximity
industry_number_of_markets <- colSums(Calc2_matrix)
combs$n1 <- industry_number_of_markets[as.character(combs$industry1)]
combs$n2 <- industry_number_of_markets[as.character(combs$industry2)]
combs$max <- pmax(combs$n1, combs$n2)
combs$prox <- combs$comb/combs$max
combs$prox[is.na(combs$prox)] <- 0

# remove unnecessary columns
combs$n1 <- combs$n2 <- NULL
#combs$product1 <- as.integer(combs$product1) ## NAs introduced. Testing whether this is necessary.
#combs$product2 <- as.integer(combs$product2)

# calculate industries' Centrality (sum of all prox / number of products)
Centrality <- rbind(combs %>% group_by(industry1) %>% summarise(Centrality=sum(prox)),
combs %>% group_by(industry2) %>% summarise(Centrality=sum(prox)) %>% rename(industry1=industry2)) %>%
group_by(industry1) %>% summarise(Centrality=sum(Centrality)) %>% ungroup()
Centrality$Centrality <- Centrality$Centrality/(nrow(Centrality)-1)
colnames(Centrality) <- c("Industry", "Centrality")

# normalize the Centrality results, with the idea that 1="Center of the Network"
## not sure about the logic here
normalize <- function(x){(x-min(x))/(max(x)-min(x))}
Centrality$Centrality <- normalize(Centrality$Centrality)


## Removed this section because Market Centrality no longer used.
# calculate Region Centrality
# RCentrality <- Template
# RCentrality[, 2:ncol(RCentrality)] <- sweep(Template[, 2:ncol(Template)], 2, Centrality$Centrality, '*')
# RCentrality <- apply(RCentrality[, 2:ncol(RCentrality)], 1, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })

## Proximity and Centrality section ends here

## Distance section begins here

## Distance measures the likelihood of adding a product based upon what it currently produces

Template <- spread(Calc2, Industry, Binary, fill=0)
Distance <- Template
#convert to long format (the variable will be called CalcDist, though it is not CalcDist yet)
Distance <- gather(Distance, Industry, CalcDist, -Region)
Distance <- Distance[order(Distance$Region, Distance$Industry), ]

#split products for running by parts
N_split <- 10
industries_split <- split(colnames(Template)[-1], ceiling(seq_along(colnames(Template)[-1])/(length(colnames(Template)[-1])/N_split)))

Top50_CalcDist <- data.frame()
#run in a loop
for (ii in 1:N_split) {
#create test from combs in long format - take only top 50 by proximity for every product1
test_Top50 <- combs %>% filter(industry1 %in% industries_split[[ii]]) %>% group_by(industry1) %>% arrange(desc(prox)) %>% filter(row_number() %in% c(2:51)) %>% ungroup() %>% arrange(industry1)
#extend test_Top50 with market variable - repeat test_Top50 for every region
test_Top50_region <- test_Top50[rep(seq_len(nrow(test_Top50)), length(Template$Region)), ]
test_Top50_region$Region <- rep(Template$Region, each=nrow(test_Top50))
#for every market and product2, add CalcDist variable (=have variable in your code) from Distance (you added it from Template)
test_Top50_region <- left_join(test_Top50_region, Distance, by=c("industry2"="Industry", "Region"="Region"))
test_Top50_region$CalcDist[is.na(test_Top50_region$CalcDist)] <- 0
#now real CalcDist may be calculated - at once for every industry1
Top50_CalcDist <- rbind(Top50_CalcDist,
test_Top50_region %>% group_by(industry1, Region) %>% summarise(CalcDist=sum(prox*CalcDist)/sum(prox)) %>% ungroup())
}

#sort the same way as Distance was sorted - so that I may replace calculated CalcDist by 1 where Distance=1 (your verification Template[i,j+1]==1)
Top50_CalcDist <- Top50_CalcDist[order(Top50_CalcDist$Region, Top50_CalcDist$industry1), ]
Top50_CalcDist$CalcDist[Distance$CalcDist==1] <- 1

#convert to wide fomat
Distance <- spread(Top50_CalcDist, industry1, CalcDist, fill=0)
