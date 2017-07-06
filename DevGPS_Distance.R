Distance <- Template
#convert to long format (the variable will be called CalcDist though it is not CalcDist yet)
Distance <- gather(Distance, Product, CalcDist, -Market)
Distance <- Distance[order(Distance$Market, Distance$Product), ]

#split products for running by parts
N_split <- 10
products_splitted <- split(colnames(Template)[-1], ceiling(seq_along(colnames(Template)[-1])/(length(colnames(Template)[-1])/N_split)))

Top50_CalcDist <- data.frame()
#run in a loop
for (ii in 1:N_split) {
#create test from combs in long format - take only top 50 by proximity for every product1
test_Top50 <- combs %>% filter(product1 %in% products_splitted[[ii]]) %>% group_by(product1) %>% arrange(desc(prox)) %>% filter(row_number() %in% c(2:51)) %>% ungroup() %>% arrange(product1)
#extend test_Top50 with market variable - repeat test_Top50 for every market
test_Top50_market <- test_Top50[rep(seq_len(nrow(test_Top50)), length(Template$Market)), ]
test_Top50_market$Market <- rep(Template$Market, each=nrow(test_Top50))
#for every market and product2, add CalcDist variable (=have variable in your code) from Distance (you added it from Template)
test_Top50_market <- left_join(test_Top50_market, Distance, by=c("product2"="Product", "Market"="Market"))
#test_Top50_market$CalcDist[is.na(test_Top50_market$CalcDist)] <- 0
#now real CalcDist may be calculated - at once for every product1
Top50_CalcDist <- rbind(Top50_CalcDist,
test_Top50_market %>% group_by(product1, Market) %>% summarise(CalcDist=sum(prox*CalcDist)/sum(prox)) %>% ungroup())
}

#sort the same way as Distance was sorted - so that I may replace calculated CalcDist by 1 where Distance=1 (your verification Template[i,j+1]==1)
Top50_CalcDist <- Top50_CalcDist[order(Top50_CalcDist$Market, Top50_CalcDist$product1), ]
Top50_CalcDist$CalcDist[Distance$CalcDist==1] <- 1

#convert to wide fomat, as you initially wanted
Distance <- spread(Top50_CalcDist, product1, CalcDist, fill=0)