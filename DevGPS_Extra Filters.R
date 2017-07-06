## Need to aggregate to 4 digits or Top 10 or something to make visualization easier
Data <- Data[Data$Product != "------",]
Data$Product <- substr(Data$Product,1,nchar(Data$Product)-3)
Data <- Data %>% group_by(Market, Product) %>% summarise(Quantity=sum(Quantity))

# load subset markets/counties/cities
MS_Markets <- read.table("MS_Counties.csv", header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)

Data <- Data %>% ungroup()

Count_Emp <- spread(Data, Product, Quantity, fill=0)
Count_Emp <- filter(Count_Emp, Market %in% MS_Markets$Code)
Count_Emp$Total <- rowSums(Count_Emp)

# summarise trade value by country and product separately
DataMarketTotal <- Data %>% group_by(Market) %>% summarise(MarketTotal=sum(Quantity))
DataProductTotal <- Data %>% group_by(Product) %>% summarise(ProductTotal=sum(Quantity))

# calculate total sum of quantity
GrandTotal <- sum(DataProductTotal$ProductTotal)

Calc4 <- Data
Calc4 <- Calc4 %>% left_join(DataMarketTotal, by=c("Market"="Market"))
Calc4 <- Calc4 %>% left_join(DataProductTotal, by=c("Product"="Product"))

# calculate RCA
Calc4$RCA <- (Calc4$Quantity/Calc4$MarketTotal)/(Calc4$ProductTotal/GrandTotal)
Calc4$RCA[is.na(Calc4$RCA)] <- 0
Calc4$Quantity <- Calc4$MarketTotal <- Calc4$ProductTotal <- NULL

Count_RCA <- spread(Calc4, Product, RCA, fill=0)
Count_RCA <- filter(Count_RCA, Market %in% MS_Markets$Code)
Count_RCA$Total <- rowSums(Count_RCA)

Filtered_RCA <- Calc4
Filtered_RCA <- filter(Filtered_RCA, Market %in% MS_Markets$Code)
Calc3 <- Calc4
Calc3 <- filter(Calc3, Market %in% MS_Markets$Code)
Calc3$Binary <- ifelse(Calc3$RCA>0.75, 1, 0)
Filtered_RCA$RCA <- Filtered_RCA$RCA * Calc3$Binary
Filtered_RCA <- spread(Filtered_RCA, Product, RCA, fill=0)
Filtered_RCA$Total <- rowSums(Filtered_RCA)

Count_Prod <- Calc3
Count_Prod <- filter(Count_Prod, Market %in% MS_Markets$Code)
Count_Prod <- Count_Prod %>% group_by(Market) %>% summarise(Count = sum(Binary))

# Need to melt the tables so they display better?

write.csv(Count_Emp, "MS2014_CountEmp.csv")
write.csv(Count_RCA, "MS2014_CountRCA.csv")
write.csv(Count_Prod, "MS2014_CountProd.csv")
write.csv(Filtered_RCA, "MS2014_FilteredRCA.csv")
