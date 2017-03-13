# turn off scientific notation
options(scipen=999)

# load dplyr package for data manipulation functionality (matrix math)
library(dplyr)

# load tidyr package for formatting functionality (like spread function)
library(tidyr)

# set working directory
setwd("C:/Users/Jevon/Desktop/School/Project/Data")

# read input data
Initial_data <- read.table("baci92_2014.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
Markets <- read.table("country_code_baci92.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
Products <- read.table("product_code_baci92.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
GDP <- read.table("GDPPerCapita.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# summarise input data by products, markets, and quantifying data
Data <- Initial_data %>% group_by(i, hs6) %>% summarise(v=sum(v))

# rename columns on Data
colnames(Data) <- c("Market", "Product", "Quantity")

# modify GDP data
colnames(GDP) <- c("Code", "Country", "GDP")
GDP$Log_GDP <- log10(GDP$GDP)

# summarise trade value by country and product separately
DataMarketTotal <- Data %>% group_by(Market) %>% summarise(MarketTotal=sum(Quantity))
DataProductTotal <- Data %>% group_by(Product) %>% summarise(ProductTotal=sum(Quantity))

# calculate total sum of quantity
GrandTotal <- sum(DataProductTotal$ProductTotal)

# create data frame for calculating Relative Comparative Advantage (RCA)
Calc1 <- Data
Calc1 <- Calc1 %>% left_join(DataMarketTotal, by=c("Market"="Market"))
Calc1 <- Calc1 %>% left_join(DataProductTotal, by=c("Product"="Product"))

# calculate RCA
Calc1$RCA <- (Calc1$Quantity/Calc1$MarketTotal)/(Calc1$ProductTotal/GrandTotal)
Calc1$RCA[is.na(Calc1$RCA)] <- 0
Calc1$Quantity <- Calc1$MarketTotal <- Calc1$ProductTotal <- NULL

# convert RCA to Binary Analysis, using 0.75 as threshhold. Create Calc2 to preserve RCA in Calc1.
# Will use delta in RCA to calculate a market's Heat at later stage.
Calc2 <- Calc1
Calc2$Binary <- ifelse(Calc1$RCA>0.75, 1, 0)
Calc2$RCA <- NULL # Removing this row reduces file size, which is necessary later

# calculate KC0 and KP0 (old approach for ECI/PCI)
KC0 <- Calc2 %>% group_by(Market) %>% summarise(Count = sum(Binary))
KP0 <- Calc2 %>% group_by(Product) %>% summarise(ProductCount = sum(Binary))
Calc2 <- Calc2 %>% ungroup() # This fixes a bug. Somehow the section above created a problem.

# Convert Calc2 into matrix for multiple uses below
Template <- spread(Calc2, Product, Binary, fill=0)

# new approach calculating Market Diversity, Product Centrality, Market Centrality, and Market Clustering
Diversity <- KC0

# prepare matrix for Proximity calculation
Calc2_matrix <- spread(Calc2, Product, Binary, fill=0)
row.names(Calc2_matrix) <- Calc2_matrix$Market
Calc2_matrix$Market <- NULL
Calc2_matrix <- as.matrix(Calc2_matrix)

# calculate number of markets for all combinations with market multiplication
combs_matrix <- t(Calc2_matrix)%*%Calc2_matrix

# convert matrix to data.frame (long format)
combs_matrix <- as.data.frame(combs_matrix)
combs_matrix$product1 <- rownames(combs_matrix)
combs <- gather(combs_matrix, product2, comb, -product1)

# remove duplicated records
combs <- combs[combs$product1 < combs$product2, ]

# calcuate max number of markets and proximity
products_number_of_markets <- colSums(Calc2_matrix)
combs$n1 <- products_number_of_markets[as.character(combs$product1)]
combs$n2 <- products_number_of_markets[as.character(combs$product2)]
combs$max <- pmax(combs$n1, combs$n2)
combs$prox <- combs$comb/combs$max

# remove unnecessary columns
combs$n1 <- combs$n2 <- NULL
combs$product1 <- as.integer(combs$product1)
combs$product2 <- as.integer(combs$product2)

# calculate Product Centrality
PCentrality <- rbind(combs %>% group_by(product1) %>% summarise(PCentrality=sum(prox)),
combs %>% group_by(product2) %>% summarise(PCentrality=sum(prox)) %>% rename(product1=product2)) %>%
group_by(product1) %>% summarise(PCentrality=sum(PCentrality)) %>% ungroup()
PCentrality$PCentrality <- PCentrality$PCentrality/(nrow(PCentrality)-1)
colnames(PCentrality) <- c("Product", "PCentrality")

# calculate Market Centrality
MCentrality <- Template
MCentrality[, 2:ncol(MCentrality)] <- sweep(Template[, 2:ncol(Template)], 2, PCentrality$PCentrality, '*')
MCentrality <- apply(MCentrality[, 2:ncol(MCentrality)], 1, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })

# calculate Market Clustering - This calculation does not do what I wanted it to do. Must replace.
Clustering <- sapply(Markets$i, function(market_num) {
  market_products <- Calc2$Product[Calc2$Market==market_num & Calc2$Binary==1]
  market_combs <- combs[combs$product1 %in% market_products & combs$product2 %in% market_products, ]
  mean(market_combs$prox)
})
Clustering <- data.frame(Market=Markets$i, Clustering=Clustering)

# combine Market Diversity, Market Centrality, and Market Clustering
MarketData <- KC0
colnames(MarketData) <- c("Market", "Diversity")
MarketData <- left_join(MarketData, Clustering, by="Market")
MarketData$MCentrality <- MCentrality

# Scale the MarketData
MarketData$DiversityScaled <- scale(MarketData$Diversity)
MarketData$ClusteringScaled <- scale(MarketData$Clustering)
MarketData$MCentralityScaled <- scale(MarketData$MCentrality)

# Add GDP Per Capita data to the Markets
#MarketData <- merge(MarketData, GDP, by.x = "Market", by.y = "Code")
MarketData <- left_join(MarketData, GDP, by=c("Market"="Code"))


# here, need to insert a regression. Y-axis is Log GDP Per Capita, X-axis is factors below. I conducted this externally. Needs to account for NA values.
reg_model <- lm(Log_GDP ~ DiversityScaled+ClusteringScaled+MCentralityScaled, data=MarketData[complete.cases(MarketData), ])
summary(reg_model)

# apply coefficients from regression to create a single variable called Network


CentralityFactor <- 0.17840
DiversityFactor <- 0.09422
# ClusteringFactor <- -0.082085434
#coef(reg_model)

MarketData$Network <- MarketData$DiversityScaled * DiversityFactor + MarketData$MCentralityScaled * CentralityFactor
MarketData$Network <- predict(reg_model, MarketData)

## Diversity, Centrality, Clustering section ends.
## Return to calculating Economic Network Index and Product Network Index.

# save KD0 and KC0 for calculations below
# KDn_prev <- Density$Density
# KDn_minus2 <- Density$Density
# KCn_prev <- KC0$Count
# KCn_minus2 <- KC0$Count
KCn <- MarketData$Network
KCn_minus2 <- MarketData$Network


#run loop ## still trying to understand eigenvector alternative
count <- 1
while (count<15) { # max number of times
  
  # calculate KD(n)
  KPn <- Template
  KPn[, 2:ncol(KPn)] <- sweep(Template[, 2:ncol(Template)], 1, KCn, `*`)
  KPn <- apply(KPn[, 2:ncol(KPn)], 2, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })
  if (count==1) KPn_minus2 <- KPn
  
  # stop loop if delta in all KCn and KDn (for even iterations) has become less than 1
#  if (count %% 2 == 0) {
 #   if (all(abs(KCn-KCn_minus2)<1) && all(abs(KPn-KPn_minus2)<1)) break 
  #  KCn_minus2 <- KCn
   # KDn_minus2 <- KPn
  }
  
  # set new values to be previous in new loop
  #KCn_prev <- KCn
  #KPn_prev <- KPn
      
  count <- count+1
  
  # calculate KC(n)
  KCn <- Template
  KCn[, 2:ncol(KCn)] <- sweep(Template[, 2:ncol(Template)], 2, KPn, `*`)
  KCn <- apply(KCn[, 2:ncol(KCn)], 1, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })

}
print(paste0("Calculation was done ", count, " times"))


# create ENI table
ENI <- Template[, 1:1]
ENI$KC <- KCn
ENI$ENI <- scale(KCn) #(ENI$KC-mean(ENI$KC))/sd(ENI$KC)
ENI$Market <- as.integer(ENI$Market)
ENI <- merge(ENI, Markets, by.x = "Market", by.y = "i")

# create PCI table
PNI <- data.frame(product=colnames(Template[2:ncol(Template)]), stringsAsFactors=FALSE)
PNI$KP <- KPn
PNI$PNI <- scale(KPn) #(PNI$KD-mean(PNI$KD))/sd(PNI$KD)


# convert product column to an integer in order to modify for uniformity
PNI$product <- as.integer(PNI$product)
PNI$product <- formatC(PNI$product, width = 6, format = "d", flag = "0")

PNI <- left_join(PNI, Products, by=c("product"="CODE"))

# write ECI and PCI to a CSV for export
write.csv(ENI, "ENI.csv")
write.csv(PNI, "PNI.csv")


# insert a new regression. Y-axis is Log GDP Per Capita, X-axis is ENI.
ENI <- merge(ENI, GDP, by.x = "Market", by.y = "Code")
new_reg_model <- lm(Log_GDP ~ ENI, data=ENI)
summary(new_reg_model)
