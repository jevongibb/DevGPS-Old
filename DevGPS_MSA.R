# turn off scientific notation
options(scipen=999)

# load dplyr and tidyr package for data manipulation functionality (matrix calcs)
library(dplyr)
library(tidyr)

# set working directory
setwd("C:/Users/Jevon/Desktop/DevGPS/Data")

# read input data
Initial_data <- read.table("cbp14msa.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
Regions <- read.table("Cities_2012.csv", header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)
Industries <- read.table("NAICS.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
GDP <- read.table("2014_GDP_MSA.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, check.names = FALSE)

# summarise input data by products, markets, and quantifying data
Data <- Initial_data %>% group_by(msa, naics) %>% summarise(emp=sum(emp))

# rename columns on Data
colnames(Data) <- c("Market", "Product", "Quantity")

# modify GDP data
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

# Calc3
Calc3 <- Calc1
Calc3$RCA <- Calc3$Quantity/Calc3$ProductTotal
Calc3$RCA[is.na(Calc3$RCA)] <- 0
Calc3$Quantity <- Calc3$MarketTotal <- Calc3$ProductTotal <- NULL

# calculate RCA
Calc1$RCA <- (Calc1$Quantity/Calc1$MarketTotal)/(Calc1$ProductTotal/GrandTotal)
Calc1$RCA[is.na(Calc1$RCA)] <- 0
Calc1$Quantity <- Calc1$MarketTotal <- Calc1$ProductTotal <- NULL

# convert RCA to Binary Analysis, using 0.75 as threshhold
# create Calc2 to preserve RCA in Calc1
# will use delta in RCA to calculate a market's Heat at later stage
Calc2 <- Calc1
Calc2$Binary <- ifelse(Calc1$RCA>0.75, 1, 0)
Calc2$RCA <- NULL # Removing this row reduces file size, which is necessary later

# calculate KC0 and KP0
KC0 <- Calc1 %>% group_by(Region) %>% summarise(Count = sum(RA))
KP0 <- Calc1 %>% group_by(Industry) %>% summarise(ProductCount = sum(RA))
Calc2 <- Calc2 %>% ungroup() # Must ungroup after grouping in the two lines above

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
#combs$product1 <- as.integer(combs$product1) ## NAs introduced. Testing whether this is necessary.
#combs$product2 <- as.integer(combs$product2)

# calculate Product Centrality (sum of all prox / number of products)
PCentrality <- rbind(combs %>% group_by(product1) %>% summarise(PCentrality=sum(prox)),
combs %>% group_by(product2) %>% summarise(PCentrality=sum(prox)) %>% rename(product1=product2)) %>%
group_by(product1) %>% summarise(PCentrality=sum(PCentrality)) %>% ungroup()
PCentrality$PCentrality <- PCentrality$PCentrality/(nrow(PCentrality)-1)
colnames(PCentrality) <- c("Product", "PCentrality")

## Removed this section because Market Centrality no longer used.
# calculate Market Centrality (multiply PCentrality by RCA)
# MCentrality <- Template
# MCentrality[, 2:ncol(MCentrality)] <- sweep(Template[, 2:ncol(Template)], 2, PCentrality$PCentrality, '*')
# MCentrality <- apply(MCentrality[, 2:ncol(MCentrality)], 1, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })


# save KC0 and KP0 for calculations below
KCn_prev <- KC0$Count
KCn_minus2 <- KC0$Count
KPn_prev <- KP0$ProductCount
KPn_minus2 <- KP0$ProductCount

#run loop
count <- 0
while (count<20) { # max number of times
  count <- count+1
  
  # calculate KC(n)
  KCn <- Template
  KCn[, 2:ncol(KCn)] <- sweep(Template[, 2:ncol(Template)], 2, KPn_prev, `*`)
  KCn <- apply(KCn[, 2:ncol(KCn)], 1, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })
  
  # calculate KP(n)
  KPn <- Template
  KPn[, 2:ncol(KPn)] <- sweep(Template[, 2:ncol(Template)], 1, KCn_prev, `*`)
  KPn <- apply(KPn[, 2:ncol(KPn)], 2, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })
  
  # stop loop if delta in all KCn and KPn (for even iterations) has become less than 1
  if (count %% 2 == 0) {
    if (all(abs(KCn-KCn_minus2)<1) && all(abs(KPn-KPn_minus2)<1)) break 
    KCn_minus2 <- KCn
    KPn_minus2 <- KPn
  }
    
  # set new values to be previous in new loop
  KCn_prev <- KCn
  KPn_prev <- KPn

}
print(paste0("Calculation was done ", count, " times"))



# create ENI table
ENI <- Template[, 1:1]
ENI$KC <- KCn
ENI$ENI <- c(scale(KCn))
ENI$Market <- as.integer(ENI$Market)
ENI <- merge(ENI, Markets, by.x = "Market", by.y = "Code")

# create PCI table
PNI <- data.frame(product=colnames(Template[2:ncol(Template)]), stringsAsFactors=FALSE)
PNI$KP <- KPn
PNI$PNI <- c(scale(KPn)) #(PNI$KD-mean(PNI$KD))/sd(PNI$KD)


# convert product column to an integer in order to modify for uniformity
# PNI$product <- as.integer(PNI$product)
# PNI$product <- formatC(PNI$product, width = 6, format = "d", flag = "0")

PNI <- left_join(PNI, Products, by=c("product"="Code"))

# write ECI and PCI to a CSV for export
write.csv(ENI, "1999_ENI_MSA.csv")
write.csv(PNI, "1999_PNI_MSA.csv")


# test correlation with Log GDP per capita
ENI2 <- left_join(ENI, GDP, by=c("Market"="MSA"))
new_reg_model <- lm(Log_GDP ~ ENI, data=ENI2)
summary(new_reg_model)
