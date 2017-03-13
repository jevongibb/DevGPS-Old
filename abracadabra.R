options(scipen=999)
library(dplyr)
library(tidyr)

# set working directory
setwd("C:/Users/Jevon/Desktop/School/Project/Data")

# read input data
Initial_data <- read.table("baci92_2014.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
Markets <- read.table("country_code_baci92.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
Products <- read.table("product_code_baci92.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# summarise input data by products, markets, and quantifying data
Data <- Initial_data %>% group_by(i, hs6) %>% summarise(v=sum(v))

# rename columns on Data
colnames(Data) <- c("Market", "Product", "Quantity")

# summarise trade value by country and product separately
DataMarketTotal <- Data %>% group_by(Market) %>% summarise(MarketTotal=sum(Quantity))
DataProductTotal <- Data %>% group_by(Product) %>% summarise(ProductTotal=sum(Quantity))

# calculate total sum of quantity
GrandTotal <- sum(DataProductTotal$ProductTotal)

# create data frame for calculating RCA, Diversity (KC0), and Ubiquity (KP0)
Calc1 <- Data
Calc1 <- Calc1 %>% left_join(DataMarketTotal, by=c("Market"="Market"))
Calc1 <- Calc1 %>% left_join(DataProductTotal, by=c("Product"="Product"))

# calculate Relative Comparative Advantage (RCA)
Calc1$RCA <- (Calc1$Quantity/Calc1$MarketTotal)/(Calc1$ProductTotal/GrandTotal)
Calc1$RCA[is.na(Calc1$RCA)] <- 0
Calc1$Quantity <- Calc1$MarketTotal <- Calc1$ProductTotal <- NULL

# convert RCA to Binary Analysis, using 0.75 as threshhold. Create Calc2 to preserve RCA in Calc1.
Calc2 <- Calc1
Calc2$Binary <- ifelse(Calc1$RCA>0.75, 1, 0)
Calc2$RCA <- NULL # Removing this row reduces file size, which is necessary later

# calculate KC0 and KP0
KC0 <- Calc2 %>% group_by(Market) %>% summarise(Count = sum(Binary))
KP0 <- Calc2 %>% group_by(Product) %>% summarise(ProductCount = sum(Binary))



## This section creates Proximity and Density
Calc2 <- Calc2 %>% ungroup() # This fixes a bug. Somehow the section above created a problem.

# prepare matrix for calculation
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

# resort matrix by product1 column
combs <- combs[order(combs$product1),]

# convert Proximity to Density
Density <- combs %>% group_by(product1) %>% summarise(Density=mean(prox))

## Proximity section ends. Return to calculating ECI/PCI.

# create matrix template for below
Template <- spread(Calc2, Product, Binary, fill=0)

# save KP0 and KC0 for calculations below ## I WILL NOT USE KP0 FOR THIS VERSION
KPn_prev <- KP0
KPn_minus2 <- KP0
KCn_prev <- KC0
KCn_minus2 <- KC0


## NEED HELP FROM HERE DOWN

#run loop ## consider eigenvector command instead, use p.24 of the doc sent by Mohammed
count <- 0
while (count<50) { # max number of times
  count <- count+1
  
  # calculate KC(n)
  KCn <- Template
  KCn[, 2:ncol(KCn)] <- sweep(Template[, 2:ncol(Template)], 2, Density, `*`)
  KCn <- apply(KCn[, 2:ncol(KCn)], 1, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })
  
  # calculate KP(n)
  KPn <- KP0andKC0
  KPn[, 2:ncol(KPn)] <- sweep(KP0andKC0[, 2:ncol(KP0andKC0)], 1, KCn_prev, `*`)
  KPn <- apply(KPn[, 2:ncol(KPn)], 2, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })
  
  # stop loop if delta in all KCn and KPn (for even iterations) has become less than 1
  if (count %% 2 == 0) {
    if (all(abs(KCn-KCn_minus2)<1) && all(abs(KPn-KPn_minus2)<1)) break 
    KCn_minus2 <- KCn
    KPn_minus2 <- KPn
  }
    
  # set new values to be previous in new loop (only when KC & KP = even)
  KCn_prev <- KCn
  KPn_prev <- KPn

}
print(paste0("Calculation was done ", count, " times"))

# create ECI table
ECI <- Calc2[, 1:1]
ECI$KC <- KCn
ECI$ECI <- (ECI$KC-mean(ECI$KC))/sd(ECI$KC)
ECI$Market <- as.integer(ECI$Market)
ECI <- merge(ECI, Markets, by.x = "Market", by.y = "i")

# create PCI table
PCI <- data.frame(product=colnames(Calc2[2:ncol(Calc2)]), stringsAsFactors=FALSE)
PCI$KP <- KPn
PCI$PCI <- (PCI$KP-mean(PCI$KP))/sd(PCI$KP)
PCI$PCI <- PCI$PCI * -1

# convert product column to an integer in order to modify for uniformity
PCI$product <- as.integer(PCI$product)
PCI$product <- formatC(PCI$product, width = 4, format = "d", flag = "0")

PCI <- semi_join(PCI, Products2, by=c("product"="CODE"))

# write ECI and PCI to a CSV for export
write.csv(ECI, "ECI.csv")
write.csv(PCI, "PCI.csv")
