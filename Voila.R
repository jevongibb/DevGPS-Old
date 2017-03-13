options(scipen=999)
library(dplyr)
library(tidyr)

#set working directory
setwd("C:/Users/Jevon/Desktop/School/Project/Data")

# read input data
Initial_data <- read.table("baci92_2014.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
Countries <- read.table("country_code_baci92.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
Products <- read.table("product_code_baci92.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# summarise emp by msa and naics
Data <- Initial_data %>% group_by(i, hs6) %>% summarise(v=sum(v))

# add country column
# Data <- Data %>% left_join(Countries, by=c("i"="i"))

#summarise trade value by country and product separately
DataCountryTotal <- Initial_data %>% group_by(i) %>% summarise(country_v=sum(v))
DataProductTotal <- Initial_data %>% group_by(hs6) %>% summarise(product_v=sum(v))

# calculate total sum of emp
GrandTotal <- sum(DataProductTotal$product_v)

# create data frame for calculating RCA, Diversity (KC0), and Ubiquity (KP0)
Calc1 <- Data
Calc1 <- Calc1 %>% left_join(DataCountryTotal, by=c("i"="i"))
Calc1 <- Calc1 %>% left_join(DataProductTotal, by=c("hs6"="hs6"))

# apply formula to calculate RCA
Calc1$RCA <- (Calc1$v/Calc1$country_v)/(Calc1$product_v/GrandTotal)
Calc1$RCA[is.na(Calc1$RCA)] <- 0
Calc1$v <- Calc1$country_v <- Calc1$product_v <- NULL

# convert Calc1 to wide format
Calc1 <- spread(Calc1, hs6, RCA, fill=0)

# create data frame for KP0 and KC0, using 0.75 as threshhold
KP0andKC0 <- Calc1
KP0andKC0[, 2:ncol(KP0andKC0)] <- ifelse(KP0andKC0[, 2:ncol(KP0andKC0)]>0.75, 1, 0)

# calculate KP0 and KC0
KPn_prev <- apply(KP0andKC0[, 2:ncol(KP0andKC0)], 2, sum)
KPn_minus2 <- KPn_prev
KCn_prev <- apply(KP0andKC0[, 2:ncol(KP0andKC0)], 1, sum)
KCn_minus2 <- KCn_prev

#run loop
count <- 0
while (count<50) { # max number of times
  count <- count+1
  
  # calculate KC(n)
  KCn <- KP0andKC0
  KCn[, 2:ncol(KCn)] <- sweep(KP0andKC0[, 2:ncol(KP0andKC0)], 2, KPn_prev, `*`)
  KCn <- apply(KCn[, 2:ncol(KCn)], 1, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })
  # calculate average KC(n)
  
  # calculate KP(n)
  KPn <- KP0andKC0
  KPn[, 2:ncol(KPn)] <- sweep(KP0andKC0[, 2:ncol(KP0andKC0)], 1, KCn_prev, `*`)
  KPn <- apply(KPn[, 2:ncol(KPn)], 2, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })
  # calculate average KP(n)
  
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
ECI <- Calc1[, 1:1]
ECI$KC <- KCn
ECI$ECI <- (ECI$KC-mean(ECI$KC))/sd(ECI$KC)
ECI <- ECI %>% left_join(Countries, by="i")

# create PCI table
PCI <- data.frame(product=colnames(Calc1[2:ncol(Calc1)]), stringsAsFactors=FALSE)
PCI$KP <- KPn
PCI$PCI <- (PCI$KP-mean(PCI$KP))/sd(PCI$KP)
PCI$PCI <- PCI$PCI * -1

# convert product column to an integer in order to modify for uniformity
PCI$product <- as.integer(PCI$product)
PCI$product <- formatC(PCI$product, width = 6, format = "d", flag = "0")

PCI <- PCI %>% left_join(Products, by=c("product"="CODE"))

# write ECI and PCI to a CSV for export
write.csv(ECI, "ECI.csv")
write.csv(PCI, "PCI.csv")
