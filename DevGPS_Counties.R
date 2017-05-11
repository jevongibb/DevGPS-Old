# turn off scientific notation
options(scipen=999)

# load dplyr, tidyr, reshape2 package for data manipulation functionality (matrix calcs)
library(dplyr)
library(tidyr)
library(reshape2)

# set working directory
setwd("C:/Users/Jevon/Desktop/DevGPS/Data")

# read input data
Initial_data <- read.table("cbp05co.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
Regions <- read.table("Counties.txt", header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)
Industries <- read.table("NAICS.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
GDP <- read.table("2014gdpcounty.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, check.names = FALSE)

# modify Input and Markets data to combine State/City into single value
Initial_data$Code <- Initial_data$fipstate*1000+Initial_data$fipscty # 2015 uses ALLCAPS
Regions$Code <- Regions$fipstate*1000+Regions$fipscty
Regions$fipstate <- Regions$fipscty <- NULL
colnames(Regions) <- c("Region", "Code")

# summarise input data by products, markets, and quantifying data
Data <- Initial_data %>% group_by(Code, naics) %>% summarise(emp=sum(emp)) # 2015 ALLCAPS

# rename columns on Data
colnames(Data) <- c("Region", "Industry", "Quantity")

# remove statewide entries
Data <- Data[ grep("999", Data$Region, invert = TRUE),]
Regions <- Regions[ grep("999", Regions$Code, invert = TRUE),]

# modify GDP data
GDP$PI <- as.integer(GDP$PI) #NAs will get introduced. Data initiates w/ NAs.
GDP$Log_GDP <- log10(GDP$PI)

# summarise trade value by country and product separately
DataRegionTotal <- Data %>% group_by(Region) %>% summarise(RegionTotal=sum(Quantity))
DataIndustryTotal <- Data %>% group_by(Industry) %>% summarise(IndustryTotal=sum(Quantity))

# calculate total sum of quantity
GrandTotal <- sum(DataIndustryTotal$IndustryTotal)

# create data frame for calculating Relative Comparative Advantage (RCA)
Calc1 <- Data
Calc1 <- Calc1 %>% left_join(DataRegionTotal, by=c("Region"="Region"))
Calc1 <- Calc1 %>% left_join(DataIndustryTotal, by=c("Industry"="Industry"))

# calculate RA
Calc1$RA <- (Calc1$Quantity/Calc1$RegionTotal)/(Calc1$IndustryTotal/GrandTotal)
Calc1$RA[is.na(Calc1$RA)] <- 0
Calc1$Quantity <- Calc1$RegionTotal <- Calc1$IndustryTotal <- NULL

# calculate KC0 and KP0
KC0 <- Calc1 %>% group_by(Region) %>% summarise(Count = sum(RA))
KP0 <- Calc1 %>% group_by(Industry) %>% summarise(Count = sum(RA))
Calc1 <- Calc1 %>% ungroup() # Must ungroup after grouping in the two lines above

Filtered <- spread(Calc1, Industry, RA, fill=0)

# new filter for county-level analysis to remove the small producers that skew data
Filtered$RA <- KC0$Count # Add RA column to Template in order to filter
filter_metric <- 80 # Use this to set the level for filtering. If 80, then you remove the 20% with lowest RCA.
Filtered <- subset(Filtered, RA > quantile(RA, prob = 1 - filter_metric/100)) # Filters. The number of observations should go down by (1-filter_metric).
Filtered$RA <- NULL # Remove RCA column
Filtered <- melt(Filtered, id = "Region")
colnames(Filtered) <- c("Region", "Industry", "RA")
Filtered <- Filtered[order(Filtered$Region),]
rownames(Filtered) <- NULL
KC0 <- Filtered %>% group_by(Region) %>% summarise(Count = sum(RA))
KP0 <- Filtered %>% group_by(Industry) %>% summarise(Count = sum(RA))
Filtered <- Filtered %>% ungroup() # Must ungroup after grouping in the two lines above

# convert RA to Binary Analysis, using 0.75 as threshhold
# create Calc2 to preserve RA in Calc1
# will use delta in RA to calculate a market's Heat at later stage
#Calc2 <- Filtered
Calc2 <- Filtered
Calc2$Binary <- ifelse(Calc2$RA>0.75, 1, 0)
Calc2$RA <- NULL # Removing this row reduces file size, which is necessary later

# Convert Calc2 into matrix for multiple uses below
Template <- spread(Calc2, Industry, Binary, fill=0)


# convert Template to matrix for matrix math
M <- as.matrix(Template)
M<-M[,-1]

# Function to calculate eci and pci, this code adapted from ecipci.ado (STATA code)
ecipci <- function(M){
  kc0 <- M %*% matrix(1, nrow=ncol(M), ncol=ncol(M))
  kp0 <- matrix(1, nrow=nrow(M), ncol = nrow(M)) %*% M
  kc0_all <- M %*% matrix(1, nrow=ncol(M), ncol=ncol(M))
  
  tempMat <- M/kp0
  tempMat2 <- tempMat/kc0
  tempMat2[is.na(tempMat2)] <- 0
  
  Mptilde <- t(tempMat2) %*% M
  
  eigenAnalysis <- eigen(Mptilde)
  kp <- as.numeric(eigenAnalysis$vectors[,2])
  kc <- (M/kc0_all) %*% kp
  kc[is.na(kc)] <- 0
  
  kc01d <- M %*% matrix(1, nrow=ncol(M), ncol=1)
  eigensign <- as.numeric(sign(cor(kc01d, kc)))
  kp <- eigensign * kp
  kc <- eigensign * kc
  
  out <- list(kc=kc, kp=kp)
  return(out)
}

ecipciEstimate <- ecipci(M)

KC_final <- ecipciEstimate$kc
KP_final <- ecipciEstimate$kp

KC_final <- scale(KC_final)
KP_final <- scale(KP_final)

RNS <- data.frame(Template$Region, KC_final)
colnames(RNS) <- c("Region", "RNS")

INS <- as.data.frame(colnames(Template))
INS <- data.frame(INS[-1,])
colnames(INS) <- "Industry"
INS$INS <- KP_final

# test correlation with Log GDP per capita
RNS2 <- left_join(RNS, GDP, by=c("Region"="GeoFips"))
new_reg_model <- lm(Log_GDP ~ RNS, data=RNS2)
summary(new_reg_model)
