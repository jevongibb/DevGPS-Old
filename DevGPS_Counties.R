# turn off scientific notation
options(scipen=999)

# load dplyr and tidyr package for data manipulation functionality (matrix calcs)
library(dplyr)
library(tidyr)

# set working directory
setwd("C:/Users/Jevon/Desktop/DevGPS/Data")

# read input data
Initial_data <- read.table("cbp14co.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
Regions <- read.table("Counties.txt", header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)
Industries <- read.table("NAICS.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# Load GDP data and add Log GDP
GDP <- read.table("2014gdpcounty.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, check.names = FALSE)
GDP$PI <- as.integer(GDP$PI) #NAs will get introduced. Data initiates w/ NAs.
GDP$Log_GDP <- log10(GDP$PI)

# modify Input and Markets data to combine State/City into single value
# 2015 is different format (ALLCAPS)
Initial_data$Code <- Initial_data$fipstate*1000+Initial_data$fipscty
Regions$Code <- Regions$fipstate*1000+Regions$fipscty

# summarise input data by products, markets, and quantifying data
Data <- Initial_data %>% group_by(Code, naics) %>% summarise(emp=sum(emp))

# rename columns on Data
colnames(Data) <- c("Region", "Industry", "Quantity")

## Remove all codes with less than 6 digits
Data <- Data[ grep("/", Data$Industry, invert = TRUE),]
Data <- Data[ grep("-", Data$Industry, invert = TRUE),]

# summarise trade value by country and product separately
DataRegionTotal <- Data %>% group_by(Region) %>% summarise(RegionTotal=sum(Quantity))
DataIndustryTotal <- Data %>% group_by(Industry) %>% summarise(IndustryTotal=sum(Quantity))

# calculate total sum of quantity
GrandTotal <- sum(DataIndustryTotal$IndustryTotal)

# create data frame for calculating Relative Activity (RA)
Calc1 <- Data
Calc1 <- Calc1 %>% left_join(DataRegionTotal, by=c("Region"="Region"))
Calc1 <- Calc1 %>% left_join(DataIndustryTotal, by=c("Industry"="Industry"))

# calculate RA
Calc1$RA <- (Calc1$Quantity/Calc1$RegionTotal)/(Calc1$IndustryTotal/GrandTotal)
Calc1$RA[is.na(Calc1$RA)] <- 0
Calc1$Quantity <- Calc1$RegionTotal <- Calc1$IndustryTotal <- NULL

# convert RA to Binary Analysis, using 0.75 as threshhold
# create Calc2 to preserve RA in Calc1
# will use delta in RA to calculate a market's Heat at later stage
Calc2 <- Calc1
Calc2$Binary <- ifelse(Calc1$RA>0.75, 1, 0)
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
