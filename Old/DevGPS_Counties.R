# turn off scientific notation
options(scipen=999)

# load dplyr, tidyr, reshape2 package for data manipulation functionality (matrix calcs)
library(dplyr)
library(tidyr)
library(reshape2)

# set working directory
setwd("C:/Users/Jevon/Desktop/DevGPS/Data")

# read input data
Initial_data <- read.table("cbp15co.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
Regions <- read.table("Counties.csv", header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)
Industries <- read.table("NAICS.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
GDP <- read.table("2014gdpcounty.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, check.names = FALSE)

# modify Input and Markets data to combine State/City into single value
Initial_data$Code <- Initial_data[,1]*1000+Initial_data[,2] # 2015 uses ALLCAPS

#Pull relevant data from initial data
Data <- data.frame("Region" = Initial_data$Code, 
                  "Industry" = Initial_data[,3], 
                  "Flag" = Initial_data[,4],
                  "Quantity" = Initial_data[,6]) 

# Remove all NAICS codes with less than 6 digits
Data <- Data[ grep("/", Data$Industry, invert = TRUE),]
Data <- Data[ grep("-", Data$Industry, invert = TRUE),]

# remove statewide entries
Data <- Data[ grep("999", Data$Region, invert = TRUE),]
Regions <- Regions[ grep("999", Regions$Code, invert = TRUE),]

# Insert employment quantities for Region/NAICS combinations with Data Suppression Flags
Data$Quantity[with(Data, Flag == "A")] <- 10
Data$Quantity[with(Data, Flag == "B")] <- 60
Data$Quantity[with(Data, Flag == "C")] <- 175
Data$Quantity[with(Data, Flag == "E")] <- 375
Data$Quantity[with(Data, Flag == "F")] <- 750
Data$Quantity[with(Data, Flag == "G")] <- 1750
Data$Quantity[with(Data, Flag == "H")] <- 3750
Data$Quantity[with(Data, Flag == "I")] <- 7500
Data$Quantity[with(Data, Flag == "J")] <- 17500
Data$Quantity[with(Data, Flag == "K")] <- 37500
Data$Quantity[with(Data, Flag == "L")] <- 75000
Data$Quantity[with(Data, Flag == "M")] <- 150000

# Remove Flag column from Data
Data$Flag <- NULL

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
M <- M[, -1]

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

RNS <- data.frame("Region" = Template$Region, "RNS" = KC_final)

INS <- as.data.frame(colnames(Template))
INS <- data.frame(INS[-1,])
colnames(INS) <- "Industry"
INS$INS <- KP_final

# Merge RNS with Regions, keeping all regions (Remember Filter from earlier eliminated some)
RNS <- merge(RNS, Regions, by="Region", all.y = TRUE)

# Set all counties that got filtered to the minimum RNS generated above
RNS$RNS[is.na(RNS$RNS)] <- min(KC_final)

# Format Region Code for Tableau purposes
RNS$Region <- formatC(RNS$Region, width = 5, format = "d", flag = "0")

# Generate RA and Emp for export
RA <- spread(Calc1, Industry, RA, fill=0)
Emp <- spread(Data, Industry, Quantity, fill=0)

# Export Data to CSV
write.csv(RNS, "RNS_CO_2015.csv", row.names = FALSE)
write.csv(RA, "RA_CO_2015.csv", row.names = FALSE)
write.csv(Emp, "Emp_CO_2015.csv", row.names = FALSE)

### NOTE TO SELF: If a county has ZERO activity, it might cause RA/Emp matrix to not match RNS
### Example: King County, TX 48269 



# test correlation with Log GDP per capita
RNS2 <- left_join(RNS, GDP, by=c("Region"="GeoFips"))
new_reg_model <- lm(Log_GDP ~ RNS, data=RNS2)
summary(new_reg_model)
