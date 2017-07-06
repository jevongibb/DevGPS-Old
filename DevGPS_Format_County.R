# Set the Year. 15 = 2015, 99 = 1999, etc...
Year <- 15




# turn off scientific notation
options(scipen=999)

# load dplyr, tidyr, reshape2 package for data manipulation functionality (matrix calcs)
library(dplyr)
library(tidyr)

# set working directory
setwd("C:/Users/Jevon/Desktop/DevGPS/Data")

# read input data
Temp <- paste("County/cbp", Year, "co.txt", sep = "")
Initial_data <- read.table(Temp, header=TRUE, sep=",", stringsAsFactors=FALSE)
Regions <- read.table("County/Counties.csv", header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)
Industries <- read.table("County/NAICS.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
GDP <- read.table("County/2014gdpcounty.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, check.names = FALSE)

# modify Input and Markets data to combine State/City into single value
Initial_data$Code <- Initial_data[,1]*1000+Initial_data[,2]

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
Regions <- Regions[ grep("999", Regions$Region, invert = TRUE),]

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

# calculate KC0 and KP0 for later comparison
KC0 <- Calc1 %>% group_by(Region) %>% summarise(Count = sum(RA))
KP0 <- Calc1 %>% group_by(Industry) %>% summarise(Count = sum(RA))
Calc1 <- Calc1 %>% ungroup() # Must ungroup after grouping in the two lines above

# Convert RA from Calc1 into a binary analysis for the purpose of Eigen analysis
Calc2 <- Calc1
Calc2$Binary <- ifelse(Calc2$RA>0.75, 1, 0)
Calc2$RA <- NULL