# Set the Year. 15 = 2015, 99 = 1999, etc...
Year <- as.character("15")




# turn off scientific notation
options(scipen=999)

# load dplyr and tidyr package for data manipulation functionality (matrix calcs)
library(plyr)
library(dplyr)
library(tidyr)

# set working directory
setwd("C:/Users/Jevon/Desktop/DevGPS/Data")

# read CBP input data
Temp <- paste("MSA/cbp", Year, "msa.txt", sep = "")
Initial_data <- read.table(Temp, header=TRUE, sep=",", stringsAsFactors=FALSE)

# Load different Cities/MSAs by different Census periods ('03, '07, '12)
if (Year >= 20) {
    Temp <- "MSA/Cities_1998.csv"
  } else if (Year >= 12) {
    Temp <- "MSA/Cities_2012.csv"
  } else if (Year >= 07) {
    Temp <- "MSA/Cities_2007.csv"
  } else if (Year >= 03) {
    Temp <- "MSA/Cities_2003.csv"
  } else Temp <- "MSA/Cities_1998.csv"
Regions <- read.table(Temp, header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)
Industries <- read.table("MSA/NAICS.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
GDP <- read.table("MSA/2014_GDP_MSA.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, check.names = FALSE)

#Pull relevant data from initial data
Data <- data.frame("Region" = Initial_data[,1], 
                  "Industry" = Initial_data[,2], 
                  "Flag" = Initial_data[,3],
                  "Quantity" = Initial_data[,5]) 

#Fix formatting issue with Industries
Data$Industry <- as.character(Data$Industry)

# Remove all NAICS codes with less than 6 digits
Data <- Data[ grep("/", Data$Industry, invert = TRUE),]
Data <- Data[ grep("-", Data$Industry, invert = TRUE),]

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

# Convert RA from Calc1 into a binary analysis for the purpose of Eigen analysis
Calc2 <- Calc1
Calc2$Binary <- ifelse(Calc1$RA>0.75, 1, 0)
Calc2$RA <- NULL