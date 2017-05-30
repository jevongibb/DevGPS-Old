# turn off scientific notation
options(scipen=999)

# load dplyr and tidyr package for data manipulation functionality (matrix calcs)
library(dplyr)
library(tidyr)

# set working directory
setwd("C:/Users/Jevon/Desktop/DevGPS/Data")

# read input data
Initial_data <- read.table("cbp14msa.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
# remember to load different Cities/MSAs by different Census periods ('03, '07, '12)
Regions <- read.table("Cities_2012.csv", header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)
Industries <- read.table("NAICS.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
GDP <- read.table("2014_GDP_MSA.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, check.names = FALSE)

#Pull relevant data from initial data
Data <- data.frame("Region" = Initial_data[,1], 
                  "Industry" = Initial_data[,2], 
                  "Flag" = Initial_data[,3],
                  "Quantity" = Initial_data[,5]) 

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
  
  Mptilde <- t(tempMat2) %*% M
  Mptilde[is.na(Mptilde)] <- 0
  
  eigenAnalysis <- eigen(Mptilde)
  kp <- as.numeric(eigenAnalysis$vectors[,2])
  kc <- (M/kc0_all) %*% kp
  
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

INS <- data.frame(Industry=as.integer(colnames(Template[2:ncol(Template)])), stringsAsFactors=FALSE)
INS$INS <- as.numeric(KP_final)
INS <- left_join(INS, Industries, by = c("Industry" = "NAICS"))

# Create RNS, RA, and Employees for export
RNS <- RNS %>% left_join(Regions, by = c("Region" = "Code"))
RA <- spread(Calc1, Industry, RA, fill=0)
Emp <- spread(Data, Industry, Quantity, fill=0)

# Export Data to CSV
write.csv(RNS, "RNS_MSA_2014.csv", row.names = FALSE)
write.csv(RA, "RA_MSA_2014.csv", row.names = FALSE)
write.csv(Emp, "Emp_MSA_2014.csv", row.names = FALSE)

### END OF DATA GENERATION
### FOLLOWING SECTION IS ONLY FOR ANALYSIS



# load KC0 and Employees data to compare
KC0 <- Calc1 %>% group_by(Region) %>% summarise(Count = sum(RA))
Emp <- Data %>% group_by(Region) %>% summarise(Count = sum(Quantity))

# test correlation with Log GDP per capita
RNS2 <- left_join(RNS, GDP, by=c("Region"="MSA"))
RNS2$KC0 <- KC0$Count
RNS2$Emp <- Emp$Count
gdp_reg_model <- lm(Log_GDP ~ RNS + Population + KC0 + Emp, data=RNS2)
summary(gdp_reg_model)
percapita_reg_model <- lm(Log_PerCapita ~ RNS, data=RNS2)
summary(percapita_reg_model)

library(ggplot2)
library(plotly)

colnames(RNS2)[3] <- "Name"
mygraph <- ggplot(RNS2, aes(x = RNS, y = Log_PerCapita, label = Name)) + geom_point() + geom_smooth(method = 'lm') + labs(x="Economic Network Rating", y="Log GDP Per Capita") + xlim(-1,3.6) + ylim(4.3,5.2)
ggplotly(mygraph)
