# turn off scientific notation
options(scipen=999)

# load dplyr package for data manipulation functionality (matrix math)
library(dplyr)

# load tidyr package for formatting functionality (like spread function)
library(tidyr)

# set working directory (Modified code to allow user to choose working directory)
dir <- choose.dir(getwd(), "Select your working directory")
setwd(dir)

# read input data
Initial_data <- read.table("baci92_2014.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
Markets <- read.table("country_code_baci92.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
Products <- read.table("product_code_baci92.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# convert HS6 to HS4
Initial_data$hs4 <- substr(Initial_data$hs6,1,nchar(Initial_data$hs6)-2)

# summarise input data by products, markets, and quantifying data
Data <- Initial_data %>% group_by(i, hs4) %>% summarise(v=sum(v))

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

# convert RCA to Binary Analysis, using 1 as threshhold. Create Calc2 to preserve RCA in Calc1.
Calc2 <- Calc1
Calc2$Binary <- ifelse(Calc1$RCA>1, 1, 0)
Calc2$RCA <- NULL # Removing this row reduces file size, which is necessary later



# *===========================================================================		
#   // Calculate ECI and PCI
# capture program drop ecipci
# program ecipci
# 
# mata kc0 = M*J(Npx,Npx,1) 
# mata kp0 = J(Nix,Nix,1)* M
# mata kc0_all = M*J(Npx,Npx,1)
# 
# mata Mptilde=((M:/kp0):/kc0)'*M
# 	mata eigensystem(Mptilde,Vp=.,lp=.)		
# 	mata kp=Re(Vp[.,2]) 			// complexity: second eigenvector
# 	mata kc = (M:/kc0_all) * kp				// complexity: second eigenvector
# 
# 	mata kc01d = M*J(Npx,1,1)
# 	mata eigensign = 2*(correlation((kc01d, kc))[1,2] > 0) - 1
# 	mata kp = eigensign :* kp
# 	mata kc = eigensign :* kc
# 
# 	mata kc = kc*J(1,Npx,1)
# 	mata kp1d = kp
# 	mata kp = J(Nix,1,1)*kp'
# 
# end
# *===========================================================================


# calculate KC0 and KP0
KC0 <- Calc2 %>% group_by(Market) %>% summarise(Count = sum(Binary))
KP0 <- Calc2 %>% group_by(Product) %>% summarise(ProductCount = sum(Binary))


# create matrix template for below
Calc2 <- Calc2 %>% ungroup()
Template <- spread(Calc2, Product, Binary, fill=0)

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

eni <- ecipciEstimate$kc
pni<- ecipciEstimate$kp

meaneni <- mean(eni)
sdeni <- sd(eni)

meanpni <- mean(pni)
sdpni <- sd(pni)

eni <- (eni-meaneni)/sdeni
pni <- (pni-meanpni)/sdpni

eni <- data.frame(Template$Market, eni)
colnames(eni) <- c("Market", "ENI")



# import and modify GDP data
GDP <- read.csv("GDP_WB.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
colnames(GDP) <- c("Country","Abbr","Code","GDP")
GDP$Log_GDP <- log10(GDP$GDP)
GDP$Code <- as.integer(GDP$Code)

# test eci to verify that calculations are correct. R-squared should be ~0.4.
eni2 <- left_join(eni, GDP, by=c("Market"="Code"))
new_reg_model <- lm(Log_GDP ~ ENI, data=eni2)
summary(new_reg_model)
