# turn off scientific notation
options(scipen=999)

# load dplyr and tidyr package for data manipulation functionality (matrix calcs)
library(plyr)
library(dplyr)
library(tidyr)

# set working directory
setwd("C:/Users/Jevon/Desktop/DevGPS/Data")

# prepare matrix for Distance calculation
Calc1_matrix <- spread(Calc1, Region, RA, fill=0)
allIndustry <- Calc1_matrix$Industry
Calc1_matrix$Industry <- NULL
Calc1_matrix <- as.matrix(Calc1_matrix)

###############################################################################
# Euclidian Distance
###############################################################################

matJ <- as.matrix(dist(Calc1_matrix, method = "euclidean"))
rownames(matJ) <- allIndustry 
colnames(matJ) <- allIndustry


###############################################################################
# Correlation matrix
###############################################################################

corMat <- as.matrix(cor(t(Calc1_matrix)))
corMat <- 0.5*(1+corMat)
rownames(corMat) <- allIndustry 
colnames(corMat) <- allIndustry


###############################################################################
# Calculation expected RA 
###############################################################################
                                       
topKindustry <- function(industryID, proxMat, datRA, cutDist){
  prox <- proxMat[industryID,]
  proxNeighbor <- prox[prox<=cutDist]
  industryK <- names(proxNeighbor)
  
  topProx <- data.frame(Industry=industryK, prox=as.numeric(proxNeighbor[industryK]), stringsAsFactors = F)
  
  eRAdat <- datRA[datRA$Industry %in% industryK,]
  eRAdat <- eRAdat %>% left_join(topProx, by=c("Industry"="Industry"))
  eRAdat$eRAnum <- eRAdat$RA/(1+eRAdat$prox)
  
  
  estExpectedRA <- plyr::ddply(.data = eRAdat, .variables = "Region",summarize,
                               totProx = sum(1/(1+prox), na.rm = T),
                               totRAnum = sum(eRAnum, na.rm = T))
  estExpectedRA$eRA <- estExpectedRA$totRAnum / estExpectedRA$totProx
  RegionIndExpRA <- data.frame(Industry=industryID, estExpectedRA[c("Region","eRA")], stringsAsFactors = F)
  estExpectedRA <-NULL;eRAdat<-NULL;topProx<-NULL;industryK<-NULL
  
  return(RegionIndExpRA)
}


optimalCut <- function(industryList, cutDist, proxMat, datRA){
  estDistance <- plyr::adply(industryList, 1, topKindustry, cutDist = cutDist, proxMat = proxMat, datRA= datRA)
  estDistance <- estDistance %>% left_join(datRA, by=c("Region", "Industry") )
  estDistance$RA[is.na(estDistance$RA)]<-0
  estDistance$X1<-NULL
  write.csv(estDistance, file = paste("estRA_",paste(cutDist), paste(".csv"),sep = ""),row.names = F)
  
  rmse <- sqrt(1/nrow(estDistance)*t(estDistance$RA - estDistance$eRA) %*% (estDistance$RA - estDistance$eRA))
  return(rmse)
}

cutpoints <- c(75,100,125) # the following two lines will run over 3 different cut off

# Using distance matrix
plyr::aaply(cutpoints, 1, optimalCut, industryList=allIndustry, proxMat=matJ, datRA=Calc1) #tested

# Using correlation matrix
plyr::aaply(cutpoints, 1, optimalCut, industryList=allIndustry, proxMat=corMat, datRA=Calc1) #not tested yet

