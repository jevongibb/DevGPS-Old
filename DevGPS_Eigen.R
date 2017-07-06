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