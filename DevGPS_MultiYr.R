# Use this for RA

MultiYr_RA <- Calc1
colnames(MultiYr_RA)[3] <- paste("20", Year, sep = "")

MultiYr_RA <- merge(MultiYr_RA,Calc1, all = T)
colnames(MultiYr_RA)[ncol(MultiYr_RA)] <- paste("20", Year, sep = "")

MultiYr_RA[is.na(MultiYr_RA)] <- 0



# Use this for Distance

AveDist <- melt(mat2011, id="Region")
colnames(AveDist) <- c("Industry1","Industry2","2011")

NextDist <- melt(mat2015, id="Region")
colnames(NextDist) <- c("Industry1","Industry2","2015")

NewDist$`2015` <- NextDist$`2015`


NextDist$Combined <- paste(NextDist$Industry1,NextDist$Industry2,sep = "")
AveDist$Combined <- paste(AveDist$Industry1,AveDist$Industry2,sep = "")
NewDist <- merge(NextDist, AveDist, by="Combined", all.x = T)
View(NewDist)
NewDist <- NewDist[,c(2,3,4,7)]
colnames(NewDist) <- c("Industry1","Industry2","2012","2011")

NewDist$Avg <- rowMeans(NewDist[,c(3:7)], na.rm = T)

AvgDist <- spread(Data, Industry1, Avg, fill = 0)

AvgDist$Industry2 <- NULL
row.names(AvgDist) <- allIndustry


