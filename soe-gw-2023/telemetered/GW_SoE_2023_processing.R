library(lubridate)

###Load up all the data
WTemp_in <- read.csv("~/data_processing/tdc-groundwater/2023_SoE_GW/telemetered/draft2/GW_ContWTemp_20230927.csv", header=FALSE,skip=2)
colnames(WTemp_in) <- c('Site','DateTime','WTemp')

Cond_in <- read.csv("~/data_processing/tdc-groundwater/2023_SoE_GW/telemetered/draft2/GW_ContCond_20230927.csv", header=FALSE, skip=2)
colnames(Cond_in) <- c('Site', 'DateTime', 'Cond')

Lev_in <- read.csv("~/data_processing/2023 SoE GW/GW_WLevel_ArchiveAll.csv", header=FALSE, skip=2)
colnames(Lev_in) <- c('Site', 'DateTime', 'Level')

sitecolors <- c("brown","blue","gold","violet","limegreen")

### Multi-site plots
par(mar=c(5.1,5,4.1,2), bg=NA)

Motueka_sites <- c("GW 24601 - Quayle St","GW 2629 - Lodder Lane","GW 20864 - Golf Course","GW 2614 - Fernwood")

png(filename="Motueka_WTemp.png", width=600,height=400)
site1 <- which(WTemp_in$Site == Motueka_sites[1])
plot(dmy_hms(WTemp_in$DateTime[site1]), WTemp_in$WTemp[site1], type="l",col="blue",
     main = "Motueka-Riuwaka Water Temperature", xlab="Year", ylab=expression("Temperature ("^"o"*"C)"),
     ylim=c(12,20), xlim=c(min(dmy_hms(WTemp_in$DateTime)), max(dmy_hms(WTemp_in$DateTime))))
site3 <- which(WTemp_in$Site == Motueka_sites[3])
lines(dmy_hms(WTemp_in$DateTime[site3]), WTemp_in$WTemp[site3], col="brown")
site4 <- which(WTemp_in$Site == Motueka_sites[4])
lines(dmy_hms(WTemp_in$DateTime[site4]), WTemp_in$WTemp[site4], col="violet")
site2 <- which(WTemp_in$Site == Motueka_sites[2])
lines(dmy_hms(WTemp_in$DateTime[site2]), WTemp_in$WTemp[site2], col="limegreen")
dev.off()

png(filename="Motueka_Cond.png", width=600,height=400)
site1 <- which(Cond_in$Site == Motueka_sites[1])
plot(dmy_hms(Cond_in$DateTime[site1]), Cond_in$Cond[site1], type="l", col="blue",
     main="Motueka-Riuwaka Conductivity", xlab="Year", ylab=expression(paste("Conductivity (" ,mu,"S/cm)", sep="")),
     ylim=c(50, 950), xlim=c(min(dmy_hms(Cond_in$DateTime)), max(dmy_hms(Cond_in$DateTime))))
site3 <- which(Cond_in$Site == Motueka_sites[3])
lines(dmy_hms(Cond_in$DateTime[site3]), Cond_in$Cond[site3], col="brown")
site4 <- which(Cond_in$Site == Motueka_sites[4])
lines(dmy_hms(Cond_in$DateTime[site4]), Cond_in$Cond[site4], col="violet")
site2 <- which(Cond_in$Site == Motueka_sites[2])
lines(dmy_hms(Cond_in$DateTime[site2]), Cond_in$Cond[site2], col="limegreen")
dev.off()

png(filename="Motueka_legend.png", width=600,height=400, bg = "transparent")
plot(2,2, col="white", xlim = c(0,100),ylim = c(0,100), xaxt="n", xlab="", yaxt="n", ylab="", bg="transparent")
legend(x=60, y=100, legend=Motueka_Tsite, 
       col=c("blue","limegreen","brown","violet"),bty="n", pch=19)
dev.off()

Waimea_sites <- c("GW 119 - Chipmill","GW 22160 - Lwr Queen St","GW 23953 - Redwood Rd")

png(filename="Waimea_WTemp.png", width=600,height=400)
site1 <- which(WTemp_in$Site == Waimea_sites[1])
plot(dmy_hms(WTemp_in$DateTime[site1]), WTemp_in$WTemp[site1], type="l",col="brown",
     main = "Waimea Water Temperature", xlab="Year", ylab=expression("Temperature ("^"o"*"C)"),
     ylim=c(12,20), xlim=c(min(dmy_hms(WTemp_in$DateTime)), max(dmy_hms(WTemp_in$DateTime))))
site3 <- which(WTemp_in$Site == Waimea_sites[3])
lines(dmy_hms(WTemp_in$DateTime[site3]), WTemp_in$WTemp[site3], col="gold")
site2 <- which(WTemp_in$Site == Waimea_sites[2])
lines(dmy_hms(WTemp_in$DateTime[site2]), WTemp_in$WTemp[site2], col="blue")
dev.off()

png(filename="Waimea_Cond.png", width=600,height=400)
site1 <- which(Cond_in$Site == Waimea_sites[1])
plot(dmy_hms(Cond_in$DateTime[site1]), Cond_in$Cond[site1], type="l", col="brown",
     main="Waimea Conductivity", xlab="Year", ylab=expression(paste("Conductivity (" ,mu,"S/cm)", sep="")),
     ylim=c(50, 950), xlim=c(min(dmy_hms(Cond_in$DateTime)), max(dmy_hms(Cond_in$DateTime))))
site3 <- which(Cond_in$Site == Waimea_sites[3])
lines(dmy_hms(Cond_in$DateTime[site3]), Cond_in$Cond[site3], col="gold")
site2 <- which(Cond_in$Site == Waimea_sites[2])
lines(dmy_hms(Cond_in$DateTime[site2]), Cond_in$Cond[site2], col="blue")
dev.off()

png(filename="Waimea_legend.png", width=600,height=400, bg="transparent")
plot(2,2, col="white", xlim = c(0,100),ylim = c(0,100), xaxt="n", xlab="", yaxt="n", ylab="", bg="transparent")
legend(x=60, y=100, legend=Waimea_Tsite, 
       col=c("brown","blue","gold"),bty="n", pch=19)
dev.off()



### Temperature Plots
each_Tsite <- unique(WTemp_in$Site)
for (i in 1:length(each_Tsite)) {
  one_site <- which(WTemp_in$Site == each_Tsite[i])

  png(filename=paste(each_Tsite[i],"_WTemp.jpg", sep=""), width=600,height=400)
  par(mar=c(5.1,5,4.1,2))
  plot(dmy_hms(WTemp_in$DateTime[one_site]), WTemp_in$WTemp[one_site], type="l", col="blue",
       main = paste(each_Tsite[i]), xlab="Time (years)", ylab=expression("Temperature ("^"o"*"C)"),
       ylim=c(7,27), xlim=c(min(dmy_hms(WTemp_in$DateTime)), max(dmy_hms(WTemp_in$DateTime))))
  dev.off()
  }

### Conductivity Plots
each_Csite <- unique(Cond_in$Site)
for (j in 1:length(each_Csite)) {
  one_site <- which(Cond_in$Site == each_Csite[j])
  
  png(filename=paste(each_Csite[j], "_Cond.jpg", sep=""), width=600, height=400)
  par(mar=c(5.1,5,4.1,2))
  plot(dmy_hms(Cond_in$DateTime[one_site]), Cond_in$Cond[one_site], type="l", col="red",
       main=paste(each_Csite[j]), xlab="Time (years)", ylab=expression(paste("Conductivity (" ,mu,"S/cm)", sep="")),
       ylim=c(50, 950), xlim=c(min(dmy_hms(Cond_in$DateTime)), max(dmy_hms(Cond_in$DateTime))))
  dev.off()
}

### Groundwater Level Plots
each_Lsite <- unique(Lev_in$Site)
for (k in 1:length(each_Lsite)) {
  one_site <- which(Lev_in$Site == each_Lsite[k])
  
  png(filename=paste(each_Lsite[k], "_Level.jpg", sep=""), width=600, height=400)
  par(mar=c(5.1,5,4.1,2))
  plot(dmy_hms(Lev_in$DateTime[one_site]), Lev_in$Cond[one_site], type="l",
       main=paste(each_Lsite[k]), xlab="Time (years)", ylab="Water Level (mm)",
       ylim=c(-5,200), xlim=c(min(dmy_hms(Lev_in$DateTime)), max(dmy_hms(Lev_in$DateTime))))
  dev.off()
}
