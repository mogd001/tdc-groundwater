library(lubridate)

###Load up all the data
WTemp_in <- read.csv("~/data_processing/2023 SoE GW/GW_WTemp_ArchiveAll.csv", header=FALSE,skip=2)
colnames(WTemp_in) <- c('Site','DateTime','WTemp')

Cond_in <- read.csv("~/data_processing/2023 SoE GW/GW_Cond_ArchiveAll.csv", header=FALSE, skip=2)
colnames(Cond_in) <- c('Site', 'DateTime', 'Cond')

Lev_in <- read.csv("~/data_processing/2023 SoE GW/GW_WLevel_ArchiveAll.csv", header=FALSE, skip=2)
colnames(Lev_in) <- c('Site', 'DateTime', 'Level')

WQ_in <- read.csv()

###Temperature Plots
each_Tsite <- unique(WTemp_in$Site)
for (i in 1:length(each_Tsite)) {
  one_site <- which(WTemp_in$Site == each_Tsite[i])

  png(filename=paste(each_Tsite[i],"_WTemp.jpg", sep=""), width=600,height=400)
  par(mar=c(5.1,5,4.1,2))
  plot(dmy_hms(WTemp_in$DateTime[one_site]), WTemp_in$WTemp[one_site], type="l", 
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
  plot(dmy_hms(Cond_in$DateTime[one_site]), Cond_in$Cond[one_site], type="l",
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

### Min/Max/Mean WQ Summary Table

final_table <- data.frame(matrix(ncol=73, nrow=0))
colnames(final_table) <- c("Site", 
                           "E.coli(min)", "E.coli(max)", "E.coli(avg)",
                           "Total_Coliforms(min)", "Total_Coliforms(max)","Total_Coliforms(avg)",
                           "Nitrate-N(min)","Nitrate-N(max)","Nitrate-N(avg)",
                           "pH(min)","pH(max)","pH(avg)",
                           "ORP(min)","ORP(max)","ORP(avg)",
                           "Dissolved_Oxygen_PercentSat(min)","Dissolved_Oxygen_PercentSat(max)","Dissolved_Oxygen_PercentSat(avg)",
                           "Dissolved_Oxygen(mg/L)(min)","Dissolved_Oxygen(mg/L)(max)","Dissolved_Oxygen(mg/L)(avg)",
                           "Conductivity(min)","Conductivity(max)","Conductivity(avg)",
                           "Water_Temperature(min)","Water_Temperature(max)","Water_Temperature(avg)",
                           "Bromide(min)","Bromide(max)","Bromide(avg)",
                           "Chloride(min)","Chloride(max)","Chloride(avg)",
                           "Dissolved_Calcium(min)","Dissolved_Calcium(max)","Dissolved_Calcium(avg)",
                           "Dissolved_Iron(min)","Dissolved_Iron(max)","Dissolved_Iron(avg)",
                           "Dissolved_Magnesium(min)","Dissolved_Magnesium(max)","Dissolved_Magnesium(avg)",
                           "Dissolved_Manganese(min)","Dissolved_Manganese(max)","Dissolved_Manganese(avg)",
                           "Dissolved_Potassium(min)","Dissolved_Potassium(max)","Dissolved_Potassium(avg)",
                           "Dissolved_Reactive_Phosphorus(min)","Dissolved_Reactive_Phosphorus(max)","Dissolved_Reactive_Phosphorus(avg)",
                           "Dissolved_Sodium(min)","Dissolved_Sodium(max)","Dissolved_Sodium(avg)",
                           "Free_Carbon_Dioxide(min)","Free_Carbon_Dioxide(max)","Free_Carbon_Dioxide(avg)",
                           "Fluoride(min)","Fluoride(max)","Fluoride(avg)",
                           "Reactive_Silica(min)","Reactive_Silica(max)","Reactive_Silica(avg)",
                           "Sulphate(min)","Sulphate(max)","Sulphate(avg)",
                           "Total_Ammonia(min)","Total_Ammonia(max)","Total_Ammonia(avg)",
                           "Total_Hardness(min)","Total_Hardness(max)","Total_Hardness(avg)")


