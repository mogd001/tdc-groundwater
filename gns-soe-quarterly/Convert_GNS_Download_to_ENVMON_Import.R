library(lubridate)

data_in <- read.csv("~/data_processing/tdc-groundwater/GNS_SOE_Quarterly/result_detailed.csv")
labdata <- data_in[which(data_in$Is.field.analysis. == 'false'),]

final_table <- data.frame(matrix(ncol=17, nrow=nrow(labdata)))
colnames(final_table) <- c(
"DataID","LabRefNo","CollectionDate","CollectionTime","FieldID","TestName","Units","Method","Result",
"SiteName","SiteID","HilltopDataSource","InformationSource","CollectedOn","NonDetect","ResultNumeric","SampleTypeID"
)
final_table$DataID <- labdata$Sample.ID 
final_table$LabRefNo <- labdata$Analysis.ID
final_table$CollectionDate <- date(labdata$Sample.date..NZST.)
final_table$CollectionTime <- strftime(labdata$Sample.date..NZST., format="%H:%M:%S")
final_table$FieldID <- labdata$Feature.name
final_table$TestName <- labdata$Analysis.parameter
final_table$Units <- labdata$Parameter.units
final_table$Method <- labdata$Method
final_table$Result <- labdata$Result
final_table$CollectedOn <- labdata$Sample.date..NZST.
final_table$SampleTypeID <- 1

### InformationSource
data_year <- year(labdata$Sample.date..NZST.[1])
data_month <- as.character(month(labdata$Sample.date..NZST.[1], label=TRUE, abbr=TRUE))
final_table$InformationSource <- paste(
  'DORIS\\Environmental Management\\Groundwater\\Projects\\Quarterly Sampling GNS NGMP\\Data\\',
  data_year," ",data_month, " ENVMON Import File.csv", sep="")

### SiteName, SiteID, HilltopDataSource, NonDetect, ResultNumeric ###

# possible_fieldIDs, possible_sitenames, possible_siteIDs need to be in the same order
possible_fieldIDs <- c("3314","23604","3115",
                       "32","3216","37","471",
                       "6601","24677","802",
                       "8404","8407","Main Spring")
possible_sitenames <- c("GW 3314 - Bensemann","GW 23604 - Bensemann","GW 3115 - Drummond",
                        "GW 32 - TDC", "GW 3216 - Ngati Raru", "GW 37 - Gardner","GW 471",
                        "GW 6601 - Central Takaka Water Bore", "GW 24677", "GW 802 - Waiwest",
                        "GW 8404 - Wrattens", "GW 8407 - Williams", "GW Pupu Main Spring")
possible_siteIDs <- c("369","3256","396",
                      "885","834","398","8451",
                      "884","12475","913",
                      "336","352","2008")

# possible_parameters, possible_Hilltop need to be in the same order
possible_parameters <- c("HCO3","Brom","Chlor","onductivity",
                         "Calcium","Iron","Magnesium",
                         "Manganese", "Potassium","Phosphorus",
                         "Sodium","Fluor","Nitrate","pH",
                         "Silic","Sulphate","Ammoni")
possible_Hilltop <- c("Alkalinity (HCO3)", "Bromide","Chloride","Conductivity @ 25 deg C",
                                 "Dissolved Calcium","Dissolved Iron","Dissolved Magnesium",
                                 "Dissolved Manganese","Dissolved Potassium","Dissolved Reactive Phosphorus",
                                 "Dissolved Sodium","Fluoride","Nitrate-N (IC) GNS","pH",
                                 "Silica","Sulphate","Total Ammonia")

for (i in 1:nrow(labdata)) {
  #SiteName, SiteID
  for (j in 1:length(possible_fieldIDs)) {
    findsite <- grep(possible_fieldIDs[j], labdata$Feature.name[i])
    if (length(findsite) > 0) {
      final_table$SiteName[i] <- possible_sitenames[j]
      final_table$SiteID[i] <- possible_siteIDs[j]
    }
  }
  
  #HilltopDataSource
  for (k in 1:length(possible_parameters)) {
    findparam <- grep(possible_parameters[k], labdata$Analysis.parameter[i])
    if (length(findparam) > 0) {
      final_table$HilltopDataSource[i] <- possible_Hilltop[k]
    }
  }
  
  #NonDetect, ResultNumeric
  if(length(grep("<", labdata$Result[i])) ==0) {
    final_table$NonDetect[i] <- "NULL"
    final_table$ResultNumeric[i] <- labdata$Result[i]
  }
  else {
    final_table$NonDetect[i] <- "<"
    final_table$ResultNumeric[i] <- substr(labdata$Result[i], 2,nchar(labdata$Result[i]))
  }
}

### Remove rows with temperature data, no data, etc
table_out <- final_table[which(is.na(final_table$HilltopDataSource) == FALSE),]

### write output csv file
write.csv(table_out, paste(data_year, data_month,"ENVMON Import File.csv"))
