WQ_in <- read.csv("~/data_processing/tdc-groundwater/2023_SoE_GW/WQ_summary_table/2023-08 GWQ AllSites 2010-2023.csv", header=TRUE)

### turn nondetects into zeros, turn columns with nondetects into numeric
WQ_cleaned <- data.frame(matrix(nrow=nrow(WQ_in), ncol=ncol(WQ_in)))
colnames(WQ_cleaned) <- colnames(WQ_in)

nondetects_all <- grep("<", WQ_in)
for (k in 1:length(nondetects_all)) {
  j <- nondetects_all[k]
  for (i in 1:nrow(WQ_in)) {
    if (length(grep("<", WQ_in[i,j]))==1) {WQ_cleaned[i,j] <- 0}
    else {WQ_cleaned[i,j] <- as.numeric(WQ_in[i,j])}
  }
}
for (m in 1:ncol(WQ_cleaned)) {
  if (is.na(match(m, nondetects_all))==TRUE) {
    WQ_cleaned[m] <- WQ_in[m]
  }
}

### Min/Max/Mean/Count WQ Summary Table
each_site <- unique(WQ_in$site_name)
num_parameters <- 29
final_table <- data.frame(matrix(ncol=(num_parameters*4 +1), nrow=length(each_site)))
colnames(final_table) <- c("Site", 
                           "E.coli(min)", "E.coli(max)", "E.coli(avg)","E.coli(count)",
                           "E.coli_presumptive(min)","E.coli_presumptive(max)","E.coli_presumptive(avg)","E.coli_presumptive(count)",
                           "Total_Coliforms(min)", "Total_Coliforms(max)","Total_Coliforms(avg)","Total_Coliforms(count)",
                           "Total_Coliforms_confirmed(min)","Total_Coliforms_confirmed(max)","Total_Coliforms_confirmed(avg)","Total_Coliforms_confirmed(count)",
                           "Faecal_Coliforms(min)","Faecal_Coliforms(max)","Faecal_Coliforms(avg)","Faecal_Coliforms(count)",
                           "Nitrate-N(min)","Nitrate-N(max)","Nitrate-N(avg)","Nitrate-N(count)",
                           "Lab_pH(min)","Lab_pH(max)","Lab_pH(avg)","Lab_pH(count)",
                           "Field_pH(min)","Field_pH(max)","Field_pH(avg)","Field_pH(count)",
                           "ORP(min)","ORP(max)","ORP(avg)","ORP(count)",
                           "Dissolved_Oxygen_PercentSat(min)","Dissolved_Oxygen_PercentSat(max)","Dissolved_Oxygen_PercentSat(avg)","Dissolved_Oxygen_PercentSat(count)",
                           "Dissolved_Oxygen(mg/L)(min)","Dissolved_Oxygen(mg/L)(max)","Dissolved_Oxygen(mg/L)(avg)","Dissolved_Oxygen(mg/L)(count)",
                           "Lab_Conductivity(min)","Lab_Conductivity(max)","Lab_Conductivity(avg)","Lab_Conductivity(count)",
                           "Field_Conductivity(min)","Field_Conductivity(max)","Field_Conductivity(avg)","Field_Conductivity(count)",
                           "Water_Temperature(min)","Water_Temperature(max)","Water_Temperature(avg)","Water_Temperature(count)",
                           "Bromide(min)","Bromide(max)","Bromide(avg)","Bromide(count)",
                           "Chloride(min)","Chloride(max)","Chloride(avg)","Chloride(count)",
                           "Dissolved_Calcium(min)","Dissolved_Calcium(max)","Dissolved_Calcium(avg)","Dissolved_Calcium(count)",
                           "Dissolved_Iron(min)","Dissolved_Iron(max)","Dissolved_Iron(avg)","Dissolved_Iron(count)",
                           "Dissolved_Magnesium(min)","Dissolved_Magnesium(max)","Dissolved_Magnesium(avg)","Dissolved_Magnesium(count)",
                           "Dissolved_Manganese(min)","Dissolved_Manganese(max)","Dissolved_Manganese(avg)","Dissolved_Manganese(count)",
                           "Dissolved_Potassium(min)","Dissolved_Potassium(max)","Dissolved_Potassium(avg)","Dissolved_Potassium(count)",
                           "Dissolved_Reactive_Phosphorus(min)","Dissolved_Reactive_Phosphorus(max)","Dissolved_Reactive_Phosphorus(avg)","Dissolved_Reactive_Phosphorus(count)",
                           "Dissolved_Sodium(min)","Dissolved_Sodium(max)","Dissolved_Sodium(avg)","Dissolved_Sodium(count)",
                           "Free_Carbon_Dioxide(min)","Free_Carbon_Dioxide(max)","Free_Carbon_Dioxide(avg)","Free_Carbon_Dioxide(count)",
                           "Fluoride(min)","Fluoride(max)","Fluoride(avg)","Fluoride(count)",
                           "Reactive_Silica(min)","Reactive_Silica(max)","Reactive_Silica(avg)","Reactive_Silica(count)",
                           "Sulphate(min)","Sulphate(max)","Sulphate(avg)","Sulphate(count)",
                           "Total_Ammonia(min)","Total_Ammonia(max)","Total_Ammonia(avg)","Total_Ammonia(count)",
                           "Total_Hardness(min)","Total_Hardness(max)","Total_Hardness(avg)","Total_Hardness(count)"
)

for (s in 1:length(each_site)) {
  final_table$Site[s] <- each_site[s]
  one_site <- which(WQ_cleaned$site_name == each_site[s])

  final_table$`E.coli(min)`[s] <- min(na.omit(WQ_cleaned$e_coli_.mpn.100ml.[one_site]))
  final_table$`E.coli(max)`[s] <- max(na.omit(WQ_cleaned$e_coli_.mpn.100ml.[one_site]))
  final_table$`E.coli(avg)`[s] <- mean(na.omit(WQ_cleaned$e_coli_.mpn.100ml.[one_site]))
  final_table$`E.coli(count)`[s] <- length(na.omit(WQ_cleaned$e_coli_.mpn.100ml.[one_site]))
  final_table$`E.coli_presumptive(min)`[s] <- min(na.omit(WQ_cleaned$e_coli_presumptive[one_site]))
  final_table$`E.coli_presumptive(max)`[s] <- max(na.omit(WQ_cleaned$e_coli_presumptive[one_site]))
  final_table$`E.coli_presumptive(avg)`[s] <- mean(na.omit(WQ_cleaned$e_coli_presumptive[one_site]))
  final_table$`E.coli_presumptive(count)`[s] <- length(na.omit(WQ_cleaned$e_coli_presumptive[one_site]))

  final_table$`Total_Coliforms(min)`[s] <- min(na.omit(WQ_cleaned$total_coliforms_.cfu.100ml.[one_site]))
  final_table$`Total_Coliforms(max)`[s] <- max(na.omit(WQ_cleaned$total_coliforms_.cfu.100ml.[one_site]))
  final_table$`Total_Coliforms(avg)`[s] <- mean(na.omit(WQ_cleaned$total_coliforms_.cfu.100ml.[one_site]))
  final_table$`Total_Coliforms(count)`[s] <- length(na.omit(WQ_cleaned$total_coliforms_.cfu.100ml.[one_site]))
  final_table$`Total_Coliforms_confirmed(min)`[s] <- min(na.omit(WQ_cleaned$total_coliforms_confirmed[one_site]))
  final_table$`Total_Coliforms_confirmed(max)`[s] <- max(na.omit(WQ_cleaned$total_coliforms_confirmed[one_site]))
  final_table$`Total_Coliforms_confirmed(avg)`[s] <- mean(na.omit(WQ_cleaned$total_coliforms_confirmed[one_site]))
  final_table$`Total_Coliforms_confirmed(count)`[s] <- length(na.omit(WQ_cleaned$total_coliforms_confirmed[one_site]))
  final_table$`Faecal_Coliforms(min)`[s] <- min(na.omit(WQ_cleaned$faecal._coliforms_.cfu.100ml.[one_site]))
  final_table$`Faecal_Coliforms(max)`[s] <- max(na.omit(WQ_cleaned$faecal._coliforms_.cfu.100ml.[one_site]))
  final_table$`Faecal_Coliforms(avg)`[s] <- mean(na.omit(WQ_cleaned$faecal._coliforms_.cfu.100ml.[one_site]))
  final_table$`Faecal_Coliforms(count)`[s] <- length(na.omit(WQ_cleaned$faecal._coliforms_.cfu.100ml.[one_site]))

  final_table$`Nitrate-N(min)`[s] <- min(na.omit(WQ_cleaned$nitrate_n_.g.m3.[one_site]))
  final_table$`Nitrate-N(max)`[s] <- max(na.omit(WQ_cleaned$nitrate_n_.g.m3.[one_site]))
  final_table$`Nitrate-N(avg)`[s] <- mean(na.omit(WQ_cleaned$nitrate_n_.g.m3.[one_site]))
  final_table$`Nitrate-N(count)`[s] <- length(na.omit(WQ_cleaned$nitrate_n_.g.m3.[one_site]))
  
  final_table$`Lab_pH(min)`[s] <- min(na.omit(WQ_cleaned$ph[one_site]))
  final_table$`Lab_pH(max)`[s] <- max(na.omit(WQ_cleaned$ph[one_site]))
  final_table$`Lab_pH(avg)`[s] <- mean(na.omit(WQ_cleaned$ph[one_site]))
  final_table$`Lab_pH(count)`[s] <- length(na.omit(WQ_cleaned$ph[one_site]))
  final_table$`Field_pH(min)`[s] <- min(na.omit(WQ_cleaned$ph_field[one_site]))
  final_table$`Field_pH(max)`[s] <- max(na.omit(WQ_cleaned$ph_field[one_site]))
  final_table$`Field_pH(avg)`[s] <- mean(na.omit(WQ_cleaned$ph_field[one_site]))
  final_table$`Field_pH(count)`[s] <- length(na.omit(WQ_cleaned$ph_field[one_site]))
  
  final_table$`ORP(min)`[s] <- min(na.omit(WQ_cleaned$orp_.mv.[one_site]))
  final_table$`ORP(max)`[s] <- max(na.omit(WQ_cleaned$orp_.mv.[one_site]))
  final_table$`ORP(avg)`[s] <- mean(na.omit(WQ_cleaned$orp_.mv.[one_site]))
  final_table$`ORP(count)`[s] <- length(na.omit(WQ_cleaned$orp_.mv.[one_site]))
  
  final_table$`Dissolved_Oxygen_PercentSat(min)`[s] <- min(na.omit(WQ_cleaned$do_saturation_field_.percent.[one_site]))
  final_table$`Dissolved_Oxygen_PercentSat(max)`[s] <- max(na.omit(WQ_cleaned$do_saturation_field_.percent.[one_site]))
  final_table$`Dissolved_Oxygen_PercentSat(avg)`[s] <- mean(na.omit(WQ_cleaned$do_saturation_field_.percent.[one_site]))
  final_table$`Dissolved_Oxygen_PercentSat(count)`[s] <- length(na.omit(WQ_cleaned$do_saturation_field_.percent.[one_site]))
  
  # dissolved oxygen mg/l missing
  
  final_table$`Lab_Conductivity(min)`[s] <- min(na.omit(WQ_cleaned$conductivity_at_25_degc_.ms.m.[one_site]))
  final_table$`Lab_Conductivity(max)`[s] <- max(na.omit(WQ_cleaned$conductivity_at_25_degc_.ms.m.[one_site]))
  final_table$`Lab_Conductivity(avg)`[s] <- mean(na.omit(WQ_cleaned$conductivity_at_25_degc_.ms.m.[one_site]))
  final_table$`Lab_Conductivity(count)`[s] <- length(na.omit(WQ_cleaned$conductivity_at_25_degc_.ms.m.[one_site]))
  final_table$`Field_Conductivity(min)`[s] <- min(na.omit(WQ_cleaned$WQ_cleaned$conductivity_field_.us.cm.[one_site]))
  final_table$`Field_Conductivity(max)`[s] <- max(na.omit(WQ_cleaned$WQ_cleaned$conductivity_field_.us.cm.[one_site]))
  final_table$`Field_Conductivity(avg)`[s] <- mean(na.omit(WQ_cleaned$WQ_cleaned$conductivity_field_.us.cm.[one_site]))
  final_table$`Field_Conductivity(count)`[s] <- length(na.omit(WQ_cleaned$WQ_cleaned$conductivity_field_.us.cm.[one_site]))
  
  final_table$`Water_Temperature(min)`[s] <- min(na.omit(WQ_cleaned$water_temperature_.degc.[one_site]))
  final_table$`Water_Temperature(max)`[s] <- max(na.omit(WQ_cleaned$water_temperature_.degc.[one_site]))
  final_table$`Water_Temperature(avg)`[s] <- mean(na.omit(WQ_cleaned$water_temperature_.degc.[one_site]))
  final_table$`Water_Temperature(count)`[s] <- length(na.omit(WQ_cleaned$water_temperature_.degc.[one_site]))
  
  final_table$`Bromide(min)`[s] <- min(na.omit(WQ_cleaned$bromide_.g.m3.[one_site]))
  final_table$`Bromide(max)`[s] <- max(na.omit(WQ_cleaned$bromide_.g.m3.[one_site]))
  final_table$`Bromide(avg)`[s] <- mean(na.omit(WQ_cleaned$bromide_.g.m3.[one_site]))
  final_table$`Bromide(count)`[s] <- length(na.omit(WQ_cleaned$bromide_.g.m3.[one_site]))
  
  final_table$`Chloride(min)`[s] <- min(na.omit(WQ_cleaned$chloride_.g.m3.[one_site]))
  final_table$`Chloride(max)`[s] <- max(na.omit(WQ_cleaned$chloride_.g.m3.[one_site]))
  final_table$`Chloride(avg)`[s] <- mean(na.omit(WQ_cleaned$chloride_.g.m3.[one_site]))
  final_table$`Chloride(count)`[s] <- length(na.omit(WQ_cleaned$chloride_.g.m3.[one_site]))
  
  final_table$`Dissolved_Calcium(min)`[s] <- min(na.omit(WQ_cleaned$dissolved_calcium_.g.m3.[one_site]))
  final_table$`Dissolved_Calcium(max)`[s] <- max(na.omit(WQ_cleaned$dissolved_calcium_.g.m3.[one_site]))
  final_table$`Dissolved_Calcium(avg)`[s] <- mean(na.omit(WQ_cleaned$dissolved_calcium_.g.m3.[one_site]))
  final_table$`Dissolved_Calcium(count)`[s] <- length(na.omit(WQ_cleaned$dissolved_calcium_.g.m3.[one_site]))
  
  final_table$`Dissolved_Iron(min)`[s] <- min(na.omit(WQ_cleaned$dissolved_iron_.g.m3.[one_site]))
  final_table$`Dissolved_Iron(max)`[s] <- max(na.omit(WQ_cleaned$dissolved_iron_.g.m3.[one_site]))
  final_table$`Dissolved_Iron(avg)`[s] <- mean(na.omit(WQ_cleaned$dissolved_iron_.g.m3.[one_site]))
  final_table$`Dissolved_Iron(count)`[s] <- length(na.omit(WQ_cleaned$dissolved_iron_.g.m3.[one_site]))
  
  final_table$`Dissolved_Magnesium(min)`[s] <- min(na.omit(WQ_cleaned$dissolved_magnesium_.g.m3.[one_site]))
  final_table$`Dissolved_Magnesium(max)`[s] <- max(na.omit(WQ_cleaned$dissolved_magnesium_.g.m3.[one_site]))
  final_table$`Dissolved_Magnesium(avg)`[s] <- mean(na.omit(WQ_cleaned$dissolved_magnesium_.g.m3.[one_site]))
  final_table$`Dissolved_Magnesium(count)`[s] <- length(na.omit(WQ_cleaned$dissolved_magnesium_.g.m3.[one_site]))
  
  final_table$`Dissolved_Manganese(min)`[s] <- min(na.omit(WQ_cleaned$dissolved_manganese_.g.m3.[one_site]))
  final_table$`Dissolved_Manganese(max)`[s] <- max(na.omit(WQ_cleaned$dissolved_manganese_.g.m3.[one_site]))
  final_table$`Dissolved_Manganese(avg)`[s] <- mean(na.omit(WQ_cleaned$dissolved_manganese_.g.m3.[one_site]))
  final_table$`Dissolved_Manganese(count)`[s] <- length(na.omit(WQ_cleaned$dissolved_manganese_.g.m3.[one_site]))
  
  final_table$`Dissolved_Potassium(min)`[s] <- min(na.omit(WQ_cleaned$dissolved_potassium_.g.m3.[one_site]))
  final_table$`Dissolved_Potassium(max)`[s] <- max(na.omit(WQ_cleaned$dissolved_potassium_.g.m3.[one_site]))
  final_table$`Dissolved_Potassium(avg)`[s] <- mean(na.omit(WQ_cleaned$dissolved_potassium_.g.m3.[one_site]))
  final_table$`Dissolved_Potassium(count)`[s] <- length(na.omit(WQ_cleaned$dissolved_potassium_.g.m3.[one_site]))
  
  final_table$`Dissolved_Reactive_Phosphorus(min)`[s] <- min(na.omit(WQ_cleaned$dissolved_reactive_phosphorus_.g.m3.[one_site]))
  final_table$`Dissolved_Reactive_Phosphorus(max)`[s] <- max(na.omit(WQ_cleaned$dissolved_reactive_phosphorus_.g.m3.[one_site]))
  final_table$`Dissolved_Reactive_Phosphorus(avg)`[s] <- mean(na.omit(WQ_cleaned$dissolved_reactive_phosphorus_.g.m3.[one_site]))
  final_table$`Dissolved_Reactive_Phosphorus(count)`[s] <- length(na.omit(WQ_cleaned$dissolved_reactive_phosphorus_.g.m3.[one_site]))
  
  final_table$`Dissolved_Sodium(min)`[s] <- min(na.omit(WQ_cleaned$dissolved_sodium_.g.m3.[one_site]))
  final_table$`Dissolved_Sodium(max)`[s] <- max(na.omit(WQ_cleaned$dissolved_sodium_.g.m3.[one_site]))
  final_table$`Dissolved_Sodium(avg)`[s] <- mean(na.omit(WQ_cleaned$dissolved_sodium_.g.m3.[one_site]))
  final_table$`Dissolved_Sodium(count)`[s] <- length(na.omit(WQ_cleaned$dissolved_sodium_.g.m3.[one_site]))
  
  final_table$`Free_Carbon_Dioxide(min)`[s] <- min(na.omit(WQ_cleaned$free_carbon_dioxide_at_25_degc_.g.m3.[one_site]))
  final_table$`Free_Carbon_Dioxide(max)`[s] <- max(na.omit(WQ_cleaned$free_carbon_dioxide_at_25_degc_.g.m3.[one_site]))
  final_table$`Free_Carbon_Dioxide(avg)`[s] <- mean(na.omit(WQ_cleaned$free_carbon_dioxide_at_25_degc_.g.m3.[one_site]))
  final_table$`Free_Carbon_Dioxide(count)`[s] <- length(na.omit(WQ_cleaned$free_carbon_dioxide_at_25_degc_.g.m3.[one_site]))
  
  final_table$`Fluoride(min)`[s] <- min(na.omit(WQ_cleaned$fluoride_.g.m3.[one_site]))
  final_table$`Fluoride(max)`[s] <- max(na.omit(WQ_cleaned$fluoride_.g.m3.[one_site]))
  final_table$`Fluoride(avg)`[s] <- mean(na.omit(WQ_cleaned$fluoride_.g.m3.[one_site]))
  final_table$`Fluoride(count)`[s] <- length(na.omit(WQ_cleaned$fluoride_.g.m3.[one_site]))
  
  final_table$`Reactive_Silica(min)`[s] <- min(na.omit(WQ_cleaned$silica_.g.m3_as_sio2.[one_site]))
  final_table$`Reactive_Silica(max)`[s] <- max(na.omit(WQ_cleaned$silica_.g.m3_as_sio2.[one_site]))
  final_table$`Reactive_Silica(avg)`[s] <- mean(na.omit(WQ_cleaned$silica_.g.m3_as_sio2.[one_site]))
  final_table$`Reactive_Silica(count)`[s] <- length(na.omit(WQ_cleaned$silica_.g.m3_as_sio2.[one_site]))
  
  final_table$`Sulphate(min)`[s] <- min(na.omit(WQ_cleaned$sulphate_.g.m3.[one_site]))
  final_table$`Sulphate(max)`[s] <- max(na.omit(WQ_cleaned$sulphate_.g.m3.[one_site]))
  final_table$`Sulphate(avg)`[s] <- mean(na.omit(WQ_cleaned$sulphate_.g.m3.[one_site]))
  final_table$`Sulphate(count)`[s] <- length(na.omit(WQ_cleaned$sulphate_.g.m3.[one_site]))
  
  final_table$`Total_Ammonia(min)`[s] <- min(na.omit(WQ_cleaned$total_ammonia_.g.m3.[one_site]))
  final_table$`Total_Ammonia(max)`[s] <- max(na.omit(WQ_cleaned$total_ammonia_.g.m3.[one_site]))
  final_table$`Total_Ammonia(avg)`[s] <- mean(na.omit(WQ_cleaned$total_ammonia_.g.m3.[one_site]))
  final_table$`Total_Ammonia(count)`[s] <- length(na.omit(WQ_cleaned$total_ammonia_.g.m3.[one_site]))
  
  final_table$`Total_Hardness(min)`[s] <- min(na.omit(WQ_cleaned$total_hardness_.g.m3_as_caco3.[one_site]))
  final_table$`Total_Hardness(max)`[s] <- max(na.omit(WQ_cleaned$total_hardness_.g.m3_as_caco3.[one_site]))
  final_table$`Total_Hardness(avg)`[s] <- mean(na.omit(WQ_cleaned$total_hardness_.g.m3_as_caco3.[one_site]))
  final_table$`Total_Hardness(count)`[s] <- length(na.omit(WQ_cleaned$total_hardness_.g.m3_as_caco3.[one_site]))
}

# Cleanup: turn all the infinities into NaNs
for (n in 2:ncol(final_table)) {
  find_infs <- which(is.infinite(final_table[,n]))
  final_table[find_infs, n] <- NaN
}

write.csv(final_table, "~/data_processing/tdc-groundwater/2023_SoE_GW/WQ_summary_table/GW_SoE_2023_WQsummary.csv")
