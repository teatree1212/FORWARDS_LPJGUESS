# Script that post-processes LPJ-GUESS raw output of disturbance losses and carbon and ecosystem loss es to create output .csv files:

#Forest_and_ecosystem_carbon_loss_data.csv
#ssp370_disturbance_all_years.csv
#ssp126_disturbance_all_years.csv

# the .csv files are then used in post-processing for plotting in python, based on Haoming Zhong's Figure design. 
# Scripts: plot_figures2_Annual_meeting.py
# these annual plots can then be post-processed into animations, using the 
# script: plot_video2_annualMeeting.py 

# carbon and ecosystem loss data (Forest_and_ecosystem_carbon_loss_data.csv) is visualised as difference plots. 
# Script: plot_difference_plots.R 

# Author: Annemarie Eckes-Shephard
# March 2026
# Fire disturbance is currently turned "off"


#######
#IMPORTANT: Path names need to be adjusted everywhere inside the scripts..

frac_lu <- read.table("../../3_analysis_ForestPaths_Exploratory_runs/landuse_change_forest_age/lu_inputfiles_noNF3/net_lu_HildaPucherMircaEurope.txt", head=TRUE)
frac_lu <- frac_lu[which(frac_lu$Year>=2024),]# subset for future run


frac_sts <- read.table("../../3_analysis_ForestPaths_Exploratory_runs/landuse_change_forest_age/lu_inputfiles_noNF3/luforest_HildaPucherMircaEurope.txt", head=TRUE)
frac_sts <- frac_sts[which(frac_sts$Year>=2024),]# subset for future run

library(DGVMTools)

v16 <- "/Volumes/Anne's Backup/ForestPaths/v16/"  
base_dir <- v16
source("../../3_analysis_ForestPaths_Exploratory_runs/scripts/helper_functions.R")



# ============================================================
# CALCULATE Total Carbon LOSSES  for whole Europe.
# ============================================================
# plot for harvest, followed by plot for Barkbeetle and wind, to better show the different dynamics 
# in the total amounts over time. The plots will be stitched together in the presentation.
for(scenario in c("ssp370","ssp126")){
  
  #retrieve the source file location + add some metadata:
  folder_name <- paste0(base_dir,management, "_fut_MPI-ESM1-2-HR_",
                        scenario, "_diston")
  source.in = defineSource(id= paste0(management,"_",scenario),
                           dir= paste0(folder_name),
                           format = GUESS,
                           name = paste0(management,"_",scenario))
  
  
    # 1. Get forest biomass (from cmass_wood_sts or diam_cmass_wood)
    myData_cmass <- getField(source = source.in, quant = "cmass_wood_sts")
    myData_cmass@data$Gridcell_area <- GridcellArea_m2(myData_cmass@data$Lat)
    myData_cmass@data <- merge(myData_cmass@data,frac_lu)
    myData_cmass@data$Forest_sum_area <- myData_cmass@data$Forest_sum * myData_cmass@data$FOREST
    myData_cmass@data$Forest_sum_MtC <- myData_cmass@data$Forest_sum_area* myData_cmass@data$Gridcell_area  / 1e9
    annual_forest <- myData_cmass@data[, .(Fcmass_annual = sum(Forest_sum_MtC)), by = Year]
    
    
    # Use Forest_sum column which is already forest-level
    # Or if using diam_cmass_wood, process as before
    
    # 2. Get fire losses; on hold (29.Jan 2026)
    myData_fire <- getField(source = source.in, quant = "cflux_forest")
    
    # 3. Get storm losses (already processed to Forest_sum)
    myData_storm <- getField(source = source.in, quant = "storm_dw_sts")
    forest_st_cols <- c("Picea", "Pinus", "Fagus", "Quercus", "Larix", "otherBL", "otherNL", "NatForest")
    frac_cols <- paste0(forest_st_cols, "_frac")
    myData_storm@data <- myData_storm@data[, c("Lon", "Lat", "Year", forest_st_cols), with = FALSE]
    frac_sts_copy <- copy(frac_sts)
    setnames(frac_sts_copy, forest_st_cols, frac_cols, skip_absent = TRUE)
    merged_storm <- myData_storm@data[frac_sts_copy, on = .(Lon, Lat, Year)]
    merged_storm[, Storm_Forest_sum := rowSums(.SD[, ..forest_st_cols] * .SD[, ..frac_cols], na.rm = TRUE)]
    myData_storm <- getField(source = source.in, quant = "storm")
    myData_storm@data$Gridcell_area <- GridcellArea_m2(myData_storm@data$Lat)
    myData_storm@data$Dwstorm_Gridcell_Mtc <- myData_storm@data$DamWoodC* myData_storm@data$Gridcell_area / 1e9 
    annual_storm <- myData_storm@data[, .(stormC_annual = sum(Dwstorm_Gridcell_Mtc)), by = Year]
    
    # 4. Get insect losses
    myData_insect <- getField(source = source.in, quant = "storm")
    myData_insect@data$Gridcell_area <- GridcellArea_m2(myData_insect@data$Lat)
    myData_insect@data$InsDWC_gcarea_Mt <- myData_insect@data$InsDWC * myData_insect@data$Gridcell_area / 1e9
    annual_insdwc <- myData_insect@data[, .(InsDWC_annual = sum(InsDWC_gcarea_Mt )), by = Year]
    
    # 5. Get insect losses - gridcell-level.
    myData_harvest <- getField(source = source.in, quant = "closs_harv_ccut")
    myData_harvest2 <- getField(source = source.in, quant = "closs_harv_thin")
    # add harvests from the sizeclasses from both harvest methods together:
    myData_harvest@data[, 4:19 := myData_harvest2@data[, 4:19] + myData_harvest@data[, 4:19]]
    
    #create total:
    myData_harvest@data$Total <- rowSums(myData_harvest@data[,4:19])
    myData_harvest@data$Gridcell_area <- GridcellArea_m2(myData_harvest@data$Lat)
    myData_harvest@data$Total_GCMtC <- myData_harvest@data$Total * myData_harvest@data$Gridcell_area / 1e9 
    annual_harvest <- myData_harvest@data[, .(harvest_annual = sum(Total_GCMtC)), by = Year]
    
    if(scenario=="ssp370"){
      plot(annual_harvest$Year,annual_harvest$harvest_annual,type="l",ylim=c(150,370),col="brown",lwd=2,main="Total European C loss MtC/year")#/annual_forest$Fcmass_annual*100,type="l",ylim=c(0,5))
      lines(annual_storm$Year,annual_storm$stormC_annual,col="blue",lwd=2) #/annual_forest$Fcmass_annual*100,col="red")
      lines(annual_insdwc$Year,annual_insdwc$InsDWC_annual,lwd=2)#/annual_forest$Fcmass_annual*100, type="l",col="green")
    }else{
      lines(annual_harvest$Year,annual_harvest$harvest_annual,type="l",col="brown",lwd=2,lty=12,main="Total European C loss MtC/year")#/annual_forest$Fcmass_annual*100,type="l",ylim=c(0,5))
      lines(annual_storm$Year,annual_storm$stormC_annual,col="blue",lwd=2, lty=12) #/annual_forest$Fcmass_annual*100,col="red")
      lines(annual_insdwc$Year,annual_insdwc$InsDWC_annual,lwd=2, lty=12)#/annual_forest$Fcmass_annual*100, type="l",col="green")
    }

}


# plot for barkbeetle and wind
for(scenario in c("ssp370","ssp126")){
  
  #retrieve the source file location + add some metadata:
  folder_name <- paste0(base_dir,management, "_fut_MPI-ESM1-2-HR_",
                        scenario, "_diston")
  source.in = defineSource(id= paste0(management,"_",scenario),
                           dir= paste0(folder_name),
                           format = GUESS,
                           name = paste0(management,"_",scenario))
  
  
  # 1. Get forest biomass (from cmass_wood_sts or diam_cmass_wood)
  myData_cmass <- getField(source = source.in, quant = "cmass_wood_sts")
  myData_cmass@data$Gridcell_area <- GridcellArea_m2(myData_cmass@data$Lat)
  myData_cmass@data <- merge(myData_cmass@data,frac_lu)
  myData_cmass@data$Forest_sum_area <- myData_cmass@data$Forest_sum * myData_cmass@data$FOREST
  myData_cmass@data$Forest_sum_MtC <- myData_cmass@data$Forest_sum_area* myData_cmass@data$Gridcell_area  / 1e9
  annual_forest <- myData_cmass@data[, .(Fcmass_annual = sum(Forest_sum_MtC)), by = Year]
  
  
  # Use Forest_sum column which is already forest-level
  # Or if using diam_cmass_wood, process as before
  
  # 2. Get fire losses; on hold (29.Jan 2026)
  myData_fire <- getField(source = source.in, quant = "cflux_forest")
  
  # 3. Get storm losses (already processed to Forest_sum)
  myData_storm <- getField(source = source.in, quant = "storm_dw_sts")
  forest_st_cols <- c("Picea", "Pinus", "Fagus", "Quercus", "Larix", "otherBL", "otherNL", "NatForest")
  frac_cols <- paste0(forest_st_cols, "_frac")
  myData_storm@data <- myData_storm@data[, c("Lon", "Lat", "Year", forest_st_cols), with = FALSE]
  frac_sts_copy <- copy(frac_sts)
  setnames(frac_sts_copy, forest_st_cols, frac_cols, skip_absent = TRUE)
  merged_storm <- myData_storm@data[frac_sts_copy, on = .(Lon, Lat, Year)]
  merged_storm[, Storm_Forest_sum := rowSums(.SD[, ..forest_st_cols] * .SD[, ..frac_cols], na.rm = TRUE)]
  myData_storm <- getField(source = source.in, quant = "storm")
  myData_storm@data$Gridcell_area <- GridcellArea_m2(myData_storm@data$Lat)
  myData_storm@data$Dwstorm_Gridcell_Mtc <- myData_storm@data$DamWoodC* myData_storm@data$Gridcell_area / 1e9 
  annual_storm <- myData_storm@data[, .(stormC_annual = sum(Dwstorm_Gridcell_Mtc)), by = Year]
  
  # 4. Get insect losses
  myData_insect <- getField(source = source.in, quant = "storm")
  myData_insect@data$Gridcell_area <- GridcellArea_m2(myData_insect@data$Lat)
  myData_insect@data$InsDWC_gcarea_Mt <- myData_insect@data$InsDWC * myData_insect@data$Gridcell_area / 1e9
  annual_insdwc <- myData_insect@data[, .(InsDWC_annual = sum(InsDWC_gcarea_Mt )), by = Year]
  
  # 5. Get insect losses - gridcell-level.
  myData_harvest <- getField(source = source.in, quant = "closs_harv_ccut")
  myData_harvest2 <- getField(source = source.in, quant = "closs_harv_thin")
  # add harvests from the sizeclasses from both harvest methods together:
  myData_harvest@data[, 4:19 := myData_harvest2@data[, 4:19] + myData_harvest@data[, 4:19]]
  
  #create total:
  myData_harvest@data$Total <- rowSums(myData_harvest@data[,4:19])
  myData_harvest@data$Gridcell_area <- GridcellArea_m2(myData_harvest@data$Lat)
  myData_harvest@data$Total_GCMtC <- myData_harvest@data$Total * myData_harvest@data$Gridcell_area / 1e9 
  annual_harvest <- myData_harvest@data[, .(harvest_annual = sum(Total_GCMtC)), by = Year]
  
  if(scenario=="ssp370"){
    plot(annual_harvest$Year,annual_harvest$harvest_annual,type="l",ylim=c(0,50),ylab="",xlab="",col="brown",lwd=2,main="Total European C loss MtC/year")#/annual_forest$Fcmass_annual*100,type="l",ylim=c(0,5))
    lines(annual_storm$Year,annual_storm$stormC_annual,col="blue",lwd=2) #/annual_forest$Fcmass_annual*100,col="red")
    lines(annual_insdwc$Year,annual_insdwc$InsDWC_annual,lwd=2)#/annual_forest$Fcmass_annual*100, type="l",col="green")
  }else{
    lines(annual_harvest$Year,annual_harvest$harvest_annual,type="l",col="brown",lwd=2,lty=12,main="Total European C loss MtC/year")#/annual_forest$Fcmass_annual*100,type="l",ylim=c(0,5))
    lines(annual_storm$Year,annual_storm$stormC_annual,col="blue",lwd=2, lty=12) #/annual_forest$Fcmass_annual*100,col="red")
    lines(annual_insdwc$Year,annual_insdwc$InsDWC_annual,lwd=2, lty=12)#/annual_forest$Fcmass_annual*100, type="l",col="green")
  }
  
}

mtext("Total losses of carbon (MtC/year)",side=2,line=2,cex=1.5)
mtext("Years",side=1,line=2.2,cex=1.5)

legend("topleft",
       legend = c( "Insect", "Storm", "Harvest"),
       fill = c( "black", "blue", "brown"))

legend("topright",
       legend = c("ssp126", "ssp370"),
       lty = c(12, 1),
       col = c("black", "black"))
#inset = c(0, 0.25))


# need: legend, y-axis
# split and display harvest "separately"
# put in context with emissions.
# add both ssps!!!
##########################



# ============================================================
# CALCULATE disturbance risks PER GRIDCELL by disturbance agent, plot spatially,
# use in animations.
# Note that Fire is not in Carbon but burnt area, but here, the colouring will 
# simply mean disturbance intensity or presence, so probably fine for now.
# ============================================================

for(scenario in c("ssp126","ssp370")){
    management="base"
    #retrieve the source file location + add some metadata:
    folder_name <- paste0(base_dir,management, "_fut_MPI-ESM1-2-HR_",
                          scenario, "_diston")
    source.in = defineSource(id= paste0(management,"_",scenario),
                             dir= paste0(folder_name),
                             format = GUESS,
                             name = paste0(management,"_",scenario))
    
    # 1. Get forest biomass (from cmass_wood_sts or diam_cmass_wood)
    myData_cmass <- getField(source = source.in, quant = "cmass_wood_sts")
    # Use Forest_sum column which is already forest-level
    # Or if using diam_cmass_wood, process as before
    
    # 2. Get fire losses
    myData_fire <- getField(source = source.in, quant = "cflux_forest")
    
    #for now, don't work with fire closs fluxes but with burnt area:
    myData_fire <- getField(source = source.in, quant = "mburned_area")
    # sum of all monthly burned area fractions, to get annual value:
    myData_fire@data <- myData_fire@data[, .(Burnt_area_frac = sum(mburned_area, na.rm = TRUE)), by = .( Lat,Lon,Year)]
    
    #this is already at gridcell-level, so no scaling necessary.
    
    # 3. Get storm losses (already processed to Forest_sum)
    myData_storm <- getField(source = source.in, quant = "storm_dw_sts")
    forest_st_cols <- c("Picea", "Pinus", "Fagus", "Quercus", "Larix", "otherBL", "otherNL", "NatForest")
    frac_cols <- paste0(forest_st_cols, "_frac")
    myData_storm@data <- myData_storm@data[, c("Lon", "Lat", "Year", forest_st_cols), with = FALSE]
    frac_sts_copy <- copy(frac_sts)
    setnames(frac_sts_copy, forest_st_cols, frac_cols, skip_absent = TRUE)
    merged_storm <- myData_storm@data[frac_sts_copy, on = .(Lon, Lat, Year)]
    merged_storm[, Storm_Forest_sum := rowSums(.SD[, ..forest_st_cols] * .SD[, ..frac_cols], na.rm = TRUE)]
    myData_storm <- getField(source = source.in, quant = "storm")
    
    
    
    # 4. Get insect losses
    myData_insect <- getField(source = source.in, quant = "storm")
    
    
    
    # ============================================================
    # MERGE ALL DATA
    # ============================================================
    
    # Start with biomass
    perc_losses <- myData_cmass@data[, .(Lon, Lat, Year, Forest_biomass = Forest_sum)]
    
    # Add fire (already at gridcell level for forest) # fire is no longer in units of carbon , but burnt area
    #perc_losses <- merge(perc_losses, 
    #                     myData_fire@data[, .(Lon, Lat, Year, Fire_loss = Fire)],
    #                     by = c("Lon", "Lat", "Year"), all.x = TRUE)
    perc_losses <- merge(perc_losses, 
                         myData_fire@data[, .(Lon, Lat, Year, Fire_loss = Burnt_area_frac)],
                         by = c("Lon", "Lat", "Year"), all.x = TRUE)
    
    # Add storm
    perc_losses <- merge(perc_losses,
                         myData_storm@data[, .(Lon, Lat, Year, Storm_loss = DamWoodC)],
                         by = c("Lon", "Lat", "Year"), all.x = TRUE)
    
    # Add insect (InsDWC is already gridcell level)
    perc_losses <- merge(perc_losses,
                         myData_insect@data[, .(Lon, Lat, Year, Insect_loss = InsDWC)],
                         by = c("Lon", "Lat", "Year"), all.x = TRUE)
    
    # ============================================================
    # CALCULATE LOSSES
    # ============================================================
    
    # Replace NAs with 0 for losses
    perc_losses[is.na(Fire_loss), Fire_loss := 0]
    perc_losses[is.na(Storm_loss), Storm_loss := 0]
    perc_losses[is.na(Insect_loss), Insect_loss := 0]
    
    # Calculate fraction of biomass loss #  (avoid division by zero) and also record burnt area fraction for fire
    perc_losses[Forest_biomass > 0, `:=`(
      Fire_frac = Fire_loss, #/ Forest_biomass ,
      Storm_frac = Storm_loss / Forest_biomass ,
      Insect_frac = Insect_loss / Forest_biomass 
    )]
    
    # Calculate fraction of biomass loss in %  (avoid division by zero)
    perc_losses[Forest_biomass > 0, `:=`(
      Fire_perc = NA,#Fire_loss / Forest_biomass * 100 ,
      Storm_perc = Storm_loss / Forest_biomass *100,
      Insect_perc = Insect_loss / Forest_biomass *100
    )]
    # Make sure data is sorted by Year within each gridcell
    perc_losses <- perc_losses[order(Lon, Lat, Year)]
    
    # Calculate cumulative sums per gridcell
    perc_losses[, `:=`(
      Fire_loss_cumsum = cumsum(Fire_frac),
      Storm_loss_cumsum = cumsum(Storm_frac),
      Insect_loss_cumsum = cumsum(Insect_frac)
    ), by = .(Lon, Lat)]
    
    # Also cumulative percentage losses if needed
    perc_losses[, `:=`(
      Fire_perc_cumsum = cumsum(Fire_perc),
      Storm_perc_cumsum = cumsum(Storm_perc),
      Insect_perc_cumsum = cumsum(Insect_perc)
    ), by = .(Lon, Lat)]
    
    # Optional: Total disturbance cumulative sum # cannot be done because fire is reported on area basis.
    # once fire is sorted, it can be revived:
    #perc_losses[, Total_loss_cumsum := Fire_loss_cumsum + Storm_loss_cumsum + Insect_loss_cumsum]
    #perc_losses[, Total_perc_cumsum := Fire_perc_cumsum + Storm_perc_cumsum + Insect_perc_cumsum]
    #perc_losses[, Total_loss := Fire_perc + Storm_perc + Insect_perc]
    
    # Set to NA where no forest
    perc_losses[Forest_biomass <= 0, `:=`(Fire_perc = NA, Storm_perc = NA, Insect_perc = NA)]
    
    # ============================================================
    # CREATE DGVMTOOLS OBJECT FOR PLOTTING
    # ============================================================
    
    # Copy structure from original data
    myData_perc <- myData_cmass
    myData_perc@data <- perc_losses #[, .(Lon, Lat, Year, Fire_perc, Storm_perc, Insect_perc)]
    
    # ============================================================
    # PLOT MAPS FOR SELECTED YEARS
    # ============================================================
    
    years_to_plot <- c(2040,2050,2060,2070,2080,2100)
    
    myData_perc <- create_running_mean(myData_perc,"Fire_loss","Fire_perc_rm", k_in=10)
    myData_perc <- create_running_mean(myData_perc,"Storm_perc","Storm_perc_rm", k_in=10)
    myData_perc <- create_running_mean(myData_perc,"Insect_perc","Insect_perc_rm", k_in=10)
    #myData_perc <- create_running_mean(myData_perc,"Total_loss","Total_loss_rm", k_in=10)
    
    # Fire % losses # doesn't show anything at the moment, because i don't use fluxes
    plotSpatial(myData_perc, 
                layers = "Fire_perc_rm", 
                years = years_to_plot,
                limits = c(0,0.04),  # adjust limits as needed
                title = "Annual fire losses (burnt area)")
    
    # Storm % losses
    plotSpatial(myData_perc, 
                layers = "Storm_perc_rm", 
                years = years_to_plot,
                limits = c(0,2),
                title = "Annual storm losses (% of forest biomass)")
    
    # Insect % losses
    plotSpatial(myData_perc, 
                layers = "Insect_perc", 
                years = years_to_plot,
                limits = c(0, 2),
                title = "Annual barkbeetle losses (% of forest biomass)")
    
    #plotSpatial(myData_perc, 
    #            layers = "Total_loss_cumsum", 
    #            years = years_to_plot)
    
    ###########################################################################
    
    
    #myData_perc <- create_running_mean(myData_perc,"Total_loss_cumsum","Total_loss_cumsum",k_in=10)
    #myData_perc <- create_running_mean(myData_perc,"Fire_loss_cumsum","Fire_loss_cumsum",k_in=10)
    myData_perc <- create_running_mean(myData_perc,"Storm_loss_cumsum","Storm_loss_cumsum",k_in=10)
    
    myData_perc <- create_running_mean(myData_perc,"Insect_loss_cumsum","Insect_loss_cumsum",k_in=10)
    
    
    #plotSpatial(myData_perc, 
    #            layers = "Total_loss_cumsum", 
    #            years = years_to_plot)
    
    tmp <- create_running_mean(myData_fire,"Burnt_area_frac","Burnt_area_frac",k_in=10)
    plotSpatial(tmp, 
                layers = "Burnt_area_frac", 
                years = years_to_plot)
    #p+scale_fill_gradient(name = "burnt_area", trans = "log")
    #it seems that I just have to work with burnt area fraction directly.
    tmp@data <- tmp@data[order(Lon, Lat, Year)]
    tmp@data$Burnt_area_frac[is.na(tmp@data$Burnt_area_frac)] <- 0
    tmp@data[, `:=`(
      Burnt_area_frac_cumsum = cumsum(Burnt_area_frac)
    ), by = .(Lon, Lat)]
    
    plotSpatial(tmp, 
                layers = "Burnt_area_frac_cumsum", 
                years = years_to_plot)
    plotSpatial(tmp, 
                layers = "Burnt_area_frac", 
                years = years_to_plot,
                limits=c(0,0.01))
    
    # replace fire with burnt area from above:
    myData_perc@data <- merge(myData_perc@data,
          tmp@data[, .(Lon, Lat, Year, Burnt_area_frac )],
          by = c("Lon", "Lat", "Year"), all.x = TRUE)
    
    myData_perc@data$Fire_frac <- myData_perc@data$Burnt_area_frac
    
    myData_perc@data <- myData_perc@data[order(Lon, Lat, Year)]
    myData_perc@data[, `:=`(
      Fire_loss_cumsum = cumsum(Burnt_area_frac)
    ), by = .(Lon, Lat)]
    
    
    plotSpatial(myData_perc, 
                layers = "Storm_loss_cumsum", 
                years = years_to_plot)
    
    plotSpatial(myData_perc, 
                layers = "Insect_loss_cumsum", 
                years = years_to_plot)
    
    
    # In your R script, export everything at once
    plot_data <- myData_perc  # All years, all disturbance agents
    # add forest fraction for correct plotting:
    
    plot_data_final <- merge(plot_data,frac_lu)
    if(scenario=="ssp126"){
      write.csv(plot_data_final, "../data/processed/ssp126_disturbance_all_years.csv", row.names = FALSE)
    }else{
      write.csv(plot_data_final, "../data/processed/ssp370_disturbance_all_years.csv", row.names = FALSE)
    }
}


################################################################################################################################################
# ============================================================
# Carbon risks - vegetation carbon
# ============================================================


cols_anomaly <- colorRampPalette(c("red", "white", "blue"))(256)  # Red-White-Blue for anomalies

##########Carbon risk --
#Baseline 

scenario ="ssp126"
    #retrieve the source file location + add some metadata:
    folder_name <- paste0(base_dir,management, "_fut_MPI-ESM1-2-HR_",
                          scenario, "_diston")
    source.in = defineSource(id= paste0(management,"_",scenario),
                             dir= paste0(folder_name),
                             format = GUESS,
                             name = paste0(management,"_",scenario))
    
    
    # 1. Get forest biomass (from cmass_wood_sts or diam_cmass_wood)
    myData_cmass_126 <- getField(source = source.in, quant = "cmass_wood_sts")
    # Use Forest_sum column which is already forest-level
    # Or if using diam_cmass_wood, process as before
    myData_cmass_126@data <- myData_cmass_126@data[,c("Lon","Lat","Year","Forest_sum")]
    
    # Filter for years 2025 and 2070, reshape to wide format
    result <- dcast(myData_cmass_126@data[Year %in% c(2025, 2070)], 
                    Lon + Lat ~ Year, 
                    value.var = "Forest_sum")
    
    # Calculate difference (2070 - 2025)
    result[, diff_2070_2025 := `2070` - `2025`]
    
    myData_cmass_126@data <- result
    
    plotSpatial(myData_cmass_126,layer="diff_2070_2025",cols=cols_anomaly, legend.title = "Delta KgC/m2",limits=c(-8,8))
    collect <- myData_cmass_126@data
    names(collect)[5] <- "ssp126VegCdiff"
    
    
    
  scenario = "ssp370"
    #retrieve the source file location + add some metadata:
    folder_name <- paste0(base_dir,management, "_fut_MPI-ESM1-2-HR_",
                          scenario, "_diston")
    source.in = defineSource(id= paste0(management,"_",scenario),
                             dir= paste0(folder_name),
                             format = GUESS,
                             name = paste0(management,"_",scenario))
    
    
    # 1. Get forest biomass (from cmass_wood_sts or diam_cmass_wood)
    myData_cmass_ssp370 <- getField(source = source.in, quant = "cmass_wood_sts")
    # Use Forest_sum column which is already forest-level
    # Or if using diam_cmass_wood, process as before
    myData_cmass_ssp370@data <- myData_cmass_ssp370@data[,c("Lon","Lat","Year","Forest_sum")]
    
    # Filter for years 2025 and 2070, reshape to wide format
    result <- dcast(myData_cmass_ssp370@data[Year %in% c(2025, 2070)], 
                    Lon + Lat ~ Year, 
                    value.var = "Forest_sum")
    
    # Calculate difference (2070 - 2025)
    result[, diff_2070_2025 := `2070` - `2025`]
    
    myData_cmass_ssp370@data <- result
    plotSpatial(myData_cmass_ssp370,layer="diff_2070_2025",cols=cols_anomaly, legend.title = "Delta KgC/m2",limits=c(-8,8))
    
    collect <- merge(collect,myData_cmass_ssp370@data[,c("Lat","Lon","diff_2070_2025")])
    names(collect)[6] <- "ssp3706VegCdiff"
    
    ################################################################################################
    
    
    ################################################################################################################################################
    
    # ============================================================
    # Carbon risks ecosystem carbon
    # ============================================================
    
    #Baseline 
    
  scenario = "ssp126"
    #retrieve the source file location + add some metadata:
    folder_name <- paste0(base_dir,management, "_fut_MPI-ESM1-2-HR_",
                          scenario, "_diston")
    source.in = defineSource(id= paste0(management,"_",scenario),
                             dir= paste0(folder_name),
                             format = GUESS,
                             name = paste0(management,"_",scenario))
    
    
    # 1. Get forest biomass (from cmass_wood_sts or diam_cmass_wood)
    myData_cmass_126 <- getField(source = source.in, quant = "cpool") # split litter and soil ; check veg litter and soil changes... check Total does not contain harvested wood products.
    # Use Forest_sum column which is already forest-level
    # Or if using diam_cmass_wood, process as before
    myData_cmass_126@data <- myData_cmass_126@data[,c("Lon","Lat","Year","Total")]
    
    # Filter for years 2025 and 2070, reshape to wide format
    result <- dcast(myData_cmass_126@data[Year %in% c(2025, 2070)], 
                    Lon + Lat ~ Year, 
                    value.var = "Total")
    
    # Calculate difference (2070 - 2025)
    result[, diff_2070_2025 := `2070` - `2025`]
    
    myData_cmass_126@data <- result
    
    plotSpatial(myData_cmass_126,layer="diff_2070_2025",cols=cols_anomaly, legend.title = "Delta KgC/m2",limits=c(-8,8))
    collect <- merge(collect,myData_cmass_126@data[,c("Lat","Lon","diff_2070_2025")])
    names(collect)[7] <- "ssp126EcosystemCdiff"
    
    
    
  scenario = "ssp370"
    #retrieve the source file location + add some metadata:
    folder_name <- paste0(base_dir,management, "_fut_MPI-ESM1-2-HR_",
                          scenario, "_diston")
    source.in = defineSource(id= paste0(management,"_",scenario),
                             dir= paste0(folder_name),
                             format = GUESS,
                             name = paste0(management,"_",scenario))
    
    
    # 1. Get forest biomass (from cmass_wood_sts or diam_cmass_wood)
    myData_cmass_ssp370 <- getField(source = source.in, quant = "cpool")
    # Use Forest_sum column which is already forest-level
    # Or if using diam_cmass_wood, process as before
    myData_cmass_ssp370@data <- myData_cmass_ssp370@data[,c("Lon","Lat","Year","Total")]
    
    # Filter for years 2025 and 2070, reshape to wide format
    result <- dcast(myData_cmass_ssp370@data[Year %in% c(2025, 2070)], 
                    Lon + Lat ~ Year, 
                    value.var = "Total")
    
    # Calculate difference (2070 - 2025)
    result[, diff_2070_2025 := `2070` - `2025`]
    
    myData_cmass_ssp370@data <- result
    plotSpatial(myData_cmass_ssp370,layer="diff_2070_2025",cols=cols_anomaly, legend.title = "Delta KgC/m2",limits=c(-8,8))
    collect <- merge(collect,myData_cmass_ssp370@data[,c("Lat","Lon","diff_2070_2025")])
    names(collect)[8] <- "ssp370EcosystemCdiff"
    
    # export as .csv
    write.csv(collect, "../data/processed/Forest_and_ecosystem_carbon_loss_data.csv", row.names = FALSE)



################################################################################################




################################################################################################

#also include "Harvest losses"

management = "base"
base_dir
    
for(scenario in c("ssp370","ssp126")){

#retrieve the source file location + add some metadata:
folder_name <- paste0(base_dir,management, "_fut_MPI-ESM1-2-HR_",
                      scenario, "_diston")
source.in = defineSource(id= paste0(management,"_",scenario),
                         dir= paste0(folder_name),
                         format = GUESS,
                         name = paste0(management,"_",scenario))


# ============================================================
# CALCULATE % LOSSES PER GRIDCELL
# ============================================================

# 1. Get forest biomass (from cmass_wood_sts or diam_cmass_wood)
myData_cmass <- getField(source = source.in, quant = "cmass_wood_sts")
myData_cmass@data$Gridcell_area <- GridcellArea_m2(myData_cmass@data$Lat)
myData_cmass@data <- merge(myData_cmass@data,frac_lu)
myData_cmass@data$Forest_sum_area <- myData_cmass@data$Forest_sum * myData_cmass@data$FOREST
myData_cmass@data$Forest_sum_MtC <- myData_cmass@data$Forest_sum_area* myData_cmass@data$Gridcell_area  / 1e9
annual_forest <- myData_cmass@data[, .(Fcmass_annual = sum(Forest_sum_MtC)), by = Year]


# Use Forest_sum column which is already forest-level
# Or if using diam_cmass_wood, process as before

# 2. Get fire losses
myData_fire <- getField(source = source.in, quant = "cflux_forest")
myData_fire@data$Gridcell_area <- GridcellArea_m2(myData_fire@data$Lat)
myData_fire@data$Fire_tot <- myData_fire@data$Gridcell_area 
myData_fire <- myData_fire@data[, .(Fire_Forest_sum = sum(Fire)), by = Year]
myData_fire$Fire_Forest_sum_GtC <- cumsum(myData_fire$Fire_Forest_sum)/ 1e12



merged_fire <- myData_fire@data[frac_lu, on = .(Lon, Lat, Year)]
merged_fire$Gridcell_area <- GridcellArea_m2(myData_fire$Lat)
merged_fire[, Fire_Forest_sum := rowSums(.SD[, Fire] * .SD[, Gridcell_area], na.rm = TRUE)]

merged_fire$Forest_area <- merged_fire$Gridcell_area 
merged_fire[, .(Fire_Forest_sum = weighted.mean(Fire, w = Forest_area, na.rm = TRUE)),
                  by = .(Year)]

# 3. Get storm losses (already processed to Forest_sum)
myData_storm <- getField(source = source.in, quant = "storm_dw_sts")
forest_st_cols <- c("Picea", "Pinus", "Fagus", "Quercus", "Larix", "otherBL", "otherNL", "NatForest")
frac_cols <- paste0(forest_st_cols, "_frac")
myData_storm@data <- myData_storm@data[, c("Lon", "Lat", "Year", forest_st_cols), with = FALSE]
frac_sts_copy <- copy(frac_sts)
setnames(frac_sts_copy, forest_st_cols, frac_cols, skip_absent = TRUE)
merged_storm <- myData_storm@data[frac_sts_copy, on = .(Lon, Lat, Year)]
merged_storm[, Storm_Forest_sum := rowSums(.SD[, ..forest_st_cols] * .SD[, ..frac_cols], na.rm = TRUE)]
myData_storm <- getField(source = source.in, quant = "storm")
myData_storm@data$Gridcell_area <- GridcellArea_m2(myData_storm@data$Lat)
myData_storm@data$Dwstorm_Gridcell_Mtc <- myData_storm@data$DamWoodC* myData_storm@data$Gridcell_area / 1e9 
annual_storm <- myData_storm@data[, .(stormC_annual = sum(Dwstorm_Gridcell_Mtc)), by = Year]



# 4. Get insect losses
myData_insect <- getField(source = source.in, quant = "storm")
myData_insect@data$Gridcell_area <- GridcellArea_m2(myData_insect@data$Lat)
myData_insect@data$InsDWC_gcarea_Mt <- myData_insect@data$InsDWC * myData_insect@data$Gridcell_area / 1e9
annual_insdwc <- myData_insect@data[, .(InsDWC_annual = sum(InsDWC_gcarea_Mt )), by = Year]



# 5. Get insect losses - gridcell-level.
myData_harvest <- getField(source = source.in, quant = "closs_harv_ccut")
myData_harvest2 <- getField(source = source.in, quant = "closs_harv_thin")
# add harvests from the sizeclasses from both harvest methods together:
myData_harvest@data[, 4:19 := myData_harvest2@data[, 4:19] + myData_harvest@data[, 4:19]]

#create total:
myData_harvest@data$Total <- rowSums(myData_harvest@data[,4:19])
myData_harvest@data$Gridcell_area <- GridcellArea_m2(myData_harvest@data$Lat)
myData_harvest@data$Total_GCMtC <- myData_harvest@data$Total * myData_harvest@data$Gridcell_area / 1e9 
annual_harvest <- myData_harvest@data[, .(harvest_annual = sum(Total_GCMtC)), by = Year]


lines(annual_insdwc$Year,annual_insdwc$InsDWC_annual)#/annual_forest$Fcmass_annual*100, type="l",col="green")
plot(annual_harvest$Year,annual_harvest$harvest_annual,type="l",ylim=c(0,300),col="brown")#/annual_forest$Fcmass_annual*100,type="l",ylim=c(0,5))
lines(annual_storm$Year,annual_storm$stormC_annual,col="blue")#/annual_forest$Fcmass_annual*100,col="red")



# ============================================================
# MERGE ALL DATA
# ============================================================

# Start with biomass
perc_losses <- myData_cmass@data[, .(Lon, Lat, Year, Forest_biomass = Forest_sum)]

# Add fire (already at gridcell level for forest)
perc_losses <- merge(perc_losses, 
                     myData_fire@data[, .(Lon, Lat, Year, Fire_loss = Fire)],
                     by = c("Lon", "Lat", "Year"), all.x = TRUE)

# Add storm
perc_losses <- merge(perc_losses,
                     myData_storm@data[, .(Lon, Lat, Year, Storm_loss = DamWoodC)],
                     by = c("Lon", "Lat", "Year"), all.x = TRUE)

# Add insect (InsDWC is already gridcell level)
perc_losses <- merge(perc_losses,
                     myData_insect@data[, .(Lon, Lat, Year, Insect_loss = InsDWC)],
                     by = c("Lon", "Lat", "Year"), all.x = TRUE)

perc_losses <- merge(perc_losses,
                        myData_harvest@data[, .(Lon, Lat, Year, harvest_loss = Total)],
                        by = c("Lon", "Lat", "Year"), all.x = TRUE)
# ============================================================
# CALCULATE LOSSES
# ============================================================

# Replace NAs with 0 for losses
perc_losses[is.na(Fire_loss), Fire_loss := 0]
perc_losses[is.na(Storm_loss), Storm_loss := 0]
perc_losses[is.na(Insect_loss), Insect_loss := 0]
perc_losses[is.na(harvest_loss), harvest_loss := 0]

# Calculate fraction of biomass loss #  (avoid division by zero)
perc_losses[Forest_biomass > 0, `:=`(
  Fire_frac = Fire_loss / Forest_biomass ,
  Storm_frac = Storm_loss / Forest_biomass ,
  Insect_frac = Insect_loss / Forest_biomass,
  harvest_frac = harvest_loss / Forest_biomass
)]

# Calculate fraction of biomass loss in %  (avoid division by zero)
perc_losses[Forest_biomass > 0, `:=`(
  Fire_perc = Fire_loss / Forest_biomass * 100 ,
  Storm_perc = Storm_loss / Forest_biomass *100,
  Insect_perc = Insect_loss / Forest_biomass *100,
  harvest_perc = harvest_loss / Forest_biomass*100
)]


# Replace NAs with 0 for losses
perc_losses[is.na(Fire_frac), Fire_frac := 0]
perc_losses[is.na(Storm_frac), Storm_frac := 0]
perc_losses[is.na(Insect_frac), Insect_frac := 0]
perc_losses[is.na(harvest_frac), harvest_frac := 0]

# Replace NAs with 0 for losses
perc_losses[is.na(Fire_perc), Fire_perc := 0]
perc_losses[is.na(Storm_perc), Storm_perc := 0]
perc_losses[is.na(Insect_perc), Insect_perc := 0]
perc_losses[is.na(harvest_perc), harvest_perc := 0]

# Make sure data is sorted by Year within each gridcell
perc_losses <- perc_losses[order(Lon, Lat, Year)]

eu_means <- perc_losses[, .(
  Forest_biomass = mean(Forest_biomass),
  Fire_loss = mean(Fire_loss),
  Storm_loss = mean(Storm_loss),
  Insect_loss = mean(Insect_loss),
  harvest_loss = mean(harvest_loss),
  Fire_frac = mean(Fire_frac),
  Storm_frac = mean(Storm_frac),
  Insect_frac = mean(Insect_frac),
  harvest_frac = mean(harvest_frac),
  Fire_perc = mean(Fire_perc),
  Storm_perc = mean(Storm_perc),
  Insect_perc = mean(Insect_perc),
  harvest_perc = mean(harvest_perc)
), by = Year]

eu_totals <- perc_losses[, .(
  Fire_loss_total = sum(Fire_loss),
  Storm_loss_total = sum(Storm_loss),
  Insect_loss_total = sum(Insect_loss),
  harvest_loss_total = sum(harvest_loss),
  Forest_biomass_total = sum(Forest_biomass)
), by = Year]


eu_totals[, `:=`(
  Fire_perc = Fire_loss_total / Forest_biomass_total,
  Storm_perc = Storm_loss_total / Forest_biomass_total,
  Insect_perc = Insect_loss_total / Forest_biomass_total,
  harvest_perc = harvest_loss_total / Forest_biomass_total
)]

if(scenario=="ssp370"){
  plot(eu_means$Year,eu_means$harvest_perc,col="brown",ylim=c(0,1), type="l")
  plot(eu_means$Year,eu_means$Fire_perc, ylim=c(0,1),col="red",type="l")
  lines(eu_means$Year,eu_means$Insect_perc,col="black")
  lines(eu_means$Year,eu_means$Storm_perc,col="blue")
  
}else{
  lines(eu_means$Year,eu_means$Fire_perc, ylim=c(0,1),col="red",type="l",lty=12)
  lines(eu_means$Year,eu_means$Insect_perc,col="black",lty=12)
  lines(eu_means$Year,eu_means$Storm_perc,col="blue",lty=12)
  lines(eu_means$Year,eu_means$harvest_perc,col="brown",lty=12)
 
}


mtext("European mean annual losses of carbon (% total biomass)",side=2,line=2)

legend("topleft",
       legend = c( "Insect", "Storm", "Harvest"),
       fill = c( "black", "blue", "brown"))

legend("topright",
       legend = c("ssp126", "ssp370"),
       lty = c(12, 1),
       col = c("black", "black"))
       #inset = c(0, 0.25))
}



myData_cmass_2050 <- myData_cmass@data[which(myData_cmass@data$Year == 2025),]#| myData_cmass@data$Year == 2070),]
myData_cmass_2070 <- myData_cmass@data[which(myData_cmass@data$Year == 2070),]

myData_cmass_diff <- myData_cmass_2050[,Forest_sum] - myData_cmass_2070[,Forest_sum]
  

myData_cmass_2050 %>% group_by(Lon,Lat) %>% summarise(differ = Forest_sum)

differ <- myData_cmass_2070$Forest_sum  - myData_cmass_2025$Forest_sum 


myData_cmass@data <- diff
plotSpatial(myData_cmass,)


#deadwood conversion
deadwood_data, 1/(mean_wood_density) * 10000


####
#Burnt area total timeseries: 

scenario="ssp370"
#retrieve the source file location + add some metadata:
folder_name <- paste0(base_dir,management, "_fut_MPI-ESM1-2-HR_",
                      scenario, "_diston")
source.in = defineSource(id= paste0(management,"_",scenario),
                         dir= paste0(folder_name),
                         format = GUESS,
                         name = paste0(management,"_",scenario))



#for now, don't work with fire closs fluxes but with burnt area:
myData_fire <- getField(source = source.in, quant = "mburned_area")
# sum of all monthly burned area fractions, to get annual value:
myData_fire@data <- myData_fire@data[, .(Burnt_area_frac = sum(mburned_area, na.rm = TRUE)), by = .( Lat,Lon,Year)]

agg <- aggregateSpatial(myData_fire,method="sum")
plotTemporal(agg)
