# ForestPaths LPJ-GUESS output post-processing and plotting script
# Author: Annemarie Eckes-Shephard
# Refactored for readability and maintainability by Claude
# post_process_output_for_Policy_lab Script, but specified for FORWARDS WP3 Stakeholder meeting, only plotting the variables in the
# draft presentation.
# To create individual country plots, change the country name for object archetype_countries and rerun.
# This is exactly the script as the post_process_output_for_Policy_lab script but ony for seleted variables and countries, to speed up the 
# execution time.

# ============================================================================
# SETUP
# ============================================================================

library(data.table)
library(DGVMTools)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reldist)

base_dir <- BASE_DIR <- "/Volumes/Anne's Backup/ForestPaths/v16/"

source("helper_functions.R")

# Configuration
PLOT_ALL <- FALSE
BASE_DIR <- "/Volumes/Anne's Backup/ForestPaths/v16/"

outputs_maps <- "../Figures_and_reports/updated_calibration/Policy_Lab_maps/"
output_folder <- "../Figures_and_reports/updated_calibration/"

#switch for LPJG-units vs ForestPaths reporting units:
LPJG_units = FALSE # otherwise set to FALSE, if ForestPaths units
# ============================================================================
# DEFINE MANAGEMENT SCENARIOS AND COUNTRIES
# ============================================================================

# Management scenarios (excluding fertfor, ceaseman, ceaseman_cct)

managements_list <- c("base", "lightthin", "intensthin", "longrot",
                      "shortrot", "rettree", "sfire", "ccf", "divers","ceaseman", "ceaseman_cct", "ccf_divers")#,"ccf")#
managementslist =  managements_list
management_df <- data.frame(
  management = managements_list,
  col = c(
    "black",    # base
    "#66BB6A",  # lightthin    — medium green   } thinning pair
    "#1B5E20",  # intensthin   — dark green     }
    "#64B5F6",  # longrot      — light blue     } rotation pair
    "#0D47A1",  # shortrot     — dark blue      }
    "#7030A0",  # rettree      — purple
    "#973735",  # sfire        — dark red
    "#FBA918",  # ccf          — amber          } ccf pair
    "#00897B",  # divers       — teal
    "#3E4FC2",  # ceaseman     — indigo         } ceaseman pair
    "#1A237E",  # ceaseman_cct — dark navy      }
    "#E65100"   # ccf_divers   — deep orange    }
  ),
  pch = c(1, 3, 3, 4, 4, 5, 7, 8, 15, 16, 17, 18),
  lty = c(1, 3, 3, 4, 4, 5, 2, 6,  2,  5,  5,  6),
  stringsAsFactors = FALSE
)

#' Drop ceaseman management columns from a per-country data list.
#'
#' @param data_list Named list of per-country data frames (columns = Year +
#'   management names).
#' @param with_ceaseman Logical. When TRUE the data is returned unchanged.
#'   When FALSE the "ceaseman" and "ceaseman_cct" columns are removed.
#'   Pass FALSE explicitly for timber variables, which always exclude these
#'   scenarios regardless of the overall run setting.
drop_mgmts <- function(data_list, with_ceaseman) {
  if (with_ceaseman) return(data_list)
  lapply(data_list, function(df)
    df[, !names(df) %in% c("ceaseman", "ceaseman_cct"), drop = FALSE])
}


# Archetype countries
archetype_countries <- c("Estonia")#,"Estonia","United Kingdom", "Switzerland", "Italy")
country_colours <- c("#54AC0C", "#AD8B1A", "#E960CC", "#DF726A")
archetypes_df <- data.frame(
  country = archetype_countries, 
  colour = country_colours
)

# All countries (should be defined elsewhere - placeholder)
country_list <- archetype_countries  # Replace with full list

# Scenarios
scenario_list <- c("ssp126", "ssp370")


# ============================================================================
# LOAD REFERENCE DATA
# ============================================================================

# Land cover fractions
landcover_fractions_from_2025 <- fread(
  "../landuse_change_forest_age/lu_inputfiles_noNF3/net_lu_HildaPucherMircaEurope.txt"
)[Year == 2025]

# Forest cover fractions
forestcover_fractions_from_2025 <- fread(
  "../landuse_change_forest_age/lu_inputfiles_noNF3/luforest_HildaPucherMircaEurope.txt"
)[Year == 2025]

# Mean wood density 
mean_wood_density <- mean_wooddensity_df[mean_wooddensity_df$Luforest == "Forest_sum", "MeanWoodDensity"]


# ============================================================================
# MAIN PROCESSING WORKFLOW
# ============================================================================

#' Main function to process all variables
#' @param with_ceaseman Logical. Include ceaseman/ceaseman_cct managements in
#'   non-timber plots? When FALSE those columns are dropped immediately after
#'   loading so that other management differences remain visible.
#'   Timber always drops ceaseman regardless (no harvest occurs there).
main <- function(with_ceaseman = TRUE) {
  
  cat("\n====================================\n")
  cat("ForestPaths Post-Processing Started\n")
  cat("  Scenario    :", scenario, "\n")
  cat("  With ceaseman:", with_ceaseman, "\n")
  cat("====================================\n\n")
  
  # 2. STAND BIOMASS
  stand_config <- list(
    name = "Stand Biomass",
    managementslist = managementslist,
    extract_fn = extract_stand_biomass,
    n_years = 77
  )
  stand_data <- process_variable(stand_config, output_folder)
  stand_data <- drop_mgmts(stand_data, with_ceaseman)
  
  create_country_plots(
    stand_data,
    list(
      filename = "StandC_kgCm2.pdf",
      title = "stand biomass, kgC/m2",
      ylab = "stand biomass, kgC/m2"
    )
  )
  
  stand_data_diff <- calculate_baseline_diff(stand_data)
  create_country_plots(
    stand_data_diff,
    list(
      filename = "StandC_kgCm2_diff.pdf",
      title =  expression(Delta ~"stand biomass, kgC/m2"),
      ylab =  expression(Delta ~"stand biomass, kgC/m2")
    )
  )
  
  #unit change to tCO2/ha
  stand_data_tco2ha <- convert_units(stand_data,0.001/0.0001*3.664) # kgC to tonco2/ha
  create_country_plots(
    stand_data_tco2ha,
    list(
      filename = "StandC_tCO2ha.pdf",
      title = "stand biomass, tCO2/ha",
      ylab = "stand biomass, tCO2/ha"
    )
  )
  
  stand_data_tco2ha_diff <- calculate_baseline_diff(stand_data_tco2ha)
  create_country_plots(
    stand_data_tco2ha_diff,
    list(
      filename = "StandC_tCO2ha_diff.pdf",
      title = expression(Delta ~"stand biomass, tCO2/ha"),
      ylab = expression(Delta ~"stand biomass, tCO2/ha")
    )
  )
  
  
  # 4. NBP
  nbp_config <- list(
    name = "NBP",
    managementslist = managementslist,
    extract_fn = extract_nbp,
    n_years = 77
  )
  nbp_data <- process_variable(nbp_config, output_folder)
  nbp_data <- drop_mgmts(nbp_data, with_ceaseman)
  
  create_country_plots(
    nbp_data,
    list(
      filename = "NBP_cumul_kgCm2.pdf",
      title = "cumulative NBP, kgC/m2",
      ylab = "cumulative NBP, kgC/m2"
    )
  )
  
  #unit change to tCO2/ha
  nbp_data_tco2ha <- convert_units(nbp_data,0.001/0.0001*3.664) # kgC to tonco2/ha
  create_country_plots(
    nbp_data_tco2ha,
    list(
      filename = "NBP_cumul_tco2ha.pdf",
      title = "cumulative NBP, tCO2/ha",
      ylab = "cumulative NBP, tCO2/ha"
    )
  )
  
  # 8. DISTURBANCE MORTALITY
  disturbance_mortality_config <- list(
    name = "Disturbance Mortality",
    managementslist = managementslist,
    extract_fn = extract_disturbance_mortality,
    n_years = 77
  )
  disturbance_mortality_data <- process_variable(disturbance_mortality_config, output_folder)
  disturbance_mortality_data <- drop_mgmts(disturbance_mortality_data, with_ceaseman)
  
  # apply running mean before plotting:
  disturbance_mortality_data_smoothed <- apply_rolling_mean(disturbance_mortality_data, n = 10, align = "left")
  # Plot absolute values
  create_country_plots(
    disturbance_mortality_data_smoothed,
    list(
      filename = "mortality_disturbance_kgCm2year.pdf",
      title = "Carbon lost through disturbance mortality, kgC/m2/year",
      ylab = "Carbon lost through disturbance mortality, kgC/m2/year",
      cex = 1.7
    )
  )
  
  # apply running mean before plotting:
  # Calculate as percentage of stand biomass
  disturbance_mortality_pct <- lapply(names(stand_data), function(country) {
    dfB <- stand_data[[country]]
    dfM <- disturbance_mortality_data[[country]]
    mgmt_cols <- setdiff(colnames(dfB), "Year")
    rate_df <- dfM
    rate_df[, mgmt_cols] <- (dfM[, mgmt_cols] / dfB[, mgmt_cols]) * 100
    rate_df
  })
  names(disturbance_mortality_pct) <- names(stand_data)
  
  
  # apply running mean before plotting:
  disturbance_mortality_pct_smoothed <- apply_rolling_mean(disturbance_mortality_pct,n = 10,align="left")
  create_country_plots(
    disturbance_mortality_pct_smoothed,
    list(
      filename = "mortality_disturbance_percyear.pdf",
      title = "Disturbance mortality rate, %/year",
      ylab = "Disturbance mortality rate, %/year",
      cex = 1.7
    )
  )
  
  
  # Plot differences from baseline (percentage)
  disturbance_mortality_pct_diff <- calculate_baseline_diff(disturbance_mortality_pct)
  # apply running mean before plotting:
  disturbance_mortality_pct_diff_smoothed <- apply_rolling_mean(disturbance_mortality_pct_diff,n = 10,align="left")
  create_country_plots(
    disturbance_mortality_pct_diff_smoothed,
    list(
      filename = "mortality_disturbance_percyear_diff.pdf",
      title = expression(Delta ~ "Disturbance mortality rate, %/year"),
      ylab = expression(Delta ~ "Disturbance mortality rate, %/year"),
      cex = 1.7
    )
  )
  
  
  # Plot differences from baseline (absolute)
  disturbance_mortality_diff <- calculate_baseline_diff(disturbance_mortality_data)
  # apply running mean before plotting:
  disturbance_mortality_pct_diff_smoothed <- apply_rolling_mean(disturbance_mortality_diff,n = 10,align="left")
  create_country_plots(
    disturbance_mortality_pct_diff_smoothed,
    list(
      filename = "mortality_disturbance_kgCm2year_diff.pdf",
      title = expression(Delta ~ "Carbon lost through disturbance mortality, kgC/m2/year"),
      ylab = expression(Delta ~ "Carbon lost through disturbance mortality, kgC/m2/year"),
      cex = 1.7
    )
  )
  
  
  
  cat("\n====================================\n")
  cat("Post-Processing Complete!\n")
  cat("====================================\n\n")
}

# Run the main workflow
if (interactive() || !exists("testing_mode")) {
  
  for (scenario in scenario_list) {
    for (with_ceaseman in c(TRUE, FALSE)) {
      
      ceaseman_tag <- if (with_ceaseman) "with_ceaseman" else "no_ceaseman"
      OUTPUT_DIR   <- paste0(output_folder, scenario, "/", ceaseman_tag, "/")
      if (!dir.exists(OUTPUT_DIR))
        dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
      
      main(with_ceaseman = with_ceaseman)
    }
  }
}



#============================================================================================================





