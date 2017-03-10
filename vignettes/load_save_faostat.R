# Load and save all FS emissions data


# Setup -------------------------------------------------------------------

# Installing packages
packs <- c("plyr", "tidyverse", "dplyr", "tidyr","readxl", "stringr", 
           "gridExtra", "grid", "ggplot2", "ggthemes", "scales", "devtools")
lapply(packs[!packs %in% installed.packages()[,1]], 
       install.packages,
       dependencies = TRUE)
lapply(packs, require, character.only = TRUE)

options(scipen=999)

# Loading locally developed functions
l_ply(str_c("R/", list.files("R/", pattern="*.R")), source)

# Loading data ------------------------------------------------------------


filse <- 
  c("data/Emissions_Agriculture_Agriculture_total_E_All_Data_(Norm).csv",
    "data/Emissions_Agriculture_Burning_crop_residues_E_All_Data_(Norm).csv",
    "data/Emissions_Agriculture_Burning_Savanna_E_All_Data_(Norm).csv",
    "data/Emissions_Agriculture_Crop_Residues_E_All_Data_(Norm).csv",
    "data/Emissions_Agriculture_Cultivated_Organic_Soils_E_All_Data_(Norm).csv",
    "data/Emissions_Agriculture_Enteric_Fermentation_E_All_Data_(Norm).csv",
    "data/Emissions_Agriculture_Manure_applied_to_soils_E_All_Data_(Norm).csv",
    "data/Emissions_Agriculture_Manure_left_on_pasture_E_All_Data_(Norm).csv",
    "data/Emissions_Agriculture_Manure_Management_E_All_Data_(Norm).csv",
    "data/Emissions_Agriculture_Rice_Cultivation_E_All_Data_(Norm).csv",
    "data/Emissions_Agriculture_Burning_crop_residues_E_All_Data_(Norm).csv",
    "data/Emissions_Land_Use_Burning_Biomass_E_All_Data_(Norm).csv",
    "data/Emissions_Land_Use_Cropland_E_All_Data_(Norm).csv",
    "data/Emissions_Land_Use_Forest_Land_E_All_Data_(Norm).csv",
    "data/Emissions_Land_Use_Grassland_E_All_Data_(Norm).csv",
    "data/Emissions_Land_Use_Land_Use_Total_E_All_Data_(Norm).csv")


fs <- 
  llply(filse, read.fs.bulk) %>% 
  bind_rows() %>% 
  tbl_df()

# Exporting items and elements
els <- fs %>%
  select(ElementCode, ElementName, Unit) %>% 
  distinct()

its <- fs %>%
  select(ItemCode, ItemName) %>% 
  distinct()

fs <- 
  fs %>% 
  select(AreaCode, ItemCode, ElementCode, Year, Value)

save(fs, its, els, file = "data/all_fs_emissions.Rdata")
