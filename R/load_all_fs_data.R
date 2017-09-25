#' load all FAOSTAT bulk data files from one folder

load_all_fs_data <- function(folder) {
  #' Loading FAOSTAT data
  fsRDFile <- file.path(folder, "all_fs_emissions.Rdata")
  if (!file.exists(fsRDFile)) {
    files <-
      file.path(
        folder,
        c(
          "Emissions_Agriculture_Agriculture_total_E_All_Data_(Norm).csv",
          "Emissions_Agriculture_Burning_crop_residues_E_All_Data_(Norm).csv",
          "Emissions_Agriculture_Burning_Savanna_E_All_Data_(Norm).csv",
          "Emissions_Agriculture_Crop_Residues_E_All_Data_(Norm).csv",
          "Emissions_Agriculture_Cultivated_Organic_Soils_E_All_Data_(Norm).csv",
          "Emissions_Agriculture_Enteric_Fermentation_E_All_Data_(Norm).csv",
          "Emissions_Agriculture_Manure_applied_to_soils_E_All_Data_(Norm).csv",
          "Emissions_Agriculture_Manure_left_on_pasture_E_All_Data_(Norm).csv",
          "Emissions_Agriculture_Manure_Management_E_All_Data_(Norm).csv",
          "Emissions_Agriculture_Rice_Cultivation_E_All_Data_(Norm).csv",
          "Emissions_Land_Use_Burning_Biomass_E_All_Data_(Norm).csv",
          "Emissions_Land_Use_Cropland_E_All_Data_(Norm).csv",
          "Emissions_Land_Use_Forest_Land_E_All_Data_(Norm).csv",
          "Emissions_Land_Use_Grassland_E_All_Data_(Norm).csv",
          "Emissions_Land_Use_Land_Use_Total_E_All_Data_(Norm).csv"
        ))
    domains <- c("GT","GB","GH","GA","GV","GE","GU","GP","GM","GR","GI","GC","GF","GG","GL")
    fs <-
      ddply(tibble(files, domains),
            .(files),
            function(x) {
              if (file.exists(x$files)) {
                read.fs.bulk(x$files) %>%
                  mutate(Domain = as.character(x$domains))
              }
            }) %>% tbl_df()
    els <- fs %>%
      select(Domain, ElementCode, ElementName, Unit) %>%
      distinct()
    its <- fs %>%
      select(Domain, ItemCode, ItemName) %>%
      distinct()
    fs <-
      fs %>%
      select(Domain,
             AreaCode,
             ItemCode,
             ElementCode,
             Year,
             Value,
             Unit,
             ElementName,
             ItemName)
    save(fs, its, els, file = fsRDFile)
  } else {
    load(fsRDFile)
  }
  list(fs = fs, its = its, els = els)
}