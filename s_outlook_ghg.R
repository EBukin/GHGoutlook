#' ---
#' title: "Estimating GHG emission based on the OECD-FAO Outlook projections"
#' author: Eduard Bukin
#' date: 14 March 2017
#' output:
#'    prettydoc::html_pretty:
#'        toc: yes
#'        theme: architect
#'        highlight: github
#' 
#' ---

#' *****
#' # Purpose
#' The purpose of this document is to reproduce GHG emissions data in FAOSTAT
#'   using as the activity data the OECD-FAO Agricultural Outlook numbers.
#' 
#' ## Process
#' The process is consists of several steps:  
#' 
#'  1. Mapping (aggregating) FAOSTAT areas to the Outlook areas    
#'  2. Mapping (aggregating) FAOSTAT items (activity data and GHG emissions) to the Outlook items (only activity data)  
#'  3. Andjusting Outlook activities data to the FAOSTAT historical levels (if needed)  
#'  4. QA of the activity data projections on the per country/area basis.  
#'  5. Reestimating implied emissions factors based on aggregated areas and items in the FAOSTAT  
#'  6. Extrapolating implied emissions factors for the projected years fllowing one of the predefined rules  
#'  7. Reestimating projections of the GHG emission based in the Outlook activity data and new Implicit emissions factors  
#' 
#' # Setup

#' Installing packages
#+results='hide', message = FALSE, warning = FALSE
packs <- c("plyr", "tidyverse", "dplyr", "tidyr","readxl", "stringr", "pander",
           "gridExtra", "grid", "ggplot2", "ggthemes", "scales", "devtools")
lapply(packs[!packs %in% installed.packages()[,1]], 
       install.packages,
       dependencies = TRUE)
lapply(packs, require, character.only = TRUE)

#' Making sure that the number of digits displayed is large enough.
options(scipen=20)

#' Loading locally developed functions
l_ply(str_c("R/", list.files("R/", pattern="*.R")), source)


#' # Implementation


#' ## 0. Loading data


#' ### Outlook data
#'  First we load all outlook data. If there is no data savein the Rdata file we reload all data from the CSV file.
olRDFile <- "data/outlook.Rdata"
if(!file.exists(olRDFile)) {
  olFile <- "C:/Users/Bukin/OneDrive - Food and Agriculture Organization/outlookGHG/data/base17.csv"
  if(!file.exists(olFile)) olFile <- "data/base17.csv"
  ol <- load_troll_csv(olFile, d.source = "") %>% 
    select(AreaCode, ItemCode, ElementCode, Year, Value)
  save(ol, file = olRDFile)
} else {
  load(file = olRDFile)
}

#' ### FAOSTAT data
#' Next step is loading all FAOSTAT data. Since FAOSTAT data combines data from 
#'   multiple domains, we laod it all in on .Rdata file. In csae if there is no such file,
#'   we reload all data from each domain specific file and save it in the R data file for further use.
fsRDFile <- "data/all_fs_emissions.Rdata"
if(!file.exists(fsRDFile)) {
  files <- 
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
      "data/Emissions_Land_Use_Burning_Biomass_E_All_Data_(Norm).csv",
      "data/Emissions_Land_Use_Cropland_E_All_Data_(Norm).csv",
      "data/Emissions_Land_Use_Forest_Land_E_All_Data_(Norm).csv",
      "data/Emissions_Land_Use_Grassland_E_All_Data_(Norm).csv",
      "data/Emissions_Land_Use_Land_Use_Total_E_All_Data_(Norm).csv")
  domains <- c("GT", "GB", "GH", "GA", "GV", "GE", "GU", 
               "GP", "GM", "GR", "GI", "GC", "GF", "GG", "GL")
  fs <- 
    ddply(tibble(files, domains), 
          .(files), 
          function(x) {
            if(file.exists(x$files)) {
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
    select(Domain, AreaCode, ItemCode, ElementCode, 
           Year, Value, Unit, ElementName, ItemName)
  save(fs, its, els, file = fsRDFile) 
} else {
  load("data/all_fs_emissions.Rdata")
}


#' ### Mapping tables
#' Besides data from Outlook and FAOSTAT, we also need specific mapping tables
#'   which explain mappings from FAOSTAT to Outlook areas and items.
itemsMTFile <- "mappingTables/fs_outlook_items_mt.csv"
itemsMT <- read_csv(itemsMTFile, 
                    col_types = cols(
                      ItemCode = col_integer(),
                      OutlookItemCode = col_character(),
                      ItemCodeAggSign = col_character()
                      ))

#' Table `elementsMT` describes mapping and adjustment of elements from FAOSTAT to outlook.
elementsMTFile <- "mappingTables/fs_outlook_elements_mt.csv"
elementsMT <- 
  read_csv(elementsMTFile, 
           col_types = cols(
             Item = col_integer(),
             OutlookItem = col_character(),
             Activity = col_integer(),
             OutlookActivity = col_character(),
             OutlookActivityAdjustment = col_integer()
           ))

#' Table `emissionsMT` describes mapping and assumption behind projection of the 
#'   implied emissions factor for the years of projection.
emissionsMTFile <- "mappingTables/fs_outlook_emissions_mt.csv"
emissionsMT <- 
  read_csv(emissionsMTFile, 
           col_types = cols(
             Domain = col_character(),
             Emissions = col_integer(),
             OutlookEmissions = col_character(),
             EFLag = col_integer(),
             GHG = col_character()
           ))

#' ### Initialising additional variables
#' Years of the FAOSTAT data that we are interested in:
Years <- c(2000:2030)

#' ### Cleaning loaded data TBC
#' Filtering FAOSTAT and OUTLOOK data to the list of important Items and Elements

#' ## 1. Mapping FAOSTAT Areas to Outlook
#' 
#' In this subsection we aggregate FAOSTAT areas to the outlook aggregates of 
#'   individual countries and regions. For aggregating we use funcitons `agg_ol_regions`
#'   and `map_fs2ol` and a mappin table listed in the annexes below.
fsOlAgg <-
  fs %>%
  filter(Year %in% Years) %>%
  map_fs2ol() %>%
  agg_ol_regions(., regionVar = "OutlookSubRegion", allCountries = F)
fsOlAgg <-
  fsOlAgg %>%
  bind_rows(agg_ol_regions(., regionVar = "OutlookBigRegion") %>%
              filter(!AreaCode %in% unique(fsOlAgg[["AreaCode"]])))
fsOlAgg <-
  fsOlAgg %>%
  bind_rows(
    agg_ol_regions(., regionVar = "OutlookSuperRegion") %>%
      filter(!AreaCode %in% unique(fsOlAgg[["AreaCode"]]))
  ) 
fsOlAgg <-
  fsOlAgg %>%
  bind_rows(
    agg_ol_regions(., regionVar = "OutlookSEAsia") %>%
      filter(!AreaCode %in% unique(fsOlAgg[["AreaCode"]]))
  ) %>% 
  select(Domain, AreaCode, ItemCode, ElementCode, Year, Value)

#' ## Mapping FAOSTAT Items to the Outlook
#' 
#' In the 







#'------------------------------------------------------------------------------
#' # Annexes
#' 
#' ## Funciton `map_fs2ol` for aggregating outlook countries to the regions
#+code=readLines("r/map_fs2ol.R")

#' ## Funciton `agg_ol_regions` for aggregating outlook countries to the regions
#+code=readLines("r/agg_ol_regions.R")

#' ## Mapping tabels from FAOSTAT countries to Outlook countries and regions
#+echo=FALSE
areaMT <- read_csv("mappingTables/faostat_areas_outlook_areas.csv", 
                   col_types = cols(
                     AreaCode = col_integer(),
                     AreaName = col_character(),
                     OutlookAreaCode = col_character(),
                     OutlookAreaName = col_character(),
                     OutlookStatus = col_character(),
                     OutlookSubRegion = col_character(),
                     OutlookBigRegion = col_character(),
                     OutlookSuperRegion = col_character(),
                     OutlookSEAsia = col_character()
                   ))
# knitr::kable(areaMT, digits=0, 
#              col.names = c("FS Code", "FS Name", "Outlook Code", "Outlook name", 
#                            "Status", "Sub Regions", "Big Five", "Super Region", 
#                            "Southeast Asia"))
pander(areaMT)
