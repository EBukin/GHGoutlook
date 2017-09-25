# China Project: Estimating GHG emission based on the OECD-FAO Outlook projections
# Eduard Bukin
# 23 September 2017




# Purpose -----------------------------------------------------------------

# Generic process is explained in the file "s_outlook_ghg_clean.R". Please se it for
# extensive description of the flow of the analysis.
#
# Here we focus on the needs of the China anlaysis. We are estimating GHG emissions
# for China and the world and returning resutls of the scenario and baseline.

# SETUP ------------------------------------------------------------------

# Installing correct versions of the supporting packages.
essentialPackages <-
  data.frame(
    pack = c(
      "devtools",
      "plyr",
      "dplyr",
      "tidyr",
      "tidyverse",
      "readxl",
      "stringr"
    ),
    vers = c("1.13.3", "1.8.4", "0.7.3", "0.7.1", "1.1.1", "1.0.0", "1.2.0"),
    stringsAsFactors = FALSE
  )
installedPacks <-
  as.data.frame(installed.packages(), stringsAsFactors = FALSE)

lapply(1:nrow(essentialPackages),
       function(x) {
         packToInst <- essentialPackages[x, ]
         reinstall <-
           compareVersion(a = installedPacks[installedPacks$Package == packToInst$pack, ]$Version, b = packToInst$vers)
         if (reinstall < 0) {
           install.packages(pkgs = packToInst$pack, dependencies = TRUE)
         }
       })
othePacks <-
  c(
    "DT",
    "rmarkdown",
    "gridExtra",
    "grid",
    "ggplot2",
    "ggthemes",
    "scales",
    "devtools",
    "gridGraphics"
  )
lapply(othePacks[!othePacks %in% installed.packages()[, 1]], install.packages, dependencies = TRUE)
lapply(c(othePacks, essentialPackages$pack),
       require,
       character.only = TRUE)

#' Making sure that the number of digits displayed is large enough.
options(scipen = 20)

# Sourcing functions ------------------------------------------------------

#' Loading locally developed functions
l_ply(str_c("R/", list.files("R/", pattern = "*.R")), source)

# Function that combinews logic of the GHG emissions
calc_ghg_outlook <- function(fs, ol) {
  gm <- outlook_emissions(fs, ol, DomainName = "GM")
  ge <- outlook_emissions(fs, ol, DomainName = "GE")
  gu <- outlook_emissions(fs, ol, DomainName = "GU")
  gp <- outlook_emissions(fs, ol, DomainName = "GP")
  gr <- outlook_emissions(fs, ol, DomainName = "GR")
  
  #  Reproducing GV - Cultivating Orghanic Soils -
  #  -  we repeat last know values.
  gv_fs <-
    fs %>%
    filter(Year %in% c(2000:2016), Domain == "GT") %>%
    map_fs_data(., fsYears = c(2000:2016)) %>%
    filter(ItemCode == "GV") %>%
    filter(AreaCode %in% get_ol_countries())
  gv <-
    gv_fs %>%
    filter(Year %in% (max(Year))) %>%
    mutate(Year = max(Year)) %>%
    group_by_(.dots = names(.)[!names(.) %in% c("Value")]) %>%
    summarise(Value = mean(Value)) %>%
    ungroup()
  gv <-
    ldply((max(gv$Year) + 1):2030, function(x) {
      gv %>%
        mutate(Year = x)
    }) %>%
    tbl_df() %>%
    bind_rows(gv_fs) %>%
    mutate(d.source = "Outlook")
  gv <-
    gv %>%
    bind_rows(gv_fs) %>%
    bind_rows(gv %>% filter(d.source ==  "Outlook") %>% mutate(d.source = "no adj. Outlook")) %>%
    arrange(Domain, AreaCode, ItemCode, ElementCode, Year) %>%
    agg_all_ol_regions()
  
  # Reproducing GB, GH and GA
  # As a proportion to the estimated emissions
  gtpart <-
    bind_rows(list(gm, ge, gu, gp, gr)) %>%
    agg_ghg_domains %>%
    agg_total_emissions
  
  gt <-
    outlook_emissions(
      fs,
      gtpart %>% filter(d.source == "Outlook"),
      DomainName = "GT",
      useActivity = FALSE
    ) %>%
    filter(!ItemCode %in% c("GM", "GE", "GU", "GP", "GR", "GV")) %>%
    bind_rows(gtpart, gv) %>%
    join_names()
  
  # Reproducing GI, GC, GG and GF domains
  #  reproducing data for:
  #    - GI - Burning Biomass,
  #    - GC - Cropland and
  #    - GG - Grassland
  #'   domains we assume that the values of emissions remains constant at the levels
  #'   of the last 5 years average. Blow we reproduce that.
  # Number of years lag for average projections
  nYears <- max(5 - 1, 0)
  lastYear = 2030
  # Reproducing emissions for the GI, GC, GG
  ol_lu_fs <-
    fs %>%
    filter(Year %in% c(2000:2016), Domain == "GL") %>%
    map_fs_data(., fsYears = c(2000:2016)) %>%
    filter(AreaCode %in% get_ol_countries())
  ol_lu <-
    ol_lu_fs %>%
    filter(Year %in% (max(Year) - nYears + 1):max(Year)) %>%
    mutate(Year = max(Year)) %>%
    group_by_(.dots = names(.)[!names(.) %in% c("Value")]) %>%
    summarise(Value = mean(Value)) %>%
    ungroup()
  # Expanding projected emissions for the projected period
  ol_lu <-
    ldply((max(ol_lu$Year) + 1):lastYear, function(x) {
      ol_lu %>%
        mutate(Year = x)
    }) %>%
    tbl_df() %>%
    bind_rows(ol_lu_fs) %>%
    mutate(d.source = "Outlook") %>%
    arrange(Domain, AreaCode, ItemCode, ElementCode, Year) %>%
    filter(ItemCode != "GF")
  ol_lu <-
    ol_lu %>%
    mutate(d.source = "no adj. Outlook") %>%
    bind_rows(ol_lu) %>%
    bind_rows(ol_lu_fs)
  
  
  #'  For the domain GF - Forestland we continue the last know value to the future.
  # Reproducing emissions for the GF
  gf_sf <-
    fs %>%
    filter(Year %in% c(2000:2016), Domain == "GL") %>%
    map_fs_data(., fsYears = c(2000:2016)) %>%
    filter(ItemCode == "GF")
  gf <-
    gf_sf %>%
    filter(AreaCode %in% get_ol_countries()) %>%
    filter(Year %in% (max(Year))) %>%
    mutate(Year = max(Year)) %>%
    group_by_(.dots = names(.)[!names(.) %in% c("Value")]) %>%
    summarise(Value = mean(Value)) %>%
    ungroup()
  # Expanding projected emissions for the projected period
  gf <-
    ldply((max(gf$Year) + 1):lastYear, function(x) {
      gf %>%
        mutate(Year = x)
    }) %>%
    tbl_df() %>%
    bind_rows(gf_sf) %>%
    mutate(d.source = "Outlook") %>%
    arrange(Domain, AreaCode, ItemCode, ElementCode, Year)
  gf <-
    gf %>%
    mutate(d.source = "no adj. Outlook") %>%
    bind_rows(gf) %>%
    bind_rows(gf_sf)
  
  #' Combining Landuse total emissions
  lu <-
    bind_rows(gf, ol_lu)  %>%
    agg_all_ol_regions() %>%
    join_names()
  
  
  # Exporting all calculations into one document 
  
  ghgOutlook <-
    bind_rows(lu, gt)
  
  list(ghgOutlook = ghgOutlook,
       activityBasedData = bind_rows(list(gm, ge, gu, gp, gr)) %>%
         agg_total_emissions %>%
         join_names())
}

# Running data estimation -------------------------------------------------

#    ____  _   _ _   _ 
#   |  _ \| | | | \ | |
#   | |_) | | | |  \| |
#   |  _ <| |_| | |\  |
#   |_| \_\\___/|_| \_|


# Loading FS data ---------------------------------------------------------

folder <- "data/FAOSTAT_Emissions/"
fsData <- load_all_fs_data(folder = folder)

fs <- fsData$fs
its <- fsData$its
els <- fsData$els

# ## Mapping tables
# Besides data from Outlook and FAOSTAT, we also need specific mapping tables
#   which explain mappings from FAOSTAT to Outlook areas and items.
itemsMTFile <- "mappingTables/fs_outlook_items_mt.csv"
itemsMT <- read_csv(
  itemsMTFile,
  col_types = cols(
    ItemCode = col_integer(),
    OutlookItemCode = col_character(),
    ItemCodeAggSign = col_character()
  )
)


# Table `elementsMT` describes mapping and adjustment of elements from FAOSTAT to outlook.
elementsMTFile <- "mappingTables/fs_outlook_elements_mt.csv"
elementsMT <-
  read_csv(
    elementsMTFile,
    col_types = cols(
      Domain = col_character(),
      ItemCode = col_character(),
      ElementCode = col_integer(),
      OutlookElementCode = col_character(),
      OutlookAdjustment = col_double()
    )
  )

#' Table `emissionsMT` describes mapping and assumption behind projection of the
#'   implied emissions factor for the years of projection.
emissionsMTFile <- "mappingTables/fs_outlook_emissions_mt.csv"
emissionsMT <-
  read_csv(
    emissionsMTFile,
    col_types = cols(
      Domain = col_character(),
      Emissions = col_integer(),
      OutlookEmissions = col_character(),
      ActivityElement = col_character(),
      EFLag = col_integer(),
      GHG = col_character()
    )
  )

# Calculating Baseline and scenario ghg -----------------------------------

# Baseline GHG calculations
baseLinePath <- "C:/SVNOECD/POSTMODEL/edpba/edpba2017/edpba2017csv"
baseLine <- load_troll_csv(file = baseLinePath, d.source = "Baseline")
baseLine <- select(baseLine, -`OUTPUT,0`)
baseLineGHG <- calc_ghg_outlook(fs = fs, ol = baseLine) 
baseLineGHG$ghgOutlook %>% 
# baseLineGHG$activityBasedData %>% 
  filter(AreaCode == "CHN") %>% 
  # filter(Domain == "GM") %>% 
  mutate(AreaCode = str_c(Domain, "_", AreaCode)) %>% 
  plot_group(n_page = 6, groups_var = c("AreaCode","ItemCode"), plots_var = c("ElementCode"))




# Scenario GHG calculations
scenarioPath <- "V:/2017/Master/ChinaScen/Database/CHNScen.csv"
scenarioData <- load_troll_csv(blDataPath1, d.source = "Scenario")
scenarioData <- select(scenarioData, -`OUTPUT,0`)
scenarioGHG <- 
  calc_ghg_outlook(fs = fs, ol = scenarioData) %>%
  select(Domain, AreaCode, ItemCode, ElementCode, Year, d.source, Value, ItemName, ElementName, Unit)
