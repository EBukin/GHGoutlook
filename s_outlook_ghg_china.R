# China Project: Estimating GHG emission based on the OECD-FAO Outlook projections
# Eduard Bukin
# 23 September 2017

### WARNING!!!

# Specify path to the baseline and scenario troll CSV file in the lines 390-420
# Run entire file line by line carefully reading what you are doing.
# See folder output/ for the results of the calculations
# See folder output/qa/ for the plots of the emissing quality assurance

# Purpose and process -----------------------------------------------------

# Generic process is explained in the file "s_outlook_ghg_clean.R". Please se it for
# extensive description of the flow of the analysis.
#
# Here we focus on the needs of the China anlaysis. We are estimating GHG emissions
# for China and the world and returning resutls of the scenario and baseline.

### WARNING!!!

# Specify path to the baseline and scenario troll CSV file in the lines 390-420
# See folder output/ for the results of the calculations
# See folder output/qa/ for the plots of the emissing quality assurance

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

#' # Process
#' 
#' The purpose of this document is to explain the proces of reproducing the GHG
#'   emissions data from the FAOSTAT using as the activity data the numbers 
#'   of OECD-FAO Agricultural Outlook.
#' 
#' The process is structured around particular domains, data for which has to be reproduced. 
#' 
#' For the domains GE, GM, GU, GP and GR emissions are reproduced based on the 
#' projected activity data, emissions related to other domains are treated separately 
#' and data is reproduced based on various assumptions:
#'  *   GB, GH and GA domains are projected as a constant share of total emissions
#'      assuming the share based on the 5 years average share in the last know historical
#'      period.  
#'  *   GY domain emissions data are approximared based on the area and yields of 
#'      crops relevant to the nitrogenous fertilizers consumption.  
#'  *   GV domain data is kept at the constant level as it is assumed in the 
#'      FAOSTAT. Alternatively, we test a situation, when emissions from the oranic 
#'      soils are changing with the same rate as the area utilised under the 
#'      palm oil produciton.  
#'      
#'  Below, we elaborate more explicitely on the methodology of the GHG estimation 
#'      for different domains. 

# Function that combinews logic of the GHG emissions
calc_ghg_outlook <- function(fs, ol, adj_activ = TRUE) {
  
  #' 
  #' # Implementing the process
  #' 
  #' ## Reproducing GR, GE, GU, GP and GM
  #' 
  #' Domains discussed in this part are estimated based on the activity data,
  #'   projected in the OECD-FAO Agricultural Outlook. There domains are: 
  #'  
  #'  *  GR - Rice cultivation  
  #'  *  GE - Enteric fementation
  #'  *  GM - Manure Management
  #'  *  GU - Manure applied to soils
  #'  *  GP - Manure left of pastures
  #'   
  #'   The overall process consist of several important steps. All steps are 
  #'       organised in the body of a function `outlook_emissions`. This funciotn 
  #'       utilises faostat data, outlook data and previously loaded mapping tables
  #'       for reproducing emissions for the pre-defined domain. The steps of 
  #'       reproduction are the following:
  #'   
  #'   1.  Mapping FAOSTAT Areas to the outlook regions reestimating activity data 
  #'       and emissiosn respectively. Mapping the FAOSTAT activity data to the 
  #'       outlook activity data aggregating FAOSTAT items to the outlook items 
  #'       and reestimating emissions and activity data according to aggregatings. 
  #'       This is done with the `map_fs_data` function, which uses items and elements
  #'       mapping tabels and faostat filtered to one domain data. Thisng the function
  #'       uses `map_fs2ol` and `agg_ol_regions` which does the aggregation of the 
  #'       FAOSTAT data to the outlook structure. In the mapping process, some of the 
  #'       items and elements may be agregted by substracting one from another what 
  #'       is specified with the mapping tables.
  #'       
  #'   3.  Adjusting outlook activity data to the baseline level derived from the 
  #'       FAOSTAT historical data. This step is the part of the `outlook_emissions`
  #'       function, where mapped faostat data is ued for subset the outlook data 
  #'       to the items and elements relevant for one domain with the funciton 
  #'       `subset_outlook`. After subsetting, we apply function `adjust_outlook_activity`
  #'       in order to adjust ativity data from the outlook to the levels of the
  #'       FAOSTAT in the historical period.    
  #'    
  #'   4.  At the next srep we `reestimate_emissions` data based on the activity 
  #'       if such was prepared in the OUTLOOK data.  
  #'       
  #'   5.  In some cases, for some items and elements outlook does not have any 
  #'       activity data. In such cases, we estimate the emissions for the 
  #'       missing items and elements combinations based on the constant share of 
  #'       these items and elements in the knownd and estimated emissions. 
  #'       Constant share is assumed based on the 5 years average share calculated 
  #'       on the last available. THis step is made with the funcotin `estimate_missing_emissions`.  
  #'       
  #'   6.  At the next step we convert all GHG to the GHG expressed in the CO2
  #'       equivalent with the functoin `convert_ghg`.
  #'       
  #'   7.  After the numbers are reestimated in the steps 1-4, we aggregate regions
  #'       relevant to the outlook such as "Big five" region, Cosimo and Aglink 
  #'       regions and the World total. THe regional aggregating is made using the 
  #'       function `agg_ol_regions`.    
  #'       
  #'       
  #' We perfrom all abovexplained calculations for one domain at the time. That allows 
  #'    us to apply the same functions and approaches to every domain maintaining 
  #'    methodological consistency.
  #'    
  #' Reproducing data.
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
  
  
  
  if (adj_activ) {
    ghgOutlook <-
      bind_rows(lu, gt) %>%
      filter(d.source != "no adj. Outlook")
    activityBasedData = bind_rows(list(gm, ge, gu, gp, gr)) %>%
      agg_total_emissions %>%
      join_names() %>%
      filter(d.source != "no adj. Outlook")
  } else {
    ghgOutlook <-
      bind_rows(lu, gt) %>%
      filter(d.source != "Outlook")
    activityBasedData = bind_rows(list(gm, ge, gu, gp, gr)) %>%
      agg_total_emissions %>%
      join_names() %>%
      filter(d.source != "Outlook")
  }
  
  
  list(
    ghgOutlook = ghgOutlook ,
    activityBasedData = activityBasedData
  )
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
# You need to specify the path to the baseline file here:
baseLinePath <- "C:/SVNOECD/POSTMODEL/edpba/edpba2017/edpba2017csv"

# Here you will run the calculations
baseLine <- load_troll_csv(file = baseLinePath, d.source = "Baseline")
baseLine <- select(baseLine, -`OUTPUT,0`)

# In the following obgect there are two lists:
#  1. One `baseLineGHG$ghgOutlook ` is the dataframe with all gomain specific general data
#  2. Second `baseLineGHG$activityBasedData` is with the data estimated based on the activities data from Outlook.
baseLineGHG <- calc_ghg_outlook(fs = fs, ol = baseLine) 

baseLineGHG_doamin_agg_data <- baseLineGHG$ghgOutlook
baseLineGHG_activity_based_data <- baseLineGHG$activityBasedData


# Running scenario 
# Specify path, following logic is the same.
scenarioPath <- "V:/2017/Master/ChinaScen/Database/CHNScen.csv"

scenarioData <- load_troll_csv(scenarioPath, d.source = "Scenario")
scenarioData <- select(scenarioData, -`OUTPUT,0`)
scenarioGHG <- calc_ghg_outlook(fs = fs, ol = scenarioData) 

scenarioGHG_doamin_agg_data <- scenarioGHG$ghgOutlook
scenarioGHG_activity_based_data <- scenarioGHG$activityBasedData


# Quality assurance -------------------------------------------------------

# here we visually inspect data for china in all domains specific dataframes and 
# data for each domain developed based on the activity data

combine_ghg_out_data <- function(baseline, scenario) {
  baseline <- 
    baseline %>% 
    filter(d.source %in% c("Outlook", "no adj. Outlook", "Faostat")) %>% 
    mutate(d.source = ifelse(d.source %in% c("Outlook", "no adj. Outlook"), str_c("Baseline_", d.source), d.source))
  
  scenario <- 
    scenario %>% 
    filter(d.source %in% c("Outlook", "no adj. Outlook")) %>% 
    mutate(d.source = ifelse(d.source %in% c("Outlook", "no adj. Outlook"), str_c("Scenario_", d.source), d.source))
  
  bind_rows(baseline, scenario)
  
}


# Combining scenario and outlook data and for potting all domains in one
comGHGAggDoaminData <- 
  combine_ghg_out_data(baseLineGHG_doamin_agg_data, scenarioGHG_doamin_agg_data) %>% 
  filter(d.source != "Faostat" & Year >= 2014 | d.source == "Faostat" & Year <= 2014)

comGHGActivData <- 
  combine_ghg_out_data(baseLineGHG_activity_based_data, scenarioGHG_activity_based_data) %>% 
  filter(d.source != "Faostat" & Year >= 2014 | d.source == "Faostat" & Year <= 2014)


### CHECK THIS PLOT ON THE CONSISTENCY AND FOR BEING abloe to compare scenarios
comGHGAggDoaminData %>% 
  filter(ElementCode %in% c("Emissions_CO2Eq")) %>% 
  filter(AreaCode == "CHN") %>% 
  mutate(AreaCode = str_c(Domain, "_", AreaCode)) %>% 
  plot_pages_into_pdf(n_page = 12, 
                      output_path = "output/qa/", 
                      output_name = "china_total_ghg_co2eq_by_domain",
                      groups_var = c("Domain", "AreaCode"), plots_var = c("ElementCode", "ItemCode"))

# Plotting each domain 
comGHGActivData %>% 
  filter(ElementCode %in% c("Emissions_CO2Eq", "EM_CH4Eq", "EM_N2OEq", "LI", "CI")) %>% 
  filter(AreaCode == "CHN") %>% 
  mutate(AreaCode = str_c(Domain, "_", AreaCode)) %>% 
  plot_pages_into_pdf(n_page = 4, 
                      output_path = "output/qa/", 
                      output_name = "china_total_ghg_co2eq_by_domain_",
                      files_var = "AreaCode",
                      groups_var = c("Domain", "ItemCode"), 
                      plots_var = c("ElementCode"))


# Saving data -------------------------------------------------------------

comGHGAggDoaminData %>% 
  filter(AreaCode == "CHN") %>%
  write_csv("output/by_domain_ghg_emissions_china.csv")

comGHGAggDoaminData %>% 
  write_csv("output/by_domain_ghg_emissions_all_countries.csv")

comGHGActivData %>% 
  filter(AreaCode == "CHN") %>%
  write_csv("output/by_activity_ghg_emissions_china.csv")

comGHGActivData %>% 
  write_csv("output/by_activity_ghg_emissions_all_countries.csv")
