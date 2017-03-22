#' ---
#' title: "Estimating GHG emission based on the OECD-FAO Outlook projections"
#' author: Eduard Bukin
#' date: 14 March 2017
#' output:
#'      prettydoc::html_pretty:
#'        toc: yes
#'        theme: architect
#' ---


#' *****
#' # Process
#' The purpose of this document is to reproduce GHG emissions data in FAOSTAT
#'   using as the activity data the OECD-FAO Agricultural Outlook numbers.
#' 
#' The process is consists of several steps:   
#'  1. Mapping (aggregating) FAOSTAT areas, items and elements to the Outlook
#'  2. Adjusting Outlook activities data to the FAOSTAT historical levels (if needed)  
#'  3. QA of the activity data projections on the per country/area basis.  
#'  4. Reestimating implied emissions factors based on aggregated areas and items in the FAOSTAT  
#'  5. Extrapolating implied emissions factors for the projected years fllowing one of the predefined rules  
#'  6. Reestimating projections of the GHG emission based in the 
#'     Outlook activity data and new Implicit emissions factors  
#' 
#' # Setup

#' Installing packages
#+results='hide', message = FALSE, warning = FALSE
packs <- c("plyr", "tidyverse", "dplyr", "tidyr","readxl", "stringr", "DT", "rmarkdown",
           "gridExtra", "grid", "ggplot2", "ggthemes", "scales", "devtools", "gridGraphics")
lapply(packs[!packs %in% installed.packages()[,1]], 
       install.packages,
       dependencies = TRUE)
lapply(packs, require, character.only = TRUE)

#' Making sure that the number of digits displayed is large enough.
options(scipen=20)

#' Loading locally developed functions
l_ply(str_c("R/", list.files("R/", pattern="*.R")), source)


#' # Loading data


#' ## Outlook data
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

#' ## FAOSTAT data
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


#' ## Mapping tables
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
             Domain = col_character(),
             ItemCode = col_character(),
             ElementCode = col_integer(),
             OutlookElementCode = col_character(),
             OutlookAdjustment = col_double()
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
             ActivityElement = col_character(),
             EFLag = col_integer(),
             GHG = col_character()
           ))
#' # Implementing the process  
#' 
#' We perfrom all following calculations for one domain at the time. That allows 
#'    us to apply the same functions and approaches to every domain maintaining 
#'    methodological consistency.
#'    
#' The process is organized in the followin way:
#'     
#'  1. Mapping (aggregating) FAOSTAT areas, items and elements to the Outlook using 
#'  function `map_fs_data` on the FAOSTAT data filtered for one domain;    
#'  
#'  2. Subsetting outlook activity data to the items and elements relevant to one 
#'  domain using the function `subset_outlook` and adjusting historical outlook data 
#'  to the levels of historical data in the FAOSTAT using function `adjust_outlook_activity`;    
#'    
#'  3. Recalculating implied emissions factors for reaggregated areas, item and 
#'  element of the FAOSTAT data and re-calculate emissing using implied emissions 
#'  factors calculated at the previous step and adjusted outlook activities data
#'  using function `reestimate_emissions`.   
#'  
#' All abovementioned functoin are described below.
#' 










#' # Process of re-estimating emissions
#' 
#' Bcause the outlook activity data is limited compare to the FAOSTAT we can 
#'     customise the process of reestimating GHG emissions data only for a subset
#'     of domains. For other domains, the proces will have to remain manual. 
#'     
#' With the funciton `outlook_emissions` we aggregate data for such domains like 
#'     GM, GE, GU, GP, GR. Estimations are made for all countries and than data 
#'     is aggregated to the regions.'     
#'     
outlook_emissions <-
  function(fs, ol, DomainName, fsYears = c(2000:2020), useActivity = TRUE) {
    
    # Countries list
    countries <-
      c("AF1", "AF2", "AGO", "ARG", "AS2", "ASA", "ASD", "AUS", "BFA", "BGD",
        "BRA", "CAN", "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COL",
        "DZA", "E15", "EGY", "ETH", "EUE", "EUW", "GAB", "GHA", "HTI", "IDN",
        "IND", "IRN", "IRQ", "ISR", "JPN", "KAZ", "KEN", "KHM", "KOR", "LAO",
        "LBN", "LBY", "MAR", "MDG", "MEX", "MLI", "MMR", "MOZ", "MRT", "MWI",
        "MYS", "NGA", "NMS", "NOR", "NRE", "NZL", "OCE", "OCL", "PAK", "PER",
        "PHL", "PRY", "RUS", "RWA", "SAC", "SAU", "SDN", "SEN", "SOM", "TCD",
        "THA", "TUN", "TUR", "TZA", "UGA", "UKR", "URY", "USA", "VNM", #"WLD",
        "YEM", "ZAF", "ZMB", "ZWE")
      
    # We are only concearned about subset of countr
    fsSubset<-
      fs %>% 
      filter(Year %in% fsYears, Domain == DomainName) %>% 
      map_fs_data %>% 
      filter(AreaCode %in% countries)
    
    # Subsetting and aggregating and reestinmating emissions for non adjusted
    #     Outlook activity data
    outlookEmissions <-
      ol %>%
      subset_outlook(fsSubset, "no adj. Outlook")
    if (useActivity) {
      outlookEmissions <-
        outlookEmissions  %>%
        reestimate_emissions(fsSubset) %>%
        estimate_missing_emissions(fsSubset) %>%
        convert_ghg() %>%
        distinct()
    } else {
      outlookEmissions <-
        outlookEmissions  %>%
        estimate_missing_emissions(fsSubset) %>%
        convert_ghg() %>%
        distinct()
    }
    
    
    # Subsetting and aggregating and reestinmating emissions for adjusted
    #     Outlook activity data
    outlookAdjEmissions <-
      ol %>%
      subset_outlook(fsSubset, "Outlook")
    
    if (useActivity) {
      outlookAdjEmissions <-
        outlookAdjEmissions  %>%
        adjust_outlook_activity(fsSubset) %>% 
        reestimate_emissions(fsSubset) %>%
        estimate_missing_emissions(fsSubset) %>%
        convert_ghg() %>%
        distinct()
    } else {
      outlookAdjEmissions <-
        outlookAdjEmissions  %>%
        adjust_outlook_activity(fsSubset) %>% 
        estimate_missing_emissions(fsSubset) %>%
        convert_ghg() %>%
        distinct()
    }
    
    # Bind all data convert GHG and aggregate regions
    bindedData <- 
      bind_rows(outlookEmissions, outlookAdjEmissions, fsSubset)
    
    # Aggregating big five
    bindedData <-
      bindedData %>%
      bind_rows(
        agg_ol_regions(., regionVar = "OutlookBigRegion") %>%
          filter(!AreaCode %in% unique(bindedData[["AreaCode"]]))
      )
    
    # Aggregating Cosimo Outlook
    bindedData <-
      bindedData %>%
      bind_rows(
        agg_ol_regions(., regionVar = "OutlookSuperRegion") %>%
          filter(!AreaCode %in% unique(bindedData[["AreaCode"]]))
      )
    
    # Aggregating SEA
    bindedData <-
      bindedData %>%
      bind_rows(
        agg_ol_regions(., regionVar = "OutlookSEAsia") %>%
          filter(!AreaCode %in% unique(bindedData[["AreaCode"]]))
      )
    
    # Aggregating WLD
    bindedData <-
      bindedData %>%
      bind_rows(
        agg_ol_regions(., regionVar = "Global") %>%
          filter(!AreaCode %in% unique(bindedData[["AreaCode"]]))
      )
    
    
    # Returning all data
    bindedData
   
  }




#' Reproducing those domains, which are reproducable based on activity data
gm <- 
  outlook_emissions(fs, ol, DomainName = "GM")

ge <- 
  outlook_emissions(fs, ol, DomainName = "GE")

gu <- 
  outlook_emissions(fs, ol, DomainName = "GU")

gp <- 
  outlook_emissions(fs, ol, DomainName = "GP")

gr <- 
  outlook_emissions(fs, ol, DomainName = "GR")

# Reprosducing imputed numbers
gtpart <- 
  bind_rows(list(gm, ge, gu, gp, gr)) %>% 
  agg_ghg_domains %>% 
  agg_total_emissions

gt <-
  outlook_emissions(fs,
                    gtpart %>% filter(d.source == "Outlook"),
                    DomainName = "GT", useActivity = FALSE) %>%
  filter(!ItemCode %in% c("GM", "GE", "GU", "GP", "GR")) %>% 
  bind_rows(gtpart) %>% 
  join_names()

# QA of some celeted numbers

# gt %>%
#   filter(AreaCode == "VNM") %>%
#   plot_group(n_page = 12,
#              groups_var = c("ElementCode"),
#              plots_var = "ItemCode"  )
# 
# gtt %>% 
#   filter(AreaCode == "OutlookSEAsia", ElementCode == "Emissions_CO2Eq") %>% 
#   plot_group(n_page = 6,
#              groups_var = c("ElementCode"),
#              plots_var = "ItemCode"  )
# # QUALITY ASSURANCE
# plot_group(gm ,
#            n_page = 6,
#            groups_var = c("ElementCode"),
#            plots_var = "ItemCode"
# )

# Exporting numbers
gt %>% 
  mutate(AreaCode2 = AreaCode) %>% 
  filter(d.source == "Faostat" & Year <= 2014 |
           d.source == "Outlook" & Year > 2014 ) %>% 
  arrange(Domain, AreaCode, ItemCode, ElementCode, d.source, Year) %>% 
write.csv(file = "output/preliminatyData.csv") 






#' ## QA of the adjusted activity data
#+echo = FALSE, results = 'hide', message = FALSE
# QAData <- 
#   bind_rows(activity, 
#           activity %>% 
#             select(AreaCode, ItemCode, ElementCode, Year) %>% 
#             distinct() %>% 
#             left_join(fsol) %>% 
#             filter(!is.na(Value)),
#           olSubset %>% 
#             mutate(d.source = "old_Outlook") %>% 
#             right_join(activity %>% 
#                          select(AreaCode, ItemCode, ElementCode, Year) %>% 
#                          distinct())%>% 
#             filter(!is.na(Value))) %>%
#   filter(AreaCode %in% c("WLD", "RestOfTheWorld", "OutlookSEAsia", "CHN", "KHM", 
#                          "IDN", "LAO", "MYS", "MMR", "PHL", "THA", "VNM"))
# plot_group(filter(QAData, AreaCode %in% c("WLD", "RestOfTheWorld", "OutlookSEAsia", "CHN")),
#   n_page = 4,
#   groups_var = c("ElementCode", "ItemCode"),
#   plots_var = "AreaCode"
# )
# 
# plot_group(filter(QAData, AreaCode %in% c("KHM", "IDN", "LAO", "MYS")),
#   n_page = 4,
#   groups_var = c("ElementCode", "ItemCode"),
#   plots_var = "AreaCode"
# )
# 
# plot_group(filter(QAData, AreaCode %in% c("MMR", "PHL", "THA", "VNM")),
#   n_page = 4,
#   groups_var = c("ElementCode", "ItemCode"),
#   plots_var = "AreaCode"
# )
# 

#'
#'
#' # Annexes
#' 
#' ## Funciton `map_fs2ol` for aggregating outlook countries to the regions
#+code=readLines("r/map_fs2ol.R")

#' ## Funciton `agg_ol_regions` for aggregating outlook countries to the regions
#+code=readLines("r/agg_ol_regions.R")

#' ## Mapping tabels from FAOSTAT countries to Outlook countries and regions
#+echo=FALSE
options(markdown.HTML.header = system.file('misc', 'datatables.html', package = 'knitr'))
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

# Changing encoding
Encoding(areaMT$AreaName) <- "latin1"
Encoding(areaMT$OutlookAreaName) <- "latin1"
# Printing the table
datatable(areaMT, 
          rownames=FALSE, 
          colnames = 
            c("FS Code", "FS Name", "Outlook Code", "Outlook name",
              "Status", "Sub Regions", "Big Five", "Super Region",
              "Southeast Asia"))


#' ## Mapping tabel for mapping FAOSTAT items to the Outlook
#+echo=FALSE
datatable(itemsMT, style = 'bootstrap', rownames=FALSE)

#' ## Mapping tabel for mapping FAOSTAT elements to the Outlook
#+echo=FALSE
datatable(elementsMT, style = 'bootstrap', rownames=FALSE)

