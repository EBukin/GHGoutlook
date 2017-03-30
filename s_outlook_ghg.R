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
#'  
#' # Setup
#' 
#' Installing packages
#+results='hide', message = FALSE, warning = FALSE
packs <- c("plyr", "tidyverse", "dplyr", "tidyr","readxl", "stringr", 
           "DT", "rmarkdown", "gridExtra", "grid", "ggplot2", "ggthemes", 
           "scales", "devtools", "gridGraphics")
lapply(packs[!packs %in% installed.packages()[,1]], install.packages,
       dependencies = TRUE)
lapply(packs, require, character.only = TRUE)

#' Making sure that the number of digits displayed is large enough.
options(scipen=20)

#' Loading locally developed functions
l_ply(str_c("R/", list.files("R/", pattern="*.R")), source)

#' # Loading data
#' 
#' ## Outlook data
#' 
#'  First we load all outlook data. If there is no data savein the Rdata file we reload all data from the CSV file.
#'  
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
#' 
#' Next step is loading all FAOSTAT data. Since FAOSTAT data combines data from 
#'   multiple domains, we laod it all in on .Rdata file. In csae if there is no such file,
#'   we reload all data from each domain specific file and save it in the R data file for further use.
#'   
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


#' 
#' ## Mapping tables
#' 
#' Besides data from Outlook and FAOSTAT, we also need specific mapping tables
#'   which explain mappings from FAOSTAT to Outlook areas and items.
#'   
itemsMTFile <- "mappingTables/fs_outlook_items_mt.csv"
itemsMT <- read_csv(itemsMTFile, 
                    col_types = cols(
                      ItemCode = col_integer(),
                      OutlookItemCode = col_character(),
                      ItemCodeAggSign = col_character()
                    ))

#' 
#' Table `elementsMT` describes mapping and adjustment of elements from FAOSTAT to outlook.
#' 
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
#' 
#' ## Reproducing GV
#' 
#' For the GV - Cultivating Orghanic Soils domain we repeat the last know values.
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
# Expanding projected emissions for the projected period
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
  bind_rows(gv %>% filter(d.source ==  "Outlook") %>% mutate(d.source = "no adj. Outlook"))%>%
  arrange(Domain, AreaCode, ItemCode, ElementCode, Year) %>% 
  agg_all_ol_regions()



#' ## Reproducing GB, GH and GA
#' 
#' Reproducing emissions for the domains Burning crop residues, Burning Savana 
#'   and crop residues. To reproduce emissions for these domains such we use 
#'   the constant share of the enissions from this dimains in the estimatable 
#'   emissions from agriculture and continue this trend to future. 
#'   
#' Projecting of these domains is made based on the total aggregates of all 
#'   estimated domains and Agriculture total domain.
#'   
#'      
gtpart <- 
  bind_rows(list(gm, ge, gu, gp, gr)) %>% 
  agg_ghg_domains %>% 
  agg_total_emissions

gt <-
  outlook_emissions(fs,
                    gtpart %>% filter(d.source == "Outlook"),
                    DomainName = "GT", useActivity = FALSE) %>%
  filter(!ItemCode %in% c("GM", "GE", "GU", "GP", "GR", "GV")) %>% 
  bind_rows(gtpart, gv) %>% 
  join_names()


#' ## Reproducing GI, GC, GG and GF domains
#' 
#' When reproducing data for the GI - Burning Biomass, GC - Cropland and GG - Grassland
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
  bind_rows(ol_lu)%>%
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

#' # Combining and exporting all emissions not adjusted data
seaData <- 
  bind_rows(lu, gt) %>% 
  filter(d.source == "Outlook") %>% 
  filter(AreaCode %in% c("WLD", "RestOfTheWorld", "OutlookSEAsia", "KHM", 
                         "IDN", "LAO", "MYS", "MMR", "PHL", "THA", "VNM"))


#' ## Adjusting organic soils and cropland 
#' 
#' This adjustment is made manually in the file, which we furtherly loaded to
#'    the main data.
#'  
#' Export relevant data for adjustment into a file
seaData %>%
  filter(AreaCode %in% c("MYS", "IDN"),
         ItemCode %in% c("GV", "GC")) %>%
  bind_rows(ol %>% filter(AreaCode %in% c("MYS", "IDN"), ItemCode == "PL", ElementCode == "AH")) %>%
  # slice(c(125, 126))
  spread(Year, Value) %>%
  write_csv("adjustmetns/baseOrganicSoilsCroplandAdjustmens.csv")

# #' Loading manually adjusted organic soils and cropland data
# seaAdjData_part1 <-
#   seaData %>%
#   filter(! (AreaCode %in% c("MYS", "IDN") & ItemCode %in% c("GV", "GC"))) %>% 
#   bind_rows(read_csv("adjustmetns/AdjustedOrganicSoilsCroplandAdjustmens.csv") %>% 
#               gather(Year, Value, 9:length(.)) %>% 
#               mutate(Year = as.integer(Year),
#                      Value = as.numeric(Value))) %>% 
#   filter(AreaCode != "OutlookSEAsia") %>% 
#   bind_rows(agg_all_ol_regions(.) %>% 
#               filter(AreaCode == "OutlookSEAsia")) %>% 
#   mutate(d.source = "Outlook organic cropland and forest")

#' ## Adjusting organic soils, cropland and forestland
#' 
#' To adjust forest land data we need to manipulate data from the forest land domain directly.
#' 
# Exporting forest data for manual fixup
fs %>%
  filter(Year %in% c(2000:2016), Domain == "GF") %>%
  map_fs_data(., fsYears = c(2000:2016)) %>%
  filter((AreaCode %in% c("MYS", "IDN") & ItemCode %in% c("FO", "FC")))  %>%
  bind_rows(ol %>% filter(AreaCode %in% c("MYS", "IDN"), ItemCode == "PL", ElementCode == "AH")) %>%
  bind_rows(seaData %>%
              filter(AreaCode %in% c("MYS", "IDN"),
                     ItemCode %in% c("GF"))) %>% 
  spread(Year, Value) %>%
  arrange(AreaCode, ItemCode, ElementCode) %>% 
  write_csv("adjustmetns/baseForestdjustmens.csv")

#' Loading manually adjusted organic soils and cropland data and forest land data
seaAdjData_part1 <-
  seaData %>%
  filter(! (AreaCode %in% c("MYS", "IDN") & ItemCode %in% c("GV", "GC", "GF"))) %>% 
  bind_rows(read_csv("adjustmetns/AdjustedOrganicSoilsCroplandAdjustmens.csv") %>% 
              gather(Year, Value, 9:length(.)) %>% 
              mutate(Year = as.integer(Year),
                     Value = as.numeric(Value))) %>% 
  bind_rows(read_csv("adjustmetns/AdjustedForest.csv") %>% 
              filter(ItemCode == "GF") %>% 
              gather(Year, Value, 9:length(.)) %>% 
              mutate(Year = as.integer(Year),
                     Value = as.numeric(Value))) %>% 
  filter(AreaCode != "OutlookSEAsia") %>% 
  bind_rows(agg_all_ol_regions(.) %>% 
              filter(AreaCode == "OutlookSEAsia")) %>% 
  mutate(d.source = "Outlook organic cropland forest")




#' Preparing forest data as a reference
#' FOREST FOR THE BASE and Extra PARTS -=------------------------------------------
#' 
#' 
#' 
#' 
#' 
gf_Extra <-
  fs %>%
  filter(Year %in% c(2000:2016), Domain == "GF") %>%
  map_fs_data(., fsYears = c(2000:2016)) %>%
  filter(ItemCode %in% c("FO", "FC"), ElementCode != "Area")  %>% 
  mutate(d.source = "Outlook") 

# Epanding data with the last available values
gf_Extra <-
  ldply(c(2000:2030), 
        function(x) {
          gf_Extra %>% 
            select(Domain, AreaCode, ItemCode, ElementCode, d.source) %>% 
            distinct() %>% 
            mutate(Year = x)}) %>% 
  tbl_df %>% 
  left_join(gf_Extra, by = c("Domain", "AreaCode", "ItemCode", "ElementCode", "d.source", "Year")) %>% 
  group_by(Domain,  AreaCode, ItemCode, ElementCode, d.source) %>% 
  arrange(Domain,  AreaCode, ItemCode, ElementCode, d.source, Year) %>% 
  fill(Value) %>% 
  ungroup() 

# Adding data from the adjustment table
gf_Extra <-
  gf_Extra %>% 
  filter(!AreaCode %in% c("MYS", "IDN")) %>% 
  # spread(Year, Value) %>% 
  bind_rows(
    read_csv("adjustmetns/AdjustedForest.csv") %>% 
      filter(ItemCode %in% c("FO", "FC")) %>% 
      gather(Year, Value, 9:length(.)) %>% 
      mutate(Year = as.integer(Year),
             Value = as.numeric(Value)) %>% 
      select(Domain, AreaCode, ItemCode, ElementCode, d.source, Year, Value)) %>% 
  mutate(d.source = "Outlook organic cropland forest") %>% 
  bind_rows(gf_Extra) %>% 
  join_names() 

gf_Extra_sea <-
  gf_Extra %>%
  agg_ol_regions(., regionVar = "OutlookSEAsia") %>%
  filter(AreaCode == "OutlookSEAsia")

# Adding other extra things such as activity data
SEA_activity <- 
  bind_rows(list(gm, ge, gu, gp, gr)) %>%
  filter(d.source == "Outlook", AreaCode == "OutlookSEAsia") %>% 
  filter(ElementCode %in% c("LI", "CI", "AH"), Domain %in% c("GR", "GM")) %>% 
  bind_rows(
    ol %>%
      agg_ol_regions(., regionVar = "OutlookSEAsia") %>%
      filter(AreaCode == "OutlookSEAsia", 
             ItemCode == "PL", 
             ElementCode == "AH")) %>% 
  mutate(d.source = "Outlook")
# 
SEA_separate_activity <-
  bind_rows(list(gm, ge, gu, gp, gr)) %>%
  filter(d.source == "Outlook", AreaCode %in% c("LAO", "VNM", "KHM", "IDN", "MYS", "PHL", "THA", "MMR")) %>%
  filter(ElementCode %in% c("LI", "CI", "AH"), Domain %in% c("GR", "GM")) %>%
  bind_rows(
    ol %>%
      agg_ol_regions(., regionVar = "OutlookSEAsia") %>%
      filter(AreaCode %in% c("LAO", "VNM", "KHM", "IDN", "MYS", "PHL", "THA", "MMR"),
             ItemCode == "PL",
             ElementCode == "AH"))%>% 
  mutate(d.source = "Outlook")

#' Exporting data for SEA total only
export <- 
  bind_rows(seaData, seaAdjData_part1, gf_Extra_sea) %>% 
  filter(Year >= 2000 & Year < 2027) %>% 
  mutate(Year = as.character(Year)) 

export <-
  export %>%
  mutate(#Year = ifelse(Year %in% as.character(c(2001:2010)), "2001-2010", Year),
    Year = ifelse(Year %in% as.character(c(2014:2016)), "2014-2016", Year)) %>%
  group_by_(.dots = names(.)[!names(.) %in% c("Value")]) %>%
  summarise(Value = mean(Value)) %>%
  filter(Year %in% c("2001-2010", "2014-2016")) %>%
  bind_rows(export) %>% 
  # filter(Year %in% c("2001-2010", "2014-2016", "2026")) %>% 
  filter(AreaCode  == "OutlookSEAsia", ElementCode == "Emissions_CO2Eq") %>%
  ungroup() %>% 
  bind_rows(SEA_activity %>% mutate(Year = as.character(Year)) %>% join_names())

BurningSavanna <- 
  filter(export, Year == "2014-2016", ItemCode == "GH") %>% 
  rename(Savanna = Value) %>% 
  select(AreaCode, ItemCode, ElementCode, d.source, Savanna )
BurningBiomass <- 
  filter(export, Year == "2014-2016", ItemCode == "GI") %>% 
  rename(Biomass = Value) %>% 
  select(AreaCode, ItemCode, ElementCode, d.source, Biomass )

# Writing all
export %>% 
  left_join(BurningSavanna , by = c("AreaCode", "ItemCode", "ElementCode", "d.source")) %>%
  left_join(BurningBiomass , by = c("AreaCode", "ItemCode", "ElementCode", "d.source")) %>%
  mutate(Value = ifelse(ItemCode == "GH", Savanna, Value ),
         Value = ifelse(ItemCode == "GI", Biomass, Value )) %>%
  select(-Biomass, -Savanna) %>%
  # slice(c(3, 71))
  spread(Year, Value) %>%
  right_join(tibble(ItemCode = c("MK", "BV", "SH", "PT", "PK", "RI", "PL", "GH",
                                 "GB", "GI", "GU", "GP", "GM", "GE", "GA",
                                 "GY", "GR", "GV", "GC", "GG", "FO", "FC",
                                 "GF"),
                    ElementCode = c("CI", "LI", "LI", "LI", "LI", "AH", "AH",
                                    "Emissions_CO2Eq", "Emissions_CO2Eq", "Emissions_CO2Eq",
                                    "Emissions_CO2Eq", "Emissions_CO2Eq", "Emissions_CO2Eq",
                                    "Emissions_CO2Eq", "Emissions_CO2Eq", "Emissions_CO2Eq",
                                    "Emissions_CO2Eq", "Emissions_CO2Eq", "Emissions_CO2Eq",
                                    "Emissions_CO2Eq", "Emissions_CO2Eq", "Emissions_CO2Eq", 
                                    "Emissions_CO2Eq"))) %>% 
  select(-ItemCode, -ElementCode, -Unit) %>%
  arrange(d.source) %>% 
  select(AreaCode, ElementName, Domain,	ItemName,	d.source, `2016`, `2026`, everything()) %>%
  write_csv("output/SEA_total_prelim_adjusted.csv")


#' Exporting data for SEA All countries not totals
export2 <- 
  bind_rows(seaData, seaAdjData_part1, gf_Extra_sea) %>% 
  filter(Year >= 2000 & Year < 2027) %>% 
  mutate(Year = as.character(Year)) 

export2 <-
  export2 %>%
  mutate(#Year = ifelse(Year %in% as.character(c(2001:2010)), "2001-2010", Year),
    Year = ifelse(Year %in% as.character(c(2014:2016)), "2014-2016", Year)) %>%
  group_by_(.dots = names(.)[!names(.) %in% c("Value")]) %>%
  summarise(Value = mean(Value)) %>%
  filter(Year %in% c("2001-2010", "2014-2016")) %>%
  bind_rows(export2) %>% 
  # filter(Year %in% c("2001-2010", "2014-2016", "2026")) %>% 
  filter(ElementCode == "Emissions_CO2Eq") %>%
  ungroup() %>% 
  bind_rows(SEA_separate_activity %>% mutate(Year = as.character(Year)) %>% join_names()) %>% 
  filter(AreaCode %in% c("LAO", "VNM", "KHM", "IDN", "MYS", "PHL", "THA", "MMR"))

BurningSavanna <- 
  filter(export2, Year == "2014-2016", ItemCode == "GH") %>% 
  rename(Savanna = Value) %>% 
  select(AreaCode, ItemCode, ElementCode, d.source, Savanna )
BurningBiomass <- 
  filter(export2, Year == "2014-2016", ItemCode == "GI") %>% 
  rename(Biomass = Value) %>% 
  select(AreaCode, ItemCode, ElementCode, d.source, Biomass )

# Writing all
export2 %>% 
  left_join(BurningSavanna , by = c("AreaCode", "ItemCode", "ElementCode", "d.source")) %>%
  left_join(BurningBiomass , by = c("AreaCode", "ItemCode", "ElementCode", "d.source")) %>%
  mutate(Value = ifelse(ItemCode == "GH", Savanna, Value ),
         Value = ifelse(ItemCode == "GI", Biomass, Value )) %>%
  select(-Biomass, -Savanna) %>%
  # slice(c(6166, 6167))
  spread(Year, Value) %>%
  right_join(tibble(ItemCode = c("MK", "BV", "SH", "PT", "PK", "RI", "PL", "GH",
                                 "GB", "GI", "GU", "GP", "GM", "GE", "GA",
                                 "GY", "GR", "GV", "GC", "GG", "FO", "FC",
                                 "GF"),
                    ElementCode = c("CI", "LI", "LI", "LI", "LI", "AH", "AH",
                                    "Emissions_CO2Eq", "Emissions_CO2Eq", "Emissions_CO2Eq",
                                    "Emissions_CO2Eq", "Emissions_CO2Eq", "Emissions_CO2Eq",
                                    "Emissions_CO2Eq", "Emissions_CO2Eq", "Emissions_CO2Eq",
                                    "Emissions_CO2Eq", "Emissions_CO2Eq", "Emissions_CO2Eq",
                                    "Emissions_CO2Eq", "Emissions_CO2Eq", "Emissions_CO2Eq", 
                                    "Emissions_CO2Eq"))) %>% 
  select(-ItemCode, -ElementCode, -Unit) %>%
  arrange(AreaCode, d.source) %>% 
  select(AreaCode, ElementName, Domain,	ItemName,	d.source, `2016`, `2026`, everything()) %>%
  write_csv("output/SEA_total_prelim_adjusted_countries.csv")



# 
# write_csv(seaData, "output/SEA_data_prelim.csv")
# 
# write_csv(, "output/SEA_Adjusted_data_prelim.csv")  


# QA of some celeted numbers

gt %>%
  filter(AreaCode == "VNM") %>%
  plot_group(n_page = 12,
             groups_var = c("ElementCode"),
             plots_var = "ItemCode"  )

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

