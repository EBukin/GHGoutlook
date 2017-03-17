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

#' ## Initialising additional variables
#' Years of the FAOSTAT data that we are interested in:
# Years <- c(2000:2030)

#' 
#' # Implementing the process  
#' 
#' We perfrom all following calculations for one domain at the time. That allows 
#'    us to apply the same functions and approach to every domain maintaining 
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
#' ### Function for mapping faostat data  
#'  
#' The functoin `map_fs_data` maps FAOSTAT data to the Outlook items, elements and areas.
#' 
#' Besides simple mapping, we also adjust some Items/Elements in order to 
#'   maintain the same units in the FAOSTAT and Outlook data. Adjustment is 
#'   specified in the mapping table in the column `OutlookAdjustment`. 
#'   This is nececery for such cases like area harvested, whic is expresse 
#'   in Outlook in ha, but in FAOSTAT it is measured in 1000 ha.   
#'   
#' *  `fsdf` is the faostat dataset relevant to one domain only  
#' *  `itemsMT` is the Items mapping table  
#' *  `elementsMT` is the elemetns mapping table  
map_fs_data <-
  function(fsdf, itemMT = itemsMT, elementMT = elementsMT, fsYears = c(2000:2020)) {
    
    elementMTSpecific <- 
      elementMT %>% 
      filter(Domain %in% unique(fsdf$Domain),
             !is.na(ItemCode))
    
    elementMTGeneric <- 
      elementMT %>% 
      filter(Domain %in% unique(fsdf$Domain),
             is.na(ItemCode))
    
    # Mapping and aggregating areas
    fsol <-
      fsdf %>%
      select(Domain, AreaCode, ItemCode, ElementCode, Year, Value) %>%
      filter(Year %in% fsYears) %>%
      map_fs2ol() %>%
      agg_ol_regions(., regionVar = "OutlookSubRegion", allCountries = F) 
    # fsol <-
    #   fsol %>%
    #   bind_rows(
    #     agg_ol_regions(., regionVar = "OutlookBigRegion") %>%
    #       filter(!AreaCode %in% unique(fsol[["AreaCode"]]))
    #   )
    # fsol <-
    #   fsol %>%
    #   bind_rows(
    #     agg_ol_regions(., regionVar = "OutlookSuperRegion") %>%
    #       filter(!AreaCode %in% unique(fsol[["AreaCode"]]))
    #   )
    # fsol <-
    #   fsol %>%
    #   bind_rows(agg_ol_regions(., regionVar = "OutlookSEAsia") %>%
    #               filter(!AreaCode %in% unique(fsol[["AreaCode"]]))) 
    
    # Mapping and aggregating items
    fsol <-
      fsol %>%
      left_join(itemMT %>% select(ItemCode, OutlookItemCode, ItemCodeAggSign), "ItemCode") %>%
      filter(!is.na(OutlookItemCode)) %>%
      mutate(
        ItemCode = OutlookItemCode,
        Value = ifelse(
          !is.na(ItemCodeAggSign) &
            ItemCodeAggSign == "-",
          -Value,
          Value
        )
      ) %>%
      select(-OutlookItemCode,-ItemCodeAggSign) %>%
      group_by_(.dots = names(.)[!names(.) %in% c("Value")]) %>%
      summarise(Value = sum(Value, na.rm = TRUE)) %>%
      ungroup() 
    
    # Mapping and aggregating elements
    # There are specific items and generic items to map
    if(nrow(elementMTSpecific) > 0) {
      fsolSpecific <-
        fsol %>%
        left_join(elementMTSpecific, by = c("Domain", "ItemCode", "ElementCode")) %>%
        filter(!is.na(OutlookElementCode)) %>%
        mutate(
          ElementCode = OutlookElementCode,
          Value = ifelse(!is.na(OutlookAdjustment), Value * OutlookAdjustment, Value)
        ) %>%
        select(-OutlookElementCode, -OutlookAdjustment) %>%
        mutate(d.source = "Faostat")
    } else {fsolSpecific <- NULL}
    
    if(nrow(elementMTGeneric) > 0) {
      fsolGeneric <-
        fsol %>%
        left_join(elementMTGeneric %>% 
                    select(Domain, ElementCode, OutlookElementCode, OutlookAdjustment), 
                  by = c("Domain", "ElementCode")) %>%
        filter(!is.na(OutlookElementCode)) %>%
        mutate(
          ElementCode = OutlookElementCode,
          Value = ifelse(!is.na(OutlookAdjustment), Value * OutlookAdjustment, Value)
        ) %>%
        select(-OutlookElementCode, -OutlookAdjustment) %>%
        mutate(d.source = "Faostat")
    } else {fsolGeneric <- NULL}
    
    bind_rows(fsolGeneric, fsolSpecific)
  }

#' ### Functoin for subsetting outlook 
#'
#'The subsetting is made based on the list of items and elements used in one 
#'   FAOSTAT domain. In practice it subsets outlook data to the list
#'   of items and elements specified in one of the mapping tables.
subset_outlook <-
  function(oldf,
           fsdf = fsSubset,
           d.sourceName = "Outlook") {
    joinBy <- names(oldf)[names(oldf) %in% c("AreaCode", "ItemCode", "ElementCode", "Domain")]
    oldf %>%
      right_join(
        fsdf %>%
          select(AreaCode, ItemCode, ElementCode, Domain) %>%
          distinct(),
        joinBy
      ) %>%
      mutate(d.source = d.sourceName) %>%
      filter(!is.na(Value))
  }


#' ### Function for adjusting Outlook activities data to the FAOSTAT historical levels  
#'    
#' In order to reproduce Binding FAOSTAT and OUTLOOK data together and performing adjustments only 
#'      in the case if absolute gifference between FAOSTAT numbers an Outlook 
#'      numbers is greater than 5%. In addition we adjust data based on `nLag` years 
#'      average in the pre projection period
#'   
#'   Preparing Outlook data
adjust_outlook_activity <-
  function(olSubset,
           fsSub = fsSubset,
           nLag = 1,
           diffThreshold = 0) {
    # Formula for calculating lagged average differences
    lagged_dif <-
      str_c("(", str_c("lag(diff,", 1:nLag, ")", collapse = " + "), ") / ", nLag)
    
    activity <-
      
      # Binding Outlook and FAOSTAT data
      right_join(
        spread(fsSub, d.source, Value) %>% filter(Year < 2016),
        spread(olSubset, d.source, Value),
        by = c("AreaCode", "ItemCode", "ElementCode", "Domain", "Year")
      ) %>%
      filter(!is.na(Outlook)) %>%
      
      # Introducing grouping
      group_by_(.dots = names(.)[!names(.) %in% c("Domain", "Year", "Faostat", "Outlook")]) %>%
      mutate(diff = Outlook / Faostat)
    
    # defining starting years of differences projections
    diff_proj <-
      activity %>%
      select(Domain, AreaCode, ItemCode, ElementCode, Year, diff) %>%
      filter(is.na(diff)) %>%
      filter(Year == min(Year)) %>%
      select(-diff) %>%
      mutate(Proj = TRUE)
    
    # Adjusting data
    activity <-
      activity %>%
      left_join(diff_proj,
                by = c("Domain", "AreaCode", "ItemCode", "ElementCode", "Year")) %>%
      mutate(Proj = ifelse(!is.na(Proj), TRUE, FALSE)) %>%
      mutate_(.dots = setNames(str_c("ifelse(Proj,", lagged_dif, ", diff)"), "diff")) %>%
      
      # Adding variable which specifies if we need to adjust anything based on the
      #   Value of the difference compare to the diffThreshold
      mutate(
        Proj = ifelse(Proj & abs(diff - 1) > diffThreshold, TRUE, FALSE),
        Proj = any(Proj, na.rm = TRUE)
      ) %>%
      
      # Extrapolating last value of the difference
      fill(diff) %>%
      
      # Adjusting projections if neededs
      mutate(Outlook = if_else(Proj, Outlook / diff, Outlook)) %>%
      ungroup() %>%
      select(-diff,-Proj,-Faostat) %>%
      gather(d.source, Value, Outlook)
    
    # Returning output
    activity %>% 
      filter(!is.na(Value))
  }

#' ### Funciton for recalculating GHG emissions based on constant implied emissions factors
#' 
#' With the help of the functoin `reestimate_emissions` we use FAOSTAT data `fsSubset` 
#'    mapped and aggregated to the outlook items, areas and element for calculating GHG 
#'    implicit emissions factors and apply these factors to the activity data `outlookAct` 
#'    prepared from the outlook projections. Activity data could be adjusted and 
#'    not adjusted to the FAOSTAT historical data. This funcotin also uses 
#'    `emissionsMT` mapping table of domain specific elements and activity data.
#' 
reestimate_emissions <-
  function(outlookAct, fsSubset, emissionMT = emissionsMT) {
    # Subsetting emissions maping table to the list of items relevant
    #     to the domain
    emissionMTSubset <-
      emissionMT %>%
      filter(Domain %in% unique(fsSubset$Domain))
    
    # Preparing FAOSTAT data for calculating emissiong factors
    fsEF <-
      fsSubset %>%
      left_join(
        select(emissionMTSubset, ActivityElement) %>%
          distinct() %>%
          mutate(Element = "Activity"),
        by = c("ElementCode" = "ActivityElement")
      ) %>%
      mutate(ElementCode = ifelse(!is.na(Element), Element, ElementCode)) %>%
      select(-Element) %>%
      spread(ElementCode, Value)
    
    # Calculating Emissions factors to the FAOSTAT data rewriting `fsEF` object
    d_ply(emissionMTSubset,
          .(Domain, Emissions),
          function(x) {
            efExpr <- setNames(str_c(x$OutlookEmissions, " / Activity"),
                               str_c("EF_", x$OutlookEmissions))
            
            if (all(c("Activity", x$OutlookEmissions) %in% names(fsEF)))
              fsEF <<- mutate_(.data = fsEF, .dots = efExpr)
          })
    
    # Recalculating emissions based on outlook activity data and FAOSTAT emissions factors
    fsEF %>%
      filter(Year != 2030) %>%
      select_(.dots = c(
        "Domain",
        "AreaCode",
        "ItemCode",
        "Year",
        str_c("EF_", emissionMTSubset$OutlookEmissions)
      )) %>%
      
      # Expanding data to all years we need
      right_join(
        outlookAct %>%
          select(Domain, AreaCode, ItemCode, Year) %>%
          distinct(),
        by = c("Domain", "AreaCode", "ItemCode", "Year")
      ) %>%
      complete(Domain, AreaCode, ItemCode, Year) %>%
      gather(Element, EmissionsFactor, 5:length(.)) %>%
      
      # Adding activity data
      left_join(rename(outlookAct, Activity = Value),
                by = c("Domain", "AreaCode", "ItemCode", "Year")) %>%
      
      # Interpolating missing emissions factors repeating the last known value
      group_by(Domain, AreaCode, ItemCode, Element) %>%
      arrange(Domain, AreaCode, ItemCode, Element, Year) %>%
      fill(EmissionsFactor) %>%
      
      # Recalculating emisions
      ungroup() %>%
      mutate(Value = EmissionsFactor * Activity,
             ElementCode = str_sub(Element, 4)) %>%
      select(Domain, AreaCode, ItemCode, Year, ElementCode, d.source, Value) %>%
      filter(!is.na(Value)) %>% 
      
      # Adding activity data
      bind_rows(outlookAct) #%>%
      # bind_rows(convert_ghg)
  }

#' ### Funciton for estimaintg GHG for missing items
#' 
#' Function for estimating missing emissins data based on the share of item (s)
#'    in the total emissin of the domain. The funciton is performed on the domain
#'    specific basis withing the loop of emissins reproduction.  
#'  
#' Parameters are:  
#'    
#' `outlookEmissions`  is the dataframe with the emissions data estimated based on the outlook activity data  
#' `fsSubset`  Subset of the FAOSTAT data mapped to the outlook items and elements used for 
#' `itemMT`  items mapping table
#' `nLag`  number of years to use for average emissinos shares of missing items. 
#' 
estimate_missing_emissions <-
  function(outlookEmissions, fsSubset, itemMT = itemsMT, nLag = 1) {
    
    # Detecting not calculated items relevant to the domain
    missingItems <- 
      itemMT %>% 
      filter(OutlookItemCode %in% unique(fsSubset$ItemCode),
             !Exist) %>% 
      .[["OutlookItemCode"]] %>% 
      unique()
    
    # Calculaing share of emission for missing items in total emissins of the Domain
    if(length(missingItems) > 0) {
      EmissSharesMissingData <-
      ldply(missingItems,
            function(x) {
              
              # selectFrom <- c("Domain", "AreaCode", "ElementCode",  "Year", "d.source")
              
              # Using fsSubset data as a dasis data we aggregate all existing items
              # results <- 
                fsSubset %>%
                left_join(
                  itemMT %>%
                    select(OutlookItemCode, Exist) %>%
                    distinct(),
                  by = c("ItemCode" = "OutlookItemCode")
                ) %>%
                mutate(ItemCode = ifelse(Exist, "Other", ItemCode)) %>%
                filter(ItemCode %in% c(x, "Other")) %>% 
                group_by_(.dots = names(.)[!names(.) %in% c("Value")]) %>%
                summarise(Value = sum(Value)) %>%
                ungroup() %>%
                select(-Exist) %>%
                
                # For every non existing item (initialised using the loop)
                #     we calculate shares in emissions from other items.
                #     and retutrn data to the long format.
                spread(ItemCode, Value)  %>%
                mutate_(.dots = setNames(str_c(x, " / Other"), x)) %>%
                select(-Other) %>%
                filter_(.dots = str_c("!is.na(", x, ")")) %>% 
              
              # results %>%
                gather_(key_col = "ItemCode", 
                        value_col = "Share", 
                        gather_cols = x)#names(results)[!names(results) %in% selectFrom])
            }) %>%
      tbl_df() %>%
      
      # Filtering only relevant years and summarising shares based on the 
      #     number of years used as a lag. We use average share fo the lag
      #     If the lag provided is 1, last year share is used.
      filter(Year != 2030) %>%
      filter(Year %in% c((max(Year) - nLag + 1) : max(Year))) %>%
      group_by_(.dots = names(.)[!names(.) %in% c("Share", "Year")]) %>%
      summarise(Share = mean(Share)) %>% 
      ungroup() %>% 
      select(-d.source)
      
      output <- # Estimating emissing data for the missing items.
        outlookEmissions %>% 
        
        # Aggregating all not missing items into one to use it for estimating missing
        #     emissions
        filter(!ItemCode %in% missingItems) %>%
        group_by_(.dots = names(.)[!names(.) %in% c("Value", "ItemCode")]) %>%
        summarise(Value = sum(Value)) %>% 
        ungroup() %>% 
        
        # Joining emissions shares for the missing items 
        left_join(EmissSharesMissingData, by = c("Domain", "AreaCode", "ElementCode")) %>% 
        
        # Calculating emissins for the missing items and joinign test of the emissins data
        mutate(Value = Share * Value) %>% 
        filter(!is.na(Value)) %>% 
        select(-Share) %>% 
        bind_rows(outlookEmissions) #%>%
      # bind_rows(convert_ghg)
    } else {
      output <- # Estimating emissing data for the missing items.
        outlookEmissions
    }
    output
  }

#' Function for converting GHG to the GHG CO2 Equivalents
#' 
#' The funciton `convert_ghg` converts GHGs to the CO2 Equivalent
#' returning only GHG in CO2 Equivalent values.
convert_ghg <-
  function(df) {
    
    # Conversion
    convs <- 
      tibble(ElementCode = c("EM_N2O", "EM_CH4", "CO2"), 
             NewElementCode = c("EM_N2OEq", "EM_CH4Eq", "CO2"),
             ConversionFactor = c(310, 21, 1))
    
    # If number for GHG in CO2 Equivalent were there, remove it. 
    df <- 
      df %>% 
      filter(! ElementCode %in% convs$NewElementCode[1:2])
    
    # Converting GHG
    df %>% 
      filter(ElementCode %in% c("EM_CH4", "EM_N2O", "CO2")) %>% 
      left_join(convs, "ElementCode") %>% 
      mutate(ElementCode = NewElementCode,
             Value = Value * ConversionFactor) %>% 
      select(-NewElementCode, - ConversionFactor)%>%
      filter(!is.na(Value)) %>% 
      bind_rows(df)
  }


#' ### Funciton for aggregating data of one domain by GHG
#' 
#' The function `agg_ghg_domains` uses data on ghg emissions, and aggregates 
#' emissions total for a domain  
agg_ghg_domains <-
  function(df, emissionMT = emissionsMT, totDomainName = "GT") {
    # List of aggregatable emissions
    allGHG <-
      c("EM_CH4Eq", "EM_N2OEq", "CO2", "EM_CH4", "EM_N2O")
    
    # Aggregaing all available gases
    df %>%
      filter(ElementCode %in% allGHG) %>%
      group_by_(.dots = names(.)[!names(.) %in% c("Value", "ItemCode")]) %>%
      summarise(Value = sum(Value)) %>%
      ungroup() %>%
      mutate(ItemCode = Domain) %>% 
      mutate(Domain = totDomainName)
  }

agg_total_emissions <-
  function(df, emissionMT = emissionsMT) {
    
    # List of aggregatable emissions
    emissions <-
      tibble(ElementCode = c("EM_CH4Eq", "EM_N2OEq", "CO2"), 
             NewElementCome = "Emissions_CO2Eq")
    
    # Aggregaing all available gases
    df %>%
      filter(ElementCode %in% emissions$ElementCode) %>%
      left_join(emissions, "ElementCode") %>%
      mutate(ElementCode = NewElementCome) %>% 
      select(-NewElementCome) %>% 
      group_by_(.dots = names(.)[!names(.) %in% c("Value")]) %>%
      summarise(Value = sum(Value)) %>%
      ungroup() %>% 
      bind_rows(df)
  }

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

# Funciton for joining names
join_names <- 
  function(df, ItemNamesMT = "mappingTables/itemsNames.csv") {
    mt <- read_csv(ItemNamesMT)
    df %>% 
      left_join(mt, "ItemCode")
  }




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

gtpart <- 
  bind_rows(list(gm, ge, gu, gp, gr)) %>% 
  agg_ghg_domains %>% 
  agg_total_emissions

gt <- 
  outlook_emissions(fs, 
                    gtpart %>% filter(d.source == "Outlook"), 
                    DomainName = "GT", useActivity = FALSE) %>% 
  join_names()


gt %>% 
  filter(AreaCode == "OutlookSEAsia", ElementCode == "Emissions_CO2Eq") %>% 
  plot_group(n_page = 12,
             groups_var = c("ElementCode"),
             plots_var = "ItemCode"  )

gtt %>% 
  filter(AreaCode == "OutlookSEAsia", ElementCode == "Emissions_CO2Eq") %>% 
  plot_group(n_page = 6,
             groups_var = c("ElementCode"),
             plots_var = "ItemCode"  )




# QUALITY ASSURANCE
plot_group(gm ,
           n_page = 6,
           groups_var = c("ElementCode"),
           plots_var = "ItemCode"
)








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

