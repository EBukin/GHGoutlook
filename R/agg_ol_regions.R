
# Function for aggregating outlook countries to the regions specified in the variable "regionVar".
agg_ol_regions <- 
  function(df, 
           allCountries = TRUE, 
           regionVar = "OutlookSubRegion",
           mappingTable = "mappingTables/faostat_areas_outlook_areas.csv") {
    require("tidyverse")
    
    if(!allCountries) {
      filterExpr <- 'OutlookStatus == "Active"'
    } else {
      filterExpr <- 'OutlookStatus %in% c("Active", "Commented")'
    }
    
    faoOutlookMT <- 
      read_csv(mappingTable, 
               col_types = cols(
                 AreaCode = col_integer(),
                 AreaName = col_character(),
                 OutlookAreaCode = col_character(),
                 OutlookAreaName = col_character(),
                 OutlookStatus = col_character(),
                 OutlookSubRegion = col_character(),
                 OutlookBigRegion = col_character(),
                 OutlookSuperRegion = col_character()
               )) %>%
      filter(!is.na(OutlookStatus)) %>% 
      filter_(.dots = filterExpr) %>% 
      select_(.dots = c("OutlookAreaCode", regionVar)) %>% 
      distinct() # Important to distinct here
    
    df %>% 
      left_join(faoOutlookMT, c("AreaCode" = "OutlookAreaCode")) %>% 
      filter_(.dots = str_c("!is.na(", regionVar, ")")) %>% 
      mutate_(.dots = setNames(regionVar, "AreaCode")) %>% 
      select_(.dots = str_c("-", regionVar)) %>% 
      group_by_(.dots = names(.)[!names(.) %in% c("Value")]) %>% 
      summarise(Value = sum(Value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      return()
    
  }