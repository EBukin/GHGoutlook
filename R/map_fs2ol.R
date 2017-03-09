#
# Function for mapping FS countries to the outlook countries based on prepared mapping table
map_fs2ol <- 
  function(df, allCountries = TRUE, mappingTable = "mappingTables/faostat_areas_outlook_areas.csv") {
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
      select(AreaCode, OutlookAreaCode) %>% 
      distinct()
    
    df %>% 
      left_join(faoOutlookMT, "AreaCode") %>% 
      filter(!is.na(OutlookAreaCode)) %>% 
      mutate(AreaCode = OutlookAreaCode) %>% 
      select(-OutlookAreaCode) %>% 
      group_by_(.dots = names(.)[!names(.) %in% c("Value")]) %>% 
      summarise(Value = sum(Value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      return()
  }
