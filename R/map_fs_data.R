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
