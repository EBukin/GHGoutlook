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
