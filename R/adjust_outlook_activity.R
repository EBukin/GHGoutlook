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
