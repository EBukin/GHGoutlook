agg_all_ol_regions <- 
  function(bindedData) {
  # Aggregating big five
  bindedData <-
    bindedData %>%
    filter(AreaCode != "WLD") %>% 
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
  bindedData
  }
