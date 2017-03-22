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
