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