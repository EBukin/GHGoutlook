

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