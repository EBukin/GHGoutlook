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
    
    convs <- convs %>% filter(ElementCode %in% unique(df$ElementCode))
    
    if(nrow(convs) > 0) {
      # If number for GHG in CO2 Equivalent were there, remove it. 
      df <- 
        df %>% 
        filter(! ElementCode %in% convs$NewElementCode[1:2])
      
      # Converting GHG
      df <- 
        df %>% 
        filter(ElementCode %in% c("EM_CH4", "EM_N2O", "CO2")) %>% 
        left_join(convs, "ElementCode") %>% 
        mutate(ElementCode = NewElementCode,
               Value = Value * ConversionFactor) %>% 
        select(-NewElementCode, - ConversionFactor)%>%
        filter(!is.na(Value)) %>% 
        bind_rows(df)
    }
    
    return(df)
    
  }
