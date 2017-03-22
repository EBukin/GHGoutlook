#' ### Funciton for estimaintg GHG for missing items
#' 
#' Function for estimating missing emissins data based on the share of item (s)
#'    in the total emissin of the domain. The funciton is performed on the domain
#'    specific basis withing the loop of emissins reproduction.  
#'  
#' Parameters are:  
#'    
#' `outlookEmissions`  is the dataframe with the emissions data estimated based on the outlook activity data  
#' `fsSubset`  Subset of the FAOSTAT data mapped to the outlook items and elements used for 
#' `itemMT`  items mapping table
#' `nLag`  number of years to use for average emissinos shares of missing items. 
#' 
estimate_missing_emissions <-
  function(outlookEmissions, fsSubset, itemMT = itemsMT, nLag = 1) {
    
    # Detecting not calculated items relevant to the domain
    missingItems <- 
      itemMT %>% 
      filter(OutlookItemCode %in% unique(fsSubset$ItemCode),
             !Exist) %>% 
      .[["OutlookItemCode"]] %>% 
      unique()
    
    # Calculaing share of emission for missing items in total emissins of the Domain
    if(length(missingItems) > 0) {
      EmissSharesMissingData <-
        ldply(missingItems,
              function(x) {
                
                # selectFrom <- c("Domain", "AreaCode", "ElementCode",  "Year", "d.source")
                
                # Using fsSubset data as a dasis data we aggregate all existing items
                # results <- 
                fsSubset %>%
                  left_join(
                    itemMT %>%
                      select(OutlookItemCode, Exist) %>%
                      distinct(),
                    by = c("ItemCode" = "OutlookItemCode")
                  ) %>%
                  mutate(ItemCode = ifelse(Exist, "Other", ItemCode)) %>%
                  filter(ItemCode %in% c(x, "Other")) %>% 
                  group_by_(.dots = names(.)[!names(.) %in% c("Value")]) %>%
                  summarise(Value = sum(Value)) %>%
                  ungroup() %>%
                  select(-Exist) %>%
                  
                  # For every non existing item (initialised using the loop)
                  #     we calculate shares in emissions from other items.
                  #     and retutrn data to the long format.
                  spread(ItemCode, Value)  %>%
                  mutate_(.dots = setNames(str_c(x, " / Other"), x)) %>%
                  select(-Other) %>%
                  filter_(.dots = str_c("!is.na(", x, ")")) %>% 
                  
                  # results %>%
                  gather_(key_col = "ItemCode", 
                          value_col = "Share", 
                          gather_cols = x)#names(results)[!names(results) %in% selectFrom])
              }) %>%
        tbl_df() %>%
        
        # Filtering only relevant years and summarising shares based on the 
        #     number of years used as a lag. We use average share fo the lag
        #     If the lag provided is 1, last year share is used.
        filter(Year != 2030) %>%
        filter(Year %in% c((max(Year) - nLag + 1) : max(Year))) %>%
        group_by_(.dots = names(.)[!names(.) %in% c("Share", "Year")]) %>%
        summarise(Share = mean(Share)) %>% 
        ungroup() %>% 
        select(-d.source)
      
      output <- # Estimating emissing data for the missing items.
        outlookEmissions %>% 
        
        # Aggregating all not missing items into one to use it for estimating missing
        #     emissions
        filter(!ItemCode %in% missingItems) %>%
        group_by_(.dots = names(.)[!names(.) %in% c("Value", "ItemCode")]) %>%
        summarise(Value = sum(Value)) %>% 
        ungroup() %>% 
        
        # Joining emissions shares for the missing items 
        left_join(EmissSharesMissingData, by = c("Domain", "AreaCode", "ElementCode")) %>% 
        
        # Calculating emissins for the missing items and joinign test of the emissins data
        mutate(Value = Share * Value) %>% 
        filter(!is.na(Value)) %>% 
        select(-Share) %>% 
        bind_rows(outlookEmissions) #%>%
      # bind_rows(convert_ghg)
    } else {
      output <- # Estimating emissing data for the missing items.
        outlookEmissions
    }
    output
  }
