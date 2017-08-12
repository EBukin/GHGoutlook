#
# Function for mapping and aggregating FS Items to the outlook based on prepared mapping table
map_fs_items <-
  function(df, mappingTable = "mappingTables/fs_outlook_items_mt.csv") {
    require("tidyverse")
    
    itemsMT <- read_csv(
      itemsMTFile,
      col_types = cols(
        ItemCode = col_integer(),
        OutlookItemCode = col_character(),
        ItemCodeAggSign = col_character()
      )
    ) %>%
      distinct()
    
    df %>%
      left_join(itemsMT, "ItemCode") %>%
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
      ungroup() %>% 
      return()
    
  }