# Funciton for joining names
join_names <- 
  function(df, 
           ItemNamesMT = "mappingTables/itemsNames.csv", 
           ElementNamesMT = "mappingTables/elementsNames.csv") {
    ItMT <- read_csv(ItemNamesMT)
    ElMT <- read_csv(ElementNamesMT)
    df %>% 
      left_join(ItMT, "ItemCode") %>% 
      left_join(ElMT, "ElementCode")
  }
