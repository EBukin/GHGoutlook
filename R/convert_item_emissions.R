# # Function for converting emissiong in outlok for one item in the domain at the time
# convert_item_emissions <- 
#   function(fs, ol, convLine, its = its, els = els) {
#     
#     # convLine <- conversion[1,]
#     
#     Domains <- convLine$Domain
#     Item <- convLine$Item
#     OutlookItem <- convLine$OutlookItem
#     Activity <- convLine$Activity
#     OutlookActivity <- convLine$OutlookActivity
#     OutlookActivityAdjustment <- convLine$OutlookActivityAdjustment
#     Emissions <- convLine$Emissions
#     OutlookEmissions <- convLine$OutlookEmissions
#     nLags <- convLine$EFLag
#     GHG <- convLine$OutlookEmissions
#     
#     PresigionCoef <- 1000000
#     
#     # Items that should be aggregated for the OUTLOOK regions
#     agItems <- c(Item)
#     agElement <- c(Activity, Emissions)
#     
#     # Step 1. Agregate FAOSTAT data to the OUTLOOK SuibRegions  ----------------
#     
#     # Reference tables
#     its <- its %>% filter(Domain == Domains)
#     els <- els %>% filter(Domain == Domains)
#     
#     # Step 1. Agregate FAOSTAT data to the OUTLOOK SuibRegions, which includes level 
#     #           of SouthEastAsian Countries.
#     #         Define Elements nd Items that could be aggregated.
#     # Agg FS data to outlook areas
#     fsOlAgg <-
#       fs %>%
#       filter(Domain == Domains,
#              ItemCode %in% agItems, 
#              ElementCode %in% agElement, 
#              Year %in% Years) %>%
#       
#       # Mapping to the faostat outlook
#       map_fs2ol() %>%
#       agg_ol_regions(., regionVar = "OutlookSubRegion", allCountries = F)
#     
#     fsOlAgg <-
#       fsOlAgg %>%
#       bind_rows(agg_ol_regions(., regionVar = "OutlookBigRegion") %>%
#                   filter(!AreaCode %in% unique(fsOlAgg[["AreaCode"]])))
#     
#     fsOlAgg <-
#       fsOlAgg %>%
#       bind_rows(
#         agg_ol_regions(., regionVar = "OutlookSuperRegion") %>%
#           filter(!AreaCode %in% unique(fsOlAgg[["AreaCode"]]))
#       ) 
#     
#     fsOlAgg <-
#       fsOlAgg %>%
#       bind_rows(
#         agg_ol_regions(., regionVar = "OutlookSEAsia") %>%
#           filter(!AreaCode %in% unique(fsOlAgg[["AreaCode"]]))
#       ) %>% 
#       select(Domain, AreaCode, ItemCode, ElementCode, Year, Value)
#     
#     
#     # Step 2. Compaintg Activity data ----------------------------------------
#     
#     # Select only important activity data from outlook
#     ouActivityData <- 
#       ol  %>% 
#       filter(ItemCode == OutlookItem, ElementCode == OutlookActivity) %>% 
#       mutate(ItemCode = ifelse(ItemCode == OutlookItem, Item, ItemCode),
#              ElementCode = ifelse(ElementCode == OutlookActivity, Activity, ElementCode)) %>% 
#       mutate(Value = Value * OutlookActivityAdjustment) %>% 
#       distinct() 
#     
#     
#     # Convert FAOSTAT activity data to the OUTLOOK form
#     fsActivityData <- 
#       fsOlAgg %>% 
#       # mutate(ItemCode = ifelse(ItemCode == 27, "RI", ItemCode)) %>%
#       filter(ItemCode == Item, ElementCode == Activity) 
#     
#     
#     # Visual assurance of quality.
#     # combine_plot_data(ouActivityData%>% 
#     #                     left_join(its, by = "ItemCode") %>% 
#     #                     left_join(els, by = c("ElementCode", "Domain")),
#     #                   fsActivityData %>% 
#     #                     left_join(its, by = c("ItemCode", "Domain")) %>% 
#     #                     left_join(els, by = c("ElementCode", "Domain")))
#     
#     # Step 3. Recalculating emissions factors ---------------------------------
#     
#     # Recalculating Emissions Intencities
#     EF <-
#       fsOlAgg %>% 
#       spread(ElementCode, Value) %>% 
#       # mutate(`EFCH4` = `72255` / `5312`) %>% 
#       mutate_(.dots = setNames(str_c("`", Emissions, "` * ", PresigionCoef, " / `", Activity, "`"), "EmFactor")) %>% 
#       select(AreaCode, ItemCode, Year, EmFactor) %>% 
#       filter(Year < 2015)
#     
#     # Step 4. Recalculating Emissions -----------------------------------------
#     
#     ouEmissions <-
#       ouActivityData %>% 
#       spread(ElementCode, Value) %>% 
#       left_join(EF, by = c("AreaCode", "ItemCode", "Year")) %>% 
#       group_by(AreaCode, ItemCode) %>% 
#       arrange(AreaCode, ItemCode, Year) %>% 
#       # filter(Year %in% c(2010:2018)) %>% 
#       
#       # FIlling the gap in time series for Emissions Factors
#       mutate_(.dots = setNames( str_c("!is.na(EmFactor) & (", str_c( str_c("is.na(lead(EmFactor, n = ",seq(1,nLags),"))"), collapse = " | "), ")"), "avg")) %>% 
#       mutate(avg = ifelse(avg, EmFactor, NA),
#              avg = mean(avg, na.rm = TRUE),
#              EmFactor = ifelse(is.na(EmFactor), avg, EmFactor)) %>% 
#       select(-avg) %>% 
#       
#       # Not used simple fill anymore
#       # fill(EmFactor, .direction = "down") %>% 
#       
#       mutate_(.dots = setNames(str_c("`", Activity, "` * EmFactor / ", PresigionCoef), Emissions)) %>% 
#       gather(ElementCode, Value, 4:length(.)) %>% 
#       filter(ElementCode != "EmFactor") %>% 
#       mutate(ElementCode = as.integer(ElementCode)) %>% 
#       ungroup() %>% 
#       left_join(its, by = c("ItemCode")) %>% 
#       left_join(els, by = c("ElementCode", "Domain")) %>% 
#       
#       # aggregating Southeast asia
#       bind_rows(agg_ol_regions(., regionVar = "OutlookSEAsia")) 
#     
#     # Step 5 Quality assurance of conversion ----------------------------------
#     bind_rows(
#       ouEmissions %>% mutate(d.source = "OUTLOOK"),
#       Faostat = fsOlAgg  %>%
#         left_join(its, by = c("ItemCode", "Domain")) %>%
#         left_join(els, by = c("ElementCode", "Domain")) %>%
#         mutate(d.source = "FAOSTAT")
#     ) %>%
#       return()
#   }
