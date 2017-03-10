

# Reproducing GHG data based on projections activity numbers


# Setup -------------------------------------------------------------------

# Installing packages
packs <- c("plyr", "tidyverse", "dplyr", "tidyr","readxl", "stringr", 
           "gridExtra", "grid", "ggplot2", "ggthemes", "scales", "devtools")
lapply(packs[!packs %in% installed.packages()[,1]], 
       install.packages,
       dependencies = TRUE)
lapply(packs, require, character.only = TRUE)

options(scipen=999)

# Loading locally developed functions
l_ply(str_c("R/", list.files("R/", pattern="*.R")), source)

# Loading data ------------------------------------------------------------

# Outlook data
olFile <- "C:/Users/Bukin/OneDrive - Food and Agriculture Organization/outlookGHG/data/base17.csv"
if(!file.exists(olFile)) olFile <- "data/base17.csv"
ol <- load_troll_csv(olFile, d.source = "") %>% 
  select(AreaCode, ItemCode, ElementCode, Year, Value)

# Faostat data file
load("data/all_fs_emissions.Rdata")
# fsFile <- "data/Emissions_Agriculture_Rice_Cultivation_E_All_Data_(Norm).csv"
# fs <- 
#   read.fs.bulk(fsFile) %>% 
#   select(-Flag)

# Loaid conversion Mapping 
conversion <- read_csv("mappingTables/conversions.csv") %>% filter(Domain == "GE")

# Analysis ---------------------------------------------------------------

Years = c(2000:2030)

# Exporting items and elements
# els <- fs %>%
#   select(ElementCode, ElementName, Unit) %>% 
#   distinct()
# 
# its <- fs %>%
#   select(ItemCode, ItemName) %>% 
#   distinct()
# 
# fs <- 
#   fs %>% 
#   select(AreaCode, ItemCode, ElementCode, Year, Value)

# Important components ----------------------------------------------------

# Function for converting emissiong in outlok for one item in the domain at the time
convert_item_emissions <- 
  function(fs, ol, convLine, its = its, els = els) {
    
    # convLine <- conversion[1,]
    
    Domains <- convLine$Domain
    Item <- convLine$Item
    OutlookItem <- convLine$OutlookItem
    Activity <- convLine$Activity
    OutlookActivity <- convLine$OutlookActivity
    OutlookActivityAdjustment <- convLine$OutlookActivityAdjustment
    Emissions <- convLine$Emissions
    OutlookEmissions <- convLine$OutlookEmissions
    nLags <- convLine$EFLag
    GHG <- convLine$OutlookEmissions
    
    PresigionCoef <- 1000000
    
    # Items that should be aggregated for the OUTLOOK regions
    agItems <- c(Item)
    agElement <- c(Activity, Emissions)
    
    # Step 1. Agregate FAOSTAT data to the OUTLOOK SuibRegions  ----------------
    
    # Reference tables
    its <- its %>% filter(Domain == Domains)
    els <- els %>% filter(Domain == Domains)
    
    # Step 1. Agregate FAOSTAT data to the OUTLOOK SuibRegions, which includes level 
    #           of SouthEastAsian Countries.
    #         Define Elements nd Items that could be aggregated.
    # Agg FS data to outlook areas
    fsOlAgg <-
      fs %>%
      filter(Domain == Domains,
             ItemCode %in% agItems, 
             ElementCode %in% agElement, 
             Year %in% Years) %>%
      
      # Mapping to the faostat outlook
      map_fs2ol() %>%
      agg_ol_regions(., regionVar = "OutlookSubRegion", allCountries = F)
    
    fsOlAgg <-
      fsOlAgg %>%
      bind_rows(agg_ol_regions(., regionVar = "OutlookBigRegion") %>%
                  filter(!AreaCode %in% unique(fsOlAgg[["AreaCode"]])))
    
    fsOlAgg <-
      fsOlAgg %>%
      bind_rows(
        agg_ol_regions(., regionVar = "OutlookSuperRegion") %>%
          filter(!AreaCode %in% unique(fsOlAgg[["AreaCode"]]))
      ) 
    
    fsOlAgg <-
      fsOlAgg %>%
      bind_rows(
        agg_ol_regions(., regionVar = "OutlookSEAsia") %>%
          filter(!AreaCode %in% unique(fsOlAgg[["AreaCode"]]))
      ) %>% 
      select(Domain, AreaCode, ItemCode, ElementCode, Year, Value)
    
    
    # Step 2. Compaintg Activity data ----------------------------------------
    
    # Select only important activity data from outlook
    ouActivityData <- 
      ol  %>% 
      filter(ItemCode == OutlookItem, ElementCode == OutlookActivity) %>% 
      mutate(ItemCode = ifelse(ItemCode == OutlookItem, Item, ItemCode),
             ElementCode = ifelse(ElementCode == OutlookActivity, Activity, ElementCode)) %>% 
      mutate(Value = Value * OutlookActivityAdjustment) %>% 
      distinct() 
    
    
    # Convert FAOSTAT activity data to the OUTLOOK form
    fsActivityData <- 
      fsOlAgg %>% 
      # mutate(ItemCode = ifelse(ItemCode == 27, "RI", ItemCode)) %>%
      filter(ItemCode == Item, ElementCode == Activity) 
    
    
    # Visual assurance of quality.
    # combine_plot_data(ouActivityData%>% 
    #                     left_join(its, by = "ItemCode") %>% 
    #                     left_join(els, by = c("ElementCode", "Domain")),
    #                   fsActivityData %>% 
    #                     left_join(its, by = c("ItemCode", "Domain")) %>% 
    #                     left_join(els, by = c("ElementCode", "Domain")))
    
    # Step 3. Recalculating emissions factors ---------------------------------
    
    # Recalculating Emissions Intencities
    EF <-
      fsOlAgg %>% 
      spread(ElementCode, Value) %>% 
      # mutate(`EFCH4` = `72255` / `5312`) %>% 
      mutate_(.dots = setNames(str_c("`", Emissions, "` * ", PresigionCoef, " / `", Activity, "`"), "EmFactor")) %>% 
      select(AreaCode, ItemCode, Year, EmFactor) %>% 
      filter(Year < 2015)
    
    # Step 4. Recalculating Emissions -----------------------------------------
    
    ouEmissions <-
      ouActivityData %>% 
      spread(ElementCode, Value) %>% 
      left_join(EF, by = c("AreaCode", "ItemCode", "Year")) %>% 
      group_by(AreaCode, ItemCode) %>% 
      arrange(AreaCode, ItemCode, Year) %>% 
      # filter(Year %in% c(2010:2018)) %>% 
      
      # FIlling the gap in time series for Emissions Factors
      mutate_(.dots = setNames( str_c("!is.na(EmFactor) & (", str_c( str_c("is.na(lead(EmFactor, n = ",seq(1,nLags),"))"), collapse = " | "), ")"), "avg")) %>% 
      mutate(avg = ifelse(avg, EmFactor, NA),
             avg = mean(avg, na.rm = TRUE),
             EmFactor = ifelse(is.na(EmFactor), avg, EmFactor)) %>% 
      select(-avg) %>% 
      
      # Not used simple fill anymore
      # fill(EmFactor, .direction = "down") %>% 
      
      mutate_(.dots = setNames(str_c("`", Activity, "` * EmFactor / ", PresigionCoef), Emissions)) %>% 
      gather(ElementCode, Value, 4:length(.)) %>% 
      filter(ElementCode != "EmFactor") %>% 
      mutate(ElementCode = as.integer(ElementCode)) %>% 
      ungroup() %>% 
      left_join(its, by = c("ItemCode")) %>% 
      left_join(els, by = c("ElementCode", "Domain")) %>% 
      
      # aggregating Southeast asia
      bind_rows(agg_ol_regions(., regionVar = "OutlookSEAsia")) 
    
    # Step 5 Quality assurance of conversion ----------------------------------
    bind_rows(
      ouEmissions %>% mutate(d.source = "OUTLOOK"),
      Faostat = fsOlAgg  %>%
        left_join(its, by = c("ItemCode", "Domain")) %>%
        left_join(els, by = c("ElementCode", "Domain")) %>%
        mutate(d.source = "FAOSTAT")
    ) %>%
      return()
  }


# function for converting items specific emissions on one item basis.

result <- 
  ddply(conversion, 
      .( Domain, Item, Activity, Emissions, GHG),
      function(x) {
        convert_item_emissions(fs = fs, ol = ol, convLine = x, its = its, els = els)
      }, 
      .progress = "text") %>% 
  tbl_df()%>% 
  distinct()

# 
plot_pages_into_pdf(
  df = 
    result %>% 
    filter(AreaCode %in% c("WLD", "RestOfTheWorld", "OutlookSEAsia", 
                           "CHN", "KHM", "IDN", "LAO", "MYS", "MMR", 
                           "PHL", "THA", "VNM")) %>% 
    distinct(),
  n_page = 12, 
  write_pdf = TRUE,
  output_path = "./output",
  file_heights = 10,
  file_width = 18,
  files_var = "Domain", 
  groups_var = c("ItemCode", "ElementCode"), 
  plots_var = "AreaCode")


# combine_plot_data(ouEmissions,
#                   fsOlAgg  %>% 
#                     left_join(its, by = c("ItemCode", "Domain")) %>% 
#                     left_join(els, by = c("ElementCode", "Domain")))
