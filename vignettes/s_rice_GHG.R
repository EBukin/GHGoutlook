

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
ol <- load_troll_csv(olFile, d.source = "") %>% 
  select(AreaCode, ItemCode, ElementCode, Year, Value)

# Faostat data file
fsFile <- "data/Emissions_Agriculture_Rice_Cultivation_E_All_Data_(Norm).csv"
fs <- 
  read.fs.bulk(fsFile) %>% 
  select(-Flag)

# Analysis ---------------------------------------------------------------

Years = c(2000:2030)

# Exporting items and elements
els <- fs %>%
  select(ElementCode, ElementName, Unit) %>% 
  distinct()

its <- fs %>%
  select(ItemCode, ItemName) %>% 
  distinct()

fs <- 
  fs %>% 
  select(AreaCode, ItemCode, ElementCode, Year, Value)

# # Analyzing rice emissions
# fs %>% 
#   select(-ElementName, -ItemName, -Unit) %>% 
#   spread(ElementCode, Value) %>% 
#   mutate(EmissionsActivity = `72255` / `5312` * 100000) %>% 
#   group_by(AreaCode, AreaName) %>% 
#   filter(AreaCode == 96, `5312` != 0) %>% 
#   summarise(meanEF = mean(`72245`, na.rm = TRUE),
#             mean = mean(EmissionsActivity, na.rm = TRUE), 
#             var = sd(EmissionsActivity, na.rm = TRUE)^2) 


# Step 1. Agregate FAOSTAT data to the OUTLOOK SuibRegions  ----------------

# Step 1. Agregate FAOSTAT data to the OUTLOOK SuibRegions, which includes level 
#           of SouthEastAsian Countries.
#         Define Elements nd Items that could be aggregated.
agItems <- c(27)
agElement <- c(5312, 72255, 72315)

# Agg FS data to outlook areas
fsOlAgg <-
  fs %>%
  filter(ItemCode %in% agItems, 
         ElementCode %in% agElement, 
         Year %in% Years) %>%
  map_fs2ol() %>%
  agg_ol_regions(., regionVar = "OutlookSubRegion")

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

# Remapping Items
fsOlAgg <- 
  fsOlAgg %>% 
  mutate(ItemCode = ifelse(ItemCode == 27, "RI", ItemCode))

# Step 2. Compaintg Activity data ----------------------------------------

# Select only important activity data from outlook
ouActivityData <- 
  ol  %>% 
  filter(ItemCode == "RI", ElementCode == "AH") %>% distinct()

# Convert FAOSTAT activity data to the OUTLOOK form
fsActivityData <- 
  fsOlAgg %>% 
  filter(ElementCode == 5312) %>% 
  mutate(ElementCode = "AH", Value = Value / 1000) 

# Visual assurance of quality.
combine_plot_data(ouActivityData,fsActivityData)

# Step 3. Recalculating emissions factors ---------------------------------

# Recalculating Emissions Intencities
EFs <-
  fsOlAgg %>% 
  # slice(c(505, 11773))
  spread(ElementCode, Value) %>% 
  mutate(`EFCH4` = `72255` / `5312`) %>% 
  select(AreaCode, ItemCode, Year, EFCH4) %>% 
  filter(Year < 2015)

# Remapping Items
EFs <- 
  EFs %>% 
  mutate(ItemCode = ifelse(ItemCode == 27, "RI", ItemCode))

# Step 4. Recalculating Emissions -----------------------------------------

ouEmissions <- 
  ouActivityData %>% 
  spread(ElementCode, Value) %>% 
  left_join(EFs, by = c("AreaCode", "ItemCode", "Year")) %>% 
  group_by(AreaCode, ItemCode) %>% 
  arrange(Year) %>% 
  # mutate(filter = is.na(EFCH4) | lead(is.na(EFCH4)) | lead(is.na(EFCH4), n = 2)) %>% 
  fill(EFCH4, .direction = "down") %>% 
  mutate(EMCH4 =  AH * EFCH4 * 1000) %>% 
  gather(ElementCode, Value, 4:length(.)) %>% 
  filter(ElementCode == "EMCH4") %>% 
  ungroup()



# Step 4 Quality assurance of conversion ----------------------------------

combine_plot_data(ouEmissions,
                  fsOlAgg %>%  
                    filter(ElementCode == 72255) %>% 
                    mutate(ElementCode = "EMCH4") )








# We can use element 72245 Implied emissins factor for projecting emissions.


fs %>% 
  select(-ElementName, -ItemName, -Unit) %>% 
  spread(ElementCode, Value) %>% 
  filter(AreaCode == 137) %>% View()
