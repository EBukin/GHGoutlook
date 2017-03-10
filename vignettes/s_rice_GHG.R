

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
conversion <- read_csv("mappingTables/conversions.csv")

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
  file_heights = 9,
  file_width = 15,
  files_var = "Domain", 
  groups_var = c("ItemCode", "ElementCode"), 
  plots_var = "AreaCode")


# combine_plot_data(ouEmissions,
#                   fsOlAgg  %>% 
#                     left_join(its, by = c("ItemCode", "Domain")) %>% 
#                     left_join(els, by = c("ElementCode", "Domain")))
