# combining and plotting data on important countries
combine_plot_data <-
  function(outlook,
           faostat,
           areas = c("WLD", "RestOfTheWorld", "OutlookSEAsia", "CHN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "THA", "VNM"),
           n_page = 12) {
    dfp <-
      outlook %>%
      filter(AreaCode %in% unique(faostat$AreaCode)) %>%
      mutate(d.source = "Outlook") %>%
      bind_rows(faostat %>%
                  mutate(d.source = "Faostat"))
    
    if (any(!is.na(areas))) {
      dfp <-
        dfp %>%
        filter(AreaCode %in% areas)
    }
    
    plot_group(dfp,
               n_page = n_page,
               groups_var = c("ElementCode", "ItemCode"),
               plots_var = "AreaCode")
    
  }

