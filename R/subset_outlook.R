#' ### Functoin for subsetting outlook 
#'
#'The subsetting is made based on the list of items and elements used in one 
#'   FAOSTAT domain. In practice it subsets outlook data to the list
#'   of items and elements specified in one of the mapping tables.
subset_outlook <-
  function(oldf,
           fsdf = fsSubset,
           d.sourceName = "Outlook") {
    joinBy <- names(oldf)[names(oldf) %in% c("AreaCode", "ItemCode", "ElementCode", "Domain")]
    oldf %>%
      right_join(
        fsdf %>%
          select(AreaCode, ItemCode, ElementCode, Domain) %>%
          distinct(),
        joinBy
      ) %>%
      mutate(d.source = d.sourceName) %>%
      filter(!is.na(Value))
  }
