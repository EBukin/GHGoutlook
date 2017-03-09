plot_group <- function(df,
                       n_page = 1,
                       groups_var = NA,
                       plots_var = NA,
                       timeseries_var = "d.source",
                       all_vars = c("AreaCode", "ItemCode", "ElementCode", "d.source")) {
  
  # Everything field for files by
  df <- df %>% mutate(All = 1)
  
  # Checking if any grouping variable is provided and seting groups to all if not.
  if (any(is.na(groups_var))) groups_var <- "All"
  
  # Checking if the time series variable is provided and setting it up if not.
  if (any(is.na(timeseries_var))) {
    timeseries_var <- "d.source"
  }
  
  if (!any(timeseries_var %in% names(df))) {
    df[, timeseries_var] <- ""
  }
  
  # Check if the data has a Flag and setting one general fulgs structure
  if ("Flag" %in% names(df)) {
    flagList <- with(df %>%
                       select(Flag) %>%
                       distinct() %>%
                       mutate(shape = c(1:nrow(.))),
                     setNames(shape, Flag))
  } else {
    flagList <- NA
  }
  
  # Selecting legend
  if (any(timeseries_var %in% names(df))) {
    legend <- with(df %>%
                     select_(.dots = timeseries_var) %>%
                     distinct() %>%
                     mutate(shape = c(1:nrow(.))),
                     setNames(shape, eval(parse(text = timeseries_var))))
  } else {
    legend <- NA
  }
  
    
  # Plotting groups
  d_ply(
    df,
    groups_var,
    plot_subgroup,
    n_page,
    plots_var,
    timeseries_var,
    all_vars,
    flagList,
    legend,
    .progress = "text"
  )
}