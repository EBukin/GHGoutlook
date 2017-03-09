# Plot a subgroup only when number of plotsin agroup is bigger than nyumber of plots per page
plot_subgroup <- function(df, n_page, plots_var, timeseries_var, all_vars, flagList, legend) {
  if (any(is.na(plots_var))) plots_var <- all_vars[!all_vars %in% c(timeseries_var)]
  
  n_plots <-
    df %>%
    group_by_(.dots = plots_var) %>%
    n_groups
  
  # Split dataframe into groups
  if (n_plots > n_page) {
    df <-
      df %>%
      group_by_(.dots = plots_var) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      mutate(sub_page = rep(1:nrow(.), each = n_page)[1:nrow(.)]) %>%
      left_join(df, by = plots_var)
    
  } else {
    df <-
      df %>%
      mutate(sub_page = TRUE)
  }

  d_ply(df,
        .(sub_page),
        plot_page,
        plots_var,
        timeseries_var,
        flagList,
        n_page,
        legend)
}