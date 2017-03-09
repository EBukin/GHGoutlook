# Plot entire file TO FIX ISSUE OF MULTIPLE FINE IDENTIFIERS
plot_pages_into_pdf <-
  function(df,
           n_page = 6,
           write_pdf = TRUE,
           output_path = "./vignettes/",
           output_name = "",
           file_width = 29.7 / 1.8,
           file_heights = 21 / 1.8,
           files_var = NA,
           groups_var = NA,
           plots_var = NA,
           timeseries_var = "d.source",
           all_vars = c("AreaCode", "ItemCode", "ElementCode", "d.source")) {
    # Everything field for files by
    df <- df %>% mutate(All = 1)
    
    # Checking what are we puting tothe files
    if (is.na(files_var))
      files_var <- "All"
    
    # File path
    output_name <-
      str_c(output_path, "/", output_name)
    
    d_ply(df,
          files_var,
          function(df) {
            file_by_name <-
              str_c(unique(df[, files_var])[[1]], "_", Sys.Date(), ".pdf")
            
            if (write_pdf) {
              file_path_name <- path.expand(str_c(output_name, file_by_name))
              graphics.off()
              pdf(file_path_name,
                  width = file_width,
                  height = file_heights)
            }
            plot_group(df, n_page,
                       groups_var,
                       plots_var,
                       timeseries_var,
                       all_vars)
            if (write_pdf) {
              dev.off()
            }
          })
  }