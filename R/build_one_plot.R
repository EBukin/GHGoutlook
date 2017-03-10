n#' Building one plot of the time series data 
#' 
#' Used data sould at least contain variables Year and Value with observation 
#' specific values X and Y and extra ctegorical variables
build_one_plot <-
  function(data,
           timeseries_var,
           flagList = NA,
           legend_list = NA) {
    require(dplyr)
    require(grid)
    require(ggplot2)
    require(ggthemes)
    require(scales)
    
    if (nrow(data %>% filter(!is.na(Value))) != 0)
    {
      # To avoid error: 
      #     "geom_path: Each group consists of only one observation. 
      #         Do you need to adjust the group aesthetic?"
      # We check, if there is only one observation in each group
      many_obs <-
        data %>%
        filter(!is.na(Value)) %>%
        group_by_(.dots = timeseries_var) %>%
        summarise(num_obs = n()) %>%
        mutate(num_obs = ifelse(num_obs <= 1, FALSE, TRUE)) %>%
        .$num_obs %>%
        any()
      
      # Preparing text for lables and axes
      ItemCode_t <- unique(data$ItemCode)
      ElementCode_t <- unique(data$ElementCode)
      AreaCode_t <- unique(data$AreaCode)
      plot_title <-
        c(AreaCode_t, "_", ItemCode_t, "_", ElementCode_t) %>% 
        str_replace_na(., replacement = " ") %>% 
        str_c(collapse = "")
      plot_sub_title <-
        c(ElementCode_t, " - ", unique(data$ElementName), ";\n",
          ItemCode_t, " - ", unique(data$ItemName)) %>% 
        str_replace_na() %>% 
        str_c(collapse = "")
      if("extra_text" %in% names(data)) {
        plot_sub_title <- str_c(plot_sub_title, ";\n", str_replace_na(unique(data$extra_text)))
      }
      ylab_title <- "Value"
      xlab_title <- "Year"
      
      # If the variable with units is provided we put them on the plot
      if ("Unit" %in% names(data)) {
        # ylab_title <- 
          str_c(ylab_title, ", ", str_c(str_replace_na(unique(data$Unit), replacement = " "), collapse = ""))
      } 
    }
    
    # First big IF statement to clean data and resist against the breaking
    #    when there is no data.
    if (nrow(data %>% filter(!is.na(Value))) == 0)
    {
      g <- ggplot(data.frame(x = c(1),
                             y = c(1)),
                  aes(x = x, y = y)) +
        annotate(
          geom = "text",
          x = 1,
          y = 1,
          label = paste0("There is no data")
        )
      
    } else {
      # Initializing prots
      g <- ggplot(data = data)
      
      # Adding aesthetics
      g <-
        g +
        aes_string(
          "Year",
          "Value",
          fill = timeseries_var,
          colour = timeseries_var,
          linetype = timeseries_var
        )
      
      # If there are flags specified
      if (!all(is.na(flagList))) {
        g <- g + geom_point(aes(shape = Flag))
        g <- g + scale_shape_manual(values = flagList)
      } else {
        g <- g + geom_point()
      }
      
      # If there are any legend info specified
      if (!all(is.na(legend_list))) {
        g <-
          g +
          scale_linetype_manual(values = legend_list) +
          scale_colour_manual(values = legend_list) +
          scale_fill_manual(values = legend_list)
      }
      
      # If there were more than one observation in eeach group we can plot
      #   lines only one observation in each group
      if (many_obs) {
        g <- g + geom_line()
      }
      
      # Controlling scales
      g <-
        g +
        scale_x_continuous(minor_breaks = seq(min(data$Year) , max(data$Year), 1),
                           breaks = pretty_breaks(n = (max(data$Year) - min(data$Year)) / 2)) +
        scale_y_continuous() #+
        #expand_limits(y = 0)
      
      
      # Titles
      g <-
        g +
        ggtitle(plot_title, plot_sub_title) +
        ylab(ylab_title) +
        xlab(xlab_title) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    }
    
    
    # Additional things on the plot and returning the object
    g +
      # theme_excel() +
      # scale_colour_excel(palette = "new")  +
      geom_vline(xintercept = 2016)
    
  }
