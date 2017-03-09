#' Loading, cleaning and making tidy TROLL generated csv data file
#'
#' @param file path to the file
#' @param d.source name ofthe data created in the column d.source

load_troll_csv <-
  function(file,  d.source = NA) {
    new_mod <-
      read.csv(file,
               header = FALSE,
               stringsAsFactors = FALSE)%>%
      tbl_df()
    names(new_mod) <- 
      slice(new_mod, 2) %>% 
      mutate(V1 = "OUTPUT,0")
    new_mod <-
      new_mod %>%
      tbl_df %>%
      dplyr::slice(3:nrow(.)) %>%
      separate(
        `OUTPUT,0`,
        c("AreaCode", "ItemCode", "ElementCode"),
        sep = "_",
        remove = FALSE,
        fill = "left"
      ) %>%
      select(`OUTPUT,0`,
             AreaCode,
             ItemCode,
             ElementCode,
             matches("\\d{4}")) %>%
      gather(Year, Value, matches("\\d{4}")) %>%
      mutate(Year = as.numeric(str_sub(Year, 1, 4)),
             Value = as.numeric(Value)) %>% 
      filter(!is.na(Value)) %>% 
      distinct() 
    
    if (!is.na(d.source))
      new_mod <- new_mod %>% mutate(d.source = d.source)
    
    new_mod
  }