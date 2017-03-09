#' Reads a FAOSTAT csv data file wihch was downloaded in bulk from the website
#' 
#' Proide and path to the file and the function will return a tbl_df() object
#' Depends on the dplyr and data.frame.
#'
#' @author Eduard Bukin
#' @param pathway is a path to the file.
#' @param conv_factors specifies if the string variables should be conversted to a factor.
read.fs.bulk <- function(pathway, conv_factors = FALSE) {
  df <- dplyr::tbl_df(
    data.table::fread(
      pathway,
      sep = ",",
      data.table = FALSE,
      header = TRUE,
      stringsAsFactors = FALSE))

  names(df) <-
    make.names(iconv(names(df), "latin1", "ASCII", sub = ""))
  if("Country.Code" %in% names(df)) names(df)[names(df) == "Country.Code"] <- "AreaCode"
  if("CountryCode" %in% names(df)) names(df)[names(df) == "CountryCode"] <- "AreaCode"
  if("Country" %in% names(df)) names(df)[names(df) == "Country"] <- "AreaName"
  if("Area.Code" %in% names(df)) names(df)[names(df) == "Area.Code"] <- "AreaCode"
  if("Area" %in% names(df)) names(df)[names(df) == "Area"] <- "AreaName"
  names(df)[names(df) == "Item.Code"] <- "ItemCode"
  names(df)[names(df) == "Item"] <- "ItemName"
  names(df)[names(df) == "Element.Code"] <- "ElementCode"
  names(df)[names(df) == "Element"] <- "ElementName"
  names(df)[names(df) == "Year.Code"] <- "YearCode"

  if (conv_factors) {
    factors <- c("DomainCode", "DomainName", "AreaName",
                 "ElementName", "ItemName", "Flag", "FlagD")
    df[, factors] <- lapply(df[, factors], as.factor)
  }

  integers <- c("AreaCode", "ElementCode", "ItemCode", "Year")
  df[, integers] <- lapply(df[, integers], as.integer)

  numerics <- c("Value")
  df[, numerics] <- lapply(df[, numerics], as.numeric)

  rm(factors, integers, numerics)
  invisible(gc(verbose = FALSE))
  df[, c("AreaCode", "AreaName", "ItemCode", "ItemName",
         "ElementCode", "ElementName", "Year", "Unit", "Value", "Flag")]
}
