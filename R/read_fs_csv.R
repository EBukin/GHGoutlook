

## FUNCTION read.fs.csv --------------------------------------------------
## Reads a FAOSTAT csv file with data downloaded from the website

# @param pathway 
#		is the path to the file to be read
# @param conv_factors 
#		TRUE/FALSE indicates if the following fields should be stored as factors: 
#		"DomainCode", "DomainName", "AreaName", "ElementName", "ItemName", "Flag", "FlagD".
#		It is set to FALSE by default.


read.fs.csv <- function(pathway, conv_factors = FALSE) {
  # Reads data (using data.table the reading stops when there is a break in the lines of data)
  df <- 
	dplyr::tbl_df(
		data.table::fread(
		  pathway,
		  sep = ",",
		  data.table = FALSE,
		  header = TRUE,
		  stringsAsFactors = FALSE
		)
	)
  
  # Cleans names
  names(df) <- make.names(iconv(names(df), "latin1", "ASCII", sub=""))

  if("Area.Code" %in% names(df)) {
    names(df)[names(df) == "Area.Code"] <- "AreaCode"
  }
  
  if("Area" %in% names(df)) {
    names(df)[names(df) == "Area"] <- "AreaName"
  }
  
  if("Domain.Code" %in% names(df)) {
    names(df)[names(df) == "Domain.Code"] <- "DomainCode"
  }
  
  if("Domain" %in% names(df)) {
    names(df)[names(df) == "Domain"] <- "DomainName"
  }
  
  
  if("Element.Code" %in% names(df)) {
    names(df)[names(df) == "Element.Code"] <- "ElementCode"
  }
  
  if("Element" %in% names(df)) {
    names(df)[names(df) == "Element"] <- "ElementName"
  }
  
  if("Item.Code" %in% names(df)) {
    names(df)[names(df) == "Item.Code"] <- "ItemCode"
  }  
  
  if("Item" %in% names(df)) {
    names(df)[names(df) == "Item"] <- "ItemName"
  }  
  
  if("Year.Code" %in% names(df)) {
    df <- 
      select(df, -Year.Code)
  }  
  
  if("Flag.Description" %in% names(df)) {
    names(df)[names(df) == "Flag.Description"] <- "FlagD"
  }
  
  
  # Converts certain columns to integer or numeric
  # if conv_factor = TRUE, convert certain columns to factor
  
  integers <- c("AreaCode", "ElementCode", "ItemCode", "Year")
  df[, integers] <- lapply(df[, integers], as.integer)
  
  numerics <- c("Value")
  df[, numerics] <- lapply(df[, numerics], as.numeric)
    
  if (conv_factors) {
    factors <- c("DomainCode", "DomainName", "AreaName",
                 "ElementName", "ItemName", "Flag", "FlagD")
    df[, factors] <- lapply(df[, factors], as.factor)
  }
  
  # Cleans memory  
  invisible(gc(verbose = FALSE))
  df
}



