#' Add a column of GSS names
#' 
#' Adds a column of GSS names to match a column of GSS codes.
#'
#' Uses the names that were operational for those GSS codes at the given date/year.
#' 
#' Throws an error if there are any GSS codes which were not operational at the 
#' given date/year.
#' 
#' User must define the name of the column containing GSS codes, give a name
#' for the new column to contain GSS names, and specify the date or year for the codes and names.
#' 
#' note that 2009 codes run up to 2012 but there was one name change in 2010
#' and 2013 codes run up to 2019 but there was a name change in 2018.
#' 
#'
#' @param df_in A data frame containing gss_codes and data.
#' @param col_code A string. The name of the column which contains gss codes (defaults to
#'   \code{gss_code}).
#' @param col_name A string. The name to give the new column containing gss names. 
#' Defaults to \code{gss_name}. If the column already exists in \code{df_in} it will be
#' overwritten.
#' @param gss_date A date object specifying the point in time that the gss codes 
#' were/are operational. Only one of gss_date or gss_year can be defined. Defaults 
#' to \code{NA})
#' @param gss_year  Numeric or Integer. The year at which the gss codes were/are 
#' operational. Equivalent to setting gss_date to 31st December of that year. 
#' Only one of gss_date or gss_year can be defined. Defaults to \code{NA}) 
#' 
#'
#' @return The input dataframe with a column either added or overwritten which contains
#' the GSS names. The first two columns will be the gss codes and gss names respectively
#' 
#' @import dplyr
#' @importFrom lubridate is.Date
#' @importFrom assertthat assert_that
#' 
#' @export


add_gss_names <- function(df_in, 
                      col_code = "gss_code", 
                      col_name = "gss_name", 
                      gss_date = NA,
                      gss_year = NA) {
  
  
  .validate_add_gss_names(df_in, col_code, col_name, gss_date, gss_year)
  
  check_gss_codes(df_in, col_code, gss_date, gss_year)
  
  #TODO: just take the latest name if neither gss_date nor gss_year are defined?
  
  if (!!col_name %in% names(df_in)) {
    df_in <- df_in %>% select(-!!col_name)
  }
  
  df <- df_in %>%
    rename("gss_code" = !!col_code)
  
  
  if (!is.na(gss_year)) {
    gss_date = as.Date(paste0(gss_year,"-12-31"))
  }
  
  
  code_names <- all_lad_codes_dates %>%
    filter(start_date <= gss_date, ( is.na(end_date) | end_date >= gss_date )) %>%
    select(gss_code, gss_name) %>%
    unique()
  
  df <- df %>% left_join(code_names, by = c("gss_code"))
    
  df <- select(df, !!col_code := gss_code, !!col_name := gss_name, everything())
  
  return(df)
}



.validate_add_gss_names <- function(df_in, col_code, col_name, gss_date, gss_year) {
  
  # validate input variable data types
  assertthat::assert_that(is.data.frame(df_in),
                          msg = "in add_gss_names, df_in must be a dataframe")
  
  assertthat::assert_that(is.character(col_code),
                          msg = "in add_gss_names, col_code must be of type character")
  
  assertthat::assert_that(is.character(col_name),
                          msg = "in add_gss_names, col_name must be of type character")
  
  assertthat::assert_that(is.na(gss_date) | is.Date(gss_date),
                          msg = "in add_gss_names gss_date must be a date object")
  
  assertthat::assert_that(is.na(gss_year) | is.numeric(gss_year) | is.integer(gss_year),
                          msg = "in add_gss_names gss_year must be integer or numeric")
  
  # other validations
  assertthat::assert_that(col_code %in% names(df_in),
                          msg = paste0("in add_gss_names, specified col_code `", col_code,
                                      "` not in input dataframe"))
  
  if(col_name %in% names(df_in)) warning(paste0("in add_gss_names df_in already contains column `", col_name, "`. The data in this column will be overwritten."))
  
  assertthat::assert_that(is.na(gss_date) | is.na(gss_year),
                          msg = "in add_gss_names only one of gss_date and gss_year can be specified")
  
  assertthat::assert_that(!(is.na(gss_date) & is.na(gss_year)),
                          msg = "in add_gss_names one of gss_date or gss_year must be specified")
  
  database_year <- database_date %>% format('%Y') %>% as.numeric()
  assertthat::assert_that(is.na(gss_year) | (gss_year >= 2009 & gss_year <= database_year),
                          msg = paste0("in add_gss_names gss_year must be a number between 2009 and ", database_year, ". If your required year is later than ", database_year ," then check if the gsscoder package code change database needs updating"))
  
  assertthat::assert_that(is.na(gss_date) | (gss_date >= as.Date("2009-01-01") & gss_date <= database_date),
                          msg = paste0("in add_gss_names gss_date must be between 2009-01-01 and ", database_date, ". If your required date is later than ", database_date ," then check if the gsscoder package code change database needs updating"))

  
  invisible()
}
