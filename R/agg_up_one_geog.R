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

# TODO add somewhere that this can't have any other columns than gss_code and gss_name that are on the geography level. 

agg_up_one_geog <- function(df_in, 
                            col_code = "gss_code", 
                            col_name = NA) {
  
  df_in <- df_in %>%
    rename("gss_code" = !!col_code)
  
  # maybe I want to make sure this function doesn't add a name column as a side effect
  # if the name column is there then it will also need a date as will need to use add_gss_names after the aggregation
  
  if (!!col_name %in% names(df_in)) {
    df_in <- df_in %>% select(-!!col_name)
  }
  
  parent_codes <- filter(all_codes_dates, gss_code %in% df_in$gss_code) %>%
    select(gss_code, parent_cd) %>%
    unique()
  
  
  # hmmm I'm going to need to specify the year in case a gss_code ever gets moved from one parent code to another
  
  # must remove col_name if it exists, and then add the new col_name
  
  .validate_agg_up_one_geog(df_in,col_code, col_name)
  

}

.validate_agg_up_one_geog <-function(df_in, col_code, col_name) {
  
  # validate input variable data types
  assertthat::assert_that(is.data.frame(df_in),
                          msg = "in agg_up_one_geog, df_in must be a dataframe")
  
  assertthat::assert_that(is.character(col_code),
                          msg = "in agg_up_one_geog, col_code must be of type character")
  
  assertthat::assert_that(is.character(col_name),
                          msg = "in agg_up_one_geog, col_name must be of type character")
  
  assertthat::assert_that(is.na(gss_date) | is.Date(gss_date),
                          msg = "in agg_up_one_geog gss_date must be a date object")
  
  assertthat::assert_that(is.na(gss_year) | is.numeric(gss_year) | is.integer(gss_year),
                          msg = "in agg_up_one_geog gss_year must be integer or numeric")
  
  # other validations
  assertthat::assert_that(col_code %in% names(df_in),
                          msg = paste0("in agg_up_one_geog, specified col_code `", col_code,
                                       "` not in input dataframe"))
  
  assertthat::assert_that(is.na(gss_date) | is.na(gss_year),
                          msg = "in agg_up_one_geog only one of gss_date and gss_year can be specified")
  
  assertthat::assert_that(!(is.na(gss_date) & is.na(gss_year)),
                          msg = "in agg_up_one_geog one of gss_date or gss_year must be specified")
  
  invisible()
}
