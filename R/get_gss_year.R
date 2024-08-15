#' Get the year that a set of GSS codes were operational
#'
#' Returns the earliest year for which all of the gss codes in a dataframe were
#' operational
#' 
#' A wrapper for get_gss_date which returns the year of the first December that the 
#' codes were operational
#'
#' @param df_in A data frame containing gss_codes.
#' @param col_code A string. The name of the column which contains gss codes (defaults to
#'   \code{gss_code}).
#'
#' @return An integer  The year of the first December that the 
#' codes were operational
#' 
#' @import dplyr
#' 
#' @export

get_gss_year <- function(df_in, 
                         col_code = "gss_code") {
  
  date_range <- get_gss_date(df_in, col_code)
  
  # TODO check that date_range$earliest is a date with formate "yyyy-mm-dd"
  
  year <- date_range$earliest %>% 
    format('%Y') %>% as.numeric()
  
  return(year)
  
}

