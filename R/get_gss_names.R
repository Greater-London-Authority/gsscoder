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
#' for the new column containing GSS names, and specify the date or year for the codes.
#' 
#'
#' @param df_in A data frame containing gss_codes and data.
#' @param col_geog A string. The name of the column which contains gss codes (defaults to
#'   \code{gss_code}).
#' @param col_gss_name A string. The name to give the new column containing gss names. 
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
#' 
#' @export


get_gss_names <- function(df_in, 
                      col_geog = "gss_code", 
                      col_gss_name = "gss_name", 
                      gss_date = NA,
                      gss_year = NA) {
  
  #TODO: add tests for inputs, including that the codes are fine for the year/date
  #TODO: check that only one of gss_year or gss_date are defined
  #TODO: just take the latest name if neither gss_date nor gss_year are defined?

  
  if (!!col_gss_name %in% names(df_in)) {
    df_in <- df_in %>% select(-!!col_gss_name)
  }
  
  df <- df_in %>%
    rename("gss_code" = !!col_geog)
  
  
  if (!is.na(gss_year)) {
    gss_date = as.Date(paste0(gss_year,"-12-31"))
  }
  
  
  code_names <- all_lad_codes_dates %>%
    filter(start_date <= gss_date, ( is.na(end_date) | end_date >= gss_date )) %>%
    select(gss_code, gss_name) %>%
    unique()
  
  df <- df %>% left_join(code_names, by = c("gss_code"))
    
  df <- select(df, !!col_geog := gss_code, !!col_gss_name := gss_name, everything())
  
  return(df)
}
