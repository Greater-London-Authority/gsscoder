#' Gets the geography levels present in data
#'
#' Returns a list of strings describing the geography levels present in the data.
#' Strings can be "lad", "county", "region" or "country"
#'
#' Note that Wales will be treated as a region if there are English regions in the 
#' data but no English country.
#'
#' @param df_in A data frame containing gss_codes.
#' @param col_code A string. The name of the column which contains gss codes (defaults to
#'   \code{gss_code}).
#'
#' @return A list of one or more of the following strings: \code{"lad"}, \code{"county"}, \code{"region"} or \code{"country"}
#' 
#' @import dplyr
#' @importFrom assertthat assert_that

#' 
#' @export

get_geog_levels <- function(df_in, 
                          col_code = "gss_code") {
  
  .validate_get_geog_levels(df_in, col_code)
  
  df_in <- df_in %>%
    rename("gss_code" = !!col_code)
  
  entity_levels <- .sys_entity_levels
  known_entites <- unname(unlist(entity_levels))
  
  entities <- df_in %>%
    mutate(entity = substr(gss_code,1,3)) %>%
    select(entity) %>%
    unique() %>%
    pull()
  
  # check that fn can handle the geographies present
  if (!all(entities %in% known_entites)) {
    print(entities[!entities %in% known_entites])
    stop("in get_geog_levels the data in df_in contains geography levels that the function can't handle (listed above)")
  }
  
  # If there are English regions (E12) present then W92 is a region unless E92 is also present (English regions + Wales is a common geography for the GLA to use)
  if ("E12" %in% entities & !"E92" %in% entities) {
    entity_levels$country <- NULL
  }
  
  levels_TF <- lapply(entity_levels, function(x) any(entities %in% x))
  levels <- names(entity_levels)[which(unlist(unname(levels_TF)))]
  
  return(levels)

}


.validate_get_geog_levels <- function(df_in, col_code) {
  # validate input variable data types
  assertthat::assert_that(is.data.frame(df_in),
                          msg = "in get_geog_levels, df_in must be a dataframe")
  
  assertthat::assert_that(is.character(col_code),
                          msg = "in get_geog_levels, col_code must of type character")
  
  # other validations
  assertthat::assert_that(col_code %in% names(df_in),
                          msg = paste0("in get_geog_levels, specified col_code `", col_code,
                                       "` not in input dataframe"))
}
