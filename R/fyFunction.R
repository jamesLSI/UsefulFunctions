#' Fiscal Year Function
#'
#' @param .data is a dataframe
#' @param date_field is a date column in the dataframe
#'
#' @return an additional column in the dataframe with the correct fiscal year
#' @export
#'
#' @examples
#' data_With_fy <- data_frame %>% 
#'   mutate(fy_function(., new_col = fy_to, date_field = To))
fy_function <- function(.data, new_col, date_field){
  
  return(.data %>% 
           mutate({{new_col}} := if_else(month(date_field) > 3,
                                         paste0(year(date_field),
                                                "/",
                                                str_sub(year(date_field)+1,
                                                        3,4)),
                                         paste0(year(date_field)-1,
                                                "/",
                                                str_sub(year(date_field),
                                                        3,4)))))
}