#' CRN Cleansing Fucntion
#'
#' @param crn_list can be a tibble where the first column is crns or an atomic vector
#'
#' @return a tibble with original and cleansed crns
#' @export
#'
#' @examples
#' as tibble
#' crn_list <- trans_data %>% 
#'     distinct(Crn)
#' clean_crn_function(crn_list)
#' 
#' or atomic vector 
#' clean_crn_trans_data <- trans_data %>% 
#'     mutate(clean_crn = clean_crn_function(Crn)) 
#' 
clean_crn_function <- function(crn_list) {
  
  
  if (is.data.frame(crn_list) == T) {
    
    cleansed <- crn_list %>%
      rename(crn = 1) %>% 
      # 6 digits
      mutate(clean_crn = str_replace(crn, "^(\\d{6})$", "00\\1")) %>%
      # 7 digits
      mutate(clean_crn = str_replace(crn, "^(\\d{7})$", "0\\1")) %>%
      # IP withour trailing R
      mutate(clean_crn = str_replace(crn, "^IP(\\d{5})$", "IP\\1R")) %>%
      # two letter four digits
      mutate(clean_crn = str_replace(crn, "^([A-Z]{2})(\\d{4})$", "\\100\\2")) %>%
      # two letters five digits
      mutate(clean_crn = str_replace(crn, "^([A-Z]{2})(\\d{5})$", "\\10\\2"))
    
    clean_crn_list <<- cleansed
    
  } else {
    
    return(crn_list %>%
             # 6 digits
             str_replace("^(\\d{6})$", "00\\1") %>%
             # 7 digits
             str_replace("^(\\d{7})$", "0\\1") %>%
             # IP withour trailing R
             str_replace("^IP(\\d{5})$", "IP\\1R") %>%
             # two letter four digits
             str_replace("^([A-Z]{2})(\\d{4})$", "\\100\\2") %>%
             # two letters five digits
             str_replace("^([A-Z]{2})(\\d{5})$", "\\10\\2")
    )
    
    
  }
}