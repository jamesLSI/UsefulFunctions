# function to tidy up CRNs

#' CRN Cleansing Fucntion
#'
#' @param crn_list is a tibble where the first column is crns
#'
#' @return a tibble with original and cleansed crns
#'
#' @examples
#' crn_list <- trans_data %>% 
#'     distinct(Crn)
#' clean_crn_function(crn_list)
clean_crn_function <- function(crn_list) {

    cleansed <- crn_list %>%
      rename(crn = 1) %>% 
      # 6 digits
      mutate(crn_cleansed = str_replace(crn, "^(\\d{6})$", "00\\1")) %>%
      # 7 digits
      mutate(crn_cleansed = str_replace(crn, "^(\\d{7})$", "0\\1")) %>%
      # IP withour trailing R
      mutate(crn_cleansed = str_replace(crn, "^IP(\\d{5})$", "IP\\1R")) %>%
      # two letter four digits
      mutate(crn_cleansed = str_replace(crn, "^([A-Z]{2})(\\d{4})$", "\\100\\2")) %>%
      # two letters five digits
      mutate(crn_cleansed = str_replace(crn, "^([A-Z]{2})(\\d{5})$", "\\10\\2"))
    
    clean_crn_list <<- cleansed
}