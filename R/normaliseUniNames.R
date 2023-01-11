#' Normalise University Names Function
#'
#' @param uni_name_list is a tibble with the first column university names or an atomic vector
#'
#' @return a tibble called normalised_uni_name_list with original and normalised names or a mutated atomic vector
#' @export
#'
#' @examples
#' for tibble
#' uni_name_list <- trans_data %>% 
#'   filter(EnterpriseSizeClean == "Academic") %>% 
#'   distinct(ParticipantName)
#'   
#'   normalise_university_name_function(uni_name_list)
#'   
#'   or atomic vector
#'   trans_data %>%
#'     filter(EnterpriseSize == "Academic") %>% 
#'     mutate(UniversityName = normalise_university_name_function(ParticipantName))
normalise_university_name_function <- function(uni_name_list) {
  
  if (is.data.frame(uni_name_list) == T) {
    clean_name_list <- uni_name_list %>%
      rename(uni_name = 1) %>% 
      mutate(uni_name_clean = toupper(uni_name)) %>%
      
      # abbreviations
      mutate(uni_name_clean = str_replace(uni_name_clean, "^LSE$", "LONDON SCHOOL OF ECONOMICS")) %>%
      mutate(uni_name_clean = str_replace(uni_name_clean,"^UCL *","UNIVERSITY COLLEGE LONDON")) %>%
      
      # general standardisation
      # remove initial the
      mutate(uni_name_clean = str_replace(uni_name_clean,"^THE (.*)$","\\1")) %>%
      # remove full stops
      mutate(uni_name_clean = str_replace(uni_name_clean,"\\.","")) %>%
      # remove apostrophes
      mutate(uni_name_clean = str_replace_all(uni_name_clean,"\'","")) %>%
      # remove parenthetic additions
      mutate(uni_name_clean = str_replace(uni_name_clean," *\\(.*\\)","")) %>%
      # put university to beginning 1
      mutate(uni_name_clean = str_replace(uni_name_clean,"^(.*) UNIVERSITY (.*)$","UNIVERSITY \\1 \\2")) %>%
      # put university to beginning 2
      mutate(uni_name_clean = str_replace(uni_name_clean,"^(.*) UNIVERSITY$","UNIVERSITY \\1")) %>%
      # take out comma before London
      mutate(uni_name_clean = str_replace(uni_name_clean,", LONDON$"," LONDON")) %>%
      # remove final punctuation
      mutate(uni_name_clean = str_replace(uni_name_clean,"^(.*)[[:punct:]].*$","\\1")) %>%
      # remove of
      mutate(uni_name_clean = str_replace(uni_name_clean,"UNIVERSITY OF ","UNIVERSITY ")) %>%
      # put college at beginning 1
      mutate(uni_name_clean = str_replace(uni_name_clean,"^(.*) COLLEGE (.*)$","COLLEGE \\1 \\2")) %>%
      # put college at beginning 2
      mutate(uni_name_clean = str_replace(uni_name_clean,"^(.*) COLLEGE$","COLLEGE \\1")) %>%
      # rmeove of
      mutate(uni_name_clean = str_replace(uni_name_clean,"^(.*) SCHOOL OF ([[:alpha:]]*).*$","\\1 SCHOOL \\2")) %>%
      # remove trailing spaces
      mutate(uni_name_clean = str_trim(uni_name_clean)) %>%
      # remove higher education
      mutate(uni_name_clean = str_replace(uni_name_clean,"^(.*) HIGHER EDUCATION.*$","\\1")) %>%
      # remove any internal the
      mutate(uni_name_clean = str_replace(uni_name_clean," THE "," ")) %>%
      # remove any final of
      mutate(uni_name_clean = str_replace(uni_name_clean," OF "," ")) %>%
      
      # some specifics
      # Newcastle
      mutate(uni_name_clean = str_replace(uni_name_clean," UPON TYNE","")) %>%
      # Northumbria
      mutate(uni_name_clean = str_replace(uni_name_clean,"^(.*) AT (.*)$","\\1")) %>%
      # Welsh universities - and foreign accents
      mutate(uni_name_clean = str_replace(uni_name_clean,"PRIFYSGOL","UNIVERSITY") %>%
               iconv(from='UTF-8',to='ASCII//TRANSLIT')) %>%
      # Solent
      mutate(uni_name_clean = str_replace(uni_name_clean,"UNIVERSITY SOLENT","UNIVERSITY SOUTHAMPTON SOLENT")) %>%
      # Leeds Beckett (changed name)
      mutate(uni_name_clean = str_replace(uni_name_clean,"LEEDS BECKETT","LEEDS METROPOLITAN")) %>%
      # Abertay
      mutate(uni_name_clean = str_replace(uni_name_clean,"ABERTAY$","ABERTAY DUNDEE")) %>%
      # Birkbeck
      mutate(uni_name_clean = str_replace(uni_name_clean,"UNIVERSITY BIRKBECK","BIRKBECK"))
    
    normalised_uni_name_list <<- clean_name_list
  } else {
    
    return(uni_name_list %>%
             toupper() %>%
             
             # abbreviations
             str_replace("^LSE$","LONDON SCHOOL OF ECONOMICS") %>%
             str_replace("^UCL *","UNIVERSITY COLLEGE LONDON") %>%
             
             # general standardisation
             # remove initial the
             str_replace("^THE (.*)$","\\1") %>%
             # remove full stops
             str_replace("\\.","") %>%
             # remove apostrophes
             str_replace_all("\'","") %>%
             # remove parenthetic additions
             str_replace(" *\\(.*\\)","") %>%
             # put university to beginning 1
             str_replace("^(.*) UNIVERSITY (.*)$","UNIVERSITY \\1 \\2") %>%
             # put university to beginning 2
             str_replace("^(.*) UNIVERSITY$","UNIVERSITY \\1") %>%
             # take out comma before London
             str_replace(", LONDON$"," LONDON") %>%
             # remove final punctuation
             str_replace("^(.*)[[:punct:]].*$","\\1") %>%
             # remove of
             str_replace("UNIVERSITY OF ","UNIVERSITY ") %>%
             # put college at beginning 1
             str_replace("^(.*) COLLEGE (.*)$","COLLEGE \\1 \\2") %>%
             # put college at beginning 2
             str_replace("^(.*) COLLEGE$","COLLEGE \\1") %>%
             # rmeove of
             str_replace("^(.*) SCHOOL OF ([[:alpha:]]*).*$","\\1 SCHOOL \\2") %>%
             # remove trailing spaces
             str_trim() %>%
             # remove higher education
             str_replace("^(.*) HIGHER EDUCATION.*$","\\1") %>%
             # remove any internal the
             str_replace(" THE "," ") %>%
             # remove any final of
             str_replace(" OF "," ") %>%
             
             # some specifics
             # Newcastle
             str_replace(" UPON TYNE","") %>%
             # Northumbria
             str_replace("^(.*) AT (.*)$","\\1") %>%
             # Welsh universities - and foreign accents
             str_replace("PRIFYSGOL","UNIVERSITY") %>%
             iconv(from='UTF-8',to='ASCII//TRANSLIT') %>%
             # Solent
             str_replace("UNIVERSITY SOLENT","UNIVERSITY SOUTHAMPTON SOLENT") %>%
             # Leeds Beckett (changed name)
             str_replace("LEEDS BECKETT","LEEDS METROPOLITAN") %>%
             # Abertay
             str_replace("ABERTAY$","ABERTAY DUNDEE") %>%
             # Birkbeck
             str_replace("UNIVERSITY BIRKBECK","BIRKBECK"))
  }
}