options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))


### names function ####

#' Name Repair Function
#'
#' @param nms used only in the .name_repair argument of readxl::read_excel and without parentheses
#'
#' @return variable names in dataframe in upper camel
#' @export
#'
#' @examples output <- readxl::read_excel("C:/data.xlsx",
#'                              .name_repair = namesFunction)
namesFunction <- function(nms) {
  janitor::make_clean_names(nms, case = "upper_camel")}


### data warehouse ####

#' Data Warehouse Function
#'
#' @param table_choice is the data warehouse table we want to return, leaving this blank will return the available tables in the console
#'
#' @return a data frame
#' @export
#'
#' @examples dataWarehouse_function_specific("Application Summaries")
dataWarehouse_function_specific <- function(table_choice = "Tables"){

    con <- DBI::dbConnect(drv      = RMySQL::MySQL(),
                        username = Sys.getenv("usernameDw"),
                        password = Sys.getenv("passwordDw"),
                        host     = Sys.getenv("dwUrl"),
                        port     = 3306,
                        dbname   = "dw")

  all_tables <- DBI::dbListTables(con)

  tables <- tibble::tibble(name = c("Applications Summaries",
                                    "Application Scores",
                                    "Assessor Comments",
                                    "Application Questions",
                                    "Projects",
                                    "Claims / Forecast",
                                    "Monitoring",
                                    "Competitions",
                                    "Change Requests",
                                    "Trans Data",
                                    "Funders",
                                    "EDI",
                                    "PCF",
                                    "Warehouse"))
  start <- Sys.time()

  if (table_choice == "Applications Summaries") {

    print("This could take up to 6 minutes")


    applicantTable <- DBI::dbReadTable(con, "vw_Dashboard_IFSApplicant")

    applicationTable <- DBI::dbReadTable(con, "vw_Dashboard_IFSApplication")

    application_id_changes <- DBI::dbReadTable(con, "vw_IFS_ApplicationID_Changes") %>%
      select(ApplicationID = application_id,
             previous_application_id) %>%
      mutate(ApplicationID = as.character(ApplicationID),
             previous_application_id = as.character(previous_application_id))

    comps <- DBI::dbReadTable(con, "vw_Dashboard_IFSCompetition")
    compsFunding <- DBI::dbReadTable(con, "vw_Dashboard_IFSCompetitionFunder")
    compsMilestones <- DBI::dbReadTable(con, "vw_Dashboard_IFSCompetitionMilestone")

    comps_output <- compsMilestones %>%
      select(CompetitionKey,
             localDate,
             type) %>%
      arrange(desc(localDate)) %>%
      distinct(CompetitionKey,
               type,
               .keep_all = T) %>%
      pivot_wider(names_from = type,
                  values_from = localDate) %>%
      left_join(comps,
                by = "CompetitionKey") %>%
      left_join(compsFunding,
                Joining, by = c("CompetitionKey", "CompetitionID", "CompetitionName"))

    output <- comps_output %>%
      filter(!CompetitionKey == 0) %>%
      distinct(CompetitionKey,
               CompetitionID,
               CompetitionName,
               OPEN_DATE) %>%
      left_join(x = applicationTable,
                y = .,
                by = "CompetitionKey") %>%
      left_join(applicantTable,
                by = "ApplicationKey") %>%
      rename(ApplicationID = ApplicationID.x,
             CompetitionKey = CompetitionKey.x) %>%
      select(-c(ApplicationID.y,
                CompetitionKey.y)) %>%
      mutate(ApplicantKey = as.character(ApplicantKey),
             ApplicationID = as.character(ApplicationID)) %>%
      mutate(OPEN_DATE = as.character(OPEN_DATE),
             OPEN_DATE_clean = str_sub(OPEN_DATE,
                                       0,
                                       10),
             OPEN_DATE_clean = ymd(OPEN_DATE_clean)) %>%
      left_join(application_id_changes)

    application_data <<- output

  } else if (table_choice == "Assessor Comments") {

    print("This could take over 25 mins")

    output <- DBI::dbReadTable(con, "vw_IFSAssessorResponse ") %>%
      mutate(ApplicationID = as.character(ApplicationID))

    assessor_comments <<- output

  } else if (table_choice == "Application Scores") {

    print("This could take over 25 mins")

    output <- DBI::dbReadTable(con, "vw_IFSAssessorScores_data") %>%
      mutate(ApplicationID = as.character(ApplicationID))

    application_scores <<- output

  } else if (table_choice == "Application Questions") {

    print("This could take up to 7 minutes")

    output <- DBI::dbReadTable(con, "vw_IFSApplicationResponses_data") %>%
      mutate(ApplicationID = as.character(ApplicationID))

    application_questions <<- output

  } else if (table_choice == "Spend and Forecast") {

    print("This could take up to 4 minutes")
    output <- DBI::dbReadTable(con, "vw_Dashboard_ProjectParticipantPeriodLevel")
    spend_forecast <<- output

  } else if (table_choice == "Projects") {

    print("This could take up to 2 minutes")

    projectTable <- DBI::dbReadTable(con, "vw_Dashboard_IFSPA_Project")

    participantTable <- DBI::dbReadTable(con, "vw_Dashboard_IFSPA_ProjectParticipant")

    output <- projectTable %>%
      # select(-IsOkToPublish) %>%
      left_join(participantTable,
                by = "ProjectNumber") %>%
      select(-contains(".y"))

    names(output) <- gsub("\\.x", "", names(output))

    projects_data <<- output

  } else if (table_choice == "Monitoring") {

    print("This could take up to 2 minutes")

    output <- DBI::dbReadTable(con, "vw_Dashboard_IFSPA_MonitoringData")

    monitoring_data <<- output

  } else if (table_choice == "Competitions") {

    print("This could take up to a minute")

    comps <- DBI::dbReadTable(con, "vw_Dashboard_IFSCompetition")
    compsFunding <- DBI::dbReadTable(con, "vw_Dashboard_IFSCompetitionFunder")
    compsMilestones <- DBI::dbReadTable(con, "vw_Dashboard_IFSCompetitionMilestone")

    output <- compsMilestones %>%
      select(CompetitionKey,
             localDate,
             type) %>%
      arrange(desc(localDate)) %>%
      distinct(CompetitionKey,
               type,
               .keep_all = T) %>%
      pivot_wider(names_from = type,
                  values_from = localDate) %>%
      left_join(comps,
                by = "CompetitionKey") %>%
      left_join(compsFunding,
                Joining, by = c("CompetitionKey", "CompetitionID", "CompetitionName"))

    comps_data <<- output

  } else if (table_choice == "Change Requests") {

    print("This could take up to a minute")

    output <- DBI::dbReadTable(con, "vw_Dashboard_IFSPA_ProjectChangeRequest")

    change_request_data <<- output

  } else if (table_choice == "Trans Data") {

    print("This could take up to a minute")

    # output <- DBI::dbReadTable(con, "vw_report_publictransparencydata")
    output <- DBI::dbReadTable(con, "mv_report_publictransparencydata") %>%
      mutate(ProjectNumber = as.character(ProjectNumber))


    trans_data <<- output

  } else if (table_choice == "Funders") {

    print("This could take up to a minute")

    output <- DBI::dbReadTable(con, "vw_funderspanel_data")

    funders_data <<- output

  } else if (table_choice == "Claims / Forecast") {

    print("This could take up to 5 minutes")

    output <- DBI::dbReadTable(con, "vw_Dashboard_ProjectParticipantPeriodLevel")

    claims_forecast_data <<- output

  } else if (table_choice == "EDI") {

    print("This could take up to a minute")

    output <- DBI::dbReadTable(con, "vw_Sensitive_EDIData_JC")

    edi_data <<- output

  } else if (table_choice == "PCF") {

    print("This could take up to a minutes")

    output <- DBI::dbReadTable(con, "mv_iuk_projectcompletionform")

    pcf_data <<- output

  } else if (table_choice == "Warehouse") {

    print(all_tables)

  } else if (table_choice == "Tables") {

    print(tables)
  }

  end <- Sys.time()
  print(end-start)

}

### trans data function ####

#' Transparency Data Function
#' Pre-processing includes: normalising Enterprise Sizes, adding a Enterprise Class,
#' normalising Address Region names, separating LEPs where a participant is in two, 
#' and the removal of wirhtdrawn projects and participants
#'
#' @return the latest version of our published transparency data set 
#' @export
#'
#' @examples transDataFunction()
transDataFunction <- function() {
  link <- rvest::read_html("https://www.ukri.org/publications/innovate-uk-funded-projects-since-2004/") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    str_extract('^http.+lsx$') %>%
    .[which(!is.na(.))]

  httr::GET(link[[1]], httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

  namesFunction <- function(nms) janitor::make_clean_names(nms, case = "upper_camel")

  transData <- read_excel(tf, .name_repair = namesFunction)

  transData <- transData %>%
    mutate(AddressRegion = if_else(AddressRegion == "Yorkshire and the Humber",
                                   "Yorkshire and The Humber",
                                   AddressRegion)) %>%
    mutate(EnterpriseSize = if_else(EnterpriseSize == "Micro/small",
                                    "Micro/Small",
                                    EnterpriseSize)) %>% 
    mutate(EnterpriseSizeClean = if_else(EnterpriseSize %in% c("Start-Up",
                                                               "Sole Trader",
                                                               "Micro/Small",
                                                               "Micro or small",
                                                               "Small",
                                                               "Micro"),
                                         "Micro/Small",
                                         if_else(EnterpriseSize %in% c("Non UK",
                                                                       "Other",
                                                                       "Charity",
                                                                       "Unknown"),
                                                 "Other",
                                                 if_else(EnterpriseSize %in% c("PSO",
                                                                               "PSRE"),
                                                         "Public Sector",
                                                         EnterpriseSize)))) %>%
    mutate(EnterpriseClass = if_else(EnterpriseSizeClean %in% c("Micro/Small",
                                                                "Medium",
                                                                "SME",
                                                                "Large"),
                                     "Business",
                                     "Other")) %>%
    separate(AddressLep,
             into = c("priLep",
                      "secondaryLep"),
             sep = "; ",
             remove = F) %>%
    mutate(AddressLep = if_else(is.na(secondaryLep),
                                AddressLep,
                                priLep)) %>%
    select(-priLep) %>%
    mutate(AddressLep = if_else(AddressLep == "Northamptonshire",
                                "South East Midlands",
                                if_else(AddressLep == "Herefordshire",
                                        "The Marches",
                                        if_else(AddressLep == "City of London",
                                                "London",
                                                AddressLep)))) %>%
    mutate(ProjectNumber = as.character(ProjectNumber)) %>%
    filter(!ProjectStatus == "Withdrawn",
           ParticipantWithdrawnFromProject == "Active") #%>%
    #rename("GrantOffered" = "AwardOffered")

  rm(link, tf)

  trans_data <<- transData

}

### companies house function ####

#' Companies House Data
#'
#' @param crn_list is a tibble of companies house reference numbers
#'
#' @return a tibble containing information from the companies house API 
#' @export
#'
#' @examples 
#' crn_list <- data %>%
#'     distinct(crn)
#' companies_house_function(crn_list = crn_list)
companies_house_function <- function(crn_list){

  if(exists("co_ho_api_output") == T){
    rm(co_ho_api_output)
  }

  crn_list <- crn_list %>%
    rename(company_number = 1) %>%
    rowwise() %>%
    mutate(missing_zeros = 8-nchar(company_number)) %>%
    mutate(add_on = strrep("0",
                           missing_zeros)) %>%
    mutate(Company_number_clean = paste0(add_on,
                                       company_number)) %>%
    select(Company_number_clean)

  for (i in 1:nrow(crn_list)) {
    try({
      apiReturn <- httr::GET(url = paste0("https://api.companieshouse.gov.uk/company/", crn_list[i,1]),
                       authenticate(user = Sys.getenv("co_ho_api_key"), password = ""))
      apiJson <- suppressMessages(jsonlite::fromJSON(content(apiReturn, "text")))
      apiJson <- as.data.frame(unlist(apiJson)) %>%
        rename(value = "unlist(apiJson)")
      apiJson <- jsonlite::flatten(apiJson) %>%
        rownames_to_column() %>%
        mutate(id = 1) %>%
        mutate(rowname = if_else(rowname == "sic_codes",
                                 "sic_codes1",
                                 rowname)) %>%
        pivot_wider(names_from = rowname,
                    values_from = value)
      if(exists("co_ho_api_output") == F){
        co_ho_api_output <- apiJson

      } else {
        co_ho_api_output <- bind_rows(co_ho_api_output,
                                      apiJson)
      }
      if (i %% 600 == 0) {
        Sys.sleep(301)
      } else if (i %% 100 == 0) {
        print(i)
      }
    })
    rm(apiJson,
       apiReturn)
  }
  co_ho_api_output <- co_ho_api_output %>%
    mutate(Sic2007 = sic_codes1)

  co_ho_company_output <<- co_ho_api_output
}

### officer code function ####

#' Companies House Officer Data
#'
#' @param crn_list is a tibble of companies house reference numbers
#'
#' @return a tibble containing information from the companies house officers API
#' @export
#'
#' @examples
#' crn_list <- data %>%
#'     distinct(crn)
#' co_ho_officers_function(crn_list = crn_list)
co_ho_officers_function <- function(crn_list){

  if(exists("api_output_officer") == T){
    rm(co_ho_api_output)
  }
  for (i in 1:nrow(crn_list)) {
    try({
      apiReturn <- httr::GET(url = paste0("https://api.companieshouse.gov.uk/company/", crn_list[i,1], "/officers"),
                       authenticate(user = Sys.getenv("co_ho_api_key"), password = ""))
      apiJson <- suppressMessages(jsonlite::fromJSON(content(apiReturn, "text")))
      apiJson <- as.data.frame(unlist(apiJson)) %>%
        rename(value = "unlist(apiJson)")
      apiJson <- jsonlite::flatten(apiJson) %>%
        rownames_to_column() %>%
        mutate(crn = crn_list[[i,1]])
      if(exists("api_output_officer") == F){
        api_output_officer <- apiJson

      } else {
        api_output_officer <- bind_rows(api_output_officer, apiJson)
      }
      if (i %% 600 == 0) {
        Sys.sleep(301)
      } else if (i %% 100 == 0) {
        print(i)
      }
    })
    rm(apiJson,
       apiReturn)
  }
  return(api_output_officer)

  co_ho_output_officer <<- api_output_officer
}

### significant control function ####

#' Companies House Significant Control Data
#'
#' @param crn_list is a tibble of companies house reference numbers
#'
#' @return a tibble containing information from the companies house significant control API
#' @export
#'
#' @examples
#' crn_list <- data %>%
#'     distinct(crn)
#' co_ho_signif_control_function(crn_list = crn_list)
co_ho_signif_control_function <- function(crn_list){

  if(exists("signif_control_output") == T){
    rm(signif_control_output)
  }
  for (i in 1:nrow(crn_list)) {
    try({
      apiReturn <- httr::GET(url = paste0("https://api.companieshouse.gov.uk/company/", crn_list[i,1], "/persons-with-significant-control"),
                       authenticate(user = Sys.getenv("co_ho_api_key"), password = ""))
      apiJson <- suppressMessages(jsonlite::fromJSON(content(apiReturn, "text")))
      apiJson <- as.data.frame(unlist(apiJson)) %>%
        rename(value = "unlist(apiJson)")
      apiJson <- jsonlite::flatten(apiJson) %>%
        rownames_to_column() %>%
        mutate(crn = crn_list[[i,1]])
      if(exists("signif_control_output") == F){
        signif_control_output <- apiJson

      } else {
        signif_control_output <- bind_rows(signif_control_output, apiJson)
      }
      if (i %% 600 == 0) {
        Sys.sleep(301)
      } else if (i %% 100 == 0) {
        print(i)
      }
    })
    rm(apiJson,
       apiReturn)
  }
  co_ho_signif_control_output <<- signif_control_output
}

### charges function ####

#' Companies House Charges Data
#'
#' @param crn_list is a tibble of companies house reference numbers
#'
#' @return a tibble containing information from the companies house charges API
#' @export
#'
#' @examples
#' crn_list <- data %>%
#'     distinct(crn)
#' co_ho_charges_function(crn_list = crn_list)
co_ho_charges_function <- function(crn_list){

  if(exists("charge_api_output") == T){
    rm(charge_api_output)
  }
  
  crn_list <- crn_list %>% 
    rename(company_number = 1) %>% 
    rowwise() %>% 
    mutate(missing_zeros = 8-nchar(company_number)) %>% 
    mutate(add_on = strrep("0",
                           missing_zeros)) %>% 
    mutate(Company_number_clean = paste0(add_on,
                                         company_number)) %>% 
    select(Company_number_clean)
  
  for (i in 1:nrow(crn_list)) {
    try({
      apiReturn <- httr::GET(url = paste0("https://api.companieshouse.gov.uk/company/", crn_list[i,1], "/charges"),
                       authenticate(user = Sys.getenv("co_ho_api_key"), password = ""))
      apiJson <- suppressMessages(jsonlite::fromJSON(content(apiReturn, "text")))
      apiJson <- as.data.frame(unlist(apiJson)) %>% 
        rename(value = "unlist(apiJson)")
      apiJson <- jsonlite::flatten(apiJson) %>%
        rownames_to_column() %>% 
        mutate(id = 1) %>% 
        # mutate(rowname = if_else(rowname == "sic_codes",
        #                          "sic_codes1",
        #                          rowname)) %>% 
        pivot_wider(names_from = rowname,
                    values_from = value) %>% 
        mutate(company_number = crn_list$Company_number_clean[i])
      if(exists("charge_api_output") == F){
        charge_api_output <- apiJson
        
      } else {
        charge_api_output <- bind_rows(charge_api_output,
                                      apiJson)
      }
      if (i %% 600 == 0) {
        Sys.sleep(301)
      } else if (i %% 100 == 0) {
        print(i)
      }
    })
    rm(apiJson,
       apiReturn)
  }
  # co_ho_api_output <- co_ho_api_output %>% 
  #   mutate(Sic2007 = sic_codes1)
  co_ho_charges_output <<- charge_api_output
}
### post code function ####

#' Additional geo data from postcodes
#'
#' @param post_code_list is a tibble postcodes
#'
#' @return a tibble containing additional geographic data from the postcodes.io API
#' @export
#'
#' @examples
#' post_code_list <- data %>%
#'     distinct(postcode)
#' postcode_function(post_code_list = post_code_list)
postcode_function <- function(post_code_list){

  for (j in 1:nrow(post_code_list)) {

    try({

      postcodesForApi <- post_code_list[[j,1]]

      postcodesForApi <- gsub(" ", "", postcodesForApi)

      apiReturn <- httr::GET(url = paste0("https://api.postcodes.io/postcodes/", postcodesForApi))
      apiJson <- suppressMessages(jsonlite::fromJSON(httr::content(apiReturn, "text")))
      apiJson <- as.data.frame(unlist(apiJson)) %>%
        rename(value = "unlist(apiJson)")
      apiJson <- jsonlite::flatten(apiJson) %>%
        rownames_to_column() %>%
        mutate(postcode = post_code_list[[j,1]])
      apiJson <- apiJson %>%
        pivot_wider(names_from = rowname,
                    values_from = value)

      if(exists("postcode_api_output") == F){
        postcode_api_output <- apiJson

      } else {
        postcode_api_output <- bind_rows(postcode_api_output,
                                         apiJson)
      }

      if (j %% 100 == 0) {
        print(j)
      }

    })

    rm(apiJson,
       apiReturn)


  }

  postcode_api_output <<- postcode_api_output
}


### qualtrics survey id function ####

#' Qualtrics Survey IDs
#'
#' @return a tibble of survey ids and associated names
#' @export
#'
#' @examples
#' survey_id_function()
survey_id_function <- function(){

  surveys <- qualtRics::all_surveys() %>%
    filter(!str_detect(name,
                       "DO NOT USE")) %>%
    arrange(name)

  survey_ids <<- surveys
}

### qualtrics survey responses function ####

#' Qualtrics Survey Responses
#'
#' @return a tibble of survey responses
#' @export
#'
#' @examples
#' survey_response_function()
survey_response_function <- function(){

    surveys <- qualtRics::all_surveys() %>%
    filter(!str_detect(name,
                       "DO NOT USE")) %>%
    arrange(name)

  choice <- menu(surveys$name)

  pcf <- surveys$id[choice]

  print(pcf)

  survey_output <- qualtRics::fetch_survey(surveyID = pcf,
                           force_request = TRUE,
                           import_id = F,
                           convert = F)
  survey_responses <<- survey_output


}

### qualtrics survey questions function ####

#' Qualtrics Survey Questions
#'
#' @return a tibble of survey questions
#' @export
#'
#' @examples
#' survey_response_function()
survey_questions_function <- function(){

  surveys <- qualtRics::all_surveys() %>%
    filter(!str_detect(name,
                       "DO NOT USE")) %>%
    arrange(name)

  choice <- menu(surveys$name)

  pcf <- surveys$id[choice]

  survey_questions_output <- qualtRics::survey_questions(surveyID = pcf)

  survey_questions_output <<- survey_questions_output


}




### companies House appointments survey questions function ####

#' Companies House Appointments
#'
#' @return a tibble of appointments for a selection of officers
#' @export
#'
#' @examples
#' Requires a tibble with the officer's appointments links and a generated id column
#' officer_list <- test %>% 
#'  filter(str_detect(rowname,
#'                  "items.links.officer.appointments|items.name")) %>% 
#'  mutate(id = if_else(rowname %in% c("items.name","items.links.officer.appointments"),
#'                      1,
#'                      parse_number(str_remove_all(rowname,
#'                                                  "[:punct:]")))) %>%
#'  mutate(variable = if_else(str_detect(rowname,
#'                                       "items.name"),
#'                            "name",
#'                            if_else(str_detect(rowname,
#'                                               "items.links.officer.appointments"),
#'                                    "appointments",
#'                                    "error"))) %>% 
#'  select(-rowname) %>% 
#'  pivot_wider(names_from = variable,
#'              values_from = value) %>% 
#'  mutate(officer_id = str_remove_all(appointments,
#'                                     "/officers/|/appointments")) %>% 
#'  distinct(officer_id,
#'           .keep_all = T)
#'           
#' co_ho_appointments_function(officer_list)
co_ho_appointments_function <- function (officer_list) 
{
  if (exists("api_output_officer") == T) {
    rm(co_ho_api_output)
  }
  for (i in 1:nrow(officer_list)) {
    try({
      apiReturn <- httr::GET(url = paste0("https://api.companieshouse.gov.uk/", 
                                          officer_list$appointments[i]), 
                             authenticate(user = Sys.getenv("co_ho_api_key"), 
                                          password = ""))
      apiJson <- suppressMessages(jsonlite::fromJSON(content(apiReturn, 
                                                             "text")))
      apiJson <- as.data.frame(unlist(apiJson)) %>% rename(value = "unlist(apiJson)")
      apiJson <- jsonlite::flatten(apiJson) %>% rownames_to_column() %>% 
        mutate(officer = officer_list$officer_id[i])
      if (exists("api_output_officer") == F) {
        api_output_officer <- apiJson
      }
      else {
        api_output_officer <- bind_rows(api_output_officer, 
                                        apiJson)
      }
      if (i%%600 == 0) {
        Sys.sleep(301)
      }
      else if (i%%100 == 0) {
        print(i)
      }
    })
    rm(apiJson, apiReturn)
  }
  return(api_output_officer)
  co_ho_output_officer_appoints <<- api_output_officer
}



### lm summary dataframe function ####

#' lm() summary dataframe function
#'
#' @return a tibble of releavtn output from the summary function for a linear model
#' @export
#'
#' @examples
#' Requires a lm model
#' summary_table <- summary(linear_model) %>% 
#'  lm_summary_table_function()
lm_summary_table_function <- function(summary_table){
  
  co_effs <- summary_table$coefficients %>% 
    as_tibble(.name_repair = namesFunction) %>% 
    mutate(Estimate = round(Estimate,
                            10),
           StdError = round(StdError,
                            10))
  
  if (str_detect(as.character(summary_table$call$formula)[[3]],
                 "\\+")==T) {
    
    model_variable_names <- as.character(summary_table$call$formula) %>% 
      tibble() %>%
      rename(variable = 1) %>% 
      filter(!variable %in% c("~",
                              "values")) %>% 
      mutate(variable = str_replace_all(variable,
                                        "\\+",
                                        "&")) %>%
      separate_wider_delim(variable,
                           names = c("f","s","t","fo","fi","si","se","ei","ni","te"),
                           delim = "&",
                           too_few = "align_start") %>% 
      pivot_longer(1:ncol(.)) %>% 
      filter(!is.na(value)) %>% 
      select(variable = value,
             -name) %>% 
      mutate(variable = str_squish(variable))
    
    variable_names <- tibble(variable = "intercept") %>% 
      bind_rows(model_variable_names)
    
  } else {
    
    variable_names <- tibble(variable = "intercept") %>% 
      bind_rows(as.character(summary_table$call$formula) %>% 
                  tibble() %>%
                  rename(variable = 1) %>% 
                  filter(!variable %in% c("~",
                                          "values")))
    
  }
  
  output <- variable_names %>% 
    bind_cols(co_effs) %>% 
    mutate(signif_level = if_else(PrT < 0.001,
                                  "***",
                                  if_else(PrT < 0.01,
                                          "**",
                                          if_else(PrT < 0.05,
                                                  "*",
                                                  if_else(PrT < 0.1,
                                                          ".",
                                                          " "))))) %>% 
    mutate(Estimate = paste(Estimate,
                            signif_level,
                            sep = " ")) %>% 
    mutate(StdError = paste0("(",StdError,")")) %>% 
    select(-c(TValue,
              PrT,
              signif_level)) %>% 
    pivot_longer(2:3) %>% 
    select(-name)
  
  
  return(output)
  
}

### update useful functions function ####

#' update usefulfunctions
#'
#' @return the lastest version of the package installed from github
#' @export
#'
#' @examples
#' Will overwrite current installed version but will not update out of date dependancies
#' update_useful_functions()
update_useful_functions <- function(){
  
  detach("package:UsefulFunctions", unload=TRUE)
  devtools::install_github("jamesLSI/UsefulFunctions",
                           force = TRUE,
                           upgrade = "never")
}
