options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))


### names function ####

namesFunction <- function(nms) janitor::make_clean_names(nms, case = "upper_camel")

### root folder ####
rootFolder <- "~/"

### data warehouse ####


# rootFolder <- "C:/Users/jcur01/OneDrive - UKRI/R/"

dataWarehouseFunction <- function(){

  # source("C:/Users/jcur01/OneDrive - UKRI/R/dwCreds.R",
  #        chdir = T)

  con <- DBI::dbConnect(drv      = RMySQL::MySQL(),
                        username = Sys.getenv("usernameDw"),
                        password = Sys.getenv("passwordDw"),
                        host     = "dw-prod.cluster-csfwpi01op01.eu-west-2.rds.amazonaws.com",
                        port     = 3306,
                        dbname   = "dw")

  all_tables <- DBI::dbListTables(con)

  tables <- tibble(name = c("Applications Summaries",
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
                            "Check Tables"))

  choice <- menu(tables$name)

  tableChoice <- tables$name[choice]

  start <- Sys.time()

  if (tableChoice == "Applications Summaries") {

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

  } else if (tableChoice == "Assessor Comments") {

    print("This could take over 25 mins")

    output <- DBI::dbReadTable(con, "vw_IFSAssessorResponse ") %>%
      mutate(ApplicationID = as.character(ApplicationID))

    assessor_comments <<- output

  } else if (tableChoice == "Application Scores") {

    print("This could take over 25 mins")

    output <- DBI::dbReadTable(con, "vw_IFSAssessorScores_data") %>%
      mutate(ApplicationID = as.character(ApplicationID))

    application_scores <<- output

  } else if (tableChoice == "Application Questions") {

    print("This could take up to 7 minutes")

    output <- DBI::dbReadTable(con, "vw_IFSApplicationResponses_data") %>%
      mutate(ApplicationID = as.character(ApplicationID))

    application_questions <<- output

  } else if (tableChoice == "Spend and Forecast") {

    print("This could take up to 4 minutes")
    output <- DBI::dbReadTable(con, "vw_Dashboard_ProjectParticipantPeriodLevel")
    spend_forecast <<- output

  } else if (tableChoice == "Projects") {

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

  } else if (tableChoice == "Monitoring") {

    print("This could take up to 2 minutes")

    output <- DBI::dbReadTable(con, "vw_Dashboard_IFSPA_MonitoringData")

    monitoring_data <<- output

  } else if (tableChoice == "Competitions") {

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

  } else if (tableChoice == "Change Requests") {

    print("This could take up to a minute")

    output <- DBI::dbReadTable(con, "vw_Dashboard_IFSPA_ProjectChangeRequest")

    change_request_data <<- output

  } else if (tableChoice == "Trans Data") {

    print("This could take up to a minute")

    # output <- DBI::dbReadTable(con, "vw_report_publictransparencydata")
    output <- DBI::dbReadTable(con, "mv_report_publictransparencydata") %>%
      mutate(ProjectNumber = as.character(ProjectNumber))

    trans_data <<- output

  } else if (tableChoice == "Funders") {

    print("This could take up to a minute")

    output <- DBI::dbReadTable(con, "vw_funderspanel_data")

    funders_data <<- output

  } else if (tableChoice == "Claims / Forecast") {

    print("This could take up to 5 minutes")

    output <- DBI::dbReadTable(con, "vw_Dashboard_ProjectParticipantPeriodLevel")

    claims_forecast_data <<- output

  } else if (tableChoice == "EDI") {

    print("This could take up to a minutes")

    output <- DBI::dbReadTable(con, "vw_Sensitive_EDIData_JC")
    question_id <- read_csv("C:/Users/jcur01/OneDrive - UKRI/R/dataWarehouse/edi_questions_id.csv")
    output <- output %>%
      left_join(question_id)

    edi_data <<- output

  } else if (tableChoice == "EDI Survey Monkey") {

    print("This could take up to a minutes")

    output <- DBI::dbReadTable(con, "vw_Sensitive_EDIData_JC")
    question_id <- read_csv("C:/Users/jcur01/OneDrive - UKRI/R/dataWarehouse/edi_questions_id.csv")
    output <- output %>%
      left_join(question_id)

    edi_data <<- output

  } else if (tableChoice == "PCF") {

    print("This could take up to a minutes")

    output <- DBI::dbReadTable(con, "mv_iuk_projectcompletionform")

    pcf_data <<- output

  } else if (tableChoice == "Check Tables") {

    print(all_tables)
  }

  end <- Sys.time()
  print(end-start)

}


dataWarehouse_function_specific <- function(table_choice = "Tables"){

  # source("C:/Users/jcur01/OneDrive - UKRI/R/dwCreds.R",
  #        chdir = T)

  con <- DBI::dbConnect(drv      = RMySQL::MySQL(),
                        username = Sys.getenv("usernameDw"),
                        password = Sys.getenv("passwordDw"),
                        host     = "dw-prod.cluster-csfwpi01op01.eu-west-2.rds.amazonaws.com",
                        port     = 3306,
                        dbname   = "dw")

  all_tables <- DBI::dbListTables(con)

  tables <- tibble(name = c("Applications Summaries",
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
                            "Warehouse",
                            "Check Tables"))


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
    question_id <- read_csv("C:/Users/jcur01/OneDrive - UKRI/R/dataWarehouse/edi_questions_id.csv")
    output <- output %>%
      left_join(question_id)

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

### definitions ####
smes <- c("Micro",
          "Small",
          "Medium")

core <- c("Collaborative R&D",
          "CR&D Bilateral",
          "Feasibility Studies",
          "GRD Development of Prototype",
          "GRD Proof of Concept",
          "GRD Proof of Market",
          "Knowledge Transfer Partnership",
          "Study")

### trans data function ####

transDataFunction <- function() {
  link <- read_html("https://www.ukri.org/publications/innovate-uk-funded-projects-since-2004/") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_extract('^http.+lsx$') %>%
    .[which(!is.na(.))]

  GET(link[[1]], write_disk(tf <- tempfile(fileext = ".xlsx")))

  namesFunction <- function(nms) janitor::make_clean_names(nms, case = "upper_camel")

  transData <- read_excel(tf, .name_repair = namesFunction)

  transData <- transData %>%
    mutate(AddressRegion = if_else(AddressRegion == "Yorkshire and the Humber",
                                   "Yorkshire and The Humber",
                                   AddressRegion)) %>%
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

### sic code function ####

sic_code_function <- function(crn_list){
  apiKey <- "V5ZW_NIGIu0ZAgw2n7YX28JFhBrN_oljEafaDSBX"
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
      apiReturn <- GET(url = paste0("https://api.companieshouse.gov.uk/company/", crn_list[i,1]),
                       authenticate(user = apiKey, password = ""))
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

  co_ho_api_output <<- co_ho_api_output
}

### officer code function ####

officer_code_function <- function(crn_list){
  apiKey <- "V5ZW_NIGIu0ZAgw2n7YX28JFhBrN_oljEafaDSBX"
  if(exists("api_output_officer") == T){
    rm(co_ho_api_output)
  }
  for (i in 1:nrow(crn_list)) {
    try({
      apiReturn <- GET(url = paste0("https://api.companieshouse.gov.uk/company/", crn_list[i,1], "/officers"),
                       authenticate(user = apiKey, password = ""))
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

  api_output_officer <<- api_output_officer
}

### significant control function ####

signif_control_code_function <- function(crn_list){
  apiKey <- "V5ZW_NIGIu0ZAgw2n7YX28JFhBrN_oljEafaDSBX"
  if(exists("api_output_officer") == T){
    rm(co_ho_api_output)
  }
  for (i in 1:nrow(crn_list)) {
    try({
      apiReturn <- GET(url = paste0("https://api.companieshouse.gov.uk/company/", crn_list[i,1], "/persons-with-significant-control"),
                       authenticate(user = apiKey, password = ""))
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
  signif_control_output <<- api_output_officer
}

### post code function ####

postcode_function <- function(post_code_list){

  for (j in 1:nrow(post_code_list)) {

    try({

      postcodesForApi <- post_code_list[[j,1]]

      postcodesForApi <- gsub(" ", "", postcodesForApi)

      apiReturn <- GET(url = paste0("https://api.postcodes.io/postcodes/", postcodesForApi))
      apiJson <- suppressMessages(jsonlite::fromJSON(content(apiReturn, "text")))
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


### qualtrics function ####
survey_id_function <- function(){

  surveys <- all_surveys() %>%
    filter(!str_detect(name,
                       "DO NOT USE")) %>%
    arrange(name)

  surveys <<- surveys
}

survey_response_function <- function(){

    surveys <- all_surveys() %>%
    filter(!str_detect(name,
                       "DO NOT USE")) %>%
    arrange(name)

  choice <- menu(surveys$name)

  pcf <- surveys$id[choice]

  print(pcf)

  mysurvey <- fetch_survey(surveyID = pcf,
                           force_request = TRUE,
                           import_id = F,
                           convert = F)
  survey_output <<- mysurvey


}

survey_questions_function <- function(){

  surveys <- all_surveys() %>%
    filter(!str_detect(name,
                       "DO NOT USE")) %>%
    arrange(name)

  choice <- menu(surveys$name)

  pcf <- surveys$id[choice]

  mysurvey <- survey_questions(surveyID = pcf)

  survey_questions <<- mysurvey


}

# ukri_data_hub_con <- DBI::dbConnect(odbc::odbc(),
#                  Driver = "ODBC Driver 17 for SQL Server",
#                  Server = "ausorizmidb01.fbf0f5dae340.database.windows.net",
#                  Authentication = "ActiveDirectoryIntegrated")

### colour palette ####
primaryPurple <- "#4C1354"
quantumBlue <- "#003255"
environGreen <- "#00623E"
grapheneGrey <- "#3C465F"
codeRed <- "#9E0833"
ultraViolet <- "#BD4E97"
skyBlue <- "#4da0d9"
luminousGreen <- "#2A926D"
compositeGrey <- "#9699a5"
infraRed <- "#DF5054"
