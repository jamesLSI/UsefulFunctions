### Janus ####

#' Janus Function
#'
#' @param table_choice is the Janus equivalent data warehouse table to return, leaving this blank will return the available tables in the console
#'
#' @return a data frame
#' @export
#'
#' @examples janus_function_specific("Applications")

janus_function_specific <- function(table_choice = "Tables"){
  ### set up and tables ####
  jcon <- DBI::dbConnect(drv=RPostgres::Postgres(),
                         port     = 5432,
                         user=Sys.getenv("usernameJanus"),
                         password=Sys.getenv("passwordJanus"),
                         host=Sys.getenv("urlJanus"),
                         dbname   = "postgres")
  
  all_tables <- DBI::dbListTables(jcon)
  
  tables <- tibble::tibble(name = c("Applications",
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
                                    # "EDI",
                                    "EDI Pivot",
                                    "EDI Non Pivot",
                                    "PCF",
                                    "Janus",
                                    "VOPs",
                                    "ID Changes"))
  start <- Sys.time()
  
  if (table_choice == "Applications") {
    ### Appliaction Data ####
    print("This could take quite a long time")
    
 
    output <- DBI::dbGetQuery(jcon,"SELECT
  \"ApplicationKey\",
  applicant_data.\"ApplicationID\",
  \"DurationInMonths\",
  application_data.\"ApplicationTitle\",
  \"ProjectSummary\",
  \"PublicDescription\",
  \"Scope\",
  \"StartDate\",
  \"ApplicationStatus\",
  \"ResearchCategory\",
  application_data.\"InnovationArea\",
  \"CompetitionKey\",
  \"SubmittedDate\",
  \"FundingDecision\",
  \"FileEntryName\",
  \"Completion\",
  application_data.\"Resubmission\",
  \"PreviousApplicationNumber\",
  application_data.\"ProjectSetUpStatus\",
  \"CompetitionID\",
  application_data.\"CompetitionName\",
  \"OPEN_DATE\",
  \"ApplicantKey\",
  \"ApplicantOrganisationID\",
  \"ApplicantOrganisationName\",
  \"ApplicantOrganisationRegisteredAddress_Line1\",
  \"ApplicantOrganisationRegisteredAddress_Line2\",
  \"ApplicantOrganisationRegisteredAddress_Line3\",
  \"ApplicantOrganisationRegisteredTown\",
  \"ApplicantOrganisationRegisteredPostcode\",
  \"ApplicantOrganisationRegisteredCounty\",
  -- \"ApplicantOrganisationRegisteredCountry\",
  \"ApplicantOrganisationInternationalLocation\",
  \"ApplicantOrganisationWorkPostcode\",
  \"ApplicantOrganisationWorkCountry\",
  \"ApplicantOrganisationWorkRegion\",
  \"ApplicantOrganisationWorkLongitude\",
  \"ApplicantOrganisationWorkLatitude\",
  \"ApplicantOrganisationWorkNuts\",
  \"IsLead\",
  \"ApplicantOrganisationType\",
  \"ApplicantOrganisationSize\",
  \"ApplicantCompanyHouseNumber\",
  \"MainContactNameIFS\",
  \"MainContactEmailIFS\",
  \"CostAmount\",
  \"FundingSought\",
  to_char(\"OPEN_DATE\", 'YYYY-MM-DD') AS \"OPEN_DATE_clean\"
  -- \"previous_application_id\"                                   
FROM
  (
    SELECT
      \"CompetitionKey\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'OPEN_DATE'
      ) AS \"OPEN_DATE\"
    FROM
      (
        SELECT
          *
        FROM
          (
            SELECT DISTINCT
              ON (f.\"CompetitionKey\", d.type) f.*,
              d.type
            FROM
              (
                SELECT
                  *
                FROM
                  fact_competitionmilestone
                ORDER BY
                  \"localDate\" DESC
              ) AS f
              LEFT JOIN (
                SELECT
                  *
                FROM
                  dim_competitionmilestone
                WHERE
                  \"IsActive\" = TRUE
              ) AS d USING (\"CompetitionMilestoneKey\")
          ) AS firstjoin
      ) AS firstpivot
    GROUP BY
      \"CompetitionKey\"
  ) AS grouped
  LEFT JOIN (
    SELECT
      *
    FROM
      dim_competition
    WHERE
      \"IsActive\" = TRUE
  ) AS compdata USING (\"CompetitionKey\")
  LEFT JOIN (
    SELECT
      *
    FROM
      dim_application
    WHERE
      \"IsActive\" = TRUE
  ) AS application_data USING (\"CompetitionKey\")
  LEFT JOIN (
    SELECT
      *
    FROM
      dim_applicant
    WHERE
      \"IsActive\" = TRUE
  ) AS applicant_data USING (\"ApplicationKey\", \"CompetitionKey\")
  LEFT JOIN (
    SELECT
      *
    FROM
      fact_applicationsummary
  ) AS application_funding USING (
    \"ApplicantKey\",
    \"ApplicationKey\",
    \"CompetitionKey\"
  )"
                              
                                        ) 
    
                                         
                                   
    janus_application_data <<- output
    
  } else if (table_choice == "Assessor Comments") {
    ### assessor comments ####
    print("This could take a long time")
    
    output <- DBI::dbGetQuery(jcon, "SELECT c.\"CompetitionID\" AS competitionid, 

   c.\"CompetitionName\" AS competitionname, 

   a.\"ApplicationID\" AS applicationid, 

   ass.\"AssessorNameIFS\" AS assessornameifs, 

   ass.\"AssessorEmailIFS\" AS assessoremailifs, 

   aq.\"QuestionNo\" AS questionno, 

   ass.\"AssessorResponse\" AS assessorresponse, 

   ass.\"WordCount\" AS wordcount, 

   ass.\"Status\" AS status 

  FROM dim_application a 

    LEFT JOIN fact_assessorresponses ass ON a.\"ApplicationKey\" = ass.\"ApplicationKey\" 

    LEFT JOIN dim_applicationquestion aq ON ass.\"ApplicationQuestionKey\" = aq.\"ApplicationQuestionKey\" 

    LEFT JOIN dim_competition c ON c.\"CompetitionKey\" = a.\"CompetitionKey\" 

  WHERE a.\"IsActive\" = true AND aq.\"IsActive\" = true") 
    
    janus_assessor_comments <<- output
    
  } else   if (table_choice == "Application Scores") {
    if(comp_choice == ""){
      ### appication scores ####
      print("This could take a long time")
      output <- DBI::dbGetQuery(jcon, "SELECT c.\"CompetitionID\" AS competitionid, 
        c.\"CompetitionName\" AS competitionname, 

        a.\"ApplicationID\" AS applicationid, 
    
        ass.\"AssessorNameIFS\" AS assessornameifs, 
    
        ass.\"AssessorEmailIFS\" AS assessoremailifs, 
    
        aq.\"QuestionNo\" AS questionno, 
    
        ass.\"AssessorScore\" AS assessorscore, 
    
        ass.\"Status\" AS status 
    
       FROM dim_application a 
    
         LEFT JOIN fact_assessorscores ass ON a.\"ApplicationKey\" = ass.\"ApplicationKey\" 
    
         LEFT JOIN dim_applicationquestion aq ON ass.\"ApplicationQuestionKey\" = aq.\"ApplicationQuestionKey\" 
    
         LEFT JOIN dim_competition c ON c.\"CompetitionKey\" = a.\"CompetitionKey\" 
    
      WHERE a.\"IsActive\" = true AND aq.\"IsActive\" = true") 
      
      test_application_scores <<- output
      
    } else {
      
      print(comp_choice)
      output <- DBI::dbGetQuery(jcon, 
                                paste0("SELECT c.\"CompetitionID\" AS competitionid, 
      c.\"CompetitionName\" AS competitionname, 
  
      a.\"ApplicationID\" AS applicationid, 
  
      ass.\"AssessorNameIFS\" AS assessornameifs, 
  
      ass.\"AssessorEmailIFS\" AS assessoremailifs, 
  
      aq.\"QuestionNo\" AS questionno, 
  
      ass.\"AssessorScore\" AS assessorscore, 
  
      ass.\"Status\" AS status 
    
    FROM dim_application a 

     LEFT JOIN fact_assessorscores ass ON a.\"ApplicationKey\" = ass.\"ApplicationKey\" 

     LEFT JOIN dim_applicationquestion aq ON ass.\"ApplicationQuestionKey\" = aq.\"ApplicationQuestionKey\" 

     LEFT JOIN dim_competition c ON c.\"CompetitionKey\" = a.\"CompetitionKey\" 
    
    WHERE a.\"IsActive\" = true AND aq.\"IsActive\" = true AND \"CompetitionID\" =", comp_choice))
      test_application_scores <<- output
      
    }
    
  } else if (table_choice == "Application Questions") {
    ### application questions ####
    print("This could take a fair length of time")
    
    output <- DBI::dbGetQuery(jcon, "SELECT a.\"ApplicationID\", 

 

    aq.\"QuestionNo\", 

 

    aq.\"QuestionShortName\", 

 

    aq.\"QuestionName\", 

 

    ass.\"ApplicationResponse\", 

 

    ass.\"WordCount\" 

 

   FROM dim_application a 

 

     LEFT JOIN fact_applicationresponses ass ON a.\"ApplicationKey\" = ass.\"ApplicationKey\" 

 

     LEFT JOIN dim_applicationquestion aq ON ass.\"ApplicationQuestionKey\" = aq.\"ApplicationQuestionKey\" 

 

  WHERE a.\"IsActive\" = true AND aq.\"IsActive\" = true ") 
    
    janus_application_questions <<- output
    
  } else if (table_choice == "Projects") {
    ### projects data ####
    print("This could take up to 2 minutes")
    
output <- dbGetQuery(jcon,
                  "SELECT  

        \"p\".\"ActivityCode\" AS \"ActivityCode\", 
p.\"ActivityName\",
p.\"CompetitionType\",
p.\"LegacyProjectNumber\",
p.\"ApplicationID\",
p.\"LegacyTPNumber\",
p.\"ProjectSummary\",
p.\"PublicDescription\",
p.\"ProjectDuration\",
p.\"NumberofPeriods\",
p.\"ClaimFrequency\",
p.\"MonitoringReportFrequency\",
p.\"MonitoringLevel\",
p.\"IsFlaggedProject\",
p.\"InnovationLeadName\",
p.\"MonitoringOfficerName\",
p.\"ProjectManagerName\",
p.\"CostCentreNumber\",
p.\"ISCFFunded\",
p.\"CreatedDate\",
p.\"IsOkToPublish\",
p.\"ProjectSource\",
p.\"IsInACC\",
p.\"IsInGrants\",


        \"p\".\"OpsCommitmentYear\" AS \"OpsCommitmentYear\", 

        \"c\".\"CompetitionID\" AS \"CompetitionID\", 

        \"p\".\"CompetitionCode\" AS \"CompetitionCode\", 

        \"p\".\"CompetitionName\" AS \"CompetitionName\", 

        \"p\".\"FundersPanelDate\" AS \"FundersPanelDate\", 

        \"p\".\"Product\" AS \"Product\", 

        \"p\".\"Theme\" AS \"Theme\", 

        \"p\".\"Area\" AS \"Area\", 

        \"p\".\"Sector\" AS \"Sector\", 

        \"p\".\"GrandChallenge\" AS \"GrandChallenge\", 

        CONCAT_WS(' - ', 

                \"p\".\"CostCentreNumber\", 

                \"p\".\"CostCentre\") AS \"CostCentre\", 

        \"p\".\"ProjectNumber\" AS \"ProjectNumber\", 

        \"pp\".\"ParticipantOrganisationName\" AS \"ParticipantOrganisationName\", 

        (CASE 

            WHEN (\"pp\".\"IsLeadParticipant\") THEN 'Lead' 

            ELSE 'Collaborator' 

        END) AS \"IsLeadParticipant\", 

        \"pp\".\"ParticipantStatus\" AS \"ParticipantStatus\", 

        \"p\".\"ProjectTitle\" AS \"ProjectTitle\", 

        \"p\".\"ProjectStatus\" AS \"ProjectStatus\", 

        \"p\".\"ProjectStartDate\" AS \"ProjectStartDate\", 

        \"p\".\"ProjectEndDate\" AS \"ProjectEndDate\", 

        \"olc\".\"OfferCost\" AS \"OfferCost\", 

        \"olc\".\"OfferGrant\" AS \"OfferGrant\", 

        \"pp\".\"AwardRate\" AS \"AwardRate\", 

        (COALESCE(\"cl\".\"ClaimedToDate\", 0) + COALESCE(\"prepayment\".\"PrepaymentGrant\", 0)) AS \"ClaimedToDate\", 

        COALESCE(\"cl\".\"ClaimedToDate\", 0) AS \"ClaimedToDateWithoutPrepayments\", 

        COALESCE(\"prepayment\".\"PrepaymentGrant\", 0) AS \"PrepaymentsToDate\", 

        COALESCE(\"pp\".\"ParticipantOrganisationSize\", 

                'Unknown') AS \"ParticipantOrganisationSize\", 

        COALESCE(\"pp\".\"ParticipantOrganisationType\", 

                'Unknown') AS \"ParticipantOrganisationType\", 

        \"pp\".\"CompanyRegistrationNumber\" AS \"CompanyRegistrationNumber\", 

        \"pp\".\"MainContactName\" AS \"MainContactName\", 

        \"pp\".\"MainContactEmail\" AS \"MainContactEmail\", 

        \"pper\".\"PhoneWork\" AS \"MainContactPhone\", 

        \"pp\".\"ParticipantAddressLine\" AS \"ParticipantAddressLine\", 

        \"pp\".\"ParticipantTown\" AS \"ParticipantTown\", 

        \"pp\".\"ParticipantCounty\" AS \"ParticipantCounty\", 

        \"pp\".\"ParticipantPostcode\" AS \"ParticipantPostcode\", 

        COALESCE(\"addr\".\"Region\", 'Unknown') AS \"Region\", 

        (CASE 

            WHEN (\"addr\".\"Region\" IN ('Scotland' , 'Wales', 'Northern Ireland')) THEN 'n/a' 

            ELSE COALESCE(\"addr\".\"LEP\", 'Unknown') 

        END) AS \"LEP\", 

        \"pp\".\"ACCProjectParticipantName\" AS \"ACCSourceID\", 

        (CASE 

            WHEN (\"pp\".\"ConnectProjectParticipantID\" <> -(1)) THEN \"pp\".\"ConnectProjectParticipantID\" 

            ELSE NULL 

        END) AS \"GrantsSourceID\", 

        \"pp\".\"ApplicantKey\" AS \"ApplicantKey\", 

        \"pp\".\"IsFlaggedParticipant\" AS \"IsFlaggedParticipant\", 

        \"pp\".\"IsOktoPublish\" AS \"IsOktoPublish\", 

        \"acc\".\"SICCode\" AS \"SICCode\" 

    FROM 

        ((((((((\"dim_project\" \"p\" 

        LEFT JOIN \"dim_projectparticipant\" \"pp\" ON ((\"p\".\"ProjectKey\" = \"pp\".\"ProjectKey\"))) 

        LEFT JOIN \"dim_address\" \"addr\" ON (((\"pp\".\"AddressKey\" = \"addr\".\"AddressKey\") 

            AND (\"addr\".\"IsActive\")))) 

        LEFT JOIN \"dim_person\" \"pper\" ON (((\"pp\".\"MainContactPersonKey\" = \"pper\".\"PersonKey\") 

            AND (\"pper\".\"IsActive\")))) 

        LEFT JOIN \"dim_account\" \"acc\" ON (((\"pp\".\"AccountKey\" = \"acc\".\"AccountKey\") 

            AND (\"acc\".\"IsActive\")))) 

        LEFT JOIN \"dim_competition\" \"c\" ON (((\"p\".\"CompetitionKey\" = \"c\".\"CompetitionKey\") 

            AND (\"c\".\"IsActive\")))) 

        LEFT JOIN (SELECT DISTINCT 

            \"olc\".\"ProjectParticipantKey\" AS \"ProjectParticipantKey\", 

                \"olc\".\"ParticipantCost\" AS \"OfferCost\", 

                \"olc\".\"ParticipantGrant\" AS \"OfferGrant\" 

        FROM 

            (\"fact_offerlettercost\" \"olc\" 

        JOIN \"dim_offerletter\" \"ol\" ON (((\"olc\".\"OfferLetterKey\" = \"ol\".\"OfferLetterKey\") 

            AND (\"ol\".\"IsActive\") 

            AND (\"ol\".\"IsLatest\"))))) \"olc\" ON ((\"pp\".\"ProjectParticipantKey\" = \"olc\".\"ProjectParticipantKey\"))) 

        LEFT JOIN (SELECT  

            \"fact_claimsummary\".\"ProjectParticipantKey\" AS \"ProjectParticipantKey\", 

                SUM(\"fact_claimsummary\".\"TotalClaimGrantApprovedForPayment\") AS \"ClaimedToDate\" 

        FROM 

            \"fact_claimsummary\" 

        GROUP BY \"fact_claimsummary\".\"ProjectParticipantKey\") \"cl\" ON ((\"pp\".\"ProjectParticipantKey\" = \"cl\".\"ProjectParticipantKey\"))) 

        LEFT JOIN (SELECT  

            \"cs\".\"ProjectParticipantKey\" AS \"ProjectParticipantKey\", 

                SUM(\"prep\".\"PrepaymentGrant\") AS \"PrepaymentGrant\" 

        FROM 

            (\"fact_claimitem\" \"prep\" 

        JOIN \"fact_claimsummary\" \"cs\" ON ((\"prep\".\"ClaimSummaryKey\" = \"cs\".\"ClaimSummaryKey\"))) 

        WHERE 

            ((\"prep\".\"PrepaymentGrant\" <> 0) 

                AND (\"prep\".\"PrepaymentClaimStatusName\" IN ('Approved' , 'Paid'))) 

        GROUP BY \"cs\".\"ProjectParticipantKey\") \"prepayment\" ON ((\"pp\".\"ProjectParticipantKey\" = \"prepayment\".\"ProjectParticipantKey\"))) 

    WHERE 

        ((\"p\".\"IsActive\") 

            AND (\"p\".\"IsInACC\") 

            AND (\"pp\".\"IsActive\") 

            AND (NOT \"p\".\"IsTestProject\")) 

    ORDER BY \"p\".\"ActivityCode\" , \"p\".\"ProjectNumber\" , \"pp\".\"IsLeadParticipant\" DESC , \"pp\".\"ParticipantOrganisationName\"")
    
    janus_projects_data <<- output
    
  } else if (table_choice == "Monitoring") {
    ### monitoring scores ####
    print("This could take up to 2 minutes")
    
    output <- DBI::dbGetQuery(jcon, "SELECT  

        p.\"ProjectNumber\" AS \"ProjectNumber\", 

        p.\"ProjectTitle\" AS \"ProjectTitle\", 

        p.\"ProjectStatus\" AS \"ProjectStatus\", 

        p.\"CompetitionName\" AS \"CompetitionName\", 

        p.\"Area\" AS \"Area\", 

        p.\"ActivityName\" AS \"ActivityName\", 

        p.\"MonitoringOfficerName\" AS \"MonitoringOfficerName\", 

        per.\"EmailAddress\" AS \"MonitoringOfficerEmail\", 

        fmr.\"IsLatestReport\" AS \"IsLatestReport\", 

        fmr.\"ProjectPeriodNumber\" AS \"ProjectPeriodNumber\", 

        fmr.\"ProjectPeriodEndDate\" AS \"ProjectPeriodEndDate\", 

        fmr.\"MonitoringReportStatus\" AS \"MonitoringReportStatus\", 

        fmr.\"MonitoringReportSubmittedDate\" AS \"MonitoringReportSubmittedDate\", 

        fmr.\"MonitoringReportApprovedDate\" AS \"MonitoringReportApprovedDate\", 

        fmr.\"SummaryComments\" AS \"SummaryComments\", 

        fmr.\"IssuesandActionsComments\" AS \"IssuesandActionsComments\", 

        dmq_scope.\"QuestionColour\" AS \"ScopeColour\", 

        dmq_scope.\"QuestionScore\" AS \"ScopeScoreNumber\", 

        fmr.\"ScopeComments\" AS \"ScopeComments\", 

        dmq_scope.\"QuestionText\" AS \"ScopeScoreText\", 

        dmq_time.\"QuestionColour\" AS \"TimeColour\", 

        dmq_time.\"QuestionScore\" AS \"TimeScoreNumber\", 

        fmr.\"TimeComments\" AS \"TimeComments\", 

        dmq_time.\"QuestionText\" AS \"TimeScoreText\", 

        dmq_cost.\"QuestionColour\" AS \"CostColour\", 

        dmq_cost.\"QuestionScore\" AS \"CostScoreNumber\", 

        fmr.\"CostComments\" AS \"CostComments\", 

        dmq_cost.\"QuestionText\" AS \"CostScoreText\", 

        dmq_exploitation.\"QuestionColour\" AS \"ExploitationColour\", 

        dmq_exploitation.\"QuestionScore\" AS \"ExploitationScoreNumber\", 

        fmr.\"ExploitationComments\" AS \"ExploitationComments\", 

        dmq_exploitation.\"QuestionText\" AS \"ExploitationScoreText\", 

        dmq_riskmanagement.\"QuestionColour\" AS \"RiskManagementColour\", 

        dmq_riskmanagement.\"QuestionScore\" AS \"RiskManagementScoreNumber\", 

        fmr.\"RiskManagementComments\" AS \"RiskManagementComments\", 

        dmq_riskmanagement.\"QuestionText\" AS \"RiskManagementScoreText\", 

        dmq_projectplanning.\"QuestionColour\" AS \"ProjectPlanningColour\", 

        dmq_projectplanning.\"QuestionScore\" AS \"ProjectPlanningScoreNumber\", 

        fmr.\"ProjectPlanningComments\" AS \"ProjectPlanningComments\", 

        dmq_projectplanning.\"QuestionText\" AS \"ProjectPlanningScoreText\", 

        'IFS PA' AS \"Source\" 

    FROM 

        ((((((((dim_project p 

        LEFT JOIN fact_monitoringreport fmr ON ((p. \"ProjectKey\" = fmr. \"ProjectKey\"))) 

        LEFT JOIN dim_monitoringquestion dmq_scope ON (((fmr. \"ScopeMonitoringReportQuestionKey\" = dmq_scope. \"MonitoringQuestionKey\") 

            AND (dmq_scope. \"IsActive\")))) 

        LEFT JOIN dim_monitoringquestion dmq_time ON (((fmr. \"TimeMonitoringReportQuestionKey\" = dmq_time. \"MonitoringQuestionKey\") 

            AND (dmq_time.\"IsActive\")))) 

        LEFT JOIN dim_monitoringquestion dmq_cost ON (((fmr. \"CostMonitoringReportQuestionKey\" = dmq_cost. \"MonitoringQuestionKey\") 

            AND (dmq_cost.\"IsActive\")))) 

        LEFT JOIN dim_monitoringquestion dmq_exploitation ON (((fmr. \"ExploitationMonitoringReportQuestionKey\" = dmq_exploitation. \"MonitoringQuestionKey\") 

            AND (dmq_exploitation. \"IsActive\")))) 

        LEFT JOIN dim_monitoringquestion dmq_riskmanagement ON (((fmr. \"RiskManagementMonitoringReportQuestionKey\" = dmq_riskmanagement. \"MonitoringQuestionKey\") 

            AND (dmq_riskmanagement. \"IsActive\")))) 

        LEFT JOIN dim_monitoringquestion dmq_projectplanning ON (((fmr. \"ProjectPlanningMonitoringReportQuestionKey\" = dmq_projectplanning. \"MonitoringQuestionKey\") 

            AND (dmq_projectplanning. \"IsActive\")))) 

        LEFT JOIN dim_person per ON (((p. \"MonitoringOfficerPersonKey\" = per. \"PersonKey\") 

            AND (per. \"IsActive\")))) 

    WHERE 

        ((p. \"IsActive\") 

            AND (p. \"ProjectStatus\" IS NOT NULL) 

            AND (fmr. \"MonitoringReportStatus\" IS NOT NULL) 

            AND (p. \"IsInACC\")) 

    ORDER BY p. \"ProjectNumber\" , fmr. \"ProjectPeriodNumber\" ")
    
    janus_monitoring_data <<- output
    
  } else if (table_choice == "Competitions") {
    ### competitions ####
    print("This could take up to a minute")
    
    output <- dbGetQuery(jcon,
                         "SELECT
  \"CompetitionKey\",
\"OPEN_DATE\",
\"SUBMISSION_DATE\",
\"NOTIFICATIONS\",
\"RELEASE_FEEDBACK\",
\"FUNDERS_PANEL\",
\"PANEL_DATE\",
\"ASSESSMENT_PANEL\",
\"LINE_DRAW\",
\"ASSESSOR_DEADLINE\",
\"ASSESSOR_ACCEPTS\",
\"ASSESSOR_BRIEFING\",
\"ALLOCATE_ASSESSORS\",
\"BRIEFING_EVENT\",
\"FEEDBACK_RELEASED\",
\"ASSESSMENT_CLOSED\",
\"ASSESSORS_NOTIFIED\",
\"REGISTRATION_DATE\",
\"CompetitionID\",
\"CompetitionName\",
\"CompetitionStatus\",
\"ExecutiveUserName\",
\"LeadTechnologistName\",
\"CompetitionType\",
\"InnovationSector\",
\"InnovationArea\",
\"CompetitionTypeActive\",
\"SetupComplete\",
\"ApplicationFinanceType\",
\"FunderName\",
\"FunderBudget\"
FROM
  (
    SELECT
      \"CompetitionKey\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'OPEN_DATE'
      ) AS \"OPEN_DATE\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'SUBMISSION_DATE'
      ) AS \"SUBMISSION_DATE\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'FUNDERS_PANEL'
      ) AS \"FUNDERS_PANEL\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'RELEASE_FEEDBACK'
      ) AS \"RELEASE_FEEDBACK\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'PANEL_DATE'
      ) AS \"PANEL_DATE\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'NOTIFICATIONS'
      ) AS \"NOTIFICATIONS\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'ASSESSMENT_PANEL'
      ) AS \"ASSESSMENT_PANEL\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'LINE_DRAW'
      ) AS \"LINE_DRAW\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'ASSESSOR_DEADLINE'
      ) AS \"ASSESSOR_DEADLINE\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'ASSESSOR_ACCEPTS'
      ) AS \"ASSESSOR_ACCEPTS\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'ASSESSOR_BRIEFING'
      ) AS \"ASSESSOR_BRIEFING\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'ALLOCATE_ASSESSORS'
      ) AS \"ALLOCATE_ASSESSORS\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'BRIEFING_EVENT'
      ) AS \"BRIEFING_EVENT\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'FEEDBACK_RELEASED'
      ) AS \"FEEDBACK_RELEASED\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'ASSESSMENT_CLOSED'
      ) AS \"ASSESSMENT_CLOSED\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'ASSESSORS_NOTIFIED'
      ) AS \"ASSESSORS_NOTIFIED\",
      MIN(\"localDate\") FILTER (
        WHERE
          type = 'REGISTRATION_DATE'
      ) AS \"REGISTRATION_DATE\"
    FROM
      (
        SELECT
          *
        FROM
          (
            SELECT DISTINCT
              ON (f.\"CompetitionKey\", d.type) f.*,
              d.type
            FROM
              (
                SELECT
                  *
                FROM
                  fact_competitionmilestone
                ORDER BY
                  \"localDate\" DESC
              ) AS f
              LEFT JOIN (
                SELECT
                  *
                FROM
                  dim_competitionmilestone
                WHERE
                  \"IsActive\" = TRUE
              ) AS d USING (\"CompetitionMilestoneKey\")
          ) AS firstjoin
      ) AS firstpivot
    GROUP BY
      \"CompetitionKey\"
  ) AS grouped
  LEFT JOIN (
    SELECT
      *
    FROM
      dim_competition
    WHERE
      \"IsActive\" = TRUE
  ) AS compdata USING (\"CompetitionKey\")
  LEFT JOIN (
    SELECT
      \"CompetitionKey\",
      \"CompetitionFunderKey\",
      funder_budget AS \"FunderBudget\"
    FROM
      bridge_competitionfunder
      ) AS bridge USING (\"CompetitionKey\")
  LEFT JOIN (
    SELECT
      \"CompetitionFunderKey\",
      funder AS \"FunderName\"
    FROM
      dim_competitionfunder
    WHERE
      \"IsActive\" = TRUE
  ) AS funder USING (\"CompetitionFunderKey\") ")
    
    janus_comps_data <<- output
    
  } else if (table_choice == "Change Requests") {
    ### project change requests ####
    print("This could take up to a minute")
    
    output <- DBI::dbGetQuery(jcon, "SELECT * FROM dim_projectchangerequest WHERE \"IsActive\"=TRUE")
    
    janus_change_request_data <<- output
    
  } else if (table_choice == "Trans Data") {
    ### transparency data ####
    print("This could take up to a minute")
    
    output <- DBI::dbGetQuery(jcon,"SELECT
  \"CompetitionReference\",
  \"CompetitionTitle\",
  \"ProgrammeTitle\",
  \"Sector\",
  \"ApplicationNumber\",
  \"ProjectNumber\",
  \"ProjectTitle\",
  regexp_replace(\"PublicDescription\", '[^ -\\ufffd\\n]', ' ') AS \"PublicDescription\",
  \"CompetitionYear\",
  \"InnovateUKProductType\",
  \"ParticipantName\",
  \"IsLeadParticipant\",
  \"CRN\",
  \"ProjectStartDate\",
  \"ProjectEndDate\",
  \"GrantOffered\",
  \"TotalCosts\",
  \"ActualSpendToDate\",
  \"ParticipantWithdrawnFromProject\",
  \"ProjectStatus\",
  \"EnterpriseSize\",
  \"Postcode\",
  \"AddressRegion\",
  \"AddressLEP\",
  \"LocalAuthority\",
  \"InMultipleLEPs\",
  \"ISCFFunded\",
  \"ConnectProjectParticipantID\",
  \"IFSPAParticipantID\",
  \"ActivityCode\"
FROM
  mv_report_publictransparencydata")
    
    output <- output |> 
      mutate(AwardOffered = GrantOffered) |> 
      mutate(ProjectNumber = as.character(ProjectNumber)) %>%
      mutate(AddressRegion = if_else(AddressRegion == "Yorkshire and the Humber",
                                     "Yorkshire and The Humber",
                                     AddressRegion)) %>%
      mutate(AddressRegion = if_else(AddressRegion == "Yorkshire",
                                     "Yorkshire and The Humber",
                                     AddressRegion)) %>%
      mutate(AddressRegion = if_else(AddressRegion == "Outside Uk",
                                     "Outside UK",
                                     AddressRegion)) %>%
      mutate(AddressRegion = if_else(AddressRegion == "Channel Islands",
                                     "Outside UK",
                                     AddressRegion)) %>%
      mutate(AddressRegion = if_else(AddressRegion == "W10 5PB",
                                     "London",
                                     AddressRegion)) %>%
      mutate(EnterpriseSizeClean = if_else(EnterpriseSize %in% c("Start-Up",
                                                                 "Sole Trader",
                                                                 "Micro/Small",
                                                                 "Micro/small",
                                                                 "Micro or small",
                                                                 "Micro/ Small",
                                                                 "Small",
                                                                 "Micro",
                                                                 "Industrial: SME",
                                                                 "SME"),
                                           "Micro/Small",
                                           if_else(EnterpriseSize %in% c("Industrial: Large"),
                                                   "Large",
                                                   if_else(EnterpriseSize %in% c("Non UK",
                                                                                 "Other",
                                                                                 "Charity",
                                                                                 "Unknown"),
                                                           "Other",
                                                           if_else(EnterpriseSize %in% c("PSO",
                                                                                         "PSRE"),
                                                                   "Public Sector",
                                                                   EnterpriseSize))))) %>%
      mutate(EnterpriseSizeClean = if_else(str_detect(tolower(ParticipantName),
                                                      "npl manage") == T,
                                           "PSRE",
                                           EnterpriseSizeClean)) %>% 
      mutate(EnterpriseClass = if_else(EnterpriseSizeClean %in% c("Micro/Small",
                                                                  "Medium",
                                                                  "SME",
                                                                  "Large",
                                                                  "Business"),
                                       "Business",
                                       "Other")) %>%
      separate(AddressLEP,
               into = c("priLep",
                        "secondaryLep"),
               sep = "; ",
               remove = F) %>%
      mutate(AddressLEP = if_else(is.na(secondaryLep),
                                  AddressLEP,
                                  priLep)) %>%
      select(-priLep) %>%
      mutate(AddressLEP = if_else(AddressLEP == "Northamptonshire",
                                  "South East Midlands",
                                  if_else(AddressLEP == "Herefordshire",
                                          "The Marches",
                                          if_else(AddressLEP == "City of London",
                                                  "London",
                                                  AddressLEP)))) %>%
      mutate(ProjectNumber = as.character(ProjectNumber)) %>%
      # filter(!ProjectStatus == "Withdrawn") %>%
      distinct(CompetitionReference,
               ProjectNumber,
               ParticipantName,
               ProjectStatus,
               CRN,
               AwardOffered,
               .keep_all = T)
    
    
    janus_trans_data <<- output
    
  } else if (table_choice == "Funders") {
    
    print("This could take up to a minute")
    
    output <- dbGetQuery(jcon,"SELECT
  \"a\".\"FPDataApplicantID\",
  \"c\".\"CompetitionCode\",
  \"c\".\"IFSCompetitionID\",
  \"c\".\"CompetitionName\",
  \"c\".\"PAFNo\",
  \"c\".\"BudgetRef\",
  \"c\".\"ActivityCode\" AS \"CompetitionActivityCode\",
  \"a\".\"ActivityCode\" AS \"ProjectActivityCode\",
  \"a\".\"InnovationLead\",
  \"c\".\"HeadofDept\",
  \"c\".\"RatifiedByDate\" AS \"FPRatifiedByDate\",
  \"c\".\"OpsCommitmentYear\",
  \"a\".\"ProjectNo\",
  \"a\".\"TPNo\" AS \"ApplicationNumber\",
  CASE WHEN \"a\".\"IFSApplicationID\" = -1 THEN NULL ELSE \"a\".\"IFSApplicationID\" END AS \"IFSApplicationID\",
  \"a\".\"ProjectTitle\",
  \"a\".\"LeadParticipant\",
  \"a\".\"LeadPostcode\",
  \"a\".\"LeadRegion\",
  \"a\".\"LeadLEP1\",
  \"a\".\"LeadLEP2\",
  \"a\".\"LeadLocalAuthority\",
  \"a\".\"LeadCountry\",
  \"a\".\"ProjectDuration\",
  \"a\".\"AssessorAverageScore\",
  \"a\".\"AssessorScoreSpread\",
  \"a\".\"NumAssessments\" AS \"AssessmentCount\",
  \"a\".\"NumAssessmentsonScope\" AS \"AssessmentNoOnScope\",
  \"a\".\"NumAssessmentsonReccd\" AS \"AssessmentNoOnReccd\",
  \"a\".\"ProjectCost\",
  \"a\".\"InnovateUKFunding\",
  \"a\".\"TotalCoFunding\",
  \"a\".\"TotalFunding\",
  \"act\".\"ActionText\" AS \"FundingText\",
  \"a\".\"InnovateUKDecision\",
  \"a\".\"FundersPanelSheetColour\",
  CASE
    WHEN \"a\".\"ProjectNo\" IS NOT NULL AND \"a\".\"FundersPanelSheetColour\" = 'Green' THEN 'Yes'
    WHEN \"a\".\"AssessorAverageScore\" >= 70 THEN 'Blue Zone'
    ELSE 'No'
  END AS \"IsSuccessful\",
  \"a\".\"ResearchCategory\",
  \"a\".\"AssessmentCategory\",
  \"a\".\"InnovationArea\",
  \"a\".\"Theme1\",
  \"a\".\"Theme2\",
  \"a\".\"Theme3\",
  \"c\".\"Sector\",
  \"cc\".\"CostCentreName\",
  CASE WHEN \"c\".\"FPSigned\" = B'1' THEN 'Yes' ELSE 'No' END AS \"FPSignedCopyReceived\",
  \"c\".\"StateAidArticle\",
  CASE WHEN \"p\".\"NumPartners\" > 0 THEN 1 ELSE 0 END AS \"GrantsNumProjects\",
  \"p\".\"NumPartners\" AS \"GrantsNumPartners\"
FROM funderspanel.\"source_funderspaneldb_tblfpdataapplicants\" AS \"a\"
LEFT JOIN funderspanel.\"source_funderspaneldb_tblfpdatacomps\" AS \"c\"
  ON \"a\".\"CompetitionCode\" = \"c\".\"CompetitionCode\"
LEFT JOIN funderspanel.\"source_funderspaneldb_tblfpdataactions\" AS \"act\"
  ON \"a\".\"CompetitionCode\" = \"act\".\"CompetitionCode\"
AND \"act\".\"ActionNumber\" = 1
AND \"act\".\"StandardAction\" = B'0'
LEFT JOIN (
  SELECT
    \"ProjectNumber\",
    COUNT(\"ProjectParticipantKey\") AS \"NumPartners\"
  FROM public.\"dim_projectparticipant\"
  WHERE \"IsActive\" is TRUE
  GROUP BY \"ProjectNumber\"
) AS \"p\"
  ON CAST(\"a\".\"ProjectNo\" AS varchar) = \"p\".\"ProjectNumber\"
LEFT JOIN (
  SELECT
    \"p\".\"CompetitionCode\",
    MIN(concat_ws(' - ', CAST(\"p\".\"CostCentreNumber\" AS varchar), \"p\".\"CostCentre\")) AS \"CostCentreName\"
  FROM public.\"dim_project\" AS \"p\"
  WHERE \"p\".\"IsActive\" is TRUE
    AND \"p\".\"CostCentre\" IS NOT NULL
  GROUP BY \"p\".\"CompetitionCode\"
) AS \"cc\"
  ON CAST(\"a\".\"CompetitionCode\" AS varchar) = \"cc\".\"CompetitionCode\"
WHERE \"a\".\"CompetitionCode\" IS NOT NULL;")
    
    janus_funders_data <<- output
    
  } else if (table_choice == "Claims / Forecast"|table_choice == "Spend and Forecast") {
    
    print("This could take up to 5 minutes")
    output <- dbGetQuery(jcon,"SELECT 
        \"p\".\"ProjectNumber\" AS \"ProjectNumber\",
        \"p\".\"ProjectTitle\" AS \"ProjectTitle\",
        \"p\".\"ProjectSummary\" AS \"ProjectSummary\",
        \"p\".\"PublicDescription\" AS \"PublicDescription\",
        \"p\".\"ProjectStartDate\" AS \"ProjectStartDate\",
        \"p\".\"ProjectEndDate\" AS \"ProjectEndDate\",
        \"pp\".\"ACCProjectParticipantID\" AS \"ACCProjectParticipantID\",
        \"pp\".\"ParticipantOrganisationName\" AS \"ParticipantOrganisationName\",
        \"ppd\".\"PeriodNumber\" AS \"Claim Period Number\",
        \"ppd\".\"PeriodFromDate\" AS \"Claim Period Start Date\",
        \"ppd\".\"PeriodToDate\" AS \"Claim Period End Date\",
        COALESCE(\"claims\".\"TotalClaimGrantSubmitted\",
                0.00) AS \"TotalClaimGrantSubmitted\",
        COALESCE(\"claims\".\"TotalClaimGrantApproved\", 0.00) AS \"TotalClaimGrantApproved\",
        COALESCE(\"claims\".\"TotalClaimGrantApprovedForPayment\",
                0.00) AS \"TotalClaimGrantApprovedForPayment\",
        COALESCE(\"claims\".\"TotalClaimGrantPaid\", 0.00) AS \"TotalClaimGrantPaid\",
        COALESCE(\"claims\".\"TotalClaimGrantDeferred\", 0.00) AS \"TotalClaimGrantDeferred\",
        COALESCE(\"forecast\".\"InitialForecastGrant\", 0.00) AS \"InitialForecastGrant\",
        COALESCE(\"forecast\".\"LatestForecastGrant\", 0.00) AS \"LatestForecastGrant\"
    FROM
        ((((\"dim_project\" \"p\"
        JOIN \"dim_projectparticipant\" \"pp\" ON (((\"pp\".\"ProjectNumber\" = \"p\".\"ProjectNumber\")
            AND (\"pp\".\"IsActive\"))))
        JOIN \"dim_projectperiod\" \"ppd\" ON (((\"p\".\"ProjectKey\" = \"ppd\".\"ProjectKey\")
            AND (\"ppd\".\"IsActive\"))))
        LEFT JOIN (SELECT 
            \"fcs\".\"ProjectKey\" AS \"ProjectKey\",
                \"fcs\".\"ProjectParticipantKey\" AS \"ProjectParticipantKey\",
                \"fcs\".\"ProjectPeriodKey\" AS \"ProjectPeriodKey\",
                \"fcs\".\"TotalClaimGrantSubmitted\" AS \"TotalClaimGrantSubmitted\",
                \"fcs\".\"TotalClaimGrantApproved\" AS \"TotalClaimGrantApproved\",
                \"fcs\".\"TotalClaimGrantApprovedForPayment\" AS \"TotalClaimGrantApprovedForPayment\",
                \"fcs\".\"TotalClaimGrantPaid\" AS \"TotalClaimGrantPaid\",
                \"fcs\".\"TotalDeferredGrant\" AS \"TotalClaimGrantDeferred\",
                MAX((CASE
                    WHEN
                        ((\"cs\".\"LegacyClaimStatusName\" IS NULL)
                            OR (\"cs\".\"ClaimStatusLongName\" = 'Draft'))
                    THEN
                        'Forecast'
                    ELSE NULL
                END)) AS \"LegacyClaimStatusName\"
        FROM
            ((\"fact_claimsummary\" \"fcs\"
        JOIN \"dim_projectparticipant\" \"pp\" ON (((\"fcs\".\"ProjectParticipantKey\" = \"pp\".\"ProjectParticipantKey\")
            AND (\"pp\".\"IsActive\")
            AND NOT (\"pp\".\"IsARCParticipant\"))))
        JOIN \"dim_claimstatus\" \"cs\" ON (((\"fcs\".\"ClaimStatusKey\" = \"cs\".\"ClaimStatusKey\")
            AND (\"cs\".\"IsActive\"))))
        WHERE
            (\"cs\".\"LegacyClaimStatusName\" <> 'Forecast')
        GROUP BY \"fcs\".\"ProjectKey\" , \"fcs\".\"ProjectPeriodKey\" , \"fcs\".\"ProjectParticipantKey\", \"fcs\".\"TotalClaimGrantSubmitted\",
                \"fcs\".\"TotalClaimGrantApproved\",
                \"fcs\".\"TotalClaimGrantApprovedForPayment\",
                \"fcs\".\"TotalClaimGrantPaid\",
                \"fcs\".\"TotalDeferredGrant\") \"claims\" ON (((\"p\".\"ProjectKey\" = \"claims\".\"ProjectKey\")
            AND (\"ppd\".\"ProjectPeriodKey\" = \"claims\".\"ProjectPeriodKey\")
            AND (\"pp\".\"ProjectParticipantKey\" = \"claims\".\"ProjectParticipantKey\"))))
        LEFT JOIN (SELECT 
            \"fcs\".\"ProjectKey\" AS \"ProjectKey\",
                \"fcs\".\"ProjectParticipantKey\" AS \"ProjectParticipantKey\",
                \"fcs\".\"ProjectPeriodKey\" AS \"ProjectPeriodKey\",
                SUM(COALESCE(\"fi\".\"InitialForecastGrant\", 0.00)) AS \"InitialForecastGrant\",
                SUM(COALESCE(\"fi\".\"LatestForecastGrant\", 0.00)) AS \"LatestForecastGrant\",
                MAX((CASE
                    WHEN
                        ((\"cs\".\"LegacyClaimStatusName\" IS NULL)
                            OR (\"cs\".\"ClaimStatusLongName\" = 'Draft'))
                    THEN
                        'Forecast'
                    ELSE NULL
                END)) AS \"LegacyClaimStatusName\"
        FROM
            ((\"fact_claimsummary\" \"fcs\"
        JOIN \"fact_forecastitem\" \"fi\" ON (((\"fcs\".\"ProjectParticipantKey\" = \"fi\".\"ProjectParticipantKey\")
            AND (\"fcs\".\"ProjectPeriodKey\" = \"fi\".\"ProjectPeriodKey\"))))
        JOIN \"dim_claimstatus\" \"cs\" ON (((\"fcs\".\"ClaimStatusKey\" = \"cs\".\"ClaimStatusKey\")
            AND (\"cs\".\"IsActive\"))))
        WHERE
            ((\"cs\".\"LegacyClaimStatusName\" IS NULL)
                OR (\"cs\".\"ClaimStatusLongName\" = 'Draft'))
        GROUP BY \"fcs\".\"ProjectKey\" , \"fcs\".\"ProjectParticipantKey\" , \"fcs\".\"ProjectPeriodKey\") \"forecast\" ON (((\"p\".\"ProjectKey\" = \"forecast\".\"ProjectKey\")
            AND (\"ppd\".\"ProjectPeriodKey\" = \"forecast\".\"ProjectPeriodKey\")
            AND (\"pp\".\"ProjectParticipantKey\" = \"forecast\".\"ProjectParticipantKey\"))))
    WHERE
        ((\"p\".\"IsActive\")
            AND NOT (\"p\".\"IsThirdPartyPayment\")
            AND NOT (\"p\".\"IsTestProject\")
            AND (\"p\".\"ProjectStatus\" IN ('Live' , 'Final Claim', 'On Hold'))
            AND ((\"p\".\"Theme\" <> 'Loans')
            OR (\"p\".\"Theme\" IS NULL)))")
    
    # output <- DBI::dbReadTable(con, "vw_Dashboard_ProjectParticipantPeriodLevel")
    # 
    # claims_forecast_data <<- output
    
  } else if (table_choice == "EDI Non Pivot") {
    
    print("This could take up to a minute - not yet available")
    
    # output <- DBI::dbReadTable(con, "fact_sensitive_edireporting_applicant_pivot")
    # 
    output <- dbGetQuery(jcon,"
SELECT
  fct.\"Sensitive_EDIReporting_ApplicantKey\",
  fct.\"Sensitive_EDISurveyKey\",
  fct.\"ApplicantKey\",
  da.\"ApplicationKey\",
  da.\"ApplicationID\",
  fct.\"SFContactPostcode\",
  fct.\"SFContactRegion\",
  fct.\"IsIFSApplicationLeadContact\",
  fct.\"ApplicantType\",
  fct.\"ApplicationStatus\",
  fct.\"FundingDecision\",
  fct.\"ApplicationSubmittedDate\",
  fct.\"ApplicantOrganisationWorkPostcode\",
  fct.\"ApplicantOrganisationWorkRegion\",
  fct.\"ApplicationFundingSought\",
  fct.\"ParticipantType\",
  fct.\"ProjectStatus\",
  fct.\"ProjectStartDate\",
  fct.\"ProjectEndDate\",
  fct.\"ParticipantCleansedPostcode\",
  fct.\"ParticipantCleansedRegion\",
  fct.\"ParticipantWorkPostcode\",
  fct.\"ParticipantWorkRegion\",
  fct.\"ParticipantGrantAwarded\",
  fct.\"ContactType\",
  fct.\"CompetitionCode\",
  fct.\"CompetitionID\",
  fct.\"CompetitionName\",
  fct.\"IFS_FundingType\",
  fct.\"IFS_CompetitionType\",
  fct.\"IFSPA_CompetitionType\",
  fct.\"Product\",
  fct.\"IFS_InnovationArea\",
  fct.\"IFS_InnovationSector\",
  fct.\"Sector\",
  fct.\"CostCentre\",
  fct.\"CompetitionOpenDate\",
  fct.\"FundersPanelDate\",
  fct.\"EarliestProjectStart\",
  fct.\"OpsCommitmentYear\",
  fct.\"EDIQuestionGroup\",
  fct.\"EDIQuestionText\",
  fct.\"EDIAnswerText\",
  sf.\"date_of_birth__c\" AS \"DateOfBirth\"
FROM public.fact_sensitive_edireporting_applicant fct
INNER JOIN public.dim_sensitive_edisurvey dse
  ON dse.\"Sensitive_EDISurveyKey\" = fct.\"Sensitive_EDISurveyKey\"
  AND dse.\"IsActive\" = True
INNER JOIN ifspa.personal_information__c sf
  ON sf.Id = dse.\"PersonalInformationID\"
INNER JOIN public.dim_applicant da
  ON da.\"ApplicantKey\" = fct.\"ApplicantKey\"
WHERE da.\"IsActive\" = true ;")
    janus_edi_data_nonpivot <<- output
    
  } else if (table_choice == "EDI Pivot") {
    
    # output <- DBI::dbGetQuery(con, "
    #                             SELECT
    #                             *,
    #                             CAST(IsIFSApplicationLeadContact AS SIGNED) AS IsIFSApplicationLeadContact_1
    #                             FROM vw_Sensitive_EDIData_DOB_Nonpivot
    #                            ") %>% 
    #   rename(IsIFSApplicationLeadContact_1 = ncol(.)) %>% 
    #   mutate(IsIFSApplicationLeadContact = if_else(IsIFSApplicationLeadContact_1 == "1",
    #                                                "1",
    #                                                "0"))
    # 
    # edi_data <<- output
    output <- DBI::dbGetQuery(jcon,"SELECT
  fct.\"Sensitive_EDIReporting_ApplicantKey\",
  fct.\"Sensitive_EDISurveyKey\",
  fct.\"ApplicantKey\",
  da.\"ApplicationKey\",
  da.\"ApplicationID\",
  fct.\"SFContactPostcode\",
  fct.\"SFContactRegion\",
  fct.\"IsIFSApplicationLeadContact\",
  fct.\"ApplicantType\",
  fct.\"ApplicationStatus\",
  fct.\"FundingDecision\",
  fct.\"ApplicationSubmittedDate\",
  fct.\"ApplicantOrganisationWorkPostcode\",
  fct.\"ApplicantOrganisationWorkRegion\",
  fct.\"ApplicationFundingSought\",
  fct.\"ParticipantType\",
  fct.\"ProjectStatus\",
  fct.\"ProjectStartDate\",
  fct.\"ProjectEndDate\",
  fct.\"ParticipantCleansedPostcode\",
  fct.\"ParticipantCleansedRegion\",
  fct.\"ParticipantWorkPostcode\",
  fct.\"ParticipantWorkRegion\",
  fct.\"ParticipantGrantAwarded\",
  fct.\"ContactType\",
  fct.\"CompetitionCode\",
  fct.\"CompetitionID\",
  fct.\"CompetitionName\",
  fct.\"IFS_FundingType\",
  fct.\"IFS_CompetitionType\",
  fct.\"IFSPA_CompetitionType\",
  fct.\"Product\",
  fct.\"IFS_InnovationArea\",
  fct.\"IFS_InnovationSector\",
  fct.\"Sector\",
  fct.\"CostCentre\",
  fct.\"CompetitionOpenDate\",
  fct.\"FundersPanelDate\",
  fct.\"EarliestProjectStart\",
  fct.\"OpsCommitmentYear\",
  sf.\"date_of_birth__c\" AS \"DateOfBirth\",
  date_part('year', age(current_date, sf.\"date_of_birth__c\")) AS \"age\",
  CASE
    WHEN sf.\"date_of_birth__c\" IS NULL THEN NULL
    WHEN date_part('year', age(current_date, sf.\"date_of_birth__c\")) < 20 THEN '<19'
    WHEN date_part('year', age(current_date, sf.\"date_of_birth__c\")) BETWEEN 20 AND 29 THEN '20-29'
    WHEN date_part('year', age(current_date, sf.\"date_of_birth__c\")) BETWEEN 30 AND 39 THEN '30-39'
    WHEN date_part('year', age(current_date, sf.\"date_of_birth__c\")) BETWEEN 40 AND 49 THEN '40-49'
    WHEN date_part('year', age(current_date, sf.\"date_of_birth__c\")) BETWEEN 50 AND 59 THEN '50-59'
    WHEN date_part('year', age(current_date, sf.\"date_of_birth__c\")) BETWEEN 60 AND 69 THEN '60-69'
    WHEN date_part('year', age(current_date, sf.\"date_of_birth__c\")) BETWEEN 70 AND 79 THEN '70-79'
    ELSE '80+'
  END AS \"age_group\",
  fct.\"Caring_Responsibilities_1\",
  fct.\"Caring_Responsibilities_2\",
  fct.\"Disability_1\",
  fct.\"Disability_2\",
  fct.\"Educational_Background\",
  fct.\"Ethnicity_1\",
  fct.\"Ethnicity_2\",
  fct.\"Gender\",
  fct.\"Religion\",
  fct.\"Sex\",
  fct.\"Sexual_Orientation\",
  fct.\"Socioeconomic_Background\",
  fct.\"Trans_Identity\"
FROM public.fact_sensitive_edireporting_applicant_pivot fct
INNER JOIN public.dim_sensitive_edisurvey dse
  ON dse.\"Sensitive_EDISurveyKey\" = fct.\"Sensitive_EDISurveyKey\"
  AND dse.\"IsActive\" = TRUE
INNER JOIN ifspa.personal_information__c sf
  ON sf.Id = dse.\"PersonalInformationID\"
INNER JOIN public.dim_applicant da
  ON da.\"ApplicantKey\" = fct.\"ApplicantKey\"
WHERE da.\"IsActive\" = TRUE;")
    
    janus_edi_data <<- output
    
  } else if (table_choice == "PCF") {
    
    print("This could be quite slow")
    
    output <- DBI::dbReadTable(jcon, "mv_iuk_projectcompletionform")
    
    janus_pcf_data <<- output
  } else if (table_choice == "VOPs") {
    print("This could take a long time")
    
    output <- dbReadTable(jcon,"vw_VOPs_VOPsForecastingReport")
    
    janus_vops_data <<- output
  
    
  } else if (table_choice == "ID Changes") {
    print("This should be quick")
    
    output <- dbGetQuery(jcon,"SELECT
  a.\"id\" AS \"application_id\",
  a.\"name\" AS \"name\",
  a.\"competition\" AS \"competition\",
  a.\"submitted_date\" AS \"submitted_date\",
  a.\"funding_decision\" AS \"funding_decision\",
  a.\"previous_application_id\" AS \"previous_application_id\",
  fpapp.\"ActivityCode\" AS \"activitycode\"
FROM ifs.ifs_application a
LEFT JOIN funderspanel.source_funderspaneldb_tblfpdataapplicants fpapp
  ON a.\"id\" = fpapp.\"IFSApplicationID\"
WHERE a.\"previous_application_id\" IS NOT NULL
ORDER BY a.\"competition\" ASC, a.\"id\" ASC;")
    
    janus_applicationid_changes <<- output
    
    
  } else if (table_choice == "Janus") {
    
    print(all_tables)
    
  } else if (table_choice == "Tables") {
    
    print(tables)
  }
  
  end <- Sys.time()
  print(end-start)
  
}
