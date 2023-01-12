# UsefulFunctions

This package contains a number of useful functions, data, and templates to save time and ensure consistency in analytical work at Innovate UK

Functions include:

- namesFunction
  - This is used in .name_repair to make columns Upper Camel
- dataWarehouse_function_specific
  - Pulls specific data from the IUK Data Warehouse, combining complimentary tables where appropriate 
- transDataFunction
  - Pulls the publihsed IUK Transparnecy data from the web https://www.ukri.org/publications/innovate-uk-funded-projects-since-2004/
- companies_house_function
  - returns company data from the Companies House API
- co_ho_officers_function
  - Returns data on people and entities listed as company officers from the Companies House API
- co_ho_signif_control_function
  - Returns data on people and entities with significant company control from the Companies House API
- co_ho_charges_function
  - Returns data on charges levied against companies from the Companies House API
- postcode_function
  - A wrapper to the postcodes.io API
- survey_id_function
  - Returns a table of IUK Survey IDs from the Qualtrics API  
- survey_response_function
  - Returns a table of responses to IUK Surveys from the Qualtrics API  
- survey_questions_function
  - Returns a table of questions from IUK Surveys from the Qualtrics API
- clean_crn_function
  - Cleans Companies House Reference Numbers 
- normalise_university_name_function
  - Normalises Univeristy Names

Data includes:

- product_groups
  - Innovate UK Product Types with their associated internal grouping used for reporting
- smes
  - Enterprises sizes which fall into the Small and Medium-Sized Enterprise category

Templates inlcude:

- IUK Project Template
  - Creates an IUK Project Template in the RStudio New Project Wizard for IUK Porjects. This pulls the .REnviron files from the home directry into the new project, this file contains the variables required to access a number of the APIs used. 
