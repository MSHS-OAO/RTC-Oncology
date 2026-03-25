# Needed Libraries
library(odbc)
library(DBI)
library(dbplyr)
library(glue)
library(readxl)
library(dplyr)
options(odbc.batch_rows = 1000000)

#Data Loading
file_path <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/Oncology/Data/Mappings/DataTemplates/MSSN_and_LI_Providers_and_Depts_to_add.xlsx"
new_providers_data <- read_excel(file_path, sheet = "Providers to be added")

#---Providers Mapping---
# Pre-processing and Cleaning 
process_and_clean_data <- function(input_df) {
  
  if (nrow(input_df) == 0) {
    message("The input file is empty!")
    return(NULL)
  }
  
  cleaned_data <- input_df %>%
    
    # Selects only required columns
    select(
      `PROVIDER NAME`,
      `EPIC Provider ID`,
      `Disease Group`,
      `Disease Group Specialty`,
      `Site`,
      `Type`
    ) %>%
    
    # Rename fields (New_Name = Old_Name)
    rename(
      PROVIDER_NAME = `PROVIDER NAME`,
      EPIC_PROVIDER_ID = `EPIC Provider ID`, 
      DISEASE_GROUP = `Disease Group`,
      DISEASE_GROUP_B = `Disease Group Specialty`,
      SITE = `Site`,
      PROVIDER_TYPE = `Type`
    ) %>%
    
    # Content Logic: Convert 'APP' to full name & Add DATE_ADDED
    mutate(
      PROVIDER_TYPE = ifelse(PROVIDER_TYPE == "APP", "Advanced Practice Provider", PROVIDER_TYPE),
      # this going to be changed later to update on the same day of the work
      DATE_ADDED = "2026-03-24"
    ) %>%
    
    # Prepare for SQL (convert everything to character and handle NAs)
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~coalesce(., "NULL"))) %>%
    
    # Final column order for the get_values function
    select(EPIC_PROVIDER_ID, DISEASE_GROUP, DISEASE_GROUP_B, PROVIDER_TYPE, SITE, PROVIDER_NAME, DATE_ADDED)
  
  return(cleaned_data)
}

# Values formatting 
get_values <- function(x, table_name) {
  EPIC_PROVIDER_ID <- x[1]
  DISEASE_GROUP <- x[2]
  DISEASE_GROUP_B <- x[3]
  PROVIDER_TYPE <- x[4]
  SITE <- x[5]
  PROVIDER_NAME <- x[6]
  DATE_ADDED    <- x[7]
  
  values <- glue(
    "INTO \"{table_name}\" 
     (EPIC_PROVIDER_ID, DISEASE_GROUP, DISEASE_GROUP_B, PROVIDER_TYPE, SITE, PROVIDER_NAME, DATE_ADDED) 
     VALUES ('{EPIC_PROVIDER_ID}', '{DISEASE_GROUP}', '{DISEASE_GROUP_B}', 
             '{PROVIDER_TYPE}', '{SITE}', '{PROVIDER_NAME}', 
             TO_DATE('{DATE_ADDED}', 'YYYY-MM-DD'))"
  )
  
  return(values)
}

write_temporary_table_to_database_and_merge <- function(processed_input_data) {
  
  if (is.null(processed_input_data) || nrow(processed_input_data) == 0) {
    message("The input data is empty — no changes made.")
    return(NULL)
  }
  
  processed_input_data <- processed_input_data %>%
    mutate(across(everything(), as.character))
  
  # Table name definitions
  STAGING_TABLE  <- "ONCOLOGY_STAGING_PROVIDERS"
  DISEASE_GROUPS <- "ONCOLOGY_DISEASE_GROUPINGS"
  
  # Column data types for staging table creation
  DATA_TYPES <- c(
    EPIC_PROVIDER_ID = "Varchar2(38)",
    DISEASE_GROUP    = "Varchar2(26)",
    DISEASE_GROUP_B  = "Varchar2(100)",
    PROVIDER_TYPE    = "Varchar2(120)",
    SITE             = "Varchar2(120)",
    PROVIDER_NAME    = "Varchar2(75)",
    DATE_ADDED       = "DATE"
  )
  
  # Build INSERT ALL SQL for staging load
  inserts <- lapply(
    split(processed_input_data, 1:nrow(processed_input_data)),
    function(row) get_values(as.character(row), STAGING_TABLE)
  )
  all_data_sql <- glue("INSERT ALL {glue_collapse(inserts, sep = ' ')} SELECT 1 FROM DUAL")
  
  # Build MERGE SQL
  merge_disease_sql <- glue('
    MERGE INTO {DISEASE_GROUPS} A
    USING {STAGING_TABLE} B
    ON (A."EPIC_PROVIDER_ID" = B."EPIC_PROVIDER_ID")
    WHEN MATCHED THEN
      UPDATE SET
        A."PROVIDER_NAME"   = B."PROVIDER_NAME",
        A."DISEASE_GROUP"   = B."DISEASE_GROUP",
        A."DISEASE_GROUP_B" = B."DISEASE_GROUP_B",
        A."SITE"            = B."SITE",
        A."PROVIDER_TYPE"   = B."PROVIDER_TYPE"
    WHEN NOT MATCHED THEN
      INSERT ("PROVIDER_NAME", "EPIC_PROVIDER_ID", "DISEASE_GROUP", "DISEASE_GROUP_B", "PROVIDER_TYPE", "SITE", "DATE_ADDED")
      VALUES (B."PROVIDER_NAME", B."EPIC_PROVIDER_ID", B."DISEASE_GROUP", B."DISEASE_GROUP_B",
              B."PROVIDER_TYPE", B."SITE", B."DATE_ADDED")')
  
  # Create the connection
  # Connection and Execution
  con <- dbConnect(odbc(),"OAO Cloud DB Staging", timeout = 30)
  
  # Write the data to staging table
  tryCatch({
    dbBegin(con)
    
    # Create staging table (schema only)
    dbWriteTable(con, STAGING_TABLE, processed_input_data[0, ], 
                 overwrite = TRUE, field.types = DATA_TYPES)
    
    # Bulk Insert into Staging
    dbExecute(con, all_data_sql)
    
    dbCommit(con)
    message(glue("Success! {nrow(processed_input_data)} records insered into {STAGING_TABLE}."))
    
  }, error = function(e) {
    if (exists("con")) dbRollback(con)
    message(paste("Error - Couldn't write data into {STAGING_TABLE}:", e$message))
  }, finally = {
    if (exists("con")) dbDisconnect(con)
  })
  
  con <- dbConnect(odbc(),"OAO Cloud DB Staging", timeout = 30)
  
  # Merge the data into target table
  tryCatch({
    dbBegin(con)
    
    # Merge staging into ONCOLOGY_DISEASE_GROUPINGS
    dbExecute(con, merge_disease_sql)
    
    # Cleanup staging table and commit
    dbRemoveTable(con, STAGING_TABLE)
    dbCommit(con)
    
    message("ONCOLOGY_DISEASE_GROUPINGS updated successfully via ONCOLOGY_STAGING_PROVIDERS.")
    
  }, error = function(e) {
    dbRollback(con)
    message(paste("Error - Failed merging data:", e$message))
    
  }, finally = {
    dbDisconnect(con)
  })
}

# Test
processed_input_data <- process_and_clean_data(new_providers_data)
write_temporary_table_to_database_and_merge(processed_input_data)