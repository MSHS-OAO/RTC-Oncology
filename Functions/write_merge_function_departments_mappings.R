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
new_dep_data <- read_excel(file_path, sheet = "Departments to be added")

##---Departments Mapping---
# Pre-processing and Cleaning 
process_and_clean_data <- function(input_df) {
  if (nrow(input_df) == 0) {
    message("The input file is empty!")
    return(NULL)
  }
  
  cleaned_data <- input_df %>%
    # Select only the needed columns
    select(`EPIC  Department`, `EPIC Department ID`, SITE) %>%
    rename(
      DEPARTMENT_NAME = `EPIC  Department`,
      DEPARTMENT_ID   = `EPIC Department ID`
    ) %>%
    mutate(
      # this going to be changed later to update on the same day of the work
      DATE_ADDED = "2026-03-24"
    ) %>%
    select(DEPARTMENT_NAME, DEPARTMENT_ID, SITE, DATE_ADDED) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~coalesce(., "NULL")))
  
  return(cleaned_data)
}

# Values Formatting 
get_values <- function(x, table_name) {
  DEPARTMENT_NAME <- x[1]
  DEPARTMENT_ID   <- x[2]
  SITE            <- x[3]
  DATE_ADDED      <- x[4]
  
  values <- glue(
    "INTO \"{table_name}\"
    (DEPARTMENT_NAME, DEPARTMENT_ID, SITE, DATE_ADDED)
    VALUES ('{DEPARTMENT_NAME}', '{DEPARTMENT_ID}', '{SITE}', 
    TO_DATE('{DATE_ADDED}', 'YYYY-MM-DD'))"
  )
}

#  Database Operations
write_temporary_table_to_database_and_merge <- function(processed_data) {
  
  if (is.null(processed_data) || nrow(processed_data) == 0) {
    message("No data to process.")
    return(NULL)
  }
  
  # Define Table Names Clearly
  STAGING_TABLE <- "ONCOLOGY_STAGING_DEPARTMENTS"
  TARGET_TABLE  <- "ONCOLOGY_DEPARTMENT_GROUPINGS"
  
  DATA_TYPES <- c(
    DEPARTMENT_NAME = "VARCHAR2(128)",
    DEPARTMENT_ID   = "NUMBER(38)",
    SITE            = "VARCHAR2(26)",
    DATE_ADDED      = "DATE"
  )
  
  # Build the SQL strings
  inserts <- lapply(split(processed_data, 1:nrow(processed_data)), function(row) {
    get_values(as.character(row), STAGING_TABLE)
  })
  
  all_data_sql <- glue("INSERT ALL {glue_collapse(inserts, sep = ' ')} SELECT 1 FROM DUAL")
  
  merge_sql <- glue('
    MERGE INTO {TARGET_TABLE} T
    USING {STAGING_TABLE} S
    ON (T.DEPARTMENT_ID = S.DEPARTMENT_ID)
    WHEN MATCHED THEN
      UPDATE SET 
        T.DEPARTMENT_NAME = S.DEPARTMENT_NAME,
        T.SITE            = S.SITE
    WHEN NOT MATCHED THEN
      INSERT (DEPARTMENT_NAME, DEPARTMENT_ID, SITE, DATE_ADDED)
      VALUES (S.DEPARTMENT_NAME, S.DEPARTMENT_ID, S.SITE, S.DATE_ADDED)')
  
  # Connection and Execution
  con <- dbConnect(odbc(),"OAO Cloud DB Staging", timeout = 30)
  
  # Write the data to staging table
  tryCatch({
    dbBegin(con)
    
    # Create staging table (schema only)
    dbWriteTable(con, STAGING_TABLE, processed_data[0, ], 
                 overwrite = TRUE, field.types = DATA_TYPES)
    
    # Bulk Insert into Staging
    dbExecute(con, all_data_sql)
    
    dbCommit(con)
    message(glue("Success! {nrow(processed_data)} records insered into {STAGING_TABLE}."))
    
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
    
    # Merge Staging into Final
    dbExecute(con, merge_sql)
    
    # Clean up
    if(dbExistsTable(con, STAGING_TABLE)) dbRemoveTable(con, STAGING_TABLE)
    
    dbCommit(con)
    message(glue("Success! {nrow(processed_data)} records merged into {TARGET_TABLE}."))
    
  }, error = function(e) {
    if (exists("con")) dbRollback(con)
    message(paste("Error -  Failed merging data:", e$message))
  }, finally = {
    if (exists("con")) dbDisconnect(con)
  })
}

# Test
processed_input_data <- process_and_clean_data(new_dep_data)
write_temporary_table_to_database_and_merge(processed_input_data)