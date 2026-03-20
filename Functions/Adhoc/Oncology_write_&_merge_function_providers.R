# Needed Libraries
library(odbc)
library(DBI)
library(dbplyr)
library(glue)
library(readxl)
library(dplyr)
options(odbc.batch_rows = 1000000)

#Data Loading - Manually 
new_providers_data <- read_excel(file.choose())
head(new_providers_data)

#---Providers Mapping---
# Pre-processing and Cleaning 
process_and_clean_data <- function(input_df) {
  
  if (nrow(input_df) == 0) {
    message("The input file is empty!")
    return(NULL)
  }
  
  cleaned_data <- input_df %>%
    # Drop unwanted fields
    select(-`In HSO Tool`, -`Provider NPI`, -`Active/ Historical Filter`) %>%
    
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
      DATE_ADDED = format(Sys.Date(), "%Y-%m-%d")
    ) %>%
    
    # Prepare for SQL (convert everything to character and handle NAs)
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~coalesce(., "NULL"))) %>%
    
    # Final column order for the get_values function
    select(EPIC_PROVIDER_ID, DISEASE_GROUP, DISEASE_GROUP_B, PROVIDER_TYPE, SITE, PROVIDER_NAME, DATE_ADDED)
  
  return(cleaned_data)
}
processed_input_data <- process_and_clean_data(new_providers_data)

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


# Write temporary table and merge into final table
write_temporary_table_to_database_and_merge <- function(processed_input_data, table_name = "PROVIDER_NOV") {
  
  if (nrow(processed_input_data) == 0) {
    message("The Excel file is empty!")
    return(NULL)
  }
  processed_input_data <- processed_input_data %>%
    mutate(across(everything(), as.character))
  
}

# Correct provider table data types definition 
DATA_TYPES <- c(
  EPIC_PROVIDER_ID = "Varchar2(38)",
  DISEASE_GROUP    = "Varchar2(26)",
  DISEASE_GROUP_B  = "Varchar2(100)",
  PROVIDER_TYPE    = "Varchar2(120)",
  SITE             = "Varchar2(120)",
  PROVIDER_NAME    = "Varchar2(75)",
  DATE_ADDED       = "DATE"
)

#Create the connection 
con <- dbConnect(odbc(), "OracleODBC-21_5", uid = "OAO_DEVELOPMENT", pwd = "HC*tA$4f1qMqVo")

#Define the Tables
STAGING_TABLE <- "TEMP_STAGING_PROVIDER"
table_name <- "PROVIDER_NOV"
DISEASE_GROUPS <- "ONCOLOGY_DISEASE_GROUPINGS"

# Build the SQL strings
inserts <- lapply(split(processed_input_data, 1:nrow(processed_input_data)), function(row) {
  get_values(as.character(row), STAGING_TABLE)
})

all_data_sql <- glue("INSERT ALL {glue_collapse(inserts, sep = ' ')} SELECT 1 FROM DUAL")

# Merge the Provider_Nov table with the Disease grouping table 
merge_nov_sql <- glue('
    MERGE INTO {table_name} T
    USING {STAGING_TABLE} S
    ON (T."EPIC_PROVIDER_ID" = S."EPIC_PROVIDER_ID" AND T."SITE" = S."SITE")
    WHEN MATCHED THEN
      UPDATE SET 
        T."DISEASE_GROUP" = S."DISEASE_GROUP",
        T."DISEASE_GROUP_B" = S."DISEASE_GROUP_B",
        T."PROVIDER_TYPE" = S."PROVIDER_TYPE",
        T."PROVIDER_NAME" = S."PROVIDER_NAME",
        T."DATE_ADDED" = S."DATE_ADDED"
    WHEN NOT MATCHED THEN
      INSERT ("EPIC_PROVIDER_ID", "DISEASE_GROUP", "DISEASE_GROUP_B", "PROVIDER_TYPE", "SITE", "PROVIDER_NAME", "DATE_ADDED")
      VALUES (S."EPIC_PROVIDER_ID", S."DISEASE_GROUP", S."DISEASE_GROUP_B", S."PROVIDER_TYPE", S."SITE", S."PROVIDER_NAME", S."DATE_ADDED")')

# Merge the Provider_Nov table with the Disease grouping table
merge_disease_sql <- glue('
    MERGE INTO {DISEASE_GROUPS} A
    USING {STAGING_TABLE} B
    ON (A."EPIC_PROVIDER_ID" = B."EPIC_PROVIDER_ID")
    WHEN MATCHED THEN
      UPDATE SET A."PROVIDER_NAME" = B."PROVIDER_NAME", A."DISEASE_GROUP" = B."DISEASE_GROUP",
                 A."DISEASE_GROUP_B" = B."DISEASE_GROUP_B", A."SITE" = B."SITE", A."PROVIDER_TYPE" = B."PROVIDER_TYPE"
    WHEN NOT MATCHED THEN
      INSERT ("PROVIDER_NAME", "EPIC_PROVIDER_ID", "DISEASE_GROUP", "DISEASE_GROUP_B", "PROVIDER_TYPE", "SITE", "DATE_ADDED")
      VALUES (B."PROVIDER_NAME", B."EPIC_PROVIDER_ID", B."DISEASE_GROUP", B."DISEASE_GROUP_B", B."PROVIDER_TYPE", B."SITE", B."DATE_ADDED" )')

# Transaction Execution
tryCatch({
  dbBegin(con)
  
  # Setup Staging
  dbWriteTable(con, STAGING_TABLE, processed_input_data[0, ], overwrite = TRUE, field.types = DATA_TYPES)
  
  # Load Data
  dbExecute(con, all_data_sql)
  
  # Execute both Merges
  dbExecute(con, merge_nov_sql)
  dbExecute(con, merge_disease_sql)
  
  # Cleanup & Commit
  dbRemoveTable(con, STAGING_TABLE)
  dbCommit(con)
  
  message("Both tables were updated")
  
}, error = function(e) {
  dbRollback(con)
  message(paste("Critical Error - All changes rolled back:", e$message))
}, finally = {
  dbDisconnect(con)
})