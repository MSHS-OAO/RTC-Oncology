
library(timeDate)
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)

#####Determine the path in the shared-drive
ifelse (list.files("J://") == "Presidents", user_directory <- "J:/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects", 
        user_directory <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects")

file_list_amb <- list.files(path = paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Access/Monthly"), pattern = "^(2020)|(2021)\\-[0-9]{2}\\-[0-9]{2}")

amb_list <- lapply(file_list_amb, function(x) read.csv(paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Access/Monthly/", x), stringsAsFactors = FALSE))

for (i in (1: length(amb_list))){
  #change dates that were imported as char
  amb_list[[i]][c("APPT_MADE_DTTM", "APPT_DTTM", "APPT_CANC_DTTM")] <- lapply(amb_list[[i]][c("APPT_MADE_DTTM", "APPT_DTTM", "APPT_CANC_DTTM")],as.POSIXct, tz="UTC", format="%Y-%m-%d %H:%M:%OS")
  #get the month, year, weekday for important dates
  amb_list[[i]] <- amb_list[[i]] %>%
    mutate(appt_made_date = as.Date(APPT_MADE_DTTM),
           appt_made_hour = hour(APPT_MADE_DTTM),
           appt_made_day = weekdays(APPT_MADE_DTTM),
           appt_made_month = month(APPT_MADE_DTTM, label = TRUE),
           appt_made_year = year(APPT_MADE_DTTM),
           appt_date = as.Date(APPT_DTTM),
           appt_hour = hour(APPT_DTTM),
           appt_day = weekdays(APPT_DTTM),
           appt_month = month(APPT_DTTM, label = TRUE),
           appt_year = year(APPT_DTTM),
           cancel_date = as.Date(APPT_CANC_DTTM),
           cancel_hour = hour(APPT_CANC_DTTM),
           cancel_day = weekdays(APPT_CANC_DTTM),
           cancel_month = month(APPT_CANC_DTTM, label = TRUE),
           cancel_year = year(APPT_CANC_DTTM))
}

#binding the rows of the data-list
amb_df <- bind_rows(amb_list)

MSW_Marcy <- read_excel(paste0(user_directory,"/Service Lines/Oncology/Data/Docs from Marcy/12.2020 - MSW Volume Report December 2020 - Data Automation Example - Saved 1.29.2021.xlsx"), sheet = "MSW DATA")


#read the mapping file that was provided by Marcy
mapping_file <- choose.files(default = paste0(user_directory, "/Service Lines/Oncology/Data/Docs from Marcy/*.*"), caption = "Select mapping file")

#from the mapping file import the department ID sheet
department_mapping <- read_excel(mapping_file, sheet = "OncSystem - Dept ID Mappings")
department_mapping <- department_mapping[1:(length(department_mapping)-2)]

#returns string without leading or trailing white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

##remove the space at the end and at the beginning when applicable
department_mapping$`EPIC  Department` <- trim(department_mapping$`EPIC  Department`)

#change column names for the department mapping
colnames(department_mapping) <- c("System", "DEPARTMENT_NAME", "DEPARTMENT_ID", "SITE", "ACTIVE", "Notes")

#from the mapping file import the department PRC sheet
PRC_mapping <- read_excel(mapping_file, sheet = "Visit Type 'PRC Name' -Mappings")
PRC_mapping <- PRC_mapping[1:(length(PRC_mapping)-2)]

##remove the space at the end and at the beginning when applicable
PRC_mapping$`Sch VisitTypeName/ PRC Name` <- trim(PRC_mapping$`Sch VisitTypeName/ PRC Name`)

#####change all to first word capitalized
PRC_mapping$`Association List : A`[PRC_mapping$`Association List : A` == "Lab"] <- "Labs"
PRC_mapping$`Association List : A` <- str_to_title(PRC_mapping$`Association List : A`)

PRC_mapping$`Association List: B` <- str_to_title(PRC_mapping$`Association List: B`)

PRC_mapping$`Association List: T` <- str_to_title(PRC_mapping$`Association List: T`)

#change column names for the PRC mapping
colnames(PRC_mapping) <- c("PRC_NAME", "AssociationListA", "AssociationListB", "AssociationListT", "InPersonvsTele")

#merge the ambulatory data with the grouping data
amb_df_groupings <- merge(amb_df, department_mapping, by=c("DEPARTMENT_NAME"))
amb_df_groupings_ <- merge(amb_df_groupings, PRC_mapping, by = c("PRC_NAME"))

#only keep unique visits --> unique visits are defined as the visits with different
#MRN, appt date time, PRC name, provider name, and appt status 
amb_df_groupings_unique <-
  amb_df_groupings_ %>% 
  distinct(MRN, APPT_DTTM, PRC_NAME, PROV_NAME_WID, DERIVED_STATUS_DESC,  .keep_all = TRUE)

#only keep arrived patients - dont look at duplicates
arrived_patients <- amb_df_groupings_[which(amb_df_groupings_$DERIVED_STATUS_DESC == "Arrived"),]

#compare December 2020, MSW - duplicates included
MSW_onco <- arrived_patients[which(arrived_patients$SITE == "MSW"),]
MSW_onco_dec <- MSW_onco[which(MSW_onco$appt_month == "Dec"),]

#only keep unique arrived visits
arrived_patients_unique <- amb_df_groupings_unique[which(amb_df_groupings_unique$DERIVED_STATUS_DESC == "Arrived"),]

#compare December 2020, MSW - removed duplicates
MSW_onco_unique <- arrived_patients_unique[which(arrived_patients_unique$SITE == "MSW"),]
MSW_onco_unique_dec <- MSW_onco_unique[which(MSW_onco_unique$appt_month == "Dec"),]

#get only December
MSW_Marcy_dec_2020 <- MSW_Marcy[which(MSW_Marcy$Month == "December" & MSW_Marcy$Year== "2020"),]

#pre-process marcy's data
MSW_Marcy_dec_2020$Appt_Date <- as.Date(MSW_Marcy_dec_2020$Date, format =  "%m/%d/%y")

#check the missed records
#add a unique identifier to both of the datasets based on Name, Date, duration
MSW_Marcy_dec_2020$unique_id <- paste0(MSW_Marcy_dec_2020$`Pt Name`, " ", MSW_Marcy_dec_2020$Appt_Date, " ", MSW_Marcy_dec_2020$`Sch Duration`)
MSW_onco_unique_dec$uniqe_id <- paste0(MSW_onco_unique_dec$PAT_NAME, " ", MSW_onco_unique_dec$appt_date, " ", MSW_onco_unique_dec$APPT_LENGTH)

#match the unique ids
match_df <- as.data.frame(match(MSW_Marcy_dec_2020$unique_id, MSW_onco_unique_dec$uniqe_id))




