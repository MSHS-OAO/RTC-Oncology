
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

