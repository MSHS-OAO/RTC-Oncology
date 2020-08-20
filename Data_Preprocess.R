# Packages ----------------------------------------------------------------
library("readxl")
library("lubridate")
library("dplyr")
library("magrittr")

# Data Import -------------------------------------------------------------

slot_usage <- read_excel("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/Oncology/Data/Cerner Actual data/RTC Slot usage jun01-aug31 2020.xlsx")
#data_LOC <- "C:/Users/Administrator/Downloads/Sample Slot Usage and Appointment Update Data.xlsx"
#slot_usage <- read_excel("C:/Users/Armando/Downloads/Cerner Actual data/RTC Slot usage jun01-aug31 2020.xlsx")


# Slot Usage Pre-Process --------------------------------------------------


## Using mutate columns are created for the DOW,month,hour, and 
##whether the appointment is overbooked or not
##filter out specific slot types that we are not looking for
##as well as remoove any NAs in slot_type or RESOURCE
##Order the DATA by DATE_TIME and then split the data into
##a procedure data frame and a provider data frame

#slot_usage <-
slot_usage %<>% #assignment/pass
  mutate(appt_dow = wday(DATE_TIME, label = T), 
         appt_month = month(DATE_TIME, label = T), 
         slot_hour = hour(DATE_TIME), 
         resource_time_comb = paste(RESOURCE,DATE_TIME),
         overbook = duplicated(resource_time_comb)) %>%
  filter(!SLOT_TYPE %in% 
           c("Unavailable", "RTC DO NOT BOOK", "EST PT-COVID19 SCREENING", "Non Face to Face"),
         !is.na(SLOT_TYPE),
         !is.na(RESOURCE)) %>%
  arrange(DATE_TIME) #%>%
#format(DATE_TIME, "%m-%d-%Y %H:%M:%S")

slot_usage$DATE_TIME %<>%  
  format("%m-%d-%Y %H:%M:%S")

procedure_slot_usage <-
  slot_usage %>%
  filter(RESOURCE %in%
           c("CSM Rutt Lab Room 4 Floor", "VAD 3 Floor", "VAD 4 Floor", "CSM Rutt Lab Room 3 Floor", "Procedure Room"))

provider_slot_usage <-
  slot_usage %>%
  filter(!RESOURCE %in% unique(procedure_slot_usage$RESOURCE))

# provider_slot_usage <-
#   slot_usage %>%
#   filter(RESOURCE %in% unique(provider_slot_usage$RESOURCE))

