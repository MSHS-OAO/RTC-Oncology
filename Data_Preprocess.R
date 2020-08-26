# Packages ----------------------------------------------------------------
library("readxl")
library("lubridate")
library("dplyr")
library("magrittr")
library("tibble")

# Data Import -------------------------------------------------------------

#slot_usage <- read_excel("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/Oncology/Data/Cerner Actual data/RTC Slot usage jun01-aug31 2020.xlsx")
#data_LOC <- "C:/Users/Administrator/Downloads/Sample Slot Usage and Appointment Update Data.xlsx"
slot_usage <- read_excel("C:/Users/Armando/Downloads/Cerner Actual data/RTC Slot usage jun01-aug31 2020.xlsx")


# Slot Usage Pre-Process --------------------------------------------------


# Using mutate columns are created for the DOW,month,hour, and 
# whether the appointment is overbooked or not
# filter out specific slot types that we are not looking for
# as well as remove any NAs in slot_type or RESOURCE
# Order the DATA by DATE_TIME and then split the data into
# a procedure data frame and a provider data frame

#slot_usage <-
slot_usage %<>% #assignment/pass
  mutate(appt_dow = wday(DATE_TIME, label = T), 
         appt_month = month(DATE_TIME, label = T), 
         slot_hour = hour(DATE_TIME), 
         resource_time_comb = paste(RESOURCE,DATE_TIME),
         overbook = duplicated(resource_time_comb),
         slot_minute = minute(DATE_TIME),
         slot_time = paste0(slot_hour,":",slot_minute),
         slot_date = date(DATE_TIME)) %>%
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

unique_dates <- tbl_df(unique(slot_usage$slot_date))
colnames(unique_dates) <- "date"



# create a n by 4 matrix filled with zeros. The for loop iterates through each unique
# date and filter the slot_usage based on the date.  Get the total number of appt for
# that day then get the number of appts scheduled, open,a nd calc percentage of booked appts

count <- matrix(0, nrow(unique_dates), 4)
colnames(count) <- c("scheduled", "open", "total", "percent_schedlued")
for (i in 1:nrow(unique_dates)) {
  tmp <- filter(slot_usage, slot_date == unique_dates[i,1])
  total <- nrow(filter(slot_usage, slot_date == unique_dates[i,1]))
  schedlued <- nrow(filter(tmp, STATE == "Scheduled"))
  open <- nrow(filter(tmp, STATE == "Open"))
  percent_schedlued <- schedlued/total*100 
  count[i,1] <- schedlued
  count[i,2] <- open
  count[i,3] <- total
  count[i,4] <- percent_schedlued
}
tbl_df(count)


noshow_count <- matrix(0, nrow(unique_dates), 3)
colnames(noshow_count) <- c("noshow", "total", "percent_noshow") 
for (i in 1:nrow(unique_dates)) {
  tmp <- filter(slot_usage, slot_date == unique_dates[i,1])
  total <- nrow(filter(slot_usage, slot_date == unique_dates[i,1]))
  noshow <- nrow(filter(tmp, STATUS == "NOSHOW"))
  percent_noshow <- noshow/total*100 
  noshow_count[i,1] <- noshow
  noshow_count[i,2] <- total
  noshow_count[i,3] <- percent_noshow
  
}
tbl_df(noshow_count)






