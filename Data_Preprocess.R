# Packages ----------------------------------------------------------------
library("readxl")
library("lubridate")
library("dplyr")


# Data Import -------------------------------------------------------------

#data_LOC <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/Oncology/Data/Sample Slot Usage and Appointment Update Data.xlsx"
#data_LOC <- "C:/Users/Administrator/Downloads/Sample Slot Usage and Appointment Update Data.xlsx"
slot_usage <- read_excel("C:/Users/Armando/Downloads/Cerner Actual data/RTC Slot usage jun01-aug31 2020.xlsx")


# Slot Usage Pre-Process --------------------------------------------------
#slot_usage <- slot_usage[order(as.Date(slot_usage$DATE_TIME, format="%Y-%m-%d")),] #sort slot_usage by date
#slot_usage$DATE_TIME <- format(slot_usage$DATE_TIME, "%m-%d-%Y %H:%M:%S") #refomrat Date_Time
# slot_usage$appt_dow <- weekdays(as.Date(slot_usage$DATE_TIME)) #Get the DOW based off of Date_time
# slot_usage$appt_month <- months(as.Date(slot_usage$DATE_TIME)) #get month of appt
# slot_usage$slot_hour <- hour(strptime(slot_usage$DATE_TIME, format = "%m-%d-%Y %H:%M:%S"))
#slot_usage <- slot_usage[!is.na(slot_usage$RESOURCE),] #deletes rows with NA as a rsource
# slot_usage$resource_time_comb <- paste(slot_usage$RESOURCE,slot_usage$DATE_TIME) #combine time and resource 
# slot_usage$overbook <- duplicated(slot_usage$resource_time_comb) #find duplicates in resource_time_comb
# slot_usage <- slot_usage[!(slot_usage$SLOT_TYPE == "Unavailable"),]
# slot_usage <- slot_usage[!(slot_usage$SLOT_TYPE == "RTC DO NOT BOOK"),]
# slot_usage <- slot_usage[!(slot_usage$APPT_TYPE == "EST PT-COVID19 SCREENING"),] 
# slot_usage <- slot_usage[!(slot_usage$APPT_TYPE == "Non Face to Face"),] 
# procedure_slot_usage <- slot_usage[(slot_usage$SLOT_TYPE == "Unavailable"),]
# provider_slot_usage <-



#mutate 
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
  filter(RESOURCE %in% unique(provider_slot_usage$RESOURCE))

provider_slot_usage <-
  slot_usage %>%
  filter(RESOURCE %in% unique(provider_slot_usage$RESOURCE))

# provider_slot_usage <-
#   slot_usage %>%
#   filter(!RESOURCE %in%
#            c("CSM Rutt Lab Room 4 Floor", "VAD 3 Floor", "VAD 4 Floor", "CSM Rutt Lab Room 3 Floor", "Procedure Room"))





