# Packages ----------------------------------------------------------------
library("readxl")
library("lubridate")
library("dplyr")


# Data Import -------------------------------------------------------------

#data_LOC <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/Oncology/Data/Sample Slot Usage and Appointment Update Data.xlsx"
data_LOC <- "C:/Users/Administrator/Downloads/Sample Slot Usage and Appointment Update Data.xlsx"
slot_usage <- read_excel(data_LOC, sheet = 1)
appt_updates <- read_excel(data_LOC, sheet = 2)



# Slot Usage Pre-Process --------------------------------------------------
slot_usage <- slot_usage[order(as.Date(slot_usage$DATE_TIME, format="%Y-%m-%d")),] #sort slot_usage by date
slot_usage$DATE_TIME <- format(slot_usage$DATE_TIME, "%m-%d-%Y %H:%M:%S") #refomrat Date_Time
slot_usage$appt_dow <- weekdays(as.Date(slot_usage$DATE_TIME)) #Get the DOW based off of Date_time
slot_usage$appt_month <- months(as.Date(slot_usage$DATE_TIME)) #get month of appt
slot_usage$slot_hour <- hour(strptime(slot_usage$DATE_TIME, format = "%m-%d-%Y %H:%M:%S"))
slot_usage <- slot_usage[!is.na(slot_usage$RESOURCE),] #deletes rows with NA as a rsource
resource_list <- as.list(unique(slot_usage$RESOURCE)) #list of unique providers
slot_usage$resource_time_comb <- paste(slot_usage$RESOURCE,slot_usage$DATE_TIME) #combine time and resource 
slot_usage$overbook <- duplicated(slot_usage$resource_time_comb)




# Appt Updates Pre-Process ------------------------------------------------
appt_updates$appt_dow <- weekdays(as.Date(appt_updates$APPT_DATE_TIME)) #get dow of appt
appt_updates$req_to_scheduled <- as.Date(appt_updates$APPT_DATE_TIME) - as.Date(appt_updates$ACTION_DATE_TIME) #Calculated time from appt requested to scheduled

