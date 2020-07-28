# Packages ----------------------------------------------------------------
library("readxl")


# Data Import -------------------------------------------------------------

#data_LOC <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/Oncology/Data/Sample Slot Usage and Appointment Update Data.xlsx"
data_LOC <- "C:/Users/Armando/Documents/RTC-Data/Sample Slot Usage and Appointment Update Data.xlsx"
slot_usage <- read_excel(data_LOC, sheet = 1)
appt_updates <- read_excel(data_LOC, sheet = 2)



# Slot Usage Pre-Process --------------------------------------------------
slot_usage$appt_dow <- weekdays(as.Date(slot_usage$DATE_TIME)) #Get the DOW based off of Date_time
slot_usage$appt_month <- months(as.Date(slot_usage$DATE_TIME))


# Appt Updates Pre-Process ------------------------------------------------
appt_updates$appt_dow <- weekdays(as.Date(appt_updates$APPT_DATE_TIME))
appt_updates$req_to_scheduled <- as.Date(appt_updates$APPT_DATE_TIME) - as.Date(appt_updates$ACTION_DATE_TIME) #Calculated time from appt requested to scheduled

