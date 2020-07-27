# Packages ----------------------------------------------------------------
library("readxl")


# Data Import -------------------------------------------------------------

#data_LOC <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/Oncology/Data/Sample Slot Usage and Appointment Update Data.xlsx"
data_LOC <- "C:/Users/Armando/Documents/RTC-Data/Sample Slot Usage and Appointment Update Data.xlsx"
slot_usage <- read_excel(data_LOC, sheet = 1)
appt_updates <- read_excel(data_LOC, sheet = 2)



# Slot Usage Pre-Process --------------------------------------------------
slot_usage$DOW <- weekdays(as.Date(slot_usage$DATE_TIME)) #Get the DOW based off of Date_time
appt_updates$DOW <- weekdays(as.Date(appt_updates$APPT_DATE_TIME))

