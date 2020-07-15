# Packages ----------------------------------------------------------------
library("readxl")


# Data Import -------------------------------------------------------------

data_LOC <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/Oncology/Data/Sample Slot Usage and Appointment Update Data.xlsx"
slot_usage <- read_excel(data_LOC, sheet = 1)
appt_updates <- read_excel(data_LOC, sheet = 2)
