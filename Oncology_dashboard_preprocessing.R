#install.packages("readxl")
library(timeDate)
library(readxl)
library(dplyr)
library(lubridate)


if (list.files("J://") == "Presidents") {
  user_directory <- "J:/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects"
} else {
  user_directory <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects"
}

###################### Raw Data ######################
#read the Ambulatory Care raw data 
#amb_data <- read.csv(choose.files(default = paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Access/Monthly/*.*"), caption = "Select Ambulatory Care Raw Data"), stringsAsFactors = FALSE)

##### pull Ambulatory Care Monthly data 
file_list_amb <- list.files(path = paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Access/Monthly"), pattern = "^(2020)|(2021)\\-[0-9]{2}\\-[0-9]{2}")

amb_list <- lapply(file_list_amb, function(x) read.csv(paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Access/Monthly/", x), stringsAsFactors = FALSE))

amb_df <- bind_rows(amb_list)

###################### References ######################
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

#make sure to fix the lab vs labs 
#unique(PRC_mapping$`Association List : A`) 
#--> [1] "EXCLUDE"   "Lab"       "Labs"      "Office"    "Treatment"

PRC_mapping$`Association List : A`[PRC_mapping$`Association List : A` == "Lab"] <- "Labs"

#fix capitalization
#unique(PRC_mapping$`Association List: B`) 
#[1] "EXCLUDE"           "Lab Visit"         "Lab visit"         "APH Visit"         "Established Visit"
#[6] "New Visit"         "Nurse Visit"       "Procedure"         "Telehealth Visit"  "Treatment Visit"

PRC_mapping$`Association List: B`[PRC_mapping$`Association List: B` == "Lab visit"] <- "Lab Visit"

#unique(PRC_mapping$`Association List: T`) 
#[1] "EXCLUDE"              "Non - Tx"             "Phlebotomy"           "APH Infusion"        
#[5] "Infusion"             "Transfusion"          "Injection"            "Hydration"           
#[9] "Port Flush"           "Pump Disconnect"      "Therapeutic Infusion"

#unique(PRC_mapping$`In Person vs Telehealth`) 
#[1] "In Person"  "Telehealth"

#change column names for the PRC mapping
colnames(PRC_mapping) <- c("PRC_NAME", "AssociationListA", "AssociationListB", "AssociationListT", "InPersonvsTele")

#merge the ambulatory data with the grouping data
amb_df_groupings <- merge(amb_df, department_mapping, by=c("DEPARTMENT_NAME"))
amb_df_groupings_ <- merge(amb_df_groupings, PRC_mapping, by = c("PRC_NAME"))





