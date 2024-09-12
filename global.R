suppressMessages({
  library(readxl)
  library(writexl)
  library(plyr)
  library(dplyr)
  library(data.table)
  library(zoo)
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyWidgets)
  library(htmlwidgets)
  library(lubridate)
  #library(tcltk)
  library(tidyverse)
  library(plotly)
  library(knitr)
  library(kableExtra)
  library(leaflet)
  library(grid)
  library(gridExtra)
  library(eeptools)
  library(ggQC)
  #library(zipcode)
  library(utils)
  library(scales)
  library(chron)
  library(bupaR)
  library(shiny)
  library(DT)
  library(DiagrammeR)
  library(shinyalert)
  library(edeaR)
  library(processmapR)
  library(processmonitR)
  library(processanimateR)
  library(tidyr)
  library(lubridate)
  library(RColorBrewer)
  library(DiagrammeR)
  library(ggplot2)
  library(leaflet)
  library(readr)
  library(highcharter)
  library(ggforce) # for 'geom_arc_bar'
  library(packcircles) # for packed circle graph
  library(viridis)
  library(ggiraph)
  library(treemapify)
  library(treemap)
  library(broom)
  library(extrafont)
  library(tis) # for US holidays
  library(vroom)
  library(sjmisc)
  library(tools)
  library(here)
  library(shinyBS)
  library(shinyscreenshot)
  library(fasttime)
  library(shinycssloaders)
  library(patchwork)
  library(ggtext)
  library(janitor)
  library(viridis) # Load color brewer palettes
  #library(zipcodeR)
  library(feather)
  library(reactable)
  library(rhandsontable)
  library(glue)
  library(DBI)
  library(shinydashboardPlus)
  library(shinycssloaders)
  library(shinyBS)
  library(shinyscreenshot)
})


source("global_functions.R")


con <- dbConnect(odbc::odbc(), "OAO Cloud DB", timeout = 30)
oncology_tbl <- tbl(con, "ONCOLOGY_ACCESS")
oncology_filters_tbl <- tbl(con, "ONCOLOGY_FILTERS")
mrn_treatment <- tbl(con, "ONCOLOGY_ACTIVE_TREATMENT_MRN")
oncology_filters_updated <- tbl(con, "ONCOLOGY_FILTERS_UPDATED")


### (2) Import Data ----------------------------------------------------------------------------------

# Define file paths for use later in the script
monthly_access <- here::here("Data/Access/Monthly")
monthly_slot <- here::here("Data/Slot/Monthly")
singleday_access <- here::here("Data/Access/SingleDay")
singleday_slot <- here::here("Data/Slot/SingleDay")

### (6) Data Subset -----------------------------------------------------------------------------------------------------


##Data on Rconnect
if(file.exists("J:/")){
  #### Local Data Directories
  #historical.data <- readRDS("Data/historical_data.rds")
  #population.data_filtered <- readRDS("Data/population_data_grouped.rds")
  # utilization.data <- readRDS("Data/utilization_data_grouped.rds")
  filter_path <- here::here("Filters")
}else{
  #historical.data <- as.data.frame(read_feather("/data/Oncology/Data/historical_data.feather"))
  #population.data_filtered <- as.data.frame(read_feather("/data/Oncology/Data/population_data_filtered.feather"))
  # holid <- as.data.frame(read_feather("/data/Oncology/Data/holid.feather"))
  # utilization.data <- as.data.frame(read_feather("/data/Oncology/Data/utilization_data.feather"))
  filter_path <- "/data/Oncology/Filters"
}


holid <-tbl(con, "HOLIDAYS") %>% distinct(HOLIDAY) %>% rename(holiday = HOLIDAY) %>% collect()



### Zip Code Analysis --------------------------------------------------------------------------------------------------

### (6) Shiny App Components Set-up -------------------------------------------------------------------------------

# Mater Filters 
daysOfWeek.options <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun") ## Days of Week Filter

timeOptionsHr <- c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00",
                   "10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00",
                   "20:00","21:00","22:00","23:00") ## Time Range by Hour Filter

timeOptions30m <- c("00:00","00:30","01:00","01:30","02:00","02:30","03:00","03:30","04:00","04:30",
                    "05:00","05:30","06:00","06:30","07:00","07:30","08:00","08:30","09:00","09:30",
                    "10:00","10:30","11:00","11:30","12:00","12:30","13:00","13:30","14:00","14:30",
                    "15:00","15:30","16:00","16:30","17:00","17:30","18:00","18:30","19:00","19:30",
                    "20:00","20:30","21:00","21:30","22:00","22:30","23:00","23:30") ## Time Range by 30min Filter

timeOptionsHr_filter <- c("07:00","08:00","09:00",
                          "10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00",
                          "20:00") ## Time Range by Hour Filter

timeOptions30m_filter <- c("07:00","07:30","08:00","08:30","09:00","09:30",
                           "10:00","10:30","11:00","11:30","12:00","12:30","13:00","13:30","14:00","14:30",
                           "15:00","15:30","16:00","16:30","17:00","17:30","18:00","18:30","19:00","19:30","20:00") ## Time Range by 30min Filter

monthOptions <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


# Reference dataframes, vectors, etc.
Time <- rep(timeOptionsHr, 7)
Day <- rep(daysOfWeek.options, each = 24)
byDayTime.df <- as.data.frame(cbind(Day,Time)) ## Empty data frame for day of week by time (hour)

all_provider <- read_excel("www/Mappings/Oncology System Dashboard - Data Groupings - Saved 11.10.2021.xlsx", sheet = "Provider ID Mappings") %>% filter(`Exam Treatment Utilization - Active Filter` == "Active")
all_provider <- all_provider[,1]


operating_hours_choices <- c("7:00AM", "7:30AM",  "8:00AM", "8:30AM", "9:00AM", "9:30AM", "10:00AM", "10:30AM", "11:00AM", "11:30AM", "12:00PM", "12:30PM", "1:00PM", "1:30PM", "2:00PM", "2:30PM", "3:00PM", "3:30PM", "4:00PM", "4:30PM", "5:00PM", "5:30PM", "6:00PM", "6:30PM", "7:00PM", "7:30PM", "8:00PM", "8:30PM")

callback <- callback <- JS(
  "var a = document.createElement('a');",
  "$(a).addClass('dt-button');",
  "a.href = document.getElementById('download1').href;",
  "a.download = '';",
  "$(a).attr('target', '_blank');",
  "$(a).text('Download');",
  "$('div.dwnld').append(a);",
  "$('#download1').hide();"
)

#max_date <- max(historical.data$Appt.DateYear)


#setDT(historical.data)

min_date <- "2019-01-01"

## Other datasets

# all.data.rows <- historical.data[Appt.DTTM >= min_date, which = TRUE]
# 
# arrived.data.rows <- historical.data[Appt.DTTM >= min_date & 
#                                        Appt.Status %in% c("Arrived"), which = TRUE]
# 
# canceled.bumped.rescheduled.data.rows <- historical.data[Appt.DTTM >= max_date - 1350 &
#                                                            Appt.Status %in% c("Canceled","Bumped","Rescheduled"), which = TRUE]
# 
# canceled.data.rows <- historical.data[Appt.DTTM >= min_date & 
#                                         Appt.Status %in% c("Canceled"), which = TRUE]
# 
# bumped.data.rows <- historical.data[Appt.DTTM >= min_date &
#                                       Appt.Status %in% c("Bumped"), which = TRUE]
# 
# rescheduled.data.rows <- historical.data[Appt.DTTM >= min_date &
#                                            Appt.Status %in% c("Rescheduled"), which = TRUE]
# 
# sameDay.rows <- historical.data[Appt.DTTM >= min_date &
#                                   Appt.Status %in% c("Canceled","Bumped","Rescheduled") &
#                                   Lead.Days == 0, which = TRUE]
# 
# noshow.data.rows <- historical.data[Appt.DTTM >= min_date &
#                                       Appt.Status %in% c("No Show"),
#                                     which = TRUE
# ]
# 
# noshow.data.rows <- c(sameDay.rows, noshow.data.rows)
# 
# arrivedNoShow.data.rows <-  c(noshow.data.rows, arrived.data.rows)
# 
# arrivedDisease.data.rows <- historical.data[Appt.DTTM >= min_date & 
#                                               Appt.Status %in% c("Arrived") &
#                                               !(Disease_Group %in% c("No Disease Group")), which = TRUE]
# 
# 
# ####TREND 
# 
# arrived.data.rows.trend <- historical.data[Appt.DTTM >= max_date - 1350 & 
#                                              Appt.Status %in% c("Arrived"), which = TRUE]
# 
# 
# 
# ##### Unique
# arrived.data.rows.unique <- historical.data[Appt.DTTM >= floor_date(max_date %m-% months(36), unit = "month") & 
#                                               Appt.Status %in% c("Arrived"), which = TRUE]
# 
# 
# 
# 
# 
# historical.data <- as.data.frame(historical.data)
# unique_min <- min(historical.data[arrived.data.rows.unique,]$Appt.DateYear)
# unique_max <- max(historical.data[arrived.data.rows.unique,]$Appt.DateYear)
unique_max <- "2022-01-01"
unique_min <- "2021-01-01"


arrived_data <- oncology_tbl %>% filter(APPT_STATUS %in% c("Arrived"))
arrivedNoShow_data_rows <- oncology_tbl %>% filter((APPT_STATUS %in% c("No Show", "Arrived")) | (APPT_STATUS %in% c("Canceled", "Rescheduled") & (TO_CHAR(APPT_CANC_DTTM, "YYYY-MM-DD") == TO_CHAR(APPT_DTTM, "YYYY-MM-DD"))))

groupByFilters_Trend <- function(dt, campus, department
                                 , mindateRange, maxdateRange
                                 , daysofweek, holidays, diag
                                 ){
  format <- "YYYY-MM-DD HH24:MI:SS"
  daysofweek <- toupper(daysofweek)
  maxdateRange <- as.Date(maxdateRange) + 1
  
  
  result <- dt %>% filter(SITE %in% campus, 
                          DEPARTMENT_NAME %in% department, 
                          TO_DATE(mindateRange, format) <= APPT_DTTM, 
                          TO_DATE(maxdateRange, format) > APPT_DTTM, 
                          APPT_DAY %in% daysofweek,
                          DX_GROUPER %in% diag#, 
                          #!HOLIDAY %in% holidays
  ) 
  
  if("NA" %in% diag){
    result_1 <- dt %>% filter(SITE %in% campus, 
                            DEPARTMENT_NAME %in% department, 
                            TO_DATE(mindateRange, format) <= APPT_DTTM, 
                            TO_DATE(maxdateRange, format) > APPT_DTTM, 
                            APPT_DAY %in% daysofweek,
                            is.na(DX_GROUPER))
    
    result <- result %>% union_all(result_1)
  }
  return(result)
  
}

groupByFilters_access <- function(dt, campus, department
                                 , mindateRange, maxdateRange
                                 , daysofweek, holidays, diag
){
  format <- "YYYY-MM-DD HH24:MI:SS"
  daysofweek <- toupper(daysofweek)
  maxdateRange <- as.Date(maxdateRange) + 1
  
  
  result <- dt %>% filter(SITE %in% campus, 
                          DEPARTMENT_NAME %in% department, 
                          TO_DATE(mindateRange, format) <= APPT_MADE_DATE_YEAR, 
                          TO_DATE(maxdateRange, format) > APPT_MADE_DATE_YEAR, 
                          APPT_DAY %in% daysofweek,
                          DX_GROUPER %in% diag#, 
                          #!HOLIDAY %in% holidays
  ) 
  
  if("NA" %in% diag){
    result_1 <- dt %>% filter(SITE %in% campus, 
                              DEPARTMENT_NAME %in% department, 
                              TO_DATE(mindateRange, format) <= APPT_MADE_DATE_YEAR, 
                              TO_DATE(maxdateRange, format) > APPT_MADE_DATE_YEAR, 
                              APPT_DAY %in% daysofweek,
                              is.na(DX_GROUPER))
    
    result <- result %>% union_all(result_1)
  }
  return(result)
  
}


all_provider <- read_excel("www/Mappings/Oncology System Dashboard - Data Groupings - Saved 11.10.2021.xlsx", sheet = "Provider ID Mappings") %>% filter(`Exam Treatment Utilization - Active Filter` == "Active")
all_provider <- all_provider[,1]


# default_campus <- unique(historical.data$SITE)
# arrivedDisease.data <- historical.data[arrivedDisease.data.rows,]


### Uniqie Patietns
# historical.data.unique.exam <- uniquePts_df_system(historical.data[arrived.data.rows.unique,], c("Exam"))
# historical.data.unique.exam.month <- uniquePts_df_systemMonth(historical.data[arrived.data.rows.unique,], c("Exam"))
# historical.data.unique.all <- uniquePts_df_system(historical.data[arrived.data.rows.unique,], c("Exam","Labs","Treatment"))
# historical.data.unique.all.month <- uniquePts_df_systemMonth(historical.data[arrived.data.rows.unique,], c("Exam","Labs","Treatment"))
# historical.data.unique.treatment <- uniquePts_df_system(historical.data[arrived.data.rows.unique,], c("Treatment Visit"))
# historical.data.unique.treatment.month <- uniquePts_df_systemMonth(historical.data[arrived.data.rows.unique,], c("Treatment Visit"))
# 
# 
# historical.data.site.exam.month <- uniquePts_df_siteMonth(historical.data[arrived.data.rows.unique,], c("Exam"))
# historical.data.site.exam <- uniquePts_df_site(historical.data[arrived.data.rows.unique,], c("Exam"))
# historical.data.site.all.month <- uniquePts_df_siteMonth(historical.data[arrived.data.rows.unique,], c("Exam","Labs","Treatment"))
# historical.data.site.all <- uniquePts_df_site(historical.data[arrived.data.rows.unique,], c("Exam","Labs","Treatment"))
# historical.data.site.treatment <- uniquePts_df_site(historical.data[arrived.data.rows.unique,], c("Treatment"))
# historical.data.site.treatment.month <- uniquePts_df_siteMonth(historical.data[arrived.data.rows.unique,], c("Treatment"))


get_values <- function(x,table_name){
  
  filter_name <- x[1]
  campus <- x[2]
  department <- x[3]
  days <- x[4]
  holiday <- x[5]
  diagnosis <- x[6]
  
  values <- glue("INTO \"{table_name}\" (FILTER_NAME,CAMPUS,DEPARTMENT, DAYS, HOLIDAYS, DIAGNOSIS_GROUPER) 
                 VALUES('{filter_name}','{campus}','{department}','{days}', '{holiday}', '{diagnosis}')")
  
  return(values)
}


write_filters_db <- function(df) {
  print("function_start")
  df[] <- lapply(df, as.character)
  
  df <-  df %>% mutate_at(vars(colnames(df)), ~ str_replace(., "\'", "''")) %>% 
    mutate_at(vars(colnames(df)), ~ str_replace(., "&", "' || chr(38) || '")) %>%
    select(Name, Campus, Department, Days, Holiday, Diagnosis_Grouper)
  
  TABLE_NAME <- "ONCOLOGY_FILTERS"
  
  inserts <- lapply(
    lapply(
      lapply(split(df , 
                   1:nrow(df)),
             as.list), 
      as.character),
    FUN = get_values ,TABLE_NAME)
  
  values <- glue_collapse(inserts,sep = "\n\n")
  all_data <- glue('INSERT ALL
                        {values}
                      SELECT 1 from DUAL;')
  
  all_data <<- gsub("'NA'", "''", all_data)
  
  conn <- dbConnect(odbc::odbc(), "OAO Cloud DB")
  print("after conn")
  
  dbBegin(conn)
  tryCatch({
    dbExecute(conn, all_data)
    dbCommit(conn)
    dbDisconnect(conn)
    if(isRunning()) {
      showModal(modalDialog(
        title = "Success",
        paste0("The filters have been saved successfully."),
        easyClose = TRUE,
        footer = NULL
      ))
    } else{
      print(paste0("The filters have been saved successfully."))
    }
  },
  error = function(err){
    #print(err)
    dbRollback(conn)
    dbDisconnect(conn)
    print("error")
    if(isRunning()) {
      showModal(modalDialog(
        title = "Error",
        paste0("There was an issue saving the filters."),
        easyClose = TRUE,
        footer = NULL
      ))
    } else{
      print(paste0("There was an issue saving the filters."))
    }
  })
  
}

### Set default values for master filters --------------------------------------------------------------------------------------
#default_campus <- "DBC"
#default_campus <- unique(historical.data$SITE)
dateRangetrend_start <- as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01"), format="%Y-%m-%d")

campus_choices <- oncology_tbl %>% select(SITE) %>% mutate(SITE = unique(SITE)) %>%
                       collect()
campus_choices <- sort(campus_choices$SITE, na.last = T)


default_campus <- "MSW"

#default_departments <- sort(unique(historical.data[historical.data$SITE %in% default_campus, "Department"])) 
default_departments <- oncology_tbl %>% filter(SITE %in% default_campus) %>% 
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR) %>%
  select(DEPARTMENT_NAME) %>%
  mutate(DEPARTMENT_NAME = unique(DEPARTMENT_NAME)) %>%
  collect()
default_departments <- sort(default_departments$DEPARTMENT_NAME, na.last = T)


# default_diag_grouper <- sort(unique(historical.data[historical.data$SITE %in% default_campus &
#                                                       historical.data$Department %in% default_departments, "Dx.Grouper"]), na.last = TRUE) 

default_diag_grouper <- oncology_tbl %>% filter(SITE %in% default_campus & DEPARTMENT_NAME %in% default_departments) %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") > APPT_DATE_YEAR) %>%
  select(DX_GROUPER) %>% mutate(DX_GROUPER = unique(DX_GROUPER)) %>%
  collect()
default_diag_grouper <- sort(default_diag_grouper$DX_GROUPER, na.last = T)

# default_visitType <- sort(unique(historical.data[historical.data$SITE %in% default_campus &
#                                                    historical.data$Department %in% default_departments, "AssociationListA"]))


default_visitType <- oncology_tbl %>% filter(SITE %in% default_campus & DEPARTMENT_NAME %in% default_departments) %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") > APPT_DATE_YEAR) %>%
  select(ASSOCIATIONLISTA) %>%
  mutate(ASSOCIATIONLISTA = unique(ASSOCIATIONLISTA)) %>%
  collect()
default_visitType <- sort(default_visitType$ASSOCIATIONLISTA, na.last = T)

default_visitType <- default_visitType[!is.na(default_visitType)]


# default_ApptType <- sort(unique(historical.data[historical.data$SITE %in% default_campus &
#                                                   historical.data$Department %in% default_departments &
#                                                   historical.data$AssociationListA %in% default_visitType, "AssociationListB"]))

default_ApptType <- oncology_tbl %>% filter(SITE %in% default_campus & DEPARTMENT_NAME %in% default_departments &
                                              ASSOCIATIONLISTA %in%  default_visitType) %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") > APPT_DATE_YEAR) %>%
  select(ASSOCIATIONLISTB) %>% 
  mutate(ASSOCIATIONLISTB = unique(ASSOCIATIONLISTB)) %>% 
  collect()
default_ApptType <- sort(default_ApptType$ASSOCIATIONLISTB, na.last = T)




# 
# default_TreatmentType <- sort(unique(historical.data[historical.data$SITE %in% default_campus &
#                                                        historical.data$Department %in% default_departments &
#                                                        historical.data$AssociationListA %in% default_visitType &
#                                                        historical.data$AssociationListB %in% default_ApptType, "AssociationListT"]))

default_TreatmentType <- oncology_tbl %>% filter(SITE %in% default_campus & DEPARTMENT_NAME %in% default_departments &
                                                   ASSOCIATIONLISTA %in% default_visitType &
                                                   ASSOCIATIONLISTB %in% default_ApptType) %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") > APPT_DATE_YEAR) %>%
  select(ASSOCIATIONLISTT) %>% 
  mutate(ASSOCIATIONLISTT = unique(ASSOCIATIONLISTT)) %>% 
  collect()
default_TreatmentType <- sort(default_TreatmentType$ASSOCIATIONLISTT, na.last = T)


# default_departments_disease <- sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% default_campus, "Department"]))

default_departments_disease <- oncology_tbl %>% filter(SITE %in% default_campus & APPT_STATUS %in% c("Arrived")) %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR) %>%
  select(DEPARTMENT_NAME) %>%
  mutate(DEPARTMENT_NAME = unique(DEPARTMENT_NAME)) %>%
  collect()
default_departments_disease <- sort(default_departments_disease$DEPARTMENT_NAME, na.last = T)



# default_disease_group <- sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% default_campus &
#                                                            arrivedDisease.data$Department %in% default_departments_disease, "Disease_Group"]))

default_disease_group <- oncology_tbl %>% filter(SITE %in% default_campus & APPT_STATUS %in% c("Arrived") &
                                                   DEPARTMENT_NAME %in% default_departments_disease) %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") > APPT_DATE_YEAR) %>%
  select(DISEASE_GROUP) %>%
  mutate(DISEASE_GROUP = unique(DISEASE_GROUP)) %>%
  collect()
default_disease_group <- sort(default_disease_group$DISEASE_GROUP)


default_disease_group_all <- oncology_tbl %>% filter(APPT_STATUS %in% c("Arrived")) %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") > APPT_DATE_YEAR) %>%
  select(DISEASE_GROUP) %>%
  mutate(DISEASE_GROUP = unique(DISEASE_GROUP)) %>%
  collect()


default_disease_group_all <- sort(default_disease_group_all$DISEASE_GROUP)

default_disease_group_detail <- oncology_tbl %>% filter(SITE %in% default_campus & APPT_STATUS %in% c("Arrived") &
                                                          DEPARTMENT_NAME %in% default_departments_disease &
                                                          DISEASE_GROUP %in% default_disease_group) %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR) %>%
  select(DISEASE_GROUP_DETAIL) %>%
  mutate(DISEASE_GROUP_DETAIL = unique(DISEASE_GROUP_DETAIL)) %>%
  collect()

default_disease_group_detail <- sort(default_disease_group_detail$DISEASE_GROUP_DETAIL, na.last = T)




# default_provider <- sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% default_campus &
#                                                       arrivedDisease.data$Department %in% default_departments_disease &
#                                                       arrivedDisease.data$Disease_Group %in% default_disease_group, "Provider"]))


default_provider <- oncology_tbl %>% filter(SITE %in% default_campus & APPT_STATUS %in% c("Arrived") &
                                              DEPARTMENT_NAME %in% default_departments_disease & 
                                              DISEASE_GROUP %in% default_disease_group) %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR) %>%
  select(PROVIDER) %>%
  mutate(PROVIDER = unique(PROVIDER)) %>%
  collect()
default_provider <- sort(default_provider$PROVIDER, na.last = T)


treatment_disease <- c("Benign Hematology" , "Hematology Oncology", "Liquid Tumors", "Medical Oncology", "Oncology", "Solid Tumors")
default_provider_treatment_conversions <- oncology_tbl %>% filter(SITE %in% default_campus & APPT_STATUS %in% c("Arrived") &
                                              DEPARTMENT_NAME %in% default_departments_disease & 
                                              DISEASE_GROUP %in% treatment_disease) %>%
                                            filter(DISEASE_GROUP_DETAIL != "Breast Surgery") %>%
                                            filter(PROVIDER_TYPE == "Physician") %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR) %>%
  select(PROVIDER) %>%
  mutate(PROVIDER = unique(PROVIDER)) %>%
  collect()
default_provider_treatment_conversions <- sort(default_provider_treatment_conversions$PROVIDER, na.last = T)


default_provider_unique_exam <- oncology_tbl %>% filter(SITE %in% default_campus & APPT_STATUS %in% c("Arrived") &
                                                          DEPARTMENT_NAME %in% default_departments &
                                                          ASSOCIATIONLISTA %in% c("Exam")) %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR) %>%
  select(PROVIDER) %>%
  mutate(PROVIDER = unique(PROVIDER)) %>%
  collect()
default_provider_unique_exam <- sort(default_provider_unique_exam$PROVIDER, na.last = T)

default_referring_provider <- oncology_tbl %>% filter(SITE %in% default_campus & APPT_STATUS %in% c("Arrived") &
                                                        DEPARTMENT_NAME %in% default_departments &
                                                        ASSOCIATIONLISTA %in% c("Treatment") &
                                                        ASSOCIATIONLISTB %in% c("Treatment Visit")) %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR) %>%
  select(REFERRING_PROVIDER, REFERRING_PROV_ID) %>%
  distinct(REFERRING_PROVIDER, REFERRING_PROV_ID) %>%
  collect()

referring_provider_site <- tbl(con, "ONCOLOGY_REFERRING_PROVIDER_SITE_VIEW") %>% select(EPIC_PROVIDER_ID, SITE_REFERRING, PROVIDER_NAME) %>% collect() %>% rename(REFERRING_PROV_ID = EPIC_PROVIDER_ID)

default_referring_provider <- inner_join(default_referring_provider, referring_provider_site)

 default_referring_provider <- default_referring_provider %>% filter(grepl(default_campus,SITE_REFERRING))

default_referring_provider <- sort(default_referring_provider$REFERRING_PROVIDER, na.last = T)

referring_provider_type_mapping <- tbl(con, "ONCOLOGY_DISEASE_GROUPINGS") %>% 
                                    select(EPIC_PROVIDER_ID, PROVIDER_TYPE) %>% collect() %>% rename(REFERRING_PROV_ID = EPIC_PROVIDER_ID)




# default_provider_utilization <- data.frame(Provider = sort(unique(historical.data[historical.data$SITE %in% default_campus &
#                                                               historical.data$Department %in% default_departments, "Provider"])),
#                                            stringsAsFactors=FALSE
#                                   )


default_provider_utilization <- oncology_tbl %>% filter(SITE %in% default_campus & APPT_STATUS %in% c("Arrived") &
                                                          DEPARTMENT_NAME %in% default_departments) %>%
  # filter(TO_DATE(dateRangetrend_start, "YYYY-MM-DD HH24:MI:SS") > APPT_DATE_YEAR) %>%
  select(PROVIDER) %>%
  mutate(PROVIDER = unique(PROVIDER)) %>%
  collect()
default_provider_utilization <- data.frame(Provider = sort(default_provider_utilization$PROVIDER, na.last = T), stringsAsFactors=FALSE)



default_provider_utilization <- as.character(t(inner_join(default_provider_utilization, all_provider)))


util_date_start <- as.Date(paste0(year(Sys.Date()),"-01-01"), format = "%Y-%d-%m")
# util_date_min <- min(utilization.data$Appt.DateYear)
# util_date_end = max(utilization.data$Appt.DateYear)


#dateRange_min <- min(historical.data$Appt.DateYear) 
#dateRange_min <- min(historical.data[all.data.rows,])
dateRange_min_default <- as.Date("2021-01-01")
dateRange_min_default_unique <- as.Date("2021-01-01")
#dateRange_min_default <- min(historical.data$Appt.DateYear) 
daysOfWeek.options <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
#today <- Sys.Date()

#dateRangetrend_min <- as.Date("2019-01-01")
# dateRange_max <- max(historical.data$Appt.DateYear)

dateRangetrend_min <- glue("Select min(APPT_DTTM) AS minDate FROM ONCOLOGY_ACCESS WHERE APPT_STATUS = 'Arrived'")
dateRangetrend_min <- dbGetQuery(con, dateRangetrend_min)
dateRangetrend_min <- as.Date(dateRangetrend_min$MINDATE, format="%Y-%m-%d")

#dateRangetrend_min <- as.Date("2021-01-01", format="%Y-%m-%d")



dateRange_max <- glue("Select max(APPT_DTTM) AS maxDate FROM ONCOLOGY_ACCESS WHERE APPT_STATUS = 'Arrived'")
dateRange_max <- dbGetQuery(con, dateRange_max)
dateRange_max <- as.Date(dateRange_max$MAXDATE, format="%Y-%m-%d")

dateRange_download_start <- floor_date(dateRange_max, 'month') 

# dateRangeunique_min <- min(historical.data[arrived.data.rows.unique,]$Appt.DateYear)

default_filter_choices <- oncology_filters_tbl %>% summarise(filter = unique(FILTER_NAME)) %>% collect()

header <-   dashboardHeader(title = HTML("Oncology Analytics Tool"),
                            disable = FALSE,
                            titleWidth = 400,
                            tags$li(class = "dropdown", actionButton("download10",
                                                                     label = icon("download")
                            )
                            ),
                            
                            tags$li(class = "dropdown",
                                    dropdown(
                                      box(
                                        title = "Bookmark Current Filter:",
                                        width = 12,
                                        height = "200px",
                                        solidHeader = FALSE,
                                        h5("For naming your filters please follow: 'SITE_DESC'"),#, style = "font-size:12px;"), br(),
                                        textInput("filter_name", label = NULL),
                                        actionButton("save_filters", "CLICK TO SAVE", width = "80%")
                                      ), br(), br(), br(), br(), br(), br(), br(), br(),
                                      br(), br(),
                                      style = "material-circle", size = "lg", right = TRUE, status = "default",
                                      icon = icon("save"), width = "300px",
                                      inputId = "dropdownbutton4"
                                    )
                            ),
                            
                            tags$li(class = "dropdown", dropdown(box(title = "Retrieve Previously Saved Filter:",
                                                                     width = 12,
                                                                     height = "100px",
                                                                     solidHeader = FALSE,
                                                                     pickerInput("filter_list", choices = default_filter_choices$filter, multiple = TRUE,
                                                                                 selected = NULL, options = pickerOptions(maxOptions = 1)
                                                                     ),
                                                                     actionButton("update_filters1", "CLICK TO UPDATE", width = "80%")
                            ), br(), br(), br(), br(), br(), br(),
                            br(), br(),
                            # actionButton("remove_filters", "CLICK TO REMOVE", width = "80%"), br(), br(),
                            style = "material-circle", size = "lg", right = TRUE, status = "default",
                            icon = icon("star"), width = "300px",
                            tooltip = tooltipOptions(title = "Set additional filters for graphs/tables."),
                            inputId = "dropdownbutton3"
                            ),
                            )
                            
                            #)
                            
)



header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <-  tags$a(href='https://peak.mountsinai.org/',
                                              tags$img(src='Sinai_logo_white.png',height='100%',width='30%'))

race_grouper_choices <- c( "OVERALL", "BLANK/UNKNOWN", "PATIENT DECLINED",  "AFRICAN-AMERICAN", "ASIAN", "WHITE", "OTHER")
ethnicity_grouper_choices <- c( "OVERALL","BLANK/UNKNOWN", "PATIENT DECLINED", "HISPANIC", "NOT HISPANIC OR LATINO")

# race_grouper_choices <- oncology_tbl %>% select(RACE_GROUPER) %>%  distinct() %>% collect()
# race_grouper_choices <- sort(unique(race_grouper_choices$RACE_GROUPER))

provider_type_choices <- c("Physician", "Advanced Practice Provider")

download_list <- c("villea04", "portj01", "lium10", "jwallace", "lacham01", "hughej03", "yua17", "fleurf02", "caridr02", "taddej01", "martic56", "JENKIN01")
