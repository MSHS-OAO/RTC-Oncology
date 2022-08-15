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
  library(tcltk)
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
})

source("global_functions.R")


con <- dbConnect(odbc::odbc(), "OAO Cloud DB", timeout = 30)
oncology_tbl <- tbl(con, "ONCOLOGY_ACCESS")


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
  utilization.data <- readRDS("Data/utilization_data_grouped.rds")
  holid <-as.data.frame(read_feather(here::here("Data/holid.feather")))
  filter_path <- here::here("Filters")
}else{
  #historical.data <- as.data.frame(read_feather("/data/Oncology/Data/historical_data.feather"))
  #population.data_filtered <- as.data.frame(read_feather("/data/Oncology/Data/population_data_filtered.feather"))
  holid <- as.data.frame(read_feather("/data/Oncology/Data/holid.feather"))
  utilization.data <- as.data.frame(read_feather("/data/Oncology/Data/utilization_data.feather"))
  filter_path <- "/data/Oncology/Filters"
}





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

groupByFilters_Trend <- function(dt, campus, department
                                 , mindateRange, maxdateRange
                                 , daysofweek, holidays, diag
                                 ){
  format <- "YYYY-MM-DD HH24:MI:SS"
  daysofweek <- toupper(daysofweek)
  
  
  result <- dt %>% filter(SITE %in% campus, 
                          DEPARTMENT_NAME %in% department, 
                          TO_DATE(mindateRange, format) <= APPT_DTTM, 
                          TO_DATE(maxdateRange, format) >= APPT_DTTM, 
                          APPT_DAY %in% daysofweek,
                          DX_GROUPER %in% diag#, 
                          #!HOLIDAY %in% holidays
  ) 
  
  if("NA" %in% diag){
    result_1 <- dt %>% filter(SITE %in% campus, 
                            DEPARTMENT_NAME %in% department, 
                            TO_DATE(mindateRange, format) <= APPT_DTTM, 
                            TO_DATE(maxdateRange, format) >= APPT_DTTM, 
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
