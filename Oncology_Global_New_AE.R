### (0) Install and Load Required Packages ============================================================

# install.packages("readxl")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("zoo")
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinydashboardPlus")
# install.packages("leaflet")
# install.packages("shinyWidgets")
# install.packages("htmlwidgets")
# install.packages(c("readxl","writexl"))
# install.packages("anytime")
# 
# # install.packages("htmltools")
# # require(htmltools)
# # library(htmltools)
# # update.packages("htmltools")
# 
# # Packages from the process mapping codes [NEED TO BE CLEANED UP]
# install.packages('shinydashboard')
# install.packages('dplyr')
# install.packages('bupaR', dependencies = TRUE)
# install.packages('shiny')
# install.packages('DT')
# intall.packages('DiagrammerR')
# install.packages('shinyalert')
# install.packages('edeaR', dependencies = TRUE)
# install.packages('processmapR')
# install.packages('processmonitR')
# install.packages('processanimateR')
# install.packages('DiagrammeR')
# install.packages('shiny', type='binary')
# install.packages("shinydashboardPlus")
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("ggforce")
# install.packages("packcircles")
# install.packages("treemapify")
# install.packages("treemap")
# install.packages("tis")
# install.packages("vroom")
# install.packages("lubridate")
# install.packages("plyr")
# install.packages("sjmisc")
# install.packages("shinyBS")
# install.packages("shinyscreenshot")


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
  library(zipcode)
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
})

# ### (0) Maximize R Memory Size 
memory.limit(size = 8000000)

### (1) Set aesthetics theme -----------------------------------------------------------------------------

# Color Functions for Graphs =====================================
theme_set(theme_minimal())

# Mount Sinai corporate colors 
MountSinai_colors <- c(
  `dark purple`  = "#212070",
  `dark pink`    = "#d80b8c",
  `dark blue`    = "#00aeef",
  `dark grey`    = "#7f7f7f",
  `yellow`       = "#ffc000",
  `purple`       = "#7030a0",
  `med purple`   = "#5753d0",
  `med pink`     = "#f75dbe",
  `med blue`     = "#5cd3ff",
  `med grey`     = "#a5a7a5",
  `light purple` = "#c7c6ef",
  `light pink`   = "#fcc9e9",
  `light blue`   = "#c9f0ff",
  `light grey`   = "#dddedd"
)

# Function to extract Mount Sinai colors as hex codes
# Use Character names of MountSinai_colors

MountSinai_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (MountSinai_colors)
  
  MountSinai_colors[cols]
}

# Color Function that can be used to call all colors is "MountSinai_cols()"
# Use in ggplot 

#MountSinai_cols()       # will provide all colors and their hex codes in a table 
#MountSinai_cols("pink") # will provide color name and the hex code for the pink color

# Create palettes 
MountSinai_palettes <- list(
  `all`   = MountSinai_cols("dark purple","dark pink","dark blue","dark grey",
                            "med purple","med pink","med blue","med grey", 
                            "light purple","light pink","light blue","light grey"),
  
  `dark`  =  MountSinai_cols("dark purple","dark grey","dark pink","dark blue"),
  
  `main`  = MountSinai_cols("dark purple","dark grey","dark pink","dark blue","med purple","med pink","med blue","med grey"),
  
  `purple`  = MountSinai_cols("dark purple","med purple","light purple"),
  
  `pink`  = MountSinai_cols("dark pink","med pink","light pink"),
  
  `blue`  = MountSinai_cols("dark blue", "med blue", "light blue"),
  
  `grey`  = MountSinai_cols("dark grey", "med grey", "light grey"),
  
  `purpleGrey` = MountSinai_cols("dark purple", "dark grey"),
  
  `pinkBlue` = MountSinai_cols("dark pink", "dark blue")
  
)

# MountSinai_palettes
# Return function to interpolate a Mount Sinai color palette
# default value is the main palette, reverse = True will change the order

MountSinai_pal <- function(palette = "all", reverse = FALSE, ...) {
  pal <- MountSinai_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


# Scale Function for ggplot can be used instead of scale_color_manual
scale_color_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Scale Fill for ggplot insetead of scale_fill_manual 
scale_fill_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# ggplot theme function s====================================

# font_import()
# loadfonts(device = "win")
# windowsFonts()


graph_theme <- function(legend_pos) {
  theme(
    plot.title = element_text(hjust=0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust=0.5, size = 14),
    legend.position = legend_pos,
    strip.text = element_text(size=14),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle=40, hjust=1),
    axis.text.y = element_text(size = 12),
    axis.line.x = element_blank())#,
  #plot.margin = margin(0,80,0,80))
}

theme_new_line <- function(base_size = 12,
                           base_family = "Calibri",
                           base_line_size = base_size / 170,
                           base_rect_size = base_size / 170) {
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(plot.title = element_text(hjust=0.5, face = "bold", size = 24),
          plot.subtitle = element_text(hjust=0.5, size = 16, face = "italic"),
          plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
          legend.position = "top",
          legend.text = element_text(size="18"),
          legend.direction = "horizontal",
          legend.key.size = unit(1.0,"cm"),
          legend.title = element_blank(),
          axis.title = element_text(size="18"),
          axis.text = element_text(size="18"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(size = 0.3, colour = "black"),
          plot.margin = margin(30,30,30,30))
}

#added a theme for the tables
table_theme <- function(){
    theme(
      panel.grid.minor = element_line(size = 0.3, colour = "black"),
      panel.grid.major = element_blank(),
      axis.title.x = element_text(size = 14, angle = 0, colour = "black", face= "bold"),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 14, colour = "black", face= "bold"),
      legend.position = "none",
      plot.title = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size=1),
      axis.line.x = element_line(colour = "black", size=1),
      plot.margin=unit(c(-0.5,1,1,1), "cm"))
}


### (2) Import Data ----------------------------------------------------------------------------------

#singleday_path <<- here("Data/Access/SingleDay")
#monthly_path <<- here("Data/Access/Monthly")
#monthly_access <<- here("Data/Access/Monthly")
#monthly_slot <<- here("Data/Slot/Monthly")
#singleday_access <<- here("Data/Access/SingleDay")
#singleday_slot <<- here("Data/Slot/SingleDay")

ifelse (list.files("J://") == "Presidents", user_directory <- "J:/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects", 
        user_directory <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects")

monthly_access <- paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Access/Monthly")
monthly_slot <-paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Slot/Monthly")
singleday_access <- paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Access/SingleDay")
singleday_slot <- paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Slot/SingleDay")

# Set Working Directory (PILOT)
#wdpath <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Ambulatory Dashboard/Pilot Application v1"
#wdpath <- "J:/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Ambulatory Dashboard/Pilot Application v1"
# wdpath <- "C:/Users/kweons01/Desktop/Pilot Application v1"


#wdpath <- here::here()

#setwd(wdpath)


#master.data.new_new <- data_all

# ## Utilization Data
# data.hour.scheduled <- read_csv("Data/Utilization/data.hour.scheduled.pilotV1.csv")
# data.hour.arrived <- read_csv("Data/Utilization/data.hour.arrived.pilotV1.csv")

process_data <- function(access_data,slot_data){
  slot.data.raw <- slot_data
  ## Site-Dept Reference File
  #site_ref <-  read_xlsx("Data/Department Site Crosswalk 8-24-2020.xlsx", col_names = TRUE, na = c("", "NA")) 
  #site_ref <- read_excel("Data/Ambulatory Department Mapping (Master).xlsx",sheet = "Mapping")
  site_ref <- read_excel(paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Ambulatory Department Mapping (Master).xlsx"),sheet = "Mapping")
  ### (3) Pre-process data ----------------------------------------------------------------------------------
  # SCheduling Data Pre-processing
  data.raw <- access_data # Assign scheduling Data
  data.raw$campus_new <- site_ref$`Site`[match(data.raw$DEPARTMENT_NAME,site_ref$`Department Name`)] # Crosswalk Campus to Site by Department Name
  data.raw <- data.raw %>% filter(!campus_new == "NA") %>% filter(!campus_new %in% c("Other","OTHER","EHS")) ## Exclude Mapped Sites: Other, OTHER, EHS
  
  # Dummy columns until they are added to Clarity table: SEX, FPA
  data.raw$SEX <- "Male"
  data.raw$VITALS_TAKEN_TM <- ""
  data.raw$Provider_Leave_DTTM <- ""
  
  # Data fields incldued for analysis 
  original.cols <- c("campus_new","DEPT_SPECIALTY_NAME","DEPARTMENT_NAME","PROV_NAME_WID", "REFERRING_PROV_NAME_WID",
                     "MRN","PAT_NAME","ZIP_CODE","SEX","BIRTH_DATE","FINCLASS",
                     "APPT_MADE_DTTM","APPT_DTTM","PRC_NAME","APPT_LENGTH","DERIVED_STATUS_DESC",
                     "APPT_CANC_DTTM", "CANCEL_REASON_NAME",
                     "SIGNIN_DTTM","PAGED_DTTM","CHECKIN_DTTM","ARVL_LIST_REMOVE_DTTM",
                     "ROOMED_DTTM","FIRST_ROOM_ASSIGN_DTTM","VITALS_TAKEN_TM",
                     "PHYS_ENTER_DTTM","Provider_Leave_DTTM",
                     "VISIT_END_DTTM","CHECKOUT_DTTM",
                     "TIME_IN_ROOM_MINUTES","CYCLE_TIME_MINUTES","VIS_NEW_TO_DEP_YN","LOS_NAME", "DEP_RPT_GRP_THIRTYONE", 
                     "APPT_ENTRY_USER_NAME_WID", "ACCESS_CENTER_SCHEDULED_YN", "VISIT_METHOD", "VISIT_PROV_STAFF_RESOURCE_C")
  
  # Subset raw data 
  data.subset <- data.raw[original.cols]
  
  # Rename data fields (columns) 
  new.cols <- c("Campus","Campus.Specialty","Department","Provider", "Ref.Provider",
                "MRN","Patient.Name","Zip.Code","Sex","Birth.Date","Coverage",
                "Appt.Made.DTTM","Appt.DTTM","Appt.Type","Appt.Dur","Appt.Status",
                "Appt.Cancel.DTTM", "Cancel.Reason",
                "Signin.DTTM","Paged.DTTM","Checkin.DTTM","Arrival.remove.DTTM",
                "Roomin.DTTM","Room.assigned.DTTM","Vitals.DTTM",
                "Providerin_DTTM","Providerout_DTTM",
                "Visitend.DTTM","Checkout.DTTM",
                "Time.in.room","Cycle.time","New.PT","Class.PT","Cadence",
                "Appt.Source","Access.Center","Visit.Method","Resource")
  
  colnames(data.subset) <- new.cols
  
  # Format Date and Time Columns
  dttm.cols <- c("Birth.Date","Appt.Made.DTTM","Appt.DTTM","Appt.Cancel.DTTM",
                 "Checkin.DTTM","Arrival.remove.DTTM","Roomin.DTTM","Room.assigned.DTTM",
                 "Vitals.DTTM","Providerin_DTTM","Providerout_DTTM",
                 "Visitend.DTTM","Checkout.DTTM")
  
  dttm <- function(x) {
    as.POSIXct(x,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone(),origin = "1970-01-01")
  }
  
  data.subset$Birth.Date <- dttm(data.subset$Birth.Date)
  data.subset$Appt.Made.DTTM <- dttm(data.subset$Appt.Made.DTTM)
  data.subset$Appt.DTTM <- dttm(data.subset$Appt.DTTM)
  data.subset$Appt.Cancel.DTTM <- dttm(data.subset$Appt.Cancel.DTTM)
  data.subset$Checkin.DTTM <- dttm(data.subset$Checkin.DTTM)
  data.subset$Arrival.remove.DTTM <- dttm(data.subset$Arrival.remove.DTTM)
  data.subset$Roomin.DTTM <- dttm(data.subset$Roomin.DTTM)
  data.subset$Room.assigned.DTTM <- dttm(data.subset$Room.assigned.DTTM)
  data.subset$Vitals.DTTM <- dttm(data.subset$Vitals.DTTM)
  data.subset$Providerin_DTTM <- dttm(data.subset$Providerin_DTTM)
  data.subset$Providerout_DTTM <- dttm(data.subset$Providerout_DTTM)
  data.subset$Visitend.DTTM <- dttm(data.subset$Visitend.DTTM)
  data.subset$Checkout.DTTM <- dttm(data.subset$Checkout.DTTM)
  
  # Remove Provider ID from Provider Name column
  data.subset$Provider <- trimws(gsub("\\[.*?\\]", "", data.subset$Provider))
  data.subset$Ref.Provider <- trimws(gsub("\\[.*?\\]", "", data.subset$Ref.Provider))
  
  # New Patient Classification based on level of care ("LOS_NAME")
  data.subset$New.PT2 <- ifelse(is.na(data.subset$Class.PT), "",grepl("NEW", data.subset$Class.PT, fixed = TRUE))
  # New Patient Classification based on level of care ("LOS_NAME") and Visit New to Department (New.PT) TEMPORARY
  data.subset$New.PT3 <- ifelse(data.subset$New.PT2 == "", 
                                ifelse(data.subset$New.PT == "Y", TRUE, FALSE), data.subset$New.PT2)
  
  
  # Pre-process Appointment Source: access center, entry person, zocdoc, mychart, staywell
  data.subset$Appt.Source.New <- ifelse(data.subset$Access.Center == "Y", "Access Center","")
  data.subset$Appt.Source.New <- ifelse(data.subset$Appt.Source.New == "",
                                        ifelse(grepl("ZOCDOC", data.subset$Appt.Source, fixed = TRUE)==TRUE, "Zocdoc",
                                               ifelse(grepl("MYCHART", data.subset$Appt.Source, fixed = TRUE)==TRUE, "MyChart",
                                                      ifelse(grepl("STAYWELL", data.subset$Appt.Source, fixed = TRUE)==TRUE, "StayWell","Other"))),data.subset$Appt.Source.New)
  
  
  # Notify and remove duplicates in data 
  # data.duplicates <- data.subset %>% duplicated()
  # data.duplicates <- length(data.duplicates[data.duplicates == TRUE]) ## Count of duplicated records
  # data.subset.new <- data.subset %>% distinct() ## New data set with duplicates removed
  data.subset.new <- data.subset
  
  # Create additional columns for analysis 
  data.subset.new$Appt.DateYear <- as.Date(data.subset.new$Appt.DTTM, format="%Y-%m-%d") ## Create date-year column
  data.subset.new$Appt.MonthYear <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%Y-%m") ## Create month - year column
  data.subset.new$Appt.Date <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%m-%d") ## Create date column
  data.subset.new$Appt.Year <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%Y") ## Create year column
  data.subset.new$Appt.Month <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%b") ## Create month colunm
  data.subset.new$Appt.Quarter <- quarters(as.Date(data.subset.new$Appt.DTTM)) ## Create quarter column 
  data.subset.new$Appt.Week <- floor_date(as.Date(data.subset.new$Appt.DateYear, "%Y-%m-%d"), unit="week", week_start = 1) # Create week column
  data.subset.new$Appt.Day <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%a") ## Create day of week colunm
  data.subset.new$Time <- format(as.POSIXct(as.ITime(data.subset.new$Appt.DTTM, format = "%H:%M")), "%H:%M") ## Create Time column
  data.subset.new$Appt.TM.Hr <- format(strptime(as.ITime(floor_date(data.subset.new$Appt.DTTM, "hour")), "%H:%M:%S"),'%H:%M') ## Appt time rounded by hour 
  # data.subset.new$Appt.TM.30m <- format(strptime(as.ITime(round_date(data.subset.new$Appt.DTTM, "30 minutes")), "%H:%M:%S"),'%H:%M') ## Appt time rounded by 30-min
  data.subset.new$Checkin.Hr <- format(strptime(as.ITime(round_date(data.subset.new$Checkin.DTTM, "hour")), "%H:%M:%S"),'%H:%M') ## Checkin time rounded by hour 
  # data.subset.new$Checkin.30m <- format(strptime(as.ITime(round_date(data.subset.new$Checkin.DTTM, "30 minutes")), "%H:%M:%S"),'%H:%M') ## Checkin time rounded by 30-min
  # data.subset.new$Lead.Days <- as.numeric((difftime(as.Date(data.subset.new$data.subset.new$Appt.DTTM, format="%Y-%m-%d"), as.Date(data.subset.new$data.subset.new$Appt.Cancel.DTTM, format="%Y-%m-%d"),  units = "days"))) ## Lead days for appt cancellation 
  data.subset.new$Lead.Days <- as.Date(data.subset.new$Appt.DTTM, format="%Y-%m-%d")-as.Date(data.subset.new$Appt.Cancel.DTTM, format="%Y-%m-%d") ## Lead days for appt cancellation
  data.subset.new$Wait.Time <- as.Date(data.subset.new$Appt.DTTM, format="%Y-%m-%d")-as.Date(data.subset.new$Appt.Made.DTTM, format="%Y-%m-%d")
  data.subset.new$uniqueId <- paste(data.subset.new$Department,data.subset.new$Provider,data.subset.new$MRN,data.subset.new$Appt.DTTM) ## Unique ID 
  data.subset.new$cycleTime <- as.numeric(round(difftime(data.subset.new$Visitend.DTTM,data.subset.new$Checkin.DTTM,units="mins"),1)) ## Checkin to Visitend (min)
  data.subset.new$checkinToRoomin <- as.numeric(round(difftime(data.subset.new$Roomin.DTTM,data.subset.new$Checkin.DTTM,units="mins"),1)) ## Checkin to Roomin (min)
  data.subset.new$providerinToOut <- as.numeric(round(difftime(data.subset.new$Providerout_DTTM,data.subset.new$Providerin_DTTM,units="mins"),1)) ## Provider in to out (min)
  data.subset.new$visitEndToCheckout <- as.numeric(round(difftime(data.subset.new$Checkout.DTTM,data.subset.new$Visitend.DTTM,units="mins"),1)) ## Visitend to Checkout (min)
  data.subset.new$Resource <- ifelse(data.subset.new$Resource == 1, "Provider", "Resource")
  
  ## Identify US Holidays in Data 
  hld <- tis::holidaysBetween(min(data.subset.new$Appt.DTTM, na.rm=TRUE), max(data.subset.new$Appt.DTTM, na.rm=TRUE))
  holid <- as.Date(as.character(hld), format = "%Y%m%d")
  names(holid) <- names(hld)
  holid <- as.data.frame(holid)
  holid <- cbind(names(hld), holid)
  rownames(holid) <- NULL
  colnames(holid) <- c("holiday","date")
  holid$holiday <- as.character(holid$holiday)
  holid$date <- as.character(holid$date)
  holid$date <- as.Date(holid$date, format="%Y-%m-%d")
  
  data.subset.new$holiday <- holid$holiday[match(data.subset.new$Appt.DateYear, holid$date)]
  
  # Pre-processed Scheduling Dataframe
  data.subset.new <- as.data.frame(data.subset.new)
  
  ## Pre-processing for Slot Data -----------------------------
  # Replace NAs with 0 in minutes columns
  slot.data.raw[,4:19][is.na(slot.data.raw[,4:19])] <- 0
  
  # Dept Specialty Manual Mapping to slot data 
  dept_specialty <- unique(data.subset.new[,c("Department","Campus.Specialty")])
  slot.data.raw$DEPT_SPECIALTY_NAME <- dept_specialty$Campus.Specialty[match(slot.data.raw$DEPARTMENT_NAME,dept_specialty$Department)]
  
  
  # Crosswalk Campus to Site by Department Name
  slot.data.raw$Campus_new <- site_ref$`Site`[match(slot.data.raw$DEPARTMENT_NAME,site_ref$`Department Name`)]
  slot.data.raw <- slot.data.raw %>% filter(!Campus_new == "NA") %>% filter(!Campus_new %in% c("Other","OTHER","EHS")) ## Exclude Mapped Sites: Other, OTHER, EHS
  
  # Data fields incldued for analysis
  original.cols.slots <- c("Campus_new",
                           "DEPT_SPECIALTY_NAME",
                           "DEPARTMENT_NAME","PROVIDER_NAME",
                           "SLOT_BEGIN_TIME","NUM_APTS_SCHEDULED","SLOT_LENGTH",
                           "AVAIL_MINUTES","BOOKED_MINUTES","ARRIVED_MINUTES","CANCELED_MINUTES","NOSHOW_MINUTES","LEFTWOBEINGSEEN_MINUTES",
                           "AVAIL_SLOTS","BOOKED_SLOTS","ARRIVED_SLOTS","CANCELED_SLOTS","NOSHOW_SLOTS","LEFTWOBEINGSEEN_SLOTS",
                           "ORG_REG_OPENINGS","ORG_OVBK_OPENINGS","PRIVATE_YN","DAY_UNAVAIL_YN","TIME_UNAVAIL_YN","DAY_HELD_YN","TIME_HELD_YN","OUTSIDE_TEMPLATE_YN","VISIT_PROV_STAFF_RESOURCE_C")
  
  # Subset raw slot usage data
  slot.data.subset <- slot.data.raw[original.cols.slots]
  
  # Rename data columns to match schduling data
  new.cols.slots <- c("Campus",
                      "Campus.Specialty",
                      "Department","Provider",
                      "SLOT_BEGIN_TIME","NUM_APTS_SCHEDULED","SLOT_LENGTH",
                      "AVAIL_MINUTES","BOOKED_MINUTES","ARRIVED_MINUTES","CANCELED_MINUTES","NOSHOW_MINUTES","LEFTWOBEINGSEEN_MINUTES",
                      "AVAIL_SLOTS","BOOKED_SLOTS","ARRIVED_SLOTS","CANCELED_SLOTS","NOSHOW_SLOTS","LEFTWOBEINGSEEN_SLOTS",
                      "ORG_REG_OPENINGS","ORG_OVBK_OPENINGS","PRIVATE_YN","DAY_UNAVAIL_YN","TIME_UNAVAIL_YN","DAY_HELD_YN","TIME_HELD_YN","OUTSIDE_TEMPLATE_YN","Resource")
  
  colnames(slot.data.subset) <- new.cols.slots
  
  # Create additional columns for Slot Data
  slot.data.subset$BOOKED_MINUTES <- slot.data.subset$BOOKED_MINUTES + slot.data.subset$CANCELED_MINUTES # Booked + Canceled Minutes 
  slot.data.subset$Appt.DTTM <- as.POSIXct(slot.data.subset$SLOT_BEGIN_TIME,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone(),origin = "1970-01-01")
  slot.data.subset$Appt.DateYear <- as.Date(slot.data.subset$SLOT_BEGIN_TIME, format="%Y-%m-%d") ## Create day of week colunm
  slot.data.subset$Appt.MonthYear <- format(as.Date(slot.data.subset$SLOT_BEGIN_TIME, format="%Y-%m-%d %H:%M:%S"), "%Y-%m") ## Create month - year column
  slot.data.subset$Appt.Year <- format(as.Date(slot.data.subset$Appt.DTTM, format="%Y-%m-%d %H:%M:%S"), "%Y") ## Create year column
  slot.data.subset$Appt.Month <- format(as.Date(slot.data.subset$Appt.DTTM, format="%Y-%m-%d %H:%M:%S"), "%b") ## Create month colunm
  slot.data.subset$Appt.Quarter <- quarters(as.Date(slot.data.subset$Appt.DTTM)) ## Create quarter column 
  slot.data.subset$Appt.Week <- floor_date(as.Date(slot.data.subset$Appt.DateYear, "%Y-%m-%d"), unit="week", week_start = 1) # Create week column
  slot.data.subset$Appt.Day <- format(as.Date(slot.data.subset$SLOT_BEGIN_TIME, format="%Y-%m-%d %H:%M:%S"), "%a") ## Create day of week colunm
  slot.data.subset$Time <- format(as.POSIXct(as.ITime(slot.data.subset$SLOT_BEGIN_TIME, format = "%H:%M"),origin = "1970-01-01"), "%H:%M") ## Create Slot Time column
  slot.data.subset$Appt.TM.Hr <- format(strptime(as.ITime(round_date(slot.data.subset$Appt.DTTM, "hour")), "%H:%M:%S"),'%H:%M') ## Appt time rounded by hour
  
  slot.data.subset$holiday <- holid$holiday[match(slot.data.subset$Appt.DateYear, holid$date)] ## Identify US Holidays in Data
  slot.data.subset$Visit.Method <- "IN PERSON"
  slot.data.subset$Resource <- ifelse(slot.data.subset$Resource == 1, "Provider", "Resource")
  
  
  reuturn_list <- list(slot.data.subset,data.subset.new,holid)
  return(reuturn_list)
}

monthly_path_part <- function(monthly){
  return(as.list(list.files(path = monthly,     # Identify all csv files in folder
                            pattern = "(2021)\\-[0-9]{2}\\-[0-9]{2}.csv" , full.names = F))) 
}

singleday_path_part <- function(singleday){
  return(as.list(list.files(path = singleday,     # Identify all csv files in folder
                            pattern = "*.csv", full.names = F))) 
} 


readin_data_all <- function(){
  data_all <<- list.files(path = monthly_path,     # Identify all csv files in folder
                          pattern = "(2021)\\-[0-9]{2}\\-[0-9]{2}.csv", full.names = TRUE) %>%   
    lapply(read_csv) %>%                                            # Store all files in list
    bind_rows()
  return(data_all)
}

read_singleday <- function(singleday){
  dataset <- NULL
  for (data in list.files(singleday,pattern = ".*csv", full.names = T)){
    # if data already exist, then append it together
    tempory <- read_csv(data,col_types = cols(MRN = col_character()))
    dataset <- bind_rows(dataset, tempory)
    rm(tempory)
    
    return(dataset)
  }
}

max_date_data_type <- function(data_type){
  if(!is.null(slot.data.raw$SLOT_BEGIN_TIME)){
    max_date_data_all <- date(max(slot.data.raw$SLOT_BEGIN_TIME))
  }else{
    max_date_data_all <- date(max(access.data.raw$APPT_DTTM))
  }
  return(max_date_data_all)
} 

max_date <- function(singleday,monthly){ 
  monthly_months <- data.frame(Date = file_path_sans_ext(monthly_path_part(monthly)))
  max_file_monthly <- max(as.Date(monthly_months$Date, "%Y-%m-%d",origin = "1970-01-01"))
  monthly_data <- read_csv(paste0(monthly,"/",max_file_monthly,".csv"))
  if(!is.null(monthly_data$APPT_DTTM)){
    max_date_monthly <- date(max(monthly_data$APPT_DTTM))
  }
  else{
    max_date_monthly <- date(max(monthly_data$SLOT_BEGIN_TIME))
  }
  max_month_monthly <- format(max_date_monthly,"%m") 
  singleday_dates <- data.frame(Date = file_path_sans_ext(singleday_path_part(singleday)))
  max_date_singleday <- max(as.Date(singleday_dates$Date, "%Y-%m-%d",origin = "1970-01-01"))
  max_month_singleday <- format(max_date_singleday,"%m")
  max_date_list <- list(max_date_monthly,max_month_monthly,max_month_singleday)
  return(max_date_list)
}


check_singleday <- function(singleday,monthly){
  filein_singleday = !is_empty(singleday_path_part(singleday))
  if(filein_singleday == 'TRUE'){
    singleday_data <<- read_singleday(singleday)
    max_month_monthly <- max_date(singleday,monthly)[[2]]
    max_month_singleday <- max_date(singleday,monthly)[[3]]
    curr_year <- format(Sys.Date(), "%Y")
    curr_monthly_filepath <- paste0(monthly,"/", curr_year,"-",max_month_singleday,"-01.csv")
    if(max_month_monthly < max_month_singleday){ #check for a new month 
      write_csv(singleday_data,curr_monthly_filepath)
      
    } else{
      curr_monthly_data <- read_csv(curr_monthly_filepath)
      #library(plyr)
      new_curr_monthly_data <- bind_rows(curr_monthly_data,singleday_data)
      write_csv(new_curr_monthly_data,curr_monthly_filepath)
      #detach("package:plyr", unload = TRUE)
    }
    file.remove(list.files(path = singleday,pattern = "*.csv", full.names = T))
  }
  
}

check_singleday(singleday_access,monthly_access)
check_singleday(singleday_slot,monthly_slot)


# Load Data Files
## Scheduling Data
if (!(exists("access.data.raw"))){ 
  #access.data.raw <<- list.files(path = "Data/Access/Monthly",
  #                               pattern = "*.csv", full.names = TRUE) %>%
  access.data.raw <- list.files(path = monthly_access,
                                 pattern = "(2021)\\-[0-9]{2}\\-[0-9]{2}.csv", full.names = TRUE) %>%
    lapply(read_csv) %>%
    rbind.fill()
  
  #slot.data.raw <<- list.files(path = "Data/Slot/Monthly",
  #                             pattern = "*.csv", full.names = TRUE) %>%
  slot.data.raw <- list.files(path = monthly_slot,
                               pattern = "(2021)\\-[0-9]{2}\\-[0-9]{2}.csv", full.names = TRUE) %>%
    lapply(read_csv) %>%
    rbind.fill()
  
  process_data_run <- process_data(access.data.raw,slot.data.raw)
  data.subset.new <- process_data_run[[2]]
  slot.data.subset <- process_data_run[[1]]
  holid <- process_data_run[[3]]
}



max_date_access <- max_date_data_type(data.subset.new)
max_date_monthly_access <- max_date(singleday_access,monthly_access)[[1]]
out_of_date <- !(isTRUE((all.equal(max_date_access,max_date_monthly_access))))
max_date_slot <- max_date_data_type(slot.data.raw)
#max_date_monthly_slot <- max_date(singleday_slot,monthly_slot)[[1]]
max_date_monthly_slot <- max_date_monthly_access

if(out_of_date == 'TRUE'){
  missing_dates_monthly_access <- data.frame(Date = format(as.Date(as.Date(max_date_access+1):as.Date(max_date_monthly_access), origin="1970-01-01"), "%m-%d-%Y"))
  max_month_monthly_access <- format(max_date_monthly_access,"%m")
  curr_year <- format(Sys.Date(), "%Y")
  recent_monthly_filepath_access <- paste0(monthly_access,"/",curr_year,"-",max_month_monthly_access,"-01.csv")
  recent_monthly_data_access <- read_csv(recent_monthly_filepath_access)
  missing_date_data_access <- subset(recent_monthly_data_access, APPT_DTTM >= (max_date_access+1))
  #data_all  <- bind_rows(data_all,missing_date_data)
  missing_dates_monthly_slot <- data.frame(Date = format(as.Date(as.Date(max_date_slot+1):as.Date(max_date_monthly_slot), origin="1970-01-01"), "%m-%d-%Y"))
  max_month_monthly_slot <- month(max_date_monthly_slot)
  recent_monthly_filepath_slot <- paste0(monthly_slot,"/",curr_year,"-",max_month_monthly_access,"-01.csv")
  recent_monthly_data_slot <- read_csv(recent_monthly_filepath_slot)
  missing_date_data_slot <- subset(recent_monthly_data_slot, SLOT_BEGIN_TIME >= (max_date_access+1))
  
  
  processed_dataset <- process_data(missing_date_data_access,missing_date_data_slot)
  slot.data.subset.missing <- processed_dataset[[1]]
  data.subset.new.missing <- processed_dataset[[2]]
  slot.data.subset <- bind_rows(slot.data.subset,slot.data.subset.missing)
  data.subset.new <- bind_rows(data.subset.new,data.subset.new.missing)
  #slot.comb.path <- paste0(combined_path_slot,"/",max_date_monthly)
  #access.comb.path <- paste0(combined_path_access,"/",max_date_monthly)
  #file.remove(list.files(path = slot.data.subset,pattern = "*.rds", full.names = T))
  #file.remove(list.files(path = data.subset.new,pattern = "*.rds", full.names = T))
  # slot.data.subset <- process_data(data_all)[[1]]
  # data.subset.new <- process_data(data_all)[[2]]
}


### (3) Import Site/Department Mapping File --------------------------------------------------------------------------------------
#read the mapping file that was provided by Marcy
# mapping_file <- choose.files(default = paste0(user_directory, "/Service Lines/Oncology/Data/Docs from Marcy/*.*"), caption = "Select mapping file")
#mapping_file <- choose.files("/Data/*.*", caption = "Select mapping file")

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

#####change all to first word capitalized
PRC_mapping$`Association List : A`[PRC_mapping$`Association List : A` == "Lab"] <- "Labs"
PRC_mapping$`Association List : A` <- str_to_title(PRC_mapping$`Association List : A`)

PRC_mapping$`Association List: B` <- str_to_title(PRC_mapping$`Association List: B`)

PRC_mapping$`Association List: T` <- str_to_title(PRC_mapping$`Association List: T`)

#change column names for the PRC mapping
colnames(PRC_mapping) <- c("PRC_NAME", "AssociationListA", "AssociationListB", "AssociationListT", "InPersonvsTele")


### (4) Merge pre-processed data and mapping file ----------------------------------------------------------------------------------

#merge the ambulatory data with the grouping data
amb_df_groupings <- merge(data.subset.new, department_mapping, by.x=c("Department"), by.y=c("DEPARTMENT_NAME"), all.x = TRUE)

# Oncology departments mapped as Oncology in Amb Care Mapping but not in Oncology Mapping ==========================================
# test <- amb_df_groupings %>% filter(is.na(System))
# missing <- filter(test, grepl('Oncology', Campus.Specialty))
# missing.depts <- unique(missing[,c("Campus.Specialty","Department")])

amb_df_groupings_ <- merge(amb_df_groupings, PRC_mapping, by.x=c("Appt.Type"), by.y=c("PRC_NAME"), all.x = TRUE)

# test2 <- amb_df_groupings_ %>% filter(!is.na(System))
# missing_PRC <- test2 %>% filter(is.na(ACTIVE))

############################## FILTER OUT ONCOLOGY DATA ONLY #######################################################################
amb_df_groupings_ <- amb_df_groupings_ %>% filter(!is.na(System))


### (5) Filter Out Duplicates ------------------------------------------------------------------------------------------------------

#only keep unique visits --> unique visits are defined as the visits with different
#MRN, appt date time, PRC name, provider name, and appt status 
amb_df_groupings_unique <-
  amb_df_groupings_ %>% 
  distinct(MRN, Appt.DTTM, Appt.Type, Provider, Appt.Status,  .keep_all = TRUE)



### (6) Data Subset -----------------------------------------------------------------------------------------------------
max_date <- amb_df_groupings_unique %>% filter(Appt.Status %in% c("Arrived"))
max_date <- max(max_date$Appt.DateYear) ## Or Today's Date
historical.data <- amb_df_groupings_unique %>% filter(Appt.DateYear<= max_date) ## Filter out historical data only
historical.data$Ref.Provider[is.na(historical.data$Ref.Provider)] <- "NONE"

# ## KPI datasets
# kpi.all.data <- historical.data %>% filter(Appt.DTTM >= max_date - 3*365) ## All data: Arrived, No Show, Canceled, Bumped, Rescheduled
# kpi.arrivedNoShow.data <- kpi.all.data %>% filter(Appt.Status %in% c("Arrived","No Show"))  ## Arrived + No Show data: Arrived and No Show
# kpi.arrived.data <- kpi.arrivedNoShow.data %>% filter(Appt.Status %in% c("Arrived")) ## Arrived data: Arrived
# kpi.canceled.bumped.data <- kpi.all.data %>% filter(Appt.Status %in% c("Canceled","Bumped")) ## Arrived data: Arrived
# kpi.canceled.data <- kpi.canceled.bumped.data %>% filter(Appt.Status %in% c("Canceled")) ## Canceled data: canceled appointments only
# kpi.bumped.data <-kpi.canceled.bumped.data %>% filter(Appt.Status %in% c("Bumped")) ## Bumped data: bumped appointments only

## Other datasets
all.data <- historical.data %>% filter(Appt.DTTM >= max_date - 365) ## All data: Arrived, No Show, Canceled, Bumped, Rescheduled
arrived.data <- all.data %>% filter(Appt.Status %in% c("Arrived")) ## Arrived data: Arrived
canceled.bumped.rescheduled.data <- all.data %>% filter(Appt.Status %in% c("Canceled","Bumped","Rescheduled")) ## Canceled data: canceled appointments only
canceled.data <- canceled.bumped.rescheduled.data %>% filter(Appt.Status %in% c("Canceled")) ## Canceled data: canceled appointments only
bumped.data <- canceled.bumped.rescheduled.data %>% filter(Appt.Status %in% c("Bumped")) ## Bumped data: bumped appointments only
rescheduled.data <- canceled.bumped.rescheduled.data %>% filter(Appt.Status %in% c("Rescheduled")) ## Bumped data: bumped appointments only
sameDay <- canceled.bumped.rescheduled.data %>% filter(Lead.Days == 0) # Same day canceled, rescheduled, bumped appts
noShow.data <- all.data %>% filter(Appt.Status %in% c("No Show")) ## Arrived + No Show data: Arrived and No Show
noShow.data <- rbind(noShow.data,sameDay) # No Shows + Same day canceled, bumped, rescheduled
arrivedNoShow.data <- rbind(arrived.data,noShow.data) ## Arrived + No Show data: Arrived and No Show


# ## Slot datasets
# past.slot.data <- slot.data.subset %>% filter(Appt.DTTM <= max_date, Appt.DTTM >= max_date - 365)
# future.slot.data <- slot.data.subset %>% filter(Appt.DTTM > max_date, Appt.DTTM <= max_date + 90)
# 
# 
# ### (5) Pre-processing Space Utilization Dataframe --------------------------------------------------------------------------------------
# # Filter utilization data in last 60 days
# scheduled.data <- arrivedNoShow.data %>% filter(Appt.DTTM >= max_date - 60*24*60*60) ## All appts scheduled
# 
# # Function for formatting date and time by hour
# system_date <- function(time){
#   result <- as.POSIXct(paste0(as.character(Sys.Date())," ",time), format="%Y-%m-%d %H:%M:%S")
#   return(result)
# }
# 
# util.function <- function(time, df){
#   result <- ifelse(system_date(time) %within% df$time.interval == TRUE, 
#                    ifelse(difftime(df$Appt.End.Time, system_date(time), units = "mins") >= 60, 60,
#                           as.numeric(difftime(df$Appt.End.Time, system_date(time), units = "mins"))),
#                    ifelse(floor_date(df$Appt.Start.Time, "hour") == system_date(time),
#                           ifelse(floor_date(df$Appt.End.Time, "hour") == system_date(time),
#                                  difftime(df$Appt.End.Time, df$Appt.Start.Time, units = "mins"),
#                                  difftime(system_date(time) + 60*60, df$Appt.Start.Time, units = "mins")), 0))
#   return(result)
# }
# 
# 
# # Pre-process Utilization by Hour based on Scheduled Appointment Times --------------------------------------------------
# data.hour.scheduled <- scheduled.data
# data.hour.scheduled$actual.visit.dur <- data.hour.scheduled$Appt.Dur
# 
# data.hour.scheduled$Appt.Start <- as.POSIXct(data.hour.scheduled$Appt.DTTM, format = "%H:%M")
# data.hour.scheduled$Appt.End <- as.POSIXct(data.hour.scheduled$Appt.Start + data.hour.scheduled$Appt.Dur*60, format = "%H:%M")
# 
# data.hour.scheduled$Appt.Start.Time <- as.POSIXct(paste0(Sys.Date()," ", format(data.hour.scheduled$Appt.Start, format="%H:%M:%S")))
# data.hour.scheduled$Appt.End.Time <- as.POSIXct(paste0(Sys.Date()," ", format(data.hour.scheduled$Appt.End, format="%H:%M:%S")))
# 
# 
# data.hour.scheduled$time.interval <- interval(data.hour.scheduled$Appt.Start.Time, data.hour.scheduled$Appt.End.Time)
# 
# # Excluding visits without Roomin or Visit End Tines 
# data.hour.scheduled$`00:00` <- util.function("00:00:00", data.hour.scheduled)  
# data.hour.scheduled$`01:00` <- util.function("01:00:00", data.hour.scheduled)  
# data.hour.scheduled$`02:00` <- util.function("02:00:00", data.hour.scheduled)  
# data.hour.scheduled$`03:00` <- util.function("03:00:00", data.hour.scheduled)  
# data.hour.scheduled$`04:00` <- util.function("04:00:00", data.hour.scheduled)  
# data.hour.scheduled$`05:00` <- util.function("05:00:00", data.hour.scheduled)  
# data.hour.scheduled$`06:00` <- util.function("06:00:00", data.hour.scheduled)  
# data.hour.scheduled$`07:00` <- util.function("07:00:00", data.hour.scheduled)  
# data.hour.scheduled$`08:00` <- util.function("08:00:00", data.hour.scheduled)  
# data.hour.scheduled$`09:00` <- util.function("09:00:00", data.hour.scheduled)  
# data.hour.scheduled$`10:00` <- util.function("10:00:00", data.hour.scheduled)  
# data.hour.scheduled$`11:00` <- util.function("11:00:00", data.hour.scheduled)  
# data.hour.scheduled$`12:00` <- util.function("12:00:00", data.hour.scheduled)  
# data.hour.scheduled$`13:00` <- util.function("13:00:00", data.hour.scheduled)  
# data.hour.scheduled$`14:00` <- util.function("14:00:00", data.hour.scheduled)  
# data.hour.scheduled$`15:00` <- util.function("15:00:00", data.hour.scheduled)  
# data.hour.scheduled$`16:00` <- util.function("16:00:00", data.hour.scheduled)  
# data.hour.scheduled$`17:00` <- util.function("17:00:00", data.hour.scheduled)  
# data.hour.scheduled$`18:00` <- util.function("18:00:00", data.hour.scheduled)  
# data.hour.scheduled$`19:00` <- util.function("19:00:00", data.hour.scheduled)  
# data.hour.scheduled$`20:00` <- util.function("20:00:00", data.hour.scheduled)  
# data.hour.scheduled$`21:00` <- util.function("21:00:00", data.hour.scheduled)  
# data.hour.scheduled$`22:00` <- util.function("22:00:00", data.hour.scheduled)  
# data.hour.scheduled$`23:00` <- util.function("23:00:00", data.hour.scheduled)  
# 
# # Data Validation
# # colnames(data.hour.scheduled[89])
# data.hour.scheduled$sum <- rowSums(data.hour.scheduled [,66:89])
# data.hour.scheduled$actual <- as.numeric(difftime(data.hour.scheduled$Appt.End.Time, data.hour.scheduled$Appt.Start.Time, units = "mins"))
# data.hour.scheduled$comparison <- ifelse(data.hour.scheduled$sum ==data.hour.scheduled$actual, 0, 1)
# # nrow(data.hour.scheduled %>% filter(comparison == 1))/nrow(data.hour.scheduled)
# data.hour.scheduled <- data.hour.scheduled %>% filter(comparison == 0)
# 
# 
# # Pre-process Utilization by Hour based on Actual Room in to Visit End Times ---------------------------------------------------
# data.hour.arrived.all <- scheduled.data %>% filter(Appt.Status == "Arrived")
# data.hour.arrived.all$actual.visit.dur <- round(difftime(data.hour.arrived.all$Visitend.DTTM, data.hour.arrived.all$Roomin.DTTM, units = "mins"))
# 
# ########### Analysis of % of visits with actual visit start and end times ############
# # nrow(data.hour.arrived %>% filter(!is.na(Roomin.DTTM) & !is.na(Visitend.DTTM)))/nrow(data.hour.arrived)
# 
# data.hour.arrived.all$Appt.Start <- format(strptime(as.ITime(data.hour.arrived.all$Roomin.DTTM), "%H:%M:%S"),'%H:%M:%S')
# data.hour.arrived.all$Appt.Start <- as.POSIXct(data.hour.arrived.all$Appt.Start, format = "%H:%M")
# data.hour.arrived.all$Appt.End <- as.POSIXct(data.hour.arrived.all$Appt.Start + data.hour.arrived.all$actual.visit.dur, format = "%H:%M")
# 
# data.hour.arrived.all$Appt.Start.Time <- data.hour.arrived.all$Appt.Start
# data.hour.arrived.all$Appt.End.Time <- data.hour.arrived.all$Appt.End
# 
# data.hour.arrived.all$time.interval <- interval(data.hour.arrived.all$Appt.Start.Time, data.hour.arrived.all$Appt.End.Time)
# 
# # Excluding visits without Roomin or Visit End Tines 
# data.hour.arrived <- data.hour.arrived.all %>% filter(actual.visit.dur > 0)
# 
# data.hour.arrived$`00:00` <- util.function("00:00:00", data.hour.arrived)
# data.hour.arrived$`01:00` <- util.function("01:00:00", data.hour.arrived)
# data.hour.arrived$`02:00` <- util.function("02:00:00", data.hour.arrived)
# data.hour.arrived$`03:00` <- util.function("03:00:00", data.hour.arrived)
# data.hour.arrived$`04:00` <- util.function("04:00:00", data.hour.arrived)
# data.hour.arrived$`05:00` <- util.function("05:00:00", data.hour.arrived)
# data.hour.arrived$`06:00` <- util.function("06:00:00", data.hour.arrived)
# data.hour.arrived$`07:00` <- util.function("07:00:00", data.hour.arrived)  
# data.hour.arrived$`08:00` <- util.function("08:00:00", data.hour.arrived)  
# data.hour.arrived$`09:00` <- util.function("09:00:00", data.hour.arrived)  
# data.hour.arrived$`10:00` <- util.function("10:00:00", data.hour.arrived)  
# data.hour.arrived$`11:00` <- util.function("11:00:00", data.hour.arrived)  
# data.hour.arrived$`12:00` <- util.function("12:00:00", data.hour.arrived)  
# data.hour.arrived$`13:00` <- util.function("13:00:00", data.hour.arrived)  
# data.hour.arrived$`14:00` <- util.function("14:00:00", data.hour.arrived)  
# data.hour.arrived$`15:00` <- util.function("15:00:00", data.hour.arrived)  
# data.hour.arrived$`16:00` <- util.function("16:00:00", data.hour.arrived)  
# data.hour.arrived$`17:00` <- util.function("17:00:00", data.hour.arrived)  
# data.hour.arrived$`18:00` <- util.function("18:00:00", data.hour.arrived)  
# data.hour.arrived$`19:00` <- util.function("19:00:00", data.hour.arrived)  
# data.hour.arrived$`20:00` <- util.function("20:00:00", data.hour.arrived)  
# data.hour.arrived$`21:00` <- util.function("21:00:00", data.hour.arrived)
# data.hour.arrived$`22:00` <- util.function("22:00:00", data.hour.arrived)
# data.hour.arrived$`23:00` <- util.function("23:00:00", data.hour.arrived)
# 
# # Data Validation
# # colnames(data.hour.arrived[89])
# data.hour.arrived$sum <- rowSums(data.hour.arrived [,66:89])
# data.hour.arrived$actual <- as.numeric(difftime(data.hour.arrived$Appt.End.Time, data.hour.arrived$Appt.Start.Time, units = "mins"))
# data.hour.arrived$comparison <- ifelse(data.hour.arrived$sum ==data.hour.arrived$actual, 0, 1)
# data.hour.arrived <- data.hour.arrived %>% filter(comparison == 0)
# 
# # Combine Utilization Data
# data.hour.scheduled$util.type <- "scheduled"
# data.hour.arrived$util.type <- "arrived"
# scheduled.utilization.data <- rbind(data.hour.scheduled, data.hour.arrived)
# arrived.utilization.data <- rbind(data.hour.scheduled %>% filter(Appt.Status == "Arrived"), data.hour.arrived)


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


# # KPI Filters
# KPIvolumeOptions <- c("Appointment Volume","Appointment Status")
# KPIschedulingOptions <- c("Booked Rate","Fill Rate")
# KPIaccessOptions <- c("New Patient Ratio","Appointment Lead Time","3rd Next Available")
# KPIdayOfVisitOptions <- c("Cycle Time","Wait Time")
# kpiOptions <- c("Patient Volume","Appointment Status",
#                 "Booked Rate","Fill Rate",
#                 "New Patient Ratio","New Patient Wait Time","3rd Next Available",
#                 "Check-in to Room-in Time","Provider Time")

# # Reference dataframes, vectors, etc.
# daysOfWeek.Table <- data.hour.arrived %>% group_by(Appt.Day,Appt.DateYear) %>% dplyr::summarise(count = n()) ## Total Days in the Entire Data Set 
# 
# Time <- rep(timeOptionsHr, 7)
# Day <- rep(daysOfWeek.options, each = 24)
# byDayTime.df <- as.data.frame(cbind(Day,Time)) ## Empty data frame for day of week by time (hour)
# 
# dateInData <- length(unique(data.hour.arrived$Appt.DateYear))
# Date <- rep(unique(data.hour.arrived$Appt.DateYear), each = 24)
# Time <- rep(timeOptionsHr, dateInData)
# byDateTime.df <- as.data.frame(cbind(Date,Time)) ## Empty data frame for date and time (hour)
# 
# Time <- rep(timeOptions30m, 7)
# Day <- rep(daysOfWeek.options, each = 48)
# byDayTime30m.df <- as.data.frame(cbind(Day,Time)) ## Empty data frame for day of week by time (30-min)
# 
# dateInData <- length(unique(data.hour.arrived$Appt.DateYear))
# Date <- rep(unique(data.hour.arrived$Appt.DateYear), each = 24)
# Time <- rep(timeOptionsHr, dateInData)
# byDateTime.df <- as.data.frame(cbind(Date,Time)) ## Empty data frame for date and time (30-min)
# 
# byTime.df <- as.data.frame(timeOptionsHr)
# colnames(byTime.df) <- c("Time") ## Empty data frame for time (hour)
# 
# byTime30.df <- as.data.frame(timeOptions30m)
# colnames(byTime30.df) <- c("Time") ## Empty data frame for time (hour)



# (7) Data Reactive functions ---------------------------------------------------------------------------------

## Filtered Scheduling Data

groupByFilters <- function(dt, campus, specialty, department, provider, refProvider, mindateRange, maxdateRange, daysofweek, holidays){
  result <- dt %>% filter(SITE %in% campus, Campus.Specialty %in% specialty, Department %in% department, Provider %in% provider, Ref.Provider %in% refProvider,
                          mindateRange <= Appt.DateYear, maxdateRange >= Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays)
  return(result)
}

groupByFilters_2 <- function(dt, visitType, apptType, treatmentType){
  result <- dt %>% filter(AssociationListA %in% visitType, AssociationListB %in% apptType, AssociationListT %in% treatmentType)
  return(result)
}
                 

### Function for Value Boxes ------------------------------------------------------------------
valueBoxSpark <- function(value, title, subtitle, sparkobj = NULL, info = NULL, 
                          icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    # bs3 pull-right 
    # bs4 float-right
    class = "pull-right float-right"
  )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      h4(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      em(subtitle)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

