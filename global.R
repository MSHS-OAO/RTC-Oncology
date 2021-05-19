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
# install.packages("patchwork")
# install.packages("ggtext")
# install.packages("janitor")
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/zipcode/zipcode_1.0.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")



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
  library(zipcodeR)
  library(feather)
  
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
  
  `dark`  = MountSinai_cols("dark purple","dark grey",
                             "yellow","med pink","dark pink","dark blue",
                             "med purple","med grey","med blue"),
  
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
    plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
    plot.caption = element_text(size = 12, face = "italic"),
    legend.position = legend_pos,
    legend.title = element_text(size = "14"),
    legend.text = element_text(size = "14"),
    strip.text = element_text(size=14),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 16, angle=50, hjust=1),
    axis.text.y = element_text(size = 14),
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
          plot.subtitle = element_text(hjust=0.5,vjust=-1, size = 20, face = "italic"),
          plot.caption = element_text(hjust = 0, size = 18, face = "italic"),
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
    panel.border = element_rect(colour = "black", fill = NA, size=0.5),
    axis.line.x = element_line(colour = "black", size=0.5),
    plot.margin=unit(c(-0.5,1,1,1), "cm"))
}

### (2) Import Data ----------------------------------------------------------------------------------

# Define file paths for use later in the script
monthly_access <- here::here("Data/Access/Monthly")
monthly_slot <- here::here("Data/Slot/Monthly")
singleday_access <- here::here("Data/Access/SingleDay")
singleday_slot <- here::here("Data/Slot/SingleDay")


# Set Working Directory (PILOT)
#wdpath <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Ambulatory Dashboard/Pilot Application v1"
#wdpath <- "J:/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Ambulatory Dashboard/Pilot Application v1"
# wdpath <- "C:/Users/kweons01/Desktop/Pilot Application v1"


wdpath <- here::here()

setwd(wdpath)


#master.data.new_new <- data_all


# ## Utilization Data
# data.hour.scheduled <- read_csv("Data/Utilization/data.hour.scheduled.pilotV1.csv")
# data.hour.arrived <- read_csv("Data/Utilization/data.hour.arrived.pilotV1.csv")


# process_data function includes reading in the mapping file creating an renaming slot and access columns
# the function returns a list containing slot.data.subset, data.subset.new, and holid (in the order they appear)



### (6) Data Subset -----------------------------------------------------------------------------------------------------
historical.data <- as.data.frame(read_feather(here::here("Data/historical_data.feather")))


## Other datasets
holid <-as.data.frame(read_feather(here::here("Data/holid.feather")))
all.data <- as.data.frame(read_feather(here::here("Data/all_data.feather")))
arrived.data <- as.data.frame(read_feather(here::here("Data/arrived_data.feather")))
canceled.bumped.rescheduled.data <- as.data.frame(read_feather(here::here("Data/canceled_bumped_rescheduled_data.feather")))
canceled.data <- as.data.frame(read_feather(here::here("Data/canceled_data.feather")))
bumped.data <- as.data.frame(read_feather(here::here("Data/bumped_data.feather")))
rescheduled.data <- as.data.frame(read_feather(here::here("Data/rescheduled_data.feather")))
sameDay <- as.data.frame(read_feather(here::here("Data/sameDay.feather")))
noShow.data <- as.data.frame(read_feather(here::here("Data/noShow_data.feather")))
arrivedNoShow.data <- as.data.frame(read_feather(here::here("Data/arrivedNoShow_data.feather")))
arrivedDisease.data <- as.data.frame(read_feather(here::here("Data/arrivedDisease_data.feather")))


### Zip Code Analysis --------------------------------------------------------------------------------------------------
population.data_filtered <- as.data.frame(read_feather(here::here("Data/population_data_filtered.feather")))
### Missing zip codes in Zip Code Grouper filer?

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


# (7) Data Reactive functions ---------------------------------------------------------------------------------

## Filtered Scheduling Data

groupByFilters <- function(dt, campus, department, mindateRange, maxdateRange, daysofweek, holidays){
  result <- dt %>% filter(SITE %in% campus, Department %in% department, 
                          mindateRange <= Appt.DateYear, maxdateRange >= Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays)
  return(result)
}

groupByFilters_2 <- function(dt, visitType, apptType, treatmentType){
  result <- dt %>% filter(AssociationListA %in% visitType, AssociationListB %in% apptType, AssociationListT %in% treatmentType)
  return(result)
}

groupByFilters_3 <- function(dt, diseaseGroup, provider){
  result <- dt %>% filter(Disease_Group != "No Disease Group") %>% filter(Disease_Group %in% diseaseGroup, Provider %in% provider)
  return(result)
}

## Unique Patients Functions  -----------------------------------------------------------------
uniquePts_df_system <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(AssociationListB %in% c("Treatment Visit")) 
  } else{
    data <- dt %>% filter(AssociationListA %in% visitType) 
  }
  
  result <- data %>%
    arrange(MRN, Appt.DTTM) %>% group_by(MRN) %>% mutate(uniqueSystem = row_number()) %>% ungroup() %>%
    filter(uniqueSystem == 1)
  
  return(result)
}

uniquePts_df_systemMonth <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(AssociationListB %in% c("Treatment Visit")) 
  } else{
    data <- dt %>% filter(AssociationListA %in% visitType) 
  }
  
  result <- data %>%
    arrange(MRN, Appt.DTTM) %>% group_by(MRN, Appt.MonthYear) %>% mutate(uniqueSystemMonth = row_number()) %>% ungroup() %>%
    filter(uniqueSystemMonth == 1)
  
  return(result)
}

uniquePts_df_site <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(AssociationListB %in% c("Treatment Visit")) 
  } else{
    data <- dt %>% filter(AssociationListA %in% visitType) 
  }
  
  result <- data %>%
    arrange(MRN, Appt.DTTM, SITE) %>% group_by(MRN, SITE) %>% mutate(uniqueSite = row_number()) %>% ungroup() %>%
    filter(uniqueSite == 1) 
  
  return(result)
}

uniquePts_df_siteMonth <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(AssociationListB %in% c("Treatment Visit")) 
  } else{
    data <- dt %>% filter(AssociationListA %in% visitType) 
  }
  
  result <- data %>%
    arrange(MRN, Appt.DTTM, SITE) %>% group_by(MRN, Appt.MonthYear, SITE) %>% mutate(uniqueSiteMonth = row_number()) %>% ungroup() %>%
    filter(uniqueSiteMonth == 1) 
  
  return(result)
}

uniquePts_df_siteProv <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(AssociationListB %in% c("Treatment Visit")) 
  } else{
    data <- dt %>% filter(AssociationListA %in% visitType) 
  }
  
  result <- data %>%
    arrange(MRN, EPIC_Provider_ID, Appt.DTTM, SITE) %>% group_by(MRN, EPIC_Provider_ID, SITE) %>% mutate(uniqueSiteProv = row_number()) %>% ungroup() %>%
    filter(uniqueSiteProv == 1)
  
  return(result)
}

uniquePts_df_siteProvMonth <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(AssociationListB %in% c("Treatment Visit")) 
  } else{
    data <- dt %>% filter(AssociationListA %in% visitType) 
  }
  
  result <- data %>%
    arrange(MRN, EPIC_Provider_ID, Appt.MonthYear, SITE) %>% group_by(MRN, EPIC_Provider_ID, Appt.MonthYear, SITE) %>% 
    mutate(uniqueSiteProvMonth = row_number()) %>% ungroup() %>% filter(uniqueSiteProvMonth== 1) 
  
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
