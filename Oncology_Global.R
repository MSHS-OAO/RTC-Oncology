#####Install packages
#install.packages("timeDate")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("lubridate")

#####Require packages
library(timeDate)
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)


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
  
  `main`  = MountSinai_cols("dark purple","dark grey","dark pink","med blue","light pink","light blue","light grey"),
  
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
  theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 20,
        margin = margin(0, 0, 30, 0)
      ),
      legend.position = "top",
      legend.text = element_text(size = "12"),
      legend.direction = "horizontal",
      legend.key.size = unit(1.0, "cm"),
      legend.title = element_blank(),
      axis.title = element_text(size = "14"),
      axis.text = element_text(size = "14"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 5)),
      axis.text.x = element_text(
        angle = 90,
        hjust = 0.5,
        margin = margin(t = 10)
      ),
      axis.text.y = element_text(margin = margin(l = 5, r = 5)),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(size = 0.3, colour = "black"),
      plot.margin = margin(30, 30, 30, 30)
    )
}




### (2) Import Data ----------------------------------------------------------------------------------
#####Determine the path in the shared-drive
ifelse (list.files("J://") == "Presidents", user_directory <- "J:/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects", 
        user_directory <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects")

###################### Raw Data ######################
#read the Ambulatory Care raw data 
#amb_data <- read.csv(choose.files(default = paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Access/Monthly/*.*"), caption = "Select Ambulatory Care Raw Data"), stringsAsFactors = FALSE)

##### pull Ambulatory Care Monthly data 
file_list_amb <- list.files(path = paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Access/Monthly"), pattern = "^(2020)|(2021)\\-[0-9]{2}\\-[0-9]{2}")

amb_list <- lapply(file_list_amb, function(x) read.csv(paste0(user_directory, "/System Operations/Ambulatory Dashboard/Pilot Application v1/Data/Access/Monthly/", x), stringsAsFactors = FALSE))

for (i in (1: length(amb_list))){
  #change dates that were imported as char
  amb_list[[i]][c("APPT_MADE_DTTM", "APPT_DTTM", "APPT_CANC_DTTM")] <- lapply(amb_list[[i]][c("APPT_MADE_DTTM", "APPT_DTTM", "APPT_CANC_DTTM")],as.POSIXct, tz="UTC", format="%Y-%m-%d %H:%M:%OS")
  #get the month, year, weekday for important dates
  amb_list[[i]] <- amb_list[[i]] %>%
    mutate(appt_made_day = weekdays(APPT_MADE_DTTM),
           appt_made_month = month(APPT_MADE_DTTM),
           appt_made_year = year(APPT_MADE_DTTM),
           appt_day = weekdays(APPT_DTTM),
           appt_month = month(APPT_DTTM),
           appt_year = year(APPT_DTTM),
           cancel_day = weekdays(APPT_CANC_DTTM),
           cancel_month = month(APPT_CANC_DTTM),
           cancel_year = year(APPT_CANC_DTTM))
}

#binding the rows of the data-list
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

#####change all to first word capitalized
PRC_mapping$`Association List : A`[PRC_mapping$`Association List : A` == "Lab"] <- "Labs"
PRC_mapping$`Association List : A` <- str_to_title(PRC_mapping$`Association List : A`)

PRC_mapping$`Association List: B` <- str_to_title(PRC_mapping$`Association List: B`)

PRC_mapping$`Association List: T` <- str_to_title(PRC_mapping$`Association List: T`)

#change column names for the PRC mapping
colnames(PRC_mapping) <- c("PRC_NAME", "AssociationListA", "AssociationListB", "AssociationListT", "InPersonvsTele")

### (3) Pre-process data ----------------------------------------------------------------------------------

#merge the ambulatory data with the grouping data
amb_df_groupings <- merge(amb_df, department_mapping, by=c("DEPARTMENT_NAME"))
amb_df_groupings_ <- merge(amb_df_groupings, PRC_mapping, by = c("PRC_NAME"))

#only keep unique visits --> unique visits are defined as the visits with different
#MRN, appt date time, PRC name, provider name, and appt status 
amb_df_groupings_unique <-
  amb_df_groupings_ %>% 
  distinct(MRN, APPT_DTTM, PRC_NAME, PROV_NAME_WID, DERIVED_STATUS_DESC,  .keep_all = TRUE)


