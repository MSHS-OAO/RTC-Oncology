# (7) Data Reactive functions ---------------------------------------------------------------------------------

## Filtered Scheduling Data

groupByFilters <- function(dt, campus, department, mindateRange, maxdateRange, daysofweek, holidays){
  # result <- dt %>% filter(SITE %in% campus, Department %in% department, 
  #                         mindateRange <= Appt.DateYear, maxdateRange >= Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays)
  
  format <- "YYYY-MM-DD HH24:MI:SS"
  daysofweek <- toupper(daysofweek)
  maxdateRange <- as.Date(maxdateRange) + 1
  
  result <- dt %>% filter(SITE %in% campus, 
                          DEPARTMENT_NAME %in% department, 
                          TO_DATE(mindateRange, format) <= APPT_DTTM, 
                          TO_DATE(maxdateRange, format) > APPT_DTTM, 
                          APPT_DAY %in% daysofweek, 
                          #!HOLIDAY %in% holidays
  )
                          
                          
  return(result)
}

groupByFilters_unique_trend <- function(dt, campus, department, mindateRange, maxdateRange, daysofweek, holidays){
  # result <- dt %>% filter(SITE %in% campus, Department %in% department, 
  #                         mindateRange <= Appt.DateYear, maxdateRange >= Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays)
  
  format <- "YYYY-MM-DD HH24:MI:SS"
  daysofweek <- toupper(daysofweek)
  # mindateRange <- format(as.Date(maxdateRange) %m-% months(36), "%Y-%m-%d")
  #mindateRange <- format(as.Date(maxdateRange) %m-% months(36), "%Y-%m-%d")
  maxdateRange <- as.Date(maxdateRange) + 1
  
  
  result <- dt %>% filter(SITE %in% campus, 
                          DEPARTMENT_NAME %in% department, 
                          TO_DATE(mindateRange, format) <= APPT_DTTM, 
                          TO_DATE(maxdateRange, format) > APPT_DTTM, 
                          APPT_DAY %in% daysofweek#, 
                          #!HOLIDAY %in% holidays
  )
  
  
  return(result)
}

groupByFilters_unique <- function(dt, campus, department, mindateRange, maxdateRange, daysofweek, holidays, dx){
  format <- "YYYY-MM-DD HH24:MI:SS"
  
  result <- dt %>% filter(SITE %in% campus, DEPARTMENT_NAME %in% department, 
                          TO_DATE(mindateRange, format) <= APPT_DATE_YEAR, TO_DATE(maxdateRange, format) >= APPT_DATE_YEAR, APPT_DAY %in% daysofweek, #!holiday %in% holidays,
                          DX_GROUPER %in% dx)
  return(result)
}

# groupByFilters_Trend <- function(dt, campus, department, mindateRange, maxdateRange, daysofweek, holidays){
#   result <- dt %>% filter(SITE %in% campus, Department %in% department, 
#                           mindateRange <= Appt.DateYear, maxdateRange >= Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays#,
#                           #Dx.Grouper %in% dx
#   )
#   return(result)
# }


groupByFilters_pop <- function(dt, campus, department, mindateRange, maxdateRange, daysofweek, holidays, provider, dx){
  maxdateRange <- as.Date(maxdateRange) + 1
  result <- dt %>% filter(SITE %in% campus, Department %in% department, 
                          mindateRange <= Appt.DateYear, maxdateRange > Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays, Provider %in% provider,
                          Dx.Grouper %in% dx)
  return(result)
}



groupByFilters_2 <- function(dt, visitType, apptType, treatmentType, dx){
  # result <- dt %>% filter(AssociationListA %in% visitType, AssociationListB %in% apptType, AssociationListT %in% treatmentType,
  #                         Dx.Grouper %in% dx)
  # return(result)
  
  
  result <- dt %>% filter(ASSOCIATIONLISTA %in% visitType, ASSOCIATIONLISTB %in% apptType, ASSOCIATIONLISTT %in% treatmentType, DX_GROUPER %in% dx)
  
  if("NA" %in% dx){
    result_1 <- dt %>% filter(ASSOCIATIONLISTA %in% visitType, ASSOCIATIONLISTB %in% apptType, ASSOCIATIONLISTT %in% treatmentType, is.na(DX_GROUPER))
    result <- result %>% union_all(result_1)
    
  }
  
  return(result)
}

# groupByFilters_3 <- function(dt, diseaseGroup, provider, dx){
#   result <- dt %>% filter(Disease_Group != "No Disease Group") %>% filter(Disease_Group %in% diseaseGroup, Provider %in% provider) %>%
#     filter(Dx.Grouper %in% dx)
#   return(result)
# }


groupByFilters_3 <- function(dt, provider){
  result <- dt %>% filter(PROVIDER %in% provider)
  
  return(result)
}

groupByFilters_3_detail <- function(dt, diseaseGroup, provider, dx, diseaseGroupDetail){
  result <- dt %>% filter(DISEASE_GROUP %in% diseaseGroup, PROVIDER %in% provider) %>%
    filter(DX_GROUPER %in% dx) %>% filter(DISEASE_GROUP_DETAIL %in% diseaseGroupDetail)
  
  if("NA" %in% dx){
    result_1 <- dt %>% filter(DISEASE_GROUP %in% diseaseGroup, PROVIDER %in% provider) %>%
      filter(is.na(DX_GROUPER))
    
    result <- result %>% union_all(result_1)
  }
  
  if("NA" %in% diseaseGroupDetail){
    result_2 <- dt %>% filter(DISEASE_GROUP %in% diseaseGroup, PROVIDER %in% provider) %>%
      filter(is.na(DISEASE_GROUP_DETAIL))
    
    result <- result %>% union_all(result_2)
  }
  result
  
  return(result)
}

groupByFilters_4 <- function(dt, campus, department, mindateRange, maxdateRange, daysofweek, holidays, provider){
  result <- dt %>% filter(SITE %in% campus, Department %in% department, 
                          mindateRange <= Appt.DateYear, maxdateRange >= Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays, Provider %in% provider)
  return(result)
}


groupByFilters_util <- function(dt, campus, department, provider, mindateRange, maxdateRange, daysofweek, holidays, type){
  maxdateRange <- as.Date(maxdateRange) + 1
  
  result <- dt %>% filter(SITE %in% campus, Department %in% department, Provider %in% provider,
                          mindateRange <= Appt.DateYear, maxdateRange > Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays, util.type %in% type)
  return(result)
}

groupByFilters_util_treatment <- function(dt, campus, department, mindateRange, maxdateRange, daysofweek, holidays){
  
  format <- "YYYY-MM-DD HH24:MI:SS"
  daysofweek <- toupper(daysofweek)
  maxdateRange <- as.Date(maxdateRange) + 1
  
  result <- dt %>% filter(SITE %in% campus, DEPARTMENT_NAME %in% department, #Provider %in% provider,
                          TO_DATE(mindateRange, format) <= APPT_DTTM, 
                          TO_DATE(maxdateRange, format) > APPT_DTTM, APPT_DAY %in% daysofweek
                          #, !holiday %in% holidays
                          )
                          
  return(result)
}
## Unique Patients Functions  -----------------------------------------------------------------
uniquePts_df_system <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(ASSOCIATIONLISTB %in% c("Treatment Visit")) 
  } else{
    data <- dt %>% filter(ASSOCIATIONLISTA %in% visitType) 
    # name <- ifelse(visitType == "Exam", "exam",
    #                ifelse(visitType == "Treatment","treatment", "all"), "none")
  }
  
  result <- data %>%
    arrange(MRN, APPT_DTTM) %>% group_by(MRN) %>% mutate(uniqueSystem = row_number()) %>% ungroup() %>%
    filter(uniqueSystem == 1) %>%
    select(SITE, DEPARTMENT_NAME, APPT_DATE_YEAR, APPT_YEAR, APPT_MONTH, APPT_MONTH_YEAR, uniqueSystem, APPT_DAY, DX_GROUPER)
  
  
  #result <- result[c("SITE", "Department", "Appt.DateYear", "Appt.Year", "Appt.Month", "Appt.MonthYear", "uniqueSystem","Appt.Day", "holiday", "Dx.Grouper")]
  #result <- result$uniqueId
  return(result)
}

uniquePts_df_system_zip <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(AssociationListB %in% c("Treatment Visit")) 
  } else{
    data <- dt %>% filter(AssociationListA %in% visitType) 
    # name <- ifelse(visitType == "Exam", "exam",
    #                ifelse(visitType == "Treatment","treatment", "all"), "none")
  }
  
  result <- data %>%
    arrange(MRN, Appt.DTTM) %>% group_by(MRN) %>% mutate(uniqueSystem = row_number()) %>% ungroup() %>%
    filter(uniqueSystem == 1)
  
  
  result <- result[c("SITE", "Department", "Appt.DateYear", "Appt.Year", "Appt.Month", "Appt.MonthYear", "uniqueSystem","Appt.Day", "holiday", 'Zip Code Layer: A', "Zip Code Layer: B", "longitude", "latitude")]
  #result <- result$uniqueId
  return(result)
}

uniquePts_df_systemMonth <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(ASSOCIATIONLISTB %in% c("Treatment Visit"))
  } else{
    data <- dt %>% filter(ASSOCIATIONLISTA %in% visitType) 
  }
  
  result <- data %>%
    arrange(MRN, APPT_DTTM) %>% group_by(SITE, MRN, APPT_MONTH_YEAR) %>% mutate(uniqueSystemMonth = row_number()) %>% ungroup() %>%
    filter(uniqueSystemMonth == 1) %>%
    select(SITE, DEPARTMENT_NAME, APPT_DATE_YEAR, APPT_YEAR, APPT_MONTH, APPT_MONTH_YEAR, uniqueSystemMonth, APPT_DAY, DX_GROUPER)
  
  
  #result <- result[c("SITE", "Department", "Appt.DateYear", "Appt.Year", "Appt.Month", "Appt.MonthYear", "uniqueSystemMonth","Appt.Day", "holiday", "Dx.Grouper")]
  
  return(result)
}


uniquePts_df_site <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(ASSOCIATIONLISTB %in% c("Treatment Visit")) 
  } else{
    data <- dt %>% filter(ASSOCIATIONLISTA %in% visitType) 
  }
  
  result <- data %>%
    arrange(MRN, APPT_DTTM, SITE) %>% group_by(MRN, SITE) %>% mutate(uniqueSite = row_number()) %>% ungroup() %>%
    filter(uniqueSite == 1)  %>%
    select(SITE, DEPARTMENT_NAME, APPT_DATE_YEAR, APPT_YEAR, APPT_MONTH, APPT_MONTH_YEAR, uniqueSite, APPT_DAY, DX_GROUPER)
  
  #result <- result[c("SITE", "Department", "Appt.DateYear", "Appt.Year", "Appt.Month", "Appt.MonthYear", "uniqueSite","Appt.Day", "holiday", "Dx.Grouper")]
  
  return(result)
}

uniquePts_df_site_zip <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(AssociationListB %in% c("Treatment Visit")) 
  } else{
    data <- dt %>% filter(AssociationListA %in% visitType) 
  }
  
  result <- data %>%
    arrange(MRN, Appt.DTTM, SITE) %>% group_by(MRN, SITE) %>% mutate(uniqueSite = row_number()) %>% ungroup() %>%
    filter(uniqueSite == 1) 
  
  result <- result[c("SITE", "Department", "Appt.DateYear", "Appt.Year", "Appt.Month", "Appt.MonthYear", "uniqueSite","Appt.Day", "holiday", 'Zip Code Layer: A', "Zip Code Layer: B", "longitude", "latitude")]
  
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
  
  result <- result[c("SITE", "Department", "Appt.DateYear", "Appt.Year", "Appt.Month", "Appt.MonthYear", "uniqueSiteMonth","Appt.Day", "holiday", "Dx.Grouper")]
  
  return(result)
}

uniquePts_df_siteProv <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(ASSOCIATIONLISTB %in% c("Treatment Visit")) 
  } else{
    data <- dt %>% filter(ASSOCIATIONLISTA %in% visitType) 
  }
  
  result <- data %>%
    arrange(MRN, EPIC_PROVIDER_ID, APPT_DTTM, SITE) %>% group_by(MRN, EPIC_PROVIDER_ID, SITE) %>% mutate(uniqueSiteProv = row_number()) %>% ungroup() %>%
    filter(uniqueSiteProv == 1)
  
  return(result)
}

uniquePts_df_siteProvMonth <- function(dt, visitType){
  
  if(visitType == "Treatment Visit"){
    data <- dt %>% filter(ASSOCIATIONLISTB %in% c("Treatment Visit")) 
  } else{
    data <- dt %>% filter(ASSOCIATIONLISTA %in% visitType) 
  }
  
  result <- data %>%
    arrange(MRN, EPIC_PROVIDER_ID, APPT_MONTH_YEAR, SITE) %>% group_by(MRN, EPIC_PROVIDER_ID, APPT_MONTH_YEAR, SITE) %>% 
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


types_pallete <- c("#00aeef", "#7f7f7f","#3a5fcd", "#5753d0", "#d80b8c", "#e69f00", "#8b814c", "#212070")

# Color Function that can be used to call all colors is "MountSinai_cols()"
# Use in ggplot 

#MountSinai_cols()       # will provide all colors and their hex codes in a table 
#MountSinai_cols("pink") # will provide color name and the hex code for the pink color

all_pallete <- c("#212070","#d80b8c","#00aeef","#7f7f7f","#5753d0","#f75dbe","#5cd3ff","#a5a7a5","#c7c6ef", "#fcc9e9","#c9f0ff","#dddedd")

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

plotly_font <- list(
  family = "Calibri",
  size = 14,
  color = "bold")

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
    plot.margin=unit(c(1,1,1,1), "cm")
  )
}


plotly_function <- function(plot, tooltip_data, title){
  ggplotly(plot, tooltip = tooltip_data) %>% layout(legend = list(orientation = "h", x = 0.4, y = -0.2
  )
  )
  
}


plotly_function_volume <- function(plot, tooltip_data, title){
  ggplotly(plot, tooltip = tooltip_data) %>% layout(legend = list(orientation = "h", x = 0.4, y = -0.2
  )
  )
  
}


ggplotly_graph_theme <- function(data, title){
  geom_line(aes(color=Appt.Year), size=1.1)+
    geom_point(aes(color=Appt.Year), size=3)+
    scale_color_MountSinai('dark')+
    labs(title = title, 
         subtitle = paste0("Based on data from ",isolate(input$dateRangetrend[1])," to ",isolate(input$dateRangetrend[2]),"\n"),
         y = "Patient Volume", x = NULL, fill = NULL)+
    scale_y_continuous(limits=c(0,(max(data$total))*1.3)) +
    theme(legend.position = 'top',
          legend.title=element_blank(),
          plot.title = element_text(hjust=0.5, face = "bold", size = 16),
          axis.title = element_text(size="12"),
          axis.text = element_text(size="12"),
          axis.title.x = element_blank(),
          axis.line = element_line(size = 0.3, colour = "black"),
          axis.title.y = element_text(size = 12, angle = 90)
          
    )
  
}

ggplot_line_graph <- function(df, title) {
  
  graph <- ggplot(df, aes(x=factor(APPT_MONTH, levels = monthOptions), y=total, group=APPT_YEAR))+
    geom_line(aes(color=APPT_YEAR), size=1.1)+
    geom_point(aes(color=APPT_YEAR), size=3)+
    scale_color_MountSinai('dark')+
    labs(title = title,
         y = NULL, x = NULL, fill = NULL, color = NULL)+
    scale_y_continuous(limits=c(0,(max(df$total))*1.3)) +
    theme(legend.position = 'top',
          legend.title=element_blank(),
          plot.title = element_text(hjust=0.5, face = "bold", size = 16),
          axis.title = element_text(size="12"),
          axis.text = element_text(size="12"),
          axis.title.x = element_blank(),
          axis.line = element_line(size = 0.3, colour = "black"),
          axis.title.y = element_text(size = 12, angle = 90),
          plot.tag.position = 'top'
          
    )
  
  
  ggplotly(graph, tooltip = c("total")) %>% layout(yaxis = list(mirror = T), xaxis = list(mirror = T))
  
}

ggplot_line_graph_percent <- function(df, title) {
  
  graph <- ggplot(df, aes(x=factor(APPT_MONTH, levels = monthOptions), y=total, group=APPT_YEAR))+
    geom_line(aes(color=APPT_YEAR), size=1.1)+
    geom_point(aes(color=APPT_YEAR), size=3)+
    scale_color_MountSinai('dark')+
    labs(title = title,
         y = NULL, x = NULL, fill = NULL, color = NULL)+
    scale_y_continuous(limits = c(0,max(df$total) * 1.3), labels = scales::percent_format(accuracy = 2)) +
    theme(legend.position = 'top',
          legend.title=element_blank(),
          plot.title = element_text(hjust=0.5, face = "bold", size = 16),
          axis.title = element_text(size="12"),
          axis.text = element_text(size="12"),
          axis.title.x = element_blank(),
          axis.line = element_line(size = 0.3, colour = "black"),
          axis.title.y = element_text(size = 12, angle = 90),
          plot.tag.position = 'top'
          
    )
  
  
  plot <- ggplotly(graph, tooltip = NULL) %>% layout(yaxis = list(mirror = T, tickformat = "%"), xaxis = list(mirror = T))
  
  return(plot)
  
}



ggplot_table <- function(df, hline_y) {
  monthOptions_total <- c(monthOptions, "Total")
  graph <- ggplot(df, aes(x= factor(APPT_MONTH, levels = monthOptions_total), y= APPT_YEAR))+
    labs(x=NULL, y=NULL)+
    scale_x_discrete(position = "bottom")+
    theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.key.size = unit(.8,"cm"),
          legend.text = element_text(size="10"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
          axis.text.x = element_blank(),
          axis.text.y = element_text(color= "black", margin = margin(r=15)),
          axis.text = element_text(size="14"),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
    ) +
    geom_text(aes(label= ifelse(is.na(total),"",total)), color="black", size=5, fontface="bold") +
    geom_hline(yintercept = hline_y, colour='black')+
    geom_vline(xintercept = 0, colour = 'black') +
    table_theme() +
    theme(plot.margin=unit(c(1,0,1,1), "cm"))
  
  ggplotly(graph, tooltip = NULL)
}

ggplot_table_comparison <- function(df, hline_y) {
  graph <- ggplot(df, aes(x= factor(APPT_MONTH_YEAR, levels = APPT_MONTH_YEAR), y= Total))+
    labs(x=NULL, y=NULL)+
    scale_x_discrete(position = "bottom")+
    theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.key.size = unit(.8,"cm"),
          legend.text = element_text(size="10"),
          axis.title.x = element_blank(),
          # axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          # axis.text.y = element_text(color= "black", margin = margin(r=15)),
          axis.text.y = element_blank(),
          # axis.text = element_text(size="14"),
          axis.text = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
    ) +
    geom_text(aes(label= ifelse(is.na(total),"",total)), color="black", size=5, fontface="bold") +
    geom_hline(yintercept = hline_y, colour='black')+
    geom_vline(xintercept = 0, colour = 'black') +
    table_theme() +
    theme(axis.title.y = element_text(size="14"),
          axis.text.y = element_text(size="14"))
  
  ggplotly(graph, tooltip = NULL)
}

ggplot_table_comparison_site <- function(df, hline_y) {
  graph <- ggplot(df, aes(x= factor(APPT_MONTH_YEAR, levels = APPT_MONTH_YEAR), y= SITE))+
    labs(x=NULL, y=NULL)+
    scale_x_discrete(position = "bottom")+
    theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.key.size = unit(.8,"cm"),
          legend.text = element_text(size="10"),
          axis.title.x = element_blank(),
          # axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          # axis.text.y = element_text(color= "black", margin = margin(r=15)),
          axis.text.y = element_blank(),
          # axis.text = element_text(size="14"),
          axis.text = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
    ) +
    geom_text(aes(label= ifelse(is.na(total),"",total)), color="black", size=5, fontface="bold") +
    geom_hline(yintercept = hline_y, colour='black')+
    geom_vline(xintercept = 0, colour = 'black') +
    table_theme() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank())
  
  ggplotly(graph, tooltip = NULL)
}


ggplot_bar_graph <- function(df, title, x_data, y_data, group, max) {
  graph <- ggplot(df, aes(x = x_data, y = y_data, group = group, fill = group))+
    geom_bar(position="stack",stat="identity", width=0.7)+
    scale_fill_MountSinai('dark')+
    labs(title = title,
         y = "Patient Volume", x = NULL, fill = NULL)+
    scale_y_continuous(limits=c(0,(max(max$max))*1.2))+
    theme(legend.position = 'top',
          legend.title=element_blank(),
          plot.title = element_text(hjust=0.5, face = "bold", size = 16),
          axis.title = element_text(size="12"),
          axis.text = element_text(size="11"),
          axis.title.x = element_blank(),
          axis.line = element_line(size = 0.3, colour = "black"),
          axis.title.y = element_text(size = 12, angle = 90)
          
    )
  ggplotly(graph, tooltip = c("total")) %>% layout(yaxis = list(mirror = T), xaxis = list(mirror = T))
}



ggplot_bar_table <- function(df, x_data, y_data, label, hline_y) {
  table <- ggplot(df, aes(x = x_data, y= y_data, label = label)) +
    labs(x=NULL, y=NULL)+
    scale_x_discrete(position = "bottom")+
    theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.key.size = unit(.8,"cm"),
          legend.text = element_text(size="10"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
          axis.text.x = element_blank(),
          axis.text.y = element_text(color= "black", margin = margin(r=15)),
          axis.text = element_text(size="14"),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
    ) +
    geom_text(aes(label= ifelse(is.na(df$total),"",df$total)), color="black", size=5, fontface="bold") +
    geom_hline(yintercept = hline_y, colour='black')+
    geom_vline(xintercept = 0, colour = 'black') +
    table_theme()
  
  ggplotly(table, tooltip = NULL)
  
}


site_unique_plotly_graph <- function (data) {
  unique_patients_combined <- data
  
  if(length(unique(unique_patients_combined$SITE)) == 1) {
    plot_out <- plot_ly(unique_patients_combined, x = ~APPT_MONTH_YEAR, y = ~total, type = 'bar', name = "Unique MRN",
            marker = list(color = "#212070")) %>%
      add_trace(unique_patients_combined, x=~APPT_MONTH_YEAR, y = ~perc_race_unknown, yaxis = "y2",type = "scatter", mode = 'line', name = "Race % Blank/Unk",line = list(color = "#00aeef"), marker = list(color = "#00aeef")) %>%
      layout(yaxis2 = list(overlaying = "y", side = "right", tickformat = ".0%", range = c(0,round_any(2.5*max(unique_patients_combined$perc_race_unknown),0.05, ceiling)), 
                           automargin = T)) %>%
      layout(legend = list(x = 1.10, y = 1), title = unique(unique_patients_combined$SITE)) %>%
      layout(xaxis = list(title = NA),
             yaxis = list(title = NA))
  } else {
    n = length(unique(unique_patients_combined$SITE))
    plot_list <- vector("list", n)
    anotation_list <- vector("list", n)
    unique_sites <- (unique(unique_patients_combined$SITE))
    site_list <- as.vector(unique_sites, "list")
    
    for (i in 1:n){
      single_site_unique <- unique_patients_combined %>% filter(SITE == site_list[[i]])
      if(i == 1) {
        y_var <- "y"
      } else {
        number <- 2*i-1
        y_var <- paste0("y",number)
      }
      
      if(i == 1) {
      
      plot <-  plot_ly(single_site_unique, x = ~APPT_MONTH_YEAR, y = ~total, type = 'bar', name = "Unique MRN", legendgroup=~"group1", showlegend=T,
                       marker = list(color = "#212070"), yaxis="y") %>%
        add_trace(single_site_unique, x=~APPT_MONTH_YEAR, y = ~perc_race_unknown, yaxis = "y2",type = "scatter", legendgroup=~"group2", mode = 'line', name = "Race % Blank/Unk",line = list(color = "#00aeef"), marker = list(color = "#00aeef")) %>%
        layout(xaxis = list(title = NA),
               yaxis = list(title = NA),
               legend = list(x = 1.10, y = 1),
               yaxis2 = list(overlaying = y_var, side = "right", tickformat = ".2%", range = c(0,round_any(2.5*max(single_site_unique$perc_race_unknown),0.05, ceiling)),
                             automargin = T)
        )
      
        plot_list[[i]] <- plot %>% layout(annotations = list(text = site_list[[i]], 
                                                             x = 0.5,
                                                             y = 1,
                                                             yref = "paper",
                                                             xref = "paper",
                                                             xanchor = "left",
                                                             yanchor = "top",
                                                             yshift = 20,
                                                             showarrow = FALSE,
                                                             font = list(size = 15)
                                                             )
                                          )
      } else {
        plot <-  plot_ly(single_site_unique, x = ~APPT_MONTH_YEAR, y = ~total, type = 'bar', name = "Unique MRN", legendgroup=~"group1", showlegend=F,
                         marker = list(color = "#212070"), yaxis="y") %>%
          add_trace(single_site_unique, x=~APPT_MONTH_YEAR, y = ~perc_race_unknown, yaxis = "y2",type = "scatter", legendgroup=~"group2", mode = 'line', name = "Race % Blank/Unk",line = list(color = "#00aeef"), marker = list(color = "#00aeef")) %>%
          layout(xaxis = list(title = NA),
                 yaxis = list(title = NA),
                 yaxis2 = list(overlaying = y_var, side = "right", tickformat = ".2%", range = c(0,round_any(2.5*max(single_site_unique$perc_race_unknown),0.05, ceiling)),
                               automargin = T)
          )
        plot_list[[i]] <- plot %>% layout(annotations = list(text = site_list[[i]], 
                                                             x = 0.5,
                                                             y = 1.05,
                                                             yref = "paper",
                                                             xref = "paper",
                                                             xanchor = "center",
                                                             yanchor = "bottom",
                                                             showarrow = FALSE,
                                                             font = list(size = 15))) 
      }
      
     
    }
    plot_out <- subplot(plot_list, nrows = n, margin = 0.05) %>% layout(title = NA)
  }
  
  return(plot_out)
}

