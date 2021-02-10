library(shinydashboardPlus)
library(shinycssloaders)
library(shinyBS)
library(shinyscreenshot)
default_campus <- "DBC"
campus_choices <- sort(unique(amb_df_groupings_unique$SITE))
default_specialties <- sort(unique(amb_df_groupings_unique[amb_df_groupings_unique$SITE %in% default_campus, "DEPT_SPECIALTY_NAME"]))
default_departments <- sort(unique(amb_df_groupings_unique[amb_df_groupings_unique$SITE %in% default_campus &
                                                             amb_df_groupings_unique$DEPT_SPECIALTY_NAME %in% default_specialties, "DEPARTMNET_NAME"])) 
default_visittype <- NULL
default_provider <- NULL
default_refprovider <- NULL
dateRange_min <- min(amb_df_groupings_unique$APPT_DTTM) 
dateRange_max <- max(amb_df_groupings_unique$APPT_DTTM)
daysOfWeek.options <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
holid <- NULL
default_appttypes <- NULL
default_treattype <- NULL

ui <- dashboardPage(
  dashboardHeader(title = "Oncology Analytics Tool",
                  titleWidth = 250),
  # Sidebar ------------------------------------------------------------------------------------------------------
  dashboardSidebar(
    # Customize dashboard color scheme: title bar = .logo & .navbar; side bar = .main-sidebar; background = .content-wrapper
    tags$head(tags$style(HTML('.logo {
                              background-color: #221f72 !important;
                              }
                              .navbar {
                              background-color: #221f72 !important;
                              }
                              
                              .content-wrapper {
                              background-color: white !important;
                              }'
                              
    ))),
    
    # Overwrite fixed height of dashboard sidebar
    #tags$head(tags$style(HTML('.content-wrapper { height: 6000px !important;}'))),
    
    width = 200,
    
    sidebarMenu(id = "sbm",
                menuItem("Volume", tabName = "volume", icon = icon("chart-bar"),
                         menuSubItem("Trend/Overview", tabName = "volumetrend"),
                         menuSubItem("Breakdown", tabName = "volumebreakdown"),
                         menuSubItem("Comparison", tabName = "volumecomparison")
                )
      
    ) # Close sidebarMenu
  ), # Close Dashboard Sidebar
  dashboardBody(
    tags$head(tags$style(
      HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
    )),
    fluidPage(
    # box "status" color for Mount Sinai Purple
    tags$style(HTML("
    .box.box-solid.box-primary>.box-header {
    color:#fff;
    background:#221f72
    }
    .box.box-solid.box-primary{
    border-bottom-color:#ffffff;
    border-left-color:#ffffff;
    border-right-color:#ffffff;
    border-top-color:#ffffff;
    }
                    ")),
    
    # valueBox "yellow" color for Mount Sinai Light Grey
    tags$style(".small-box.bg-yellow { background-color: 	#dddedd !important; color: #000000 !important; }"),
    # valueBox "purple" color for Mount Sinai Dark Purple
    tags$style(".small-box.bg-purple { background-color: 	#212070 !important; color: #ffffff !important; }"),
    # valueBox "fuchsia" color for Mount Sinai Dark Pink
    tags$style(".small-box.bg-fuchsia { background-color: 	#d80b8c !important; color: #ffffff !important; }"),
    # valueBox "aqua" color for Mount Sinai Dark Blue
    tags$style(".small-box.bg-aqua { background-color: 	#00aeef !important; color: #ffffff !important; }"),
    
    
    # Top align plot outputs
    tags$head(tags$style(".top-align { vertical-align: top;}  ")),
    tabItems(
# Volume Trend Tab ------------------------------------------------------------------------------------------------------
      tabItem(tabName = "volumetrend",
                column(10,
                       div("Volume Trend", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                       boxPlus(
                         title = "All Visits", width = 12, status = "primary", height = "500px",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("trend_totalvisitsgraph", height="450px") %>% 
                           withSpinner(type = 5, color = "#d80b8c"), br(),
                         tableOutput("trend_totalvisitstable") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Exam Visits", width = 12, status = "primary", height = "500px",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("trend_examvisitsgraph", height="450px") %>% 
                           withSpinner(type = 5, color = "#d80b8c"), br(),
                         tableOutput("trend_examvisitstable") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Treatment Visits", width = 12, status = "primary", height = "500px",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("trend_treatmentvisitsgraph", height="450px") %>% 
                           withSpinner(type = 5, color = "#d80b8c"), br(),
                         tableOutput("trend_treatmentvisitstable") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Lab Visits", width = 12, status = "primary", height = "500px",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("trend_labvisitsgraph", height="450px") %>% 
                           withSpinner(type = 5, color = "#d80b8c"), br(),
                         tableOutput("trend_labvisitstable") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       
                )
              ),
# Volume Breakdown Tab ------------------------------------------------------------------------------------------------------
      tabItem(tabName = "volumebreakdown",
              column(10,
                     div("Volume Breakdown", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     boxPlus(
                       title = "All Visits", width = 12, status = "primary", height = "500px",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       plotOutput("break_totalvisitsgraph", height="450px") %>% 
                         withSpinner(type = 5, color = "#d80b8c"), br(),
                       tableOutput("break_totalvisitstable") %>% 
                         withSpinner(type = 5, color = "#d80b8c")
                     ),
                     boxPlus(
                       title = "Exam Visits", width = 12, status = "primary", height = "500px",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       plotOutput("break_examvisitsgraph", height="450px") %>% 
                         withSpinner(type = 5, color = "#d80b8c"), br(),
                       tableOutput("break_examvisitstable") %>% 
                         withSpinner(type = 5, color = "#d80b8c")
                     ),
                     boxPlus(
                       title = "Treatment Visits", width = 12, status = "primary", height = "500px",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       plotOutput("break_treatmentvisitsgraph", height="450px") %>% 
                         withSpinner(type = 5, color = "#d80b8c"), br(),
                       tableOutput("break_treatmentvisitstable") %>% 
                         withSpinner(type = 5, color = "#d80b8c")
                     ),
                     boxPlus(
                       title = "Lab Visits", width = 12, status = "primary", height = "500px",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       plotOutput("break_labvisitsgraph", height="450px") %>% 
                         withSpinner(type = 5, color = "#d80b8c"), br(),
                       tableOutput("break_labvisitstable") %>% 
                         withSpinner(type = 5, color = "#d80b8c")
                     ),
                     
              )
      ), #Close Volume breakdown tab
# Volume Comparison Tab ------------------------------------------------------------------------------------------------------
      tabItem(tabName = "volumecomparison",
              column(10,
                     div("Volume Comparison", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr()
              ) #Close column
      )# Close volume Comparison
    ), #Close tab Items
# Conditional Filters ------------------------------------------------------------------------------------------------------    
    conditionalPanel(
      condition = "input.sbm == 'volumetrend' | input.sbm == 'volumebreakdown' |
        input.sbm == 'volumecomparison'", 
      column(2,
             fluidRow(
               column(2, offset = 1,
                      actionButton("download1",
                                   label = icon("download")),
                      bsTooltip("download1", "Creates a PNG file with all visible graphs on this page. Use the minimize or close buttons to hide unwanted graphs",
                                "bottom", options = list(container = "body"))
                      
               )
             ),
             br(),
             box(
               title = "Select Campus:",
               width = 12,
               height = "100px",
               solidHeader = FALSE,
               pickerInput("selectedCampus",label=NULL,
                           choices = campus_choices,
                           multiple=TRUE,
                           options = pickerOptions(
                             liveSearch = TRUE,
                             actionsBox = TRUE,
                             selectedTextFormat = "count > 1", 
                             countSelectedText = "{0}/{1} Campuses", 
                             dropupAuto = FALSE),
                           selected = default_campus
              )
            ),

            box(
              title = "Select Specialty:",
              width = 12,
              height = "100px",
              solidHeader = FALSE,
              pickerInput("selectedSpecialty",label=NULL,
                          choices = default_specialties,
                          multiple=TRUE,
                          options = pickerOptions(
                            liveSearch = TRUE,
                            actionsBox = TRUE,
                            selectedTextFormat = "count > 1",
                            countSelectedText = "{0}/{1} Specialties",
                            dropupAuto = FALSE),
                          selected = default_specialties
            )
          ),
          box(
            title = "Select Department:",
            width = 12,
            height = "100px",
            solidHeader = FALSE,
            pickerInput("selectedDepartment",label=NULL,
                        choices=default_departments,
                        multiple=TRUE,
                        options = pickerOptions(
                          liveSearch = TRUE,
                          actionsBox = TRUE,
                          selectedTextFormat = "count > 1",
                          countSelectedText = "{0}/{1} Departments",
                          dropupAuto = FALSE),
                        selected = default_departments
          )
        ),
        box(
          title = "Select Visit Type:",
          width = 12,
          height = "100px",
          solidHeader = FALSE,
          pickerInput("selectedvisitype",label=NULL,
                      choices=default_visittype,
                      multiple=TRUE,
                      options = pickerOptions(
                        liveSearch = TRUE,
                        actionsBox = TRUE,
                        selectedTextFormat = "count > 1",
                        countSelectedText = "{0}/{1} Visit Types",
                        dropupAuto = FALSE),
                      selected = default_visittype
          )
        ),
        box(
          title = "Select Provider Name:",
          width = 12,
          height = "100px",
          solidHeader = FALSE,
          pickerInput("selectedprovider",label=NULL,
                      choices=default_provider,
                      multiple=TRUE,
                      options = pickerOptions(
                        liveSearch = TRUE,
                        actionsBox = TRUE,
                        selectedTextFormat = "count > 1",
                        countSelectedText = "{0}/{1} Providers",
                        dropupAuto = FALSE),
                      selected = default_provider
          )
        ),
        box(
          title = "Select Reffered Provider Name:",
          width = 12,
          height = "100px",
          solidHeader = FALSE,
          pickerInput("selectedrefprovider",label=NULL,
                      choices=default_refprovider,
                      multiple=TRUE,
                      options = pickerOptions(
                        liveSearch = TRUE,
                        actionsBox = TRUE,
                        selectedTextFormat = "count > 1",
                        countSelectedText = "{0}/{1} Providers",
                        dropupAuto = FALSE),
                      selected = default_refprovider
          )
        ),
        box(
          title = "Select Date Range:",
          width = 12, 
          height = "100px",
          solidHeader = FALSE, 
          dateRangeInput("dateRange", label = NULL,
                         start = dateRange_min, end = dateRange_max,
                         min = dateRange_min, max = dateRange_max
                         )
          ),
        box(
          title = "Select Days of Week:",
          width = 12, 
          solidHeader = FALSE, 
          selectInput("daysOfWeek",label = NULL,
                      choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), selected = daysOfWeek.options,
                      multiple=TRUE, selectize=TRUE
                      )
          ),
        box(
          title = "Select Holidays to Exclude:",
          width = 12,
          solidHeader = FALSE,
          pickerInput("excludeHolidays",label=NULL,
                      choices= unique(holid$holiday),
                      multiple=TRUE,
                      options = pickerOptions(
                        liveSearch = TRUE,
                        actionsBox = TRUE,
                        dropupAuto = FALSE),
                      selected = unique(holid$holiday)
                      )
          )
      )# Close column
      
    ),# Close conditional Panel
    conditionalPanel(
      condition = "input.sbm == 'volumecomparison'",
      column(2,
             box(
               title = "Select Appointment Type:",
               width = 12,
               height = "100px",
               solidHeader = FALSE,
               pickerInput("selectedappointmenttype",label=NULL,
                           choices = default_appttypes,
                           multiple=TRUE,
                           options = pickerOptions(
                             liveSearch = TRUE,
                             actionsBox = TRUE,
                             selectedTextFormat = "count > 1", 
                             countSelectedText = "{0}/{1} Appointemnt Types", 
                             dropupAuto = FALSE),
                           selected = default_appttypes
               )    
        
      ),
      box(
        title = "Select Treatement Type:",
        width = 12,
        height = "100px",
        solidHeader = FALSE,
        pickerInput("selectedtreatmenttype",label=NULL,
                    choices = default_treattype,
                    multiple=TRUE,
                    options = pickerOptions(
                      liveSearch = TRUE,
                      actionsBox = TRUE,
                      selectedTextFormat = "count > 1", 
                      countSelectedText = "{0}/{1} Appointemnt Types", 
                      dropupAuto = FALSE),
                    selected = default_treattype
        )    
        
      )
    )# Close column
  ) #Close Conditional Panel
  ) #Close Fluid
  ) # Close Dashboard Body
)# Close Dashboard Page