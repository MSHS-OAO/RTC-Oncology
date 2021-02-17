library(shinydashboardPlus)
library(shinycssloaders)
library(shinyBS)
library(shinyscreenshot)

# # Remove Provider ID from Provider Name column
# amb_df_groupings_unique$PROV_NAME_WID <- trimws(gsub("\\[.*?\\]", "", amb_df_groupings_unique$PROV_NAME_WID))

### Set default values for master filters --------------------------------------------------------------------------------------

default_campus <- "DBC"
campus_choices <- sort(unique(historical.data$SITE))
default_specialties <- sort(unique(historical.data[historical.data$SITE %in% default_campus, "Campus.Specialty"]))

default_departments <- sort(unique(historical.data[historical.data$SITE %in% default_campus &
                                                     historical.data$Campus.Specialty %in% default_specialties, "Department"])) 


default_provider <- sort(unique(historical.data[historical.data$SITE %in% default_campus & 
                                                  historical.data$Campus.Specialty %in% default_specialties &
                                                  historical.data$Department %in% default_departments, "Provider"])) 


default_refProvider <- sort(unique(historical.data[historical.data$SITE %in% default_campus &
                                                     historical.data$Campus.Specialty %in% default_specialties &
                                                     historical.data$Department %in% default_departments &
                                                     historical.data$Provider %in% default_provider, "Ref.Provider"]))


default_visitType <- sort(unique(historical.data[historical.data$SITE %in% default_campus &
                                                   historical.data$Campus.Specialty %in% default_specialties &
                                                   historical.data$Department %in% default_departments &
                                                   historical.data$Provider %in% default_provider & 
                                                   historical.data$Ref.Provider %in% default_refProvider, "AssociationListA"]))

default_ApptType <- sort(unique(historical.data[historical.data$SITE %in% default_campus &
                                                   historical.data$Campus.Specialty %in% default_specialties &
                                                   historical.data$Department %in% default_departments &
                                                   historical.data$Provider %in% default_provider & 
                                                   historical.data$Ref.Provider %in% default_refProvider &
                                                  historical.data$AssociationListA %in% default_visitType, "AssociationListB"]))

default_TreatmentType <- sort(unique(historical.data[historical.data$SITE %in% default_campus &
                                                  historical.data$Campus.Specialty %in% default_specialties &
                                                  historical.data$Department %in% default_departments &
                                                  historical.data$Provider %in% default_provider & 
                                                  historical.data$Ref.Provider %in% default_refProvider &
                                                  historical.data$AssociationListA %in% default_visitType &
                                                  historical.data$AssociationListB %in% default_ApptType, "AssociationListT"]))




dateRange_min <- min(historical.data$Appt.DateYear) 
dateRange_max <- max(historical.data$Appt.DateYear)
daysOfWeek.options <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
# holid <- unique(holid$holiday)
# default_appttypes <- NULL
# default_treattype <- NULL

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
                         menuSubItem("Overview", tabName = "volumetrend"),
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
                div("Volume Trend", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                column(10,
                       boxPlus(
                         title = "All Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("trend_totalvisitsgraph", height="550px") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Exam Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("trend_examvisitsgraph", height="550px") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Treatment Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("trend_treatmentvisitsgraph", height="550px") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Lab Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("trend_labvisitsgraph", height="550px") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Annual Visit Volume Summary", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         column(3,
                                checkboxGroupButtons(
                                  inputId = "annualVolSummary",
                                  label = h3("Select Visit Type:"), 
                                  choices = c("Office", "Treatment", "Labs"),
                                  selected = c("Office", "Treatment", "Labs"),
                                  status = "warning"
                                ),
                                tableOutput("trend_visitstable")
                         ))
                )
        ),
        # Volume Breakdown Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "volumebreakdown",
                div("Volume Breakdown", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                column(10,
                       boxPlus(
                         title = "All Visits", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("break_totalvisitsgraph", height="550px") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Exam Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("break_examvisitsgraph", height="550px") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Treatment Visits", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("break_treatmentvisitsgraph", height="550px") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Visit Volume Breakdown", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         tableOutput("break_visitstable")
                       )
                       
                )
        ), #Close Volume breakdown tab
        
        # Volume Comparison Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "volumecomparison",
                div("Volume Comparison", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                column(8,
                       boxPlus(
                         title = "Visit Volume Comparison", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("volumeCompTotal_grh", height="650px") %>%
                           withSpinner(type = 5, color = "#d80b8c"), hr(),
                         plotOutput("volumeCompTrend_grh", height="650px") %>%
                           withSpinner(type = 5, color = "#d80b8c")
                       )
  
                       
                ) #Close column
        )# Close volume Comparison
      ), #Close tab Items
      
      # Conditional Filters ------------------------------------------------------------------------------------------------------  
      conditionalPanel(
        condition = "input.sbm == 'volumecomparison'", 
        column(2, #offset=8, 
                 box(
                   title = "Select Visit Type:",
                   width = 12,
                   height = "100px",
                   solidHeader = FALSE,
                   pickerInput("selectedVisitType",label=NULL,
                               choices=default_visitType,
                               multiple=TRUE,
                               options = pickerOptions(
                                 liveSearch = TRUE,
                                 actionsBox = TRUE,
                                 selectedTextFormat = "count > 1",
                                 countSelectedText = "{0}/{1} Visit Types",
                                 dropupAuto = FALSE),
                               selected = default_visitType
                   )
                 ),
                 box(
                   title = "Select Appointment Type:",
                   width = 12,
                   height = "100px",
                   solidHeader = FALSE,
                   pickerInput("selectedApptType",label=NULL,
                               choices=default_ApptType,
                               multiple=TRUE,
                               options = pickerOptions(
                                 liveSearch = TRUE,
                                 actionsBox = TRUE,
                                 selectedTextFormat = "count > 1",
                                 countSelectedText = "{0}/{1} Visit Types",
                                 dropupAuto = FALSE),
                               selected = default_ApptType
                   )
                 ),
                 box(
                   title = "Select Treatment Type:",
                   width = 12,
                   height = "100px",
                   solidHeader = FALSE,
                   pickerInput("selectedTreatmentType",label=NULL,
                               choices=default_TreatmentType,
                               multiple=TRUE,
                               options = pickerOptions(
                                 liveSearch = TRUE,
                                 actionsBox = TRUE,
                                 selectedTextFormat = "count > 1",
                                 countSelectedText = "{0}/{1} Visit Types",
                                 dropupAuto = FALSE),
                               selected = default_TreatmentType
                   )
                 ),
                  box(
                        title = "Compare by:",
                        width = 12,
                        height = "150px",
                        solidHeader = FALSE,
                        awesomeRadio(
                          inputId = "comp_choices",
                          label = NULL,
                          choices = c("All", "Site", "Specialty", "Provider"),
                          selected = "All",
                          status = "info"
                        )
                      )
               )
      ), # Close Conditional Panel
      conditionalPanel(
        condition = "input.sbm == 'volumetrend' | input.sbm == 'volumebreakdown' | input.sbm == 'volumecomparison'", 
        column(2, 
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
                             selected = default_campus)),
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
                 title = "Select Provider:",
                 width = 12,
                 height = "100px",
                 solidHeader = FALSE,
                 pickerInput("selectedProvider",label=NULL,
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
                 title = "Select Reffered Provider:",
                 width = 12,
                 height = "100px",
                 solidHeader = FALSE,
                 pickerInput("selectedrefProvider",label=NULL,
                             choices=default_refProvider,
                             multiple=TRUE,
                             options = pickerOptions(
                               liveSearch = TRUE,
                               actionsBox = TRUE,
                               selectedTextFormat = "count > 1",
                               countSelectedText = "{0}/{1} Providers",
                               dropupAuto = FALSE),
                             selected = default_refProvider
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
               ),
               fluidRow(
                 column(2, offset = 4,
                        actionButton("download1",
                                     label = icon("download"),
                                     style="color: #fff; background-color: #d80b8c; border-color: #d80b8c"),
                        bsTooltip("download1", "Creates a PNG file with all visible graphs on this page. Use the minimize or close buttons to hide unwanted graphs",
                                  "bottom", options = list(container = "body")
                        )
                  )
                )
        )# Close column
        
      )# Close conditional Panel
    ) #Close Fluid
  ) # Close Dashboard Body
)# Close Dashboard Page