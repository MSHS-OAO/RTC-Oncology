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
                         menuSubItem("Trend", tabName = "volumetrend"),
                         menuSubItem("Breakdown", tabName = "volumebreakdown"),
                         menuSubItem("Comparison", tabName = "volumecomparison")
                ),
                menuItem("Unique Patients", tabName = "uniquePts", icon = icon("hospital-user"),
                         menuSubItem("All", tabName = "uniqueAll"),
                         menuSubItem("Office", tabName = "uniqueOffice"),
                         menuSubItem("Treatment", tabName = "uniqueTreatment")
                ),
                menuItem("Population", tabName = "population", icon = icon("map-marked"),
                         menuSubItem("Zip Code Analysis", tabName = "zipCode")
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
                column(11,
                       boxPlus(
                         title = "All Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("trend_totalvisitsgraph", height = "auto") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Exam Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("trend_examvisitsgraph", height = "auto") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Treatment Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("trend_treatmentvisitsgraph", height = "auto") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Lab Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("trend_labvisitsgraph", height = "auto") %>% 
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
                column(11,
                       boxPlus(
                         title = "All Visits", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("break_totalvisitsgraph", height = "auto") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Exam Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("break_examvisitsgraph", height = "auto") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Treatment Visits", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("break_treatmentvisitsgraph", height = "auto") %>% 
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
                div("Volume Comparison", style = "color: #221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                column(11,
                       boxPlus(
                         title = "Visit Volume Comparison", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("volumeCompTotal_grh", height = "auto") %>%
                           withSpinner(type = 5, color = "#d80b8c"), hr(),
                         plotOutput("volumeCompTrend_grh", height = "auto") %>%
                           withSpinner(type = 5, color = "#d80b8c")
                       )
                       
                       
                ) #Close column
        ), # Close volume Comparison
        
        # Unique Patients Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "uniqueAll",
                div("Unique Patients - Office, Treatment, and Lab Visits", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                column(11,
                       boxPlus(
                         title = "Unique Patients by System and Site", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         valueBoxOutput("uniqueAllSystem", width=4) %>%
                           withSpinner(type = 5, color = "#d80b8c"), hr(),
                         column(12,
                                plotOutput("uniqueAllSite", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))
                       ), 
                       boxPlus(
                         title = "Unique Patients by Month and Provider", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(), 
                         column(4,
                                box(
                                  title = "Analysis by:",
                                  width = 12,
                                  height = "110px",
                                  solidHeader = FALSE, 
                                  radioButtons("selectedUniquePts", label=NULL,
                                               choices = c("System","Site"),
                                               selected = "System"))), hr(),
                         column(12,
                                plotOutput("uniqueAllTrend", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueAllMonth", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))
                       )
                )
                
        ), #Close Unique Patients - All tab
        
        tabItem(tabName = "uniqueOffice",
                div("Unique Patients - Office Visits", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                column(11,
                       boxPlus(
                         title = "Unique Patients by System and Site", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         valueBoxOutput("uniqueOfficeSystem", width=4) %>%
                           withSpinner(type = 5, color = "#d80b8c"), hr(),
                         column(12,
                                plotOutput("uniqueOfficeSite", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))
                       ), 
                       boxPlus(
                         title = "Unique Patients by Month and Provider", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(), 
                         column(4,
                                box(
                                  title = "Analysis by:",
                                  width = 12,
                                  height = "100px",
                                  solidHeader = FALSE, 
                                  radioButtons("selectedUniquePts2", label=NULL,
                                               choices = c("System","Site"),
                                               selected = "System"))), hr(),
                         column(12,
                                plotOutput("uniqueOfficeTrend", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueOfficeMonth", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))
                       )
                )
                
        ), #Close Unique Patients - Office tab
        
        tabItem(tabName = "uniqueTreatment",
                div("Unique Patients - Office Visits", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                column(11,
                       boxPlus(
                         title = "Unique Patients by System and Site", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         valueBoxOutput("uniqueTreatmentSystem", width=4) %>%
                           withSpinner(type = 5, color = "#d80b8c"), hr(),
                         column(12,
                                plotOutput("uniqueTreatmentSite", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))
                       ), 
                       boxPlus(
                         title = "Unique Patients by Month and Provider", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(), 
                         column(4,
                                box(
                                  title = "Analysis by:",
                                  width = 12,
                                  height = "100px",
                                  solidHeader = FALSE, 
                                  radioButtons("selectedUniquePts3", label=NULL,
                                               choices = c("System","Site"),
                                               selected = "System"))), hr(),
                         column(12,
                                plotOutput("uniqueTreatmentTrend", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueTreatmentMonth", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))
                       )
                )
                
        ), #Close Unique Patients - Treatment tab
        
        
        
        # Population Zip Code Analysis Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "zipCode",
                div("Patient Zip Code Analysis", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                column(11,
                       boxPlus(
                         title = "Arrived Patients by Zip Code", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         fluidRow(
                           column(4,
                                  box(
                                    title = "Zip Code Map by:",
                                    width = 12,
                                    height = "130px",
                                    solidHeader = FALSE, 
                                    radioButtons("selectedZipCodeMap", label=NULL,
                                                 choices = c("Site","Visit Type","Appointment Type"),
                                                 selected = "Site"))
                           ), hr()),
                         fluidRow(
                           column(8, leafletOutput("zipCode_map", height = "800px") %>%
                                    withSpinner(type = 5, color = "#d80b8c")),
                           column(4, tableOutput("zipCode_tb") %>%
                                    withSpinner(type = 5, color = "#d80b8c")))
                       )
                )
        ) # Close Zip Code tab
        
      ), # Close Main tab Items
      
      
      # Formatting Dropdown Buttons
      tags$head(tags$style(HTML("#dropdownbutton {color: #212070;}"))),
      tags$head(tags$style(HTML("#dropdownbutton1 {color: #212070;}"))),
      tags$head(tags$style(HTML("#dropdownheight {color: #212070;}"))),
      tags$head(tags$style(HTML("#dropdownheight {color: #212070;}"))),
      tags$head(tags$style(HTML("#dropdownUnique {color: #d80b8c;}"))),
      tags$head(tags$style(HTML("#dropdownZipCode {color: #d80b8c;}"))),
      
      
      # Conditional Filters ------------------------------------------------------------------------------------------------------  
      
      conditionalPanel(
        condition = "input.sbm == 'volumetrend' | input.sbm == 'volumebreakdown' | input.sbm == 'volumecomparison' | 
        input.sbm == 'uniqueAll' | input.sbm == 'uniqueOffice' | input.sbm == 'uniqueTreatment' | input.sbm == 'zipCode'",
        
        dropdown(
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
            title = "Select Referring Provider:",
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
            height = "150px",
            solidHeader = FALSE, 
            dateRangeInput("dateRange", label = NULL,
                           start = dateRange_min, end = dateRange_max,
                           min = dateRange_min, max = dateRange_max
            ),
            fluidRow(
              column(12, offset = 2,
                     radioGroupButtons(
                       inputId = "dateRangePreset",
                       #label = "Choices", 
                       choices = c("1M", "2M", "3M"),
                       #status = "primary"
                       selected = character(0)
                     )
              )
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
          
          style = "material-circle", size = "lg", right = TRUE, status = "default",
          icon = icon("filter"), width = "300px",
          tooltip = tooltipOptions(title = "Set filters for graphs/tables."),
          inputId = "dropdownbutton"
          
        ) # Close Drop Down Button
      ),# Close conditional Panel
      
      conditionalPanel(
        condition = "input.sbm == 'volumecomparison'", 
        br(), 
        dropdown(
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
            height = "160px",
            solidHeader = FALSE,
            awesomeRadio(
              inputId = "comp_choices",
              label = NULL,
              choices = c("All", "Site", "Specialty", "Provider"),
              selected = "All",
              status = "info"
            )
          ),
          
          box(
            title = "Analysis by:",
            width = 12,
            height = "110px",
            solidHeader = FALSE,
            awesomeRadio(
              inputId = "analysis_type",
              label = NULL,
              choices = c("Monthly", "Weekly"),
              selected = "Monthly",
              status = "info"
            )
          ),
          
          style = "material-circle", size = "lg", right = TRUE, status = "default",
          icon = icon("filter"), width = "300px",
          tooltip = tooltipOptions(title = "Set additional filters for graphs/tables."),
          inputId = "dropdownbutton1"
          
        ) # Close Drop Down Button
      ), # Close Conditional Panel
      
      
      conditionalPanel(
        condition = "input.sbm == 'volumetrend' | input.sbm == 'volumebreakdown' | input.sbm == 'volumecomparison' | 
         input.sbm == 'uniqueAll' | input.sbm == 'uniqueOffice' | input.sbm == 'uniqueTreatment' | input.sbm == 'zipCode'",
        br(),
        dropdown(
          box(
            title = "Change height:",
            width = 12,
            height = "150px",
            solidHeader = FALSE,
            sliderInput(
              inputId = "plotHeight",
              label = NULL, 
              value = 650, min = 450, max = 2000,
              ticks = FALSE
            ),
            fluidRow(
              column(2, offset = 4,
                     actionButton("resetheight", "Reset")
              )
            )
          ),
          
          
          
          # numericInput("height", "height", 300),
          
          style = "material-circle", size = "lg", right = TRUE, status = "default",
          icon = icon("gear"), width = "300px",
          
          tooltip = tooltipOptions(title = "Format graphs."),
          inputId = "dropdownheight"
          
        ), # Close Drop Down Button
        br(),
        
        tags$head(
          tags$style(HTML("
                  #download1 {
                    background: #fff;
                    color: #212070;
                    padding: 8px 15px;
                    font-size: 24px;
                    font-family: inherit;
                    height: 54px;
                    width: 54px;
                    line-height: 44px;
                    outline: none;
                    box-shadow: 0 2px 5px 0 rgba(0,0,0,.18), 0 1px 5px 0 rgba(0,0,0,.15);
                    border-radius: 50%;
                    border-color: transparent;}"))),
        
        actionButton("download1",
                     label = icon("download")),
        
        bsTooltip("download1", "Download (PNG) current tab.",
                  "right", options = list(container = "body")
        )
        
      ), # Close Conditional Panel
      
      # Info Button for Unique Patients Tab ----------
      conditionalPanel(
        condition = "input.sbm == 'uniqueAll' | input.sbm == 'uniqueOffice' | input.sbm == 'uniqueTreatment'",
        br(),
        dropdown(
          box(
            title = NULL,
            width = 50,
            height = "470px",
            solidHeader = FALSE,
            h3("TOTAL SYSTEM UNIQUE PATIENTS:"),h4("Total count of unique patients who had >= 1 visit(s) at any MSHS site (unique MRN by system)."),
            h3("TOTAL UNIQUE PATIENTS BY SITE:"),h4("Total count of unique patients who had >= 1 visit(s) at respective site (unique MRN by site)."),
            h3("SYSTEM UNIQUE PATINETS OVER TIME:"),h4("TOTAL SYSTEM UNIQUE PATIENTS by month."),
            h3("SYSTEM UNIQUE PATIENTS BY MONTH:"),h4("Total count of unique patients who had had >= 1 visit(s) at any MSHS site within respective month (unique MRN by system and month)"),
            h3("UNIQUE PATIENTS BY SITE OVER TIME:"),h4("TOTAL UNIQUE PATIENTS BY SITE by month"),
            h3("UNIQUE PATIENTS BY SITE BY MONTH:"),h4("Total count of unique patients who had had >= 1 visit(s) at respective site and month (unique MRN by site and month).")
          ),
          
          style = "material-circle", size = "lg", right = TRUE, status = "default",
          icon = icon("info"), width = "1200px",
          
          tooltip = tooltipOptions(title = "Click for addition info on the unique patient analysis."),
          inputId = "dropdownUnique"
          
        ) # Close Drop Down Button
      ), # Close Conditional Panel
      
      
      # Info Button for Zip Code Analysis Tab ----------
      conditionalPanel(
        condition = "input.sbm == 'zipCode'",
        br(),
        dropdown(
          box(
            title = NULL,
            width = 50,
            height = "600px",
            solidHeader = FALSE,
            fluidRow(column(7,tableOutput("zipCode_ref_tb1")), column(5,tableOutput("zipCode_ref_tb2")))
          ),
          
          style = "material-circle", size = "lg", right = TRUE, status = "default",
          icon = icon("info"), width = "850px",
          
          tooltip = tooltipOptions(title = "Click for zip code mapping info."),
          inputId = "dropdownZipCode"
          
        ) # Close Drop Down Button
      ) # Close Conditional Panel
      
      
      
    ) #Close Fluid
  ) # Close Dashboard Body
)# Close Dashboard Page

#shinyApp(ui, server)

