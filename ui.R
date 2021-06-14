library(shinydashboardPlus)
library(shinycssloaders)
library(shinyBS)
library(shinyscreenshot)

# # Remove Provider ID from Provider Name column
# amb_df_groupings_unique$PROV_NAME_WID <- trimws(gsub("\\[.*?\\]", "", amb_df_groupings_unique$PROV_NAME_WID))

### Set default values for master filters --------------------------------------------------------------------------------------

arrivedDisease.data <- historical.data[arrivedDisease.data.rows,]

default_campus <- "DBC"
campus_choices <- sort(unique(historical.data$SITE))

default_departments <- sort(unique(historical.data[historical.data$SITE %in% default_campus, "Department"])) 

default_departments_disease <- sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% default_campus, "Department"])) 

default_visitType <- sort(unique(historical.data[historical.data$SITE %in% default_campus &
                                                   historical.data$Department %in% default_departments, "AssociationListA"]))

default_ApptType <- sort(unique(historical.data[historical.data$SITE %in% default_campus &
                                                  historical.data$Department %in% default_departments &
                                                  historical.data$AssociationListA %in% default_visitType, "AssociationListB"]))

default_TreatmentType <- sort(unique(historical.data[historical.data$SITE %in% default_campus &
                                                       historical.data$Department %in% default_departments &
                                                       historical.data$AssociationListA %in% default_visitType &
                                                       historical.data$AssociationListB %in% default_ApptType, "AssociationListT"]))

default_disease_group <- sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% default_campus &
                                                           arrivedDisease.data$Department %in% default_departments_disease, "Disease_Group"]))

default_provider <- sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% default_campus &
                                                      arrivedDisease.data$Department %in% default_departments_disease &
                                                      arrivedDisease.data$Disease_Group %in% default_disease_group[1], "Provider"]))

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
                br(),
                # tags$div("Overview",
                #          style= "
                #  font-size: 20px;
                #  text-align: center;
                #  margin: 0;
                #  background: rgba(255, 255, 255, 0);
                #  color: #FFFFFF"),
                # tags$hr(style="border-color: #FFFFFF; margin-top: 10px;"),
                # br(),
                menuItem("Home", tabName = "homepage", icon= icon("home")),
                br(),
                tags$div("Outpatient Visits",
                         style= "
                 font-size: 20px;
                 text-align: center;
                 margin: 0;
                 background: rgba(255, 255, 255, 0);
                 color: #FFFFFF"),
                tags$hr(style="border-color: #FFFFFF; margin-top: 10px;"),
                menuItem("Volume", tabName = "volume", icon = icon("chart-bar"),
                         menuItem("By Site", tabName = "siteVolume",
                                  menuSubItem("Trend", tabName = "volumetrend"),
                                  menuSubItem("Breakdown", tabName = "volumebreakdown"),
                                  menuSubItem("Comparison", tabName = "volumecomparison")),
                         menuItem("By Provider", tabName = "providerVolume",
                                  menuSubItem("Breakdown", tabName = "provvolbreakdown"))
                ),
                menuItem("Unique Patients", tabName = "uniquePts", icon = icon("hospital-user"),
                         menuItem("By Site", tabName = "siteUnique", 
                                  menuSubItem("All", tabName = "uniqueAll"),
                                  menuSubItem("Exam", tabName = "uniqueOffice"),
                                  menuSubItem("Treatment", tabName = "uniqueTreatment")),
                         menuItem("By Provider", tabName = "provUnique", 
                                  menuSubItem("Exam", tabName = "provUniqueExam")),
                         menuItem("By Zip Code", tabName = "zipCode")
                ),
                menuItem("Scheduling", tabName = "scheduling", icon = icon("hospital-user"),
                         menuItem("Scheduled/Arrived", tabName = "schedulingArrived"),
                         menuItem("No Shows/Overbooks", tabName = "schedulingNoShows"),
                         menuItem("Bumps/Cancellations", tabName = "schedulingBumps")
                )
                # menuItem("Access", tabName = "access", icon = icon("hospital-user"),
                #          menuItem("Booked/Filled Rates", tabName = "accessBooked")
                # ),
                # br(),
                # tags$div("Surgical Cases",
                #          style= "
                #  font-size: 20px;
                #  text-align: center;
                #  margin: 0;
                #  background: rgba(255, 255, 255, 0);
                #  color: #FFFFFF"),
                # tags$hr(style="border-color: #FFFFFF; margin-top: 10px;")
                
                
                
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
        # HomePage ------------------------------------------------------------------------------------------------------------------
        tabItem(tabName = "homepage",
                column(12,
                       div("About Oncology Analytics Tool", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       
                       tags$div( id = "home_text",
                                 HTML("<p>Version: 1.0 <br> Last Updated: 5/12/2021</p>")
                       ),
                       tags$head(tags$style("#home_text{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 15px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), 
                       column(12,
                              tags$div(id = "home_description",
                                       h3("Description"),
                                       p("This is a centralized analytics tool that inlcudes all necessary KPIs and metrics that will
                                 allow the users to identify operatinal oppoutunities, make data-driven decisions, and
                                 track improvements", style = "font-size:16px")
                              )
                       ),
                       column(12,
                              tags$div(id = "home_data",
                                       h3("Data Sources"),
                                       p("The data used in this dashboard is pulled from the Clarity database using the slot and access datatables, named CRREPORT_REP.Y_DM_BOOKED_FILLED_RATE and CRREPORT_REP.MV_DM_PATIENT_ACCESS respectively.
                                      The master file including the department, PRC, and disease group mappings as well as the LOS excludions used in this analytics tool can be downloaded from the first hyperlink, while the zip code groupings file can be downloaded at the second link.",
                                         style = "font-size:16px"),
                                       a(href = "Mappings/Oncology Master Mapping File - Updated 4.22.2021.xlsx",target='blank', 'Oncology Master Mapping File', download = 'Oncology Master Mappings.xlsx', style = "font-size:16px"),
                                       br(),
                                       a(href = "Mappings/Oncology System Data - Zip Code Groupings 4.13.2021.csv",target='blank', 'Zip Code Groupings', download = 'Zip Code Groupings.csv', style = "font-size:16px")
                              ))
                       # column(12,
                       #        tags$div(id = "home_usage",
                       #                 h3("Usage"), 
                       #                 #img(src = "homepage.png", width = "500px"),
                       #                 br(),
                       #                 p("Section 1 contains the sidebar menu where all the different tabs are listed"),
                       #                 p("Section 2 includes the name of the tab currently being looked at as well as the date ranges that the tab is showing"),
                       #                 p("Section 3 included the filter menu, it is hiearchical meaning the filter choices are based off the previously selected choices. The filters effect all the the outputs in the tab.  Below the filter dropdown menu is the download button which allows the user to save all the graphs and tables on the currently viewed tab as a PNG.")
                       #                 
                       #        ))
                )),
        # Volume Trend Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "volumetrend",
                div("Volume Trend - Site", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_volumetrend"),
                tags$head(tags$style("#practiceName_volumetrend{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
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
                         br(),
                         fluidRow(
                           column(4,
                                  box(
                                    title = "Select Visit Type:",
                                    width = 12,
                                    height = "110px",
                                    solidHeader = FALSE, 
                                    radioGroupButtons(
                                      inputId = "annualVolSummary",
                                      label = NULL,
                                      choices =c("Total", "Exam", "Treatment", "Labs"),
                                      selected = c("Total"),
                                      justified = TRUE,
                                      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                    ))
                           )), br(),
                         fluidRow(
                           column(12, offset = 1,
                                  tableOutput("trend_visitstable"))
                         ))
                )
        ),
        # Volume Breakdown Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "volumebreakdown",
                div("Volume Breakdown - Site", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_volumebreakdown"),
                tags$head(tags$style("#practiceName_volumebreakdown{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
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
                       )
                       
                )
        ), #Close Volume breakdown tab
        
        # Volume Comparison Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "volumecomparison",
                div("Volume Comparison - Site", style = "color: #221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_volumecomparison"),
                tags$head(tags$style("#practiceName_volumecomparison{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Analysis Customization", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         fluidRow(
                           box(
                             title = "Select Visit Type:",
                             width = 4,
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
                                         selected = default_visitType)),
                           box(
                             title = "Select Appointment Type:",
                             width = 4,
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
                                         selected = default_ApptType)),
                           box(
                             title = "Select Treatment Type:",
                             width = 4,
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
                                         selected = default_TreatmentType))
                         ),
                         fluidRow(
                           box(
                             title = "Compare by:",
                             width = 4,
                             height = "110px",
                             solidHeader = FALSE,
                             awesomeRadio(
                               inputId = "comp_choices",
                               label = NULL,
                               choices = c("All", "Site"),
                               selected = "All",
                               status = "info")),
                           box(
                             title = "Analysis by:",
                             width = 4,
                             height = "110px",
                             solidHeader = FALSE,
                             awesomeRadio(
                               inputId = "analysis_type",
                               label = NULL,
                               choices = c("Monthly", "Weekly"),
                               selected = "Monthly",
                               status = "info"))
                         )
                       )),
                column(11,
                       boxPlus(
                         title = "Visit Volume Comparison", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("volumeCompTotal_grh", height = "auto") %>%
                           withSpinner(type = 5, color = "#d80b8c"), hr(),
                         plotOutput("volumeCompTrend_grh", height = "auto") %>%
                           withSpinner(type = 5, color = "#d80b8c"))
                ) #Close column
        ), # Close volume Comparison
        
        # Provider Volume Breakdown Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "provvolbreakdown",
                div("Volume Breakdown - Provider", style = "color: #221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_provvolbreakdown"),
                tags$head(tags$style("#practiceName_provvolbreakdown{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Analysis Customization", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         fluidRow(
                           column(4,
                                  box(
                                    title = "Select Provider Group:",
                                    width = 12,
                                    height = "100px",
                                    solidHeader = FALSE,
                                    pickerInput("selectedDisease",label=NULL,
                                                choices = default_disease_group,
                                                multiple=TRUE,
                                                options = pickerOptions(
                                                  liveSearch = TRUE,
                                                  actionsBox = TRUE,
                                                  selectedTextFormat = "count > 1",
                                                  countSelectedText = "{0}/{1} Disease Groups",
                                                  dropupAuto = FALSE,
                                                  size = 10),
                                                selected = default_disease_group[1]))),
                           column(4,
                                  box(
                                    title = "Select Provider:",
                                    width = 12,
                                    height = "100px",
                                    solidHeader = FALSE,
                                    pickerInput("selectedProvider",label=NULL,
                                                choices = default_provider,
                                                multiple=TRUE,
                                                options = pickerOptions(
                                                  liveSearch = TRUE,
                                                  actionsBox = TRUE,
                                                  selectedTextFormat = "count > 1",
                                                  countSelectedText = "{0}/{1} Providers",
                                                  dropupAuto = FALSE,
                                                  size = 10),
                                                selected = default_provider[1])))
                           
                         )),
                       boxPlus(
                         title = "Physician Visits Breakdown", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         tableOutput("provVolumeExam_tb") %>%
                           withSpinner(type = 5, color = "#d80b8c")
                       )
                       # boxPlus(
                       #   title = "Telehealth Visits Breakdown", width = 12, status = "primary",
                       #   solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                       #   tableOutput("provVolumeTele_tb") %>%
                       #     withSpinner(type = 5, color = "#d80b8c")
                       # )
                )
        ), # Close volume Comparison
        
        # Unique Patients Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "uniqueAll",
                div("Unique Patients - Exam, Treatment, and Lab Visits", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_uniqueAll"),
                tags$head(tags$style("#practiceName_uniqueAll{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Unique Patients by System", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         fluidRow(valueBoxOutput("uniqueAllSystem", width=4) %>%
                                    withSpinner(type = 5, color = "#d80b8c")), hr(),
                         column(12,
                                plotOutput("uniqueAllTrendSystem", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueAllMonthSystem", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c")) 
                       ), 
                       boxPlus(
                         title = "Unique Patients by Site", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(), 
                         column(12,
                                plotOutput("uniqueAllSite", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueAllTrendSite", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueAllMonthSite", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))
                       )
                )
        ), #Close Unique Patients - All tab
        
        tabItem(tabName = "uniqueOffice",
                div("Unique Patients - Exam Visits", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_uniqueOffice"),
                tags$head(tags$style("#practiceName_uniqueOffice{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Unique Patients by System", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         fluidRow(valueBoxOutput("uniqueOfficeSystem", width=4) %>%
                                    withSpinner(type = 5, color = "#d80b8c")), hr(),
                         column(12,
                                plotOutput("uniqueOfficeTrendSystem", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueOfficeMonthSystem", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c")) 
                       ), 
                       boxPlus(
                         title = "Unique Patients by Site", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(), 
                         column(12,
                                plotOutput("uniqueOfficeSite", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueOfficeTrendSite", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueOfficeMonthSite", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))
                       )
                )
        ), #Close Unique Patients - Exam tab
        
        tabItem(tabName = "uniqueTreatment",
                div("Unique Patients - Exam Visits", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_uniqueTreatment"),
                tags$head(tags$style("#practiceName_uniqueTreatment{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Unique Patients by System", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         fluidRow(valueBoxOutput("uniqueTreatmentSystem", width=4) %>%
                                    withSpinner(type = 5, color = "#d80b8c")), hr(),
                         column(12,
                                plotOutput("uniqueTreatmentTrendSystem", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueTreatmentMonthSystem", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c")) 
                       ), 
                       boxPlus(
                         title = "Unique Patients by Site", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(), 
                         column(12,
                                plotOutput("uniqueTreatmentSite", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueTreatmentTrendSite", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueTreatmentMonthSite", height = "auto") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))
                       )
                )
        ), #Close Unique Patients - Treatment tab
        
        # Unique Patients by Provider - Exam Visits
        tabItem(tabName = "provUniqueExam",
                div("Unique Patients by Provider - Exam Visits", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_provUniqueExam"),
                tags$head(tags$style("#practiceName_provUniqueExam{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Analysis Customization", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         fluidRow(
                           column(4,
                                  box(
                                    title = "Select Disease Group:",
                                    width = 12,
                                    height = "100px",
                                    solidHeader = FALSE,
                                    pickerInput("selectedDisease2",label=NULL,
                                                choices = default_disease_group,
                                                multiple=TRUE,
                                                options = pickerOptions(
                                                  liveSearch = TRUE,
                                                  actionsBox = TRUE,
                                                  selectedTextFormat = "count > 1",
                                                  countSelectedText = "{0}/{1} Disease Groups",
                                                  dropupAuto = FALSE,
                                                  size = 10),
                                                selected = default_disease_group[1]))),
                           column(4,
                                  box(
                                    title = "Select Provider:",
                                    width = 12,
                                    height = "100px",
                                    solidHeader = FALSE,
                                    pickerInput("selectedProvider2",label=NULL,
                                                choices = default_provider,
                                                multiple=TRUE,
                                                options = pickerOptions(
                                                  liveSearch = TRUE,
                                                  actionsBox = TRUE,
                                                  selectedTextFormat = "count > 1",
                                                  countSelectedText = "{0}/{1} Providers",
                                                  dropupAuto = FALSE,
                                                  size = 10),
                                                selected = default_provider[1])))
                           
                         )),
                       boxPlus(
                         title = "Unique Patients by Provider over Time", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         tableOutput(outputId = "uniqueProvExam_tb") %>%
                           withSpinner(type = 5, color = "#d80b8c")),
                       boxPlus(
                         title = "Unique Patients by Provider and Month", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         tableOutput(outputId ="uniqueProvMonthExam_tb") %>%
                           withSpinner(type = 5, color = "#d80b8c"))
                )
                
        ), #Close Unique Patients by Provider - Exam Visits tab
        
        
        # Unique Patients by Zip Code Tab 
        tabItem(tabName = "zipCode",
                div("Patient Zip Code Analysis", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_zipCode"),
                tags$head(tags$style("#practiceName_zipCode{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Total Unique Patients by Zip Code", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         fluidRow(
                           column(4,
                                  box(
                                    title = "Unique Patients by:",
                                    width = 12,
                                    height = "150px",
                                    solidHeader = FALSE, 
                                    radioButtons("selectedZipCodeMap", label=NULL,
                                                 choices = c("System","Site"),
                                                 selected = "System"))
                           ),
                           column(4,
                                  box(
                                    title = "Visit Type(s) Included:",
                                    width = 12,
                                    height = "150px",
                                    solidHeader = FALSE, 
                                    radioButtons("selectedZipCodeMap2", label=NULL,
                                                 choices = c("All","Exam","Treatment"),
                                                 selected = "All"))
                           )
                         ), hr(),
                         fluidRow(
                           column(4, tableOutput("zipCode_tb") %>%
                                    withSpinner(type = 5, color = "#d80b8c")),
                           column(8, leafletOutput("zipCode_map", height = "700px") %>%
                                    withSpinner(type = 5, color = "#d80b8c")))
                       )
                )
        ), # Close Zip Code tab
        
        
        # Scheduling Tab ---------------------------------------------------------------------------------------------------------
        ## Scheduled/Arrived Tab 
        tabItem(tabName = "schedulingArrived",
                div("Scheduling - Scheduled/Arrived", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_schedulingArrived"),
                tags$head(tags$style("#practiceName_schedulingArrived{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Scheduling Summary", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         valueBoxOutput("totalScheduled", width = 3) %>%
                           withSpinner(type = 5, color = "#d80b8c"),
                         valueBoxOutput("avgScheduled", width = 3) %>%
                           withSpinner(type = 5, color = "#d80b8c"),
                         valueBoxOutput("avgArrived", width = 3) %>%
                           withSpinner(type = 5, color = "#d80b8c"),
                         valueBoxOutput("avgNoShows", width = 3) %>%
                           withSpinner(type = 5, color = "#d80b8c")),
                       boxPlus(
                         title = "Patient Arrivals", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("avgPtArrival")  %>%
                           withSpinner(type = 5, color = "#d80b8c"), hr(),
                         plotOutput("ptArrivalPattern", height = "700px") %>%
                           withSpinner(type = 5, color = "#d80b8c")))
                
        ), # Close Scheduled/Arrived Tab
        
        ## No Shows/Overbooks Tab 
        tabItem(tabName = "schedulingNoShows",
                div("Scheduling - No Shows/Overbooks", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_schedulingNoShow"),
                tags$head(tags$style("#practiceName_schedulingNoShow{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                column(6,
                       boxPlus(
                         title = "No Shows", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         fluidRow(
                           valueBoxOutput("avgNoShows2", width = 6) %>%
                             withSpinner(type = 5, color = "#d80b8c"),
                           valueBoxOutput("avgNoShowsPerc", width = 6) %>%
                             withSpinner(type = 5, color = "#d80b8c")), hr(),
                         fluidRow(
                           materialSwitch(
                           inputId = "percent1",
                           label = "Percent (%)", 
                           right = TRUE,
                           status = "primary")),
                         plotOutput("avgNoShowsDist", height = "600px") %>%
                           withSpinner(type = 5, color = "#d80b8c"))),
                column(6,
                       boxPlus(
                         title = "Overbooks", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, 
                         fluidRow(
                           valueBoxOutput("avgOverbooks", width = 6) %>%
                           withSpinner(type = 5, color = "#d80b8c"),
                           valueBoxOutput("avgOverbooksProv", width = 6) %>%
                           withSpinner(type = 5, color = "#d80b8c")), hr(),
                         br(), br(), 
                         plotOutput("avgOverbooksDist", height = "600px") %>%
                           withSpinner(type = 5, color = "#d80b8c")))
                )
                
        ), # Close No Shows/Overbooks Tab
        
        ## Bumps/Cancellations Tab 
        tabItem(tabName = "schedulingBumps",
                div("Scheduling - Bumps/Cancellations", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_schedulingBump"),
                tags$head(tags$style("#practiceName_schedulingBump{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Summary", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         fluidRow(
                           valueBoxOutput("avgDailyBumps", width = 4) %>%
                             withSpinner(type = 5, color = "#d80b8c"),
                           valueBoxOutput("avgDailyCanc", width = 4) %>%
                             withSpinner(type = 5, color = "#d80b8c"),
                           valueBoxOutput("avgDailyResc", width = 4) %>%
                             withSpinner(type = 5, color = "#d80b8c")), hr(),
                         column(4, 
                                plotOutput("avgBumpsCancRescRate", height = "450px") %>%
                                  withSpinner(type = 5, color = "#d80b8c")),
                         column(4,
                                plotOutput("leadDaysBumpsCancResc", height = "450px") %>%
                                  withSpinner(type = 5, color = "#d80b8c")),
                         column(4,
                                plotOutput("sameDayBumpedCanceledRescheduled", height = "450px") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))),
                       boxPlus(
                         title = "Top Reasons to Bumps and Cancellations", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         fluidRow(
                           materialSwitch(
                             inputId = "percent2",
                             label = "Percent (%)", 
                             right = TRUE,
                             status = "primary")), br(),
                         column(6, 
                                plotOutput("reasonsBumps", height = "600px") %>%
                                  withSpinner(type = 5, color = "#d80b8c")),
                         column(6, 
                                plotOutput("reasonsCanc", height = "600px") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))
                         
                       )
                )
                
        ) # Close Bumps/Cancellations Tab
        

        
        # Slot Usage Tab ---------------------------------------------------------------------------------------------------------
        
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
        input.sbm == `provvolbreakdown` | input.sbm == `schedulingArrived` | input.sbm == `schedulingNoShows` | input.sbm == `schedulingBumps` |
        input.sbm == `accessBooked` | 
        input.sbm == 'uniqueAll' | input.sbm == 'uniqueOffice' | input.sbm == 'uniqueTreatment' | input.sbm == 'provUniqueExam' |
        input.sbm == 'zipCode'",
        
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
                          dropupAuto = FALSE,
                          size = 10),
                        selected = default_campus)),
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
                          dropupAuto = FALSE,
                          size = 10),
                        selected = default_departments
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
            # fluidRow(
            #   column(12, offset = 2,
            #          radioGroupButtons(
            #            inputId = "dateRangePreset",
            #            #label = "Choices", 
            #            choices = c("3M", "6M", "1Y"),
            #            #status = "primary"
            #            selected = character(0)
            #          )
            #   )
            # )
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
        condition = "input.sbm == 'volumetrend' | input.sbm == 'volumebreakdown' | input.sbm == 'volumecomparison' |
         input.sbm == `provvolbreakdown` | input.sbm == `schedulingArrived` | input.sbm == `schedulingNoShows` | input.sbm == `schedulingBumps` | 
         input.sbm == `accessBooked` |  
         input.sbm == 'uniqueAll' | input.sbm == 'uniqueOffice' | input.sbm == 'uniqueTreatment' | input.sbm == 'provUniqueExam' |
         input.sbm == 'zipCode'",
        br(),
        dropdown(
          box(
            title = "Change Width:",
            width = 12,
            height = "150px",
            solidHeader = FALSE,
            sliderInput(
              inputId = "plotWidth",
              label = NULL, 
              value = 1500, min = 200, max = 2000,
              ticks = FALSE
            ),
            fluidRow(
              column(2, offset = 4,
                     actionButton("resetwidth", "Reset")
              )
            )
          ),
          box(
            title = "Change Height:",
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
            height = "600px",
            solidHeader = FALSE,
            h3("TOTAL SYSTEM UNIQUE PATIENTS:"),h4("Total count of unique patients who had >= 1 visit(s) at any MSHS site (unique MRN by system)."),
            h3("SYSTEM UNIQUE PATINETS OVER TIME:"),h4("TOTAL SYSTEM UNIQUE PATIENTS by month."),
            h3("SYSTEM UNIQUE PATIENTS BY MONTH:"),h4("Total count of unique patients who had >= 1 visit(s) at any MSHS site within respective month (unique MRN by system and month)."),
            h3("TOTAL UNIQUE PATIENTS BY SITE:"),h4("Total count of unique patients who had >= 1 visit(s) at respective site (unique MRN by site)."),
            h3("UNIQUE PATIENTS BY SITE OVER TIME:"),h4("TOTAL UNIQUE PATIENTS BY SITE by month."),
            h3("UNIQUE PATIENTS BY SITE AND MONTH:"),h4("Total count of unique patients who had >= 1 visit(s) at respective site and month (unique MRN by site and month)."),
            h3("UNIQUE PATIENTS BY SITE BY SITE, MONTH, AND PROVIDER/REFERRING PROVIDER:"),h4("Total count of unique patients who had >= 1 visit(s) at respective site and month with respective provider/referring provider (unique MRN by site, month, and provider/referring provider).")
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

