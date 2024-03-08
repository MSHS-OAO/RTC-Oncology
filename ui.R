ui <- dashboardPage(

  header,
  
  # Sidebar ------------------------------------------------------------------------------------------------------
  dashboardSidebar(
    tags$head(tags$style(HTML(".dt-button{
  border: none;
  color: black;
  padding: 2px 10px;
  text-align: center;
  margin: 4px 2px;
  border: 1px solid black;
  background-color: white;
}"))),
    
    
    
    
    tags$head(tags$style(HTML("#save_filters {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute;
                                                left: 25px}"))), 
    tags$head(tags$style(HTML("#update_filters0 {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute;
                                                left: 25px}"))),
    tags$head(tags$style(HTML("#remove_filters {background-color: #221f72;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute;
                                                left: 25px}"))),
    
    tags$head(tags$style(HTML("#dropdownbutton4 {color: #fff;
                                                   background-color: #212070;
                                                   box-shadow: none;
                                                }"))),
    tags$head(tags$style(HTML("#dropdownbutton3 {color: #fff;
                                                   background-color: #212070;
                                                   box-shadow: none;
                                                }"))),
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
                menuItem("Unique Patients", tabName = "uniquePts", icon = icon("hospital-user"),
                         menuItem("By Site", tabName = "SystemUnique",
                                  #menuSubItem("All", tabName = "systemuniqueAll"),
                                  menuSubItem("Treatment & Exam", tabName = "systemuniqueOffice")#,
                                  #menuSubItem("Treatment", tabName = "systemuniqueTreatment")
                                  ),
                         # menuItem("By Site", tabName = "siteUnique",
                         #          menuSubItem("All", tabName = "uniqueAll"),
                         #          menuSubItem("Exam", tabName = "uniqueOffice"),
                         #          menuSubItem("Treatment", tabName = "uniqueTreatment")),
                         menuItem("By Provider", tabName = "provUnique",
                                  menuSubItem("Exam", tabName = "provUniqueExam"))#,
                         #menuItem("By Zip Code", tabName = "zipCode")
                ),
                menuItem("Volume", tabName = "volume", icon = icon("chart-bar"),
                         menuItem("By Site", tabName = "siteVolume",
                                  menuSubItem("Trend", tabName = "volumetrend"),
                                  menuSubItem("Breakdown", tabName = "volumebreakdown"),
                                  menuSubItem("Comparison", tabName = "volumecomparison")
                                  ),
                         menuItem("By Provider", tabName = "providerVolume",
                                  menuSubItem("Breakdown", tabName = "provvolbreakdown")
                                  ),
                         menuItem("By Referring Provider", tabName = "referringproVol",
                                  menuSubItem("Treatment", tabName = "provvoltreatment")
                                  )
                ),
                menuItem("Access", tabName = "access", icon= icon("plus-circle")),
                menuItem("Scheduling", tabName = "scheduling", icon = icon("calendar-day"),
                          menuItem("No Show", tabName = "no_show")),
                menuItem("Equity", tabName = "equity_tab", icon = icon("balance-scale"),
                #   style = "background: url('www/scale-balanced-solid.svg');
                #    background-size: contain;
                # background-position: center;
                # background-repeat: no-repeat;
                # height: 32px;
                # width: 32px;
                # display: block;"
                # ),
                            menuItem("Ethnicity/Race Capture", tabName = "ethnicity_and_race"),
                            menuItem("MyChart Activation", tabName = "my_chart_activation")
                ),
                menuItem("Utilization", tabName = "util", icon = icon("percent"),
                         #menuItem("Exam Utilization", tabName = "utilization"),
                         #menuItem("Provider Utilization", tabName = "prov_util"),
                         menuItem("Treatment Utilization", tabName = "treat_util")
                ),
                uiOutput("data_download")
                #menuItem("Data Download", tabName = "download", icon = icon("download")
                         #)                
                # menuItem("Access", tabName = "access", icon = icon("calendar-alt"),
                #          menuItem("Booked and Filled", tabName = "bookedFilled")
                #)
                
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
                
                
                
    ), # Close sidebarMenu
    
    tags$head(tags$style(HTML("
    #data_download {
    padding: 12px 5px 12px 15px;
    
    }
                    ")))
  ), # Close Dashboard Sidebar
  dashboardBody(
    
    tags$head(tags$style(HTML("
    .logo {
    image-rendering: -webkit-optimize-contrast;
    
    }
                    "))),
    tags$script('
    document.getElementById("save_filters").onclick = function() {
      var text_dept = $("[data-id=\\"selectedDepartment\\"]").attr("title");
      var text_diagnosis = $("[data-id=\\"diag_grouper\\"]").attr("title");
      Shiny.onInputChange("dept_text", text_dept);
      Shiny.onInputChange("diagnosis_text", text_diagnosis);
    };
  '),
    # 
    # box "status" color for Mount Sinai Grey
    tags$style(HTML("
    .box.box-solid.box-warning>.box-header {
    color:#000000;
    background:#e5e6e5
    }
    .box.box-solid.box-warning{
    border-bottom-color:#e5e6e5;
    border-left-color:#e5e6e5;
    border-right-color:#e5e6e5;
    border-top-color:#e5e6e5;
    }
                    ")),
    
    tags$head(tags$style(
      HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
    )),
    
    tags$head(tags$style(
      HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
    )),
    
    
    # tags$head(tags$style(
    #   HTML('.nsewdrag.drag {outline:1px solid black;}')
    # )),
    
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
        tabItem(tabName = "download",
                div("Data Download", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                column (11,
                  boxPlus(
                    title = "Volume Data", width = 12, status = "primary",
                    solidHeader = TRUE,
                    downloadButton("download1",""),
                    DTOutput("volume_data_tbl") %>% 
                      withSpinner(type = 5, color = "#d80b8c")
                  )
                )
              ),
        # HomePage ------------------------------------------------------------------------------------------------------------------
        tabItem(tabName = "homepage",
                column(12,
                       # setBackgroundImage(
                       #   src = "MS_RGB_Vrtl_test.png", shinydashboard = TRUE
                       # ),
                       div("Oncology Analytics Tool", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$div(id = "home_text",
                                HTML("<p>Version: 1.0 <br> Last Updated: 10/06/2023</p>")
                       ),
                       tags$head(tags$style("#home_text{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 15px; margin-top: -0.2em; margin-bottom: -4em; margin-left: 20px}")), 
                       br(), br(),
                       tags$image(src = "Sinai_logo_color.png", height = "200px", width = "300px", alt = "Something went wrong", deleteFiles = FALSE),
                       fluidRow(
                         column(4,
                                box(
                                  title = p("About this Tool", style = "font-size:34px; font-weight:bold"), width = 12,  height = "400px", status = "warning", solidHeader = TRUE,
                                  p("- Oncology Analytics Tool is a system-wide analytics tool that 
                                aims to provide analytical solutions to help", strong("drive data-driven decisions and operational improvements."), style = "font-size:22px"),
                                  p("- Oncology Analytics Tool provides", strong("current state assessment and historical trending"), "of key operational metrics.", style = "font-size:22px")
                                )),
                         
                         column(4,
                                tags$div(id = "home_data",
                                         box(
                                           title = p("Data Sources", style = "font-size:34px; font-weight:bold"), width = 12, height = "400px", status = "warning", solidHeader = TRUE,
                                           p("Oncology Analytics Tool is developed based on the following data from EPIC Clarity:", style = "font-size:22px; font-weight: bold"),
                                           p("1. ", strong("Scheduling Data"), " provides scheduling details on arrived appointments.", style = "font-size:22px"),
                                           p("2. ", strong("Race/Ethnicity Data"), " provides capture analysis and MyChart activation breakdown.", style = "font-size:22px"),
                                           # p("2. ", strong("Slot Availability Data"), " provides slot level details inlcuding booked and filled hours and slots.", style = "font-size:22px"),
                                         ))),
                         
                         column(4,
                                tags$div(id = "home_mapping",
                                         box(
                                           title = p("Reference Files", style = "font-size:34px; font-weight:bold"), width = 12, height = "400px", status = "warning", solidHeader = TRUE,
                                           p("- Mapping File:", style = "font-size:22px; font-weight: bold"),
                                           a(href = "https://hso-rconnect.mssm.edu/oncology-mapping",target='blank', 'Click to View', style = "font-size:22px"),
                                         )))
                       ),
                       
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
                         plotlyOutput("trend_totalvisitsgraph", height = "auto") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Exam Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotlyOutput("trend_examvisitsgraph", height = "auto") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Treatment Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotlyOutput("trend_treatmentvisitsgraph", height = "auto") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Lab Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotlyOutput("trend_labvisitsgraph", height = "auto") %>% 
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
        tabItem(tabName = "access",
                div("Access", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_access"),
                tags$head(tags$style("#practiceName_access{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
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
                             pickerInput("selectedVisitType_access",label=NULL,
                                         choices=default_visitType,
                                         multiple=TRUE,
                                         options = pickerOptions(
                                           liveSearch = TRUE,
                                           actionsBox = TRUE,
                                           selectedTextFormat = "count > 1",
                                           countSelectedText = "{0}/{1} Visit Types",
                                           dropupAuto = FALSE),
                                         selected = default_visitType)),
                           column(5,
                                  br(),
                                  br(),
                                  br(),
                                  actionButton("update_filters_access", "CLICK TO UPDATE", width = "75%"),
                                  br(),
                                  br()
                           ))),
                       boxPlus(
                         title = "Monthly Patient Wait Time", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotlyOutput("patient_wait_time", height = "auto") %>% 
                           withSpinner(type = 5, color = "#d80b8c"),
                         tableOutput("wait_time_provider_breakdown") %>%
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
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
                         plotlyOutput("break_totalvisitsgraph", height = "auto") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Exam Visits", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotlyOutput("break_examvisitsgraph", height = "auto") %>% 
                           withSpinner(type = 5, color = "#d80b8c")
                       ),
                       boxPlus(
                         title = "Treatment Visits", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotlyOutput("break_treatmentvisitsgraph", height = "auto") %>% 
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
                             title = "Select Visit Type Detail:",
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
                                           countSelectedText = "{0}/{1} Visit Detail Types",
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
                                           countSelectedText = "{0}/{1} Treatment Types",
                                           dropupAuto = FALSE),
                                         selected = default_TreatmentType)),
                           
                           column(5,
                                  actionButton("update_filters6", "CLICK TO UPDATE", width = "75%"),
                                  br(),
                                  br()
                           )
                         )
                       )),
                
                column(11,
                       boxPlus(
                         title = "Visit Volume Comparison", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
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
                               status = "info"))#,
                           # box(
                           #   title = "Analysis by:",
                           #   width = 4,
                           #   height = "110px",
                           #   solidHeader = FALSE,
                           #   awesomeRadio(
                           #     inputId = "analysis_type",
                           #     label = NULL,
                           #     choices = c("Monthly", "Weekly"),
                           #     selected = "Monthly",
                           #     status = "info"))
                         ),
                         plotlyOutput("volumeCompTotal_grh", height = "auto") %>%
                           withSpinner(type = 5, color = "#d80b8c"), hr(),
                         plotlyOutput("volumeCompTrend_grh", height = "auto") %>%
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
                           box(
                             title = "Select Disease Group:",
                             width = 3,
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
                                         selected = default_disease_group)),
                           box(
                             title = "Select Disease Group Detail:",
                             width = 3,
                             height = "100px",
                             solidHeader = FALSE,
                             pickerInput("selectedDiseaseDetail",label=NULL,
                                         choices = default_disease_group_detail,
                                         multiple=TRUE,
                                         options = pickerOptions(
                                           liveSearch = TRUE,
                                           actionsBox = TRUE,
                                           selectedTextFormat = "count > 1",
                                           countSelectedText = "{0}/{1} Disease Detail Groups",
                                           dropupAuto = FALSE,
                                           size = 10),
                                         selected = default_disease_group_detail)),
                           box(
                             title = "Select Provider Type:",
                             width = 3,
                             height = "100px",
                             solidHeader = FALSE,
                             pickerInput("provider_type_volume",label=NULL,
                                         choices = provider_type_choices,
                                         multiple=TRUE,
                                         options = pickerOptions(
                                           liveSearch = TRUE,
                                           actionsBox = TRUE,
                                           selectedTextFormat = "count > 1",
                                           countSelectedText = "{0}/{1} Provider Types",
                                           dropupAuto = FALSE,
                                           size = 10),
                                         selected = provider_type_choices)),
                           box(
                             title = "Select Provider:",
                             width = 3,
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
                                         selected = default_provider)),
                           column(5,
                                  actionButton("update_filters_exam_prov", "CLICK TO UPDATE", width = "75%"),
                                  br(),
                                  br()
                           )
                         )
                       )),
                column(11,
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
        
        
        # Provider Volume Breakdown Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "provvoltreatment",
                div("Treatment Volume - Referring Provider", style = "color: #221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_provvoltreatment"),
                tags$head(tags$style("#practiceName_provvoltreatment{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Analysis Customization", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         fluidRow(
                           box(
                             title = "Select Provider Type:",
                             width = 3,
                             height = "100px",
                             solidHeader = FALSE,
                             pickerInput("referring_provider_treatment_type",label=NULL,
                                         choices = provider_type_choices,
                                         multiple=TRUE,
                                         options = pickerOptions(
                                           liveSearch = TRUE,
                                           actionsBox = TRUE,
                                           selectedTextFormat = "count > 1",
                                           countSelectedText = "{0}/{1} Provider Types",
                                           dropupAuto = FALSE,
                                           size = 10),
                                         selected = provider_type_choices)),
                           box(
                             title = "Select Referring Provider:",
                             width = 3,
                             height = "100px",
                             solidHeader = FALSE,
                             pickerInput("selected_referring_provider_treatment",label=NULL,
                                         choices = default_referring_provider,
                                         multiple=TRUE,
                                         options = pickerOptions(
                                           liveSearch = TRUE,
                                           actionsBox = TRUE,
                                           selectedTextFormat = "count > 1",
                                           countSelectedText = "{0}/{1} Providers",
                                           dropupAuto = FALSE,
                                           size = 10),
                                         selected = default_referring_provider)),
                           column(4,
                                  br(),
                                  br(),
                                  br(),
                                  actionButton("update_filters_treatment", "CLICK TO UPDATE", width = "75%"),
                                  br(),
                                  br()
                           )
                         )
                       )),
                column(11,
                       # boxPlus(
                       #   title = "Physician Treatment Visits", width = 12, status = "primary", 
                       #   solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                       #   # plotlyOutput("provVolumeTreatment_grph", height = "auto") %>%
                       #   #   withSpinner(type = 5, color = "#d80b8c"), hr(),
                       #   tableOutput("provVolumeTreatment_tb") %>%
                       #     withSpinner(type = 5, color = "#d80b8c")
                       # ),
                       boxPlus(
                         title = "Referring Physician Treatment Visits", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         plotlyOutput("provVolumeTreatmentReferring_grph") %>%
                           withSpinner(type = 5, color = "#d80b8c"), hr(),
                         tableOutput("provVolumeTreatmentReferring_tb") %>%
                           withSpinner(type = 5, color = "#d80b8c")
                       )
                )
        ), # Close volume Comparison

        tabItem(tabName = "systemuniqueOffice",
                div("Site Unique Patients", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_systemuniqueOffice"),
                tags$head(tags$style("#practiceName_systemuniqueOffice{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Metirc Definitions", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         column(6,
                                box(
                                  title = p("Total Site Unique Patients", style = "font-size:28px; font-weight:bold"), width = 12,  height = "150px", status = "warning", solidHeader = TRUE,
                                  p("Total count of unique patients who had >= 1 visit(s) at an MSHS site (unique MRN by system).", style = "font-size:22px")
                                )),
                         column(6,
                                box(
                                  title = p("Site Unique Patients by Month", style = "font-size:28px; font-weight:bold"), width = 12,  height = "150px", status = "warning", solidHeader = TRUE,
                                  p("Total count of unique patients who had >= 1 visit(s) at an MSHS site within respective month (unique MRN by system and month).", style = "font-size:22px")
                                )
                                )
                       ),
                       boxPlus(
                         title = "Exam Unique Patients by Site", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         fluidRow(valueBoxOutput("uniqueOfficeSystem", width=4) %>%
                                    withSpinner(type = 5, color = "#d80b8c")
                                  ), hr(),
                         column(12,
                                plotlyOutput("uniqueOfficeMonthSystem") %>%
                                  withSpinner(type = 5, color = "#d80b8c")) 
                       ), 
                       
                       boxPlus(
                         title = "Treatment Unique Patients by Site", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         fluidRow(valueBoxOutput("uniqueTreatmentSystem", width=4) %>%
                                    withSpinner(type = 5, color = "#d80b8c")), hr(),
                         column(12,
                                plotlyOutput("uniqueTreatmentMonthSystem") %>%
                                  withSpinner(type = 5, color = "#d80b8c")) 
                       ),
                       
                )
        ), #Close Unique Patients - Exam tab
        
        # tabItem(tabName = "systemuniqueTreatment",
        #         div("System Unique Patients - Treatment Visits", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
        #         textOutput("practiceName_systemuniqueTreatment"),
        #         tags$head(tags$style("#practiceName_systemuniqueTreatment{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
        #         column(11,
        #                boxPlus(
        #                  title = "Unique Patients by System", width = 12, status = "primary", 
        #                  solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
        #                  br(),
        #                  fluidRow(valueBoxOutput("uniqueTreatmentSystem", width=4) %>%
        #                             withSpinner(type = 5, color = "#d80b8c")), hr(),
        #                  column(12,
        #                         plotOutput("uniqueTreatmentTrendSystem") %>%
        #                           withSpinner(type = 5, color = "#d80b8c"), hr(),
        #                         plotOutput("uniqueTreatmentMonthSystem") %>%
        #                           withSpinner(type = 5, color = "#d80b8c")) 
        #                ), 
        #                
        #         )
        # ), #Close Ystsem Unique Patients - Treatment tab
        
        
        
        # Unique Patients Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "uniqueAll",
                div("Site Unique Patients - Exam, Treatment, and Lab Visits", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_uniqueAll"),
                tags$head(tags$style("#practiceName_uniqueAll{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
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
        ), #Close Unique Patients - Exam tab
        
        tabItem(tabName = "uniqueOffice",
                div("Site Unique Patients - Exam Visits", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_uniqueOffice"),
                tags$head(tags$style("#practiceName_uniqueOffice{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Unique Patients by Site", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(), 
                         column(12,
                                plotOutput("uniqueOfficeSite") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueOfficeTrendSite") %>%
                                  withSpinner(type = 5, color = "#d80b8c"), hr(),
                                plotOutput("uniqueOfficeMonthSite") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))
                       )
                )
        ), #Close Unique Patients - Lab tab
        
        tabItem(tabName = "uniqueTreatment",
                div("Site Unique Patients - Treatment Visits", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                textOutput("practiceName_uniqueTreatment"),
                tags$head(tags$style("#practiceName_uniqueTreatment{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
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
                           box(
                             title = "Select Provider Type:",
                             width = 3,
                             height = "100px",
                             solidHeader = FALSE,
                             pickerInput("provider_type_disease",label=NULL,
                                         choices = provider_type_choices,
                                         multiple=TRUE,
                                         options = pickerOptions(
                                           liveSearch = TRUE,
                                           actionsBox = TRUE,
                                           selectedTextFormat = "count > 1",
                                           countSelectedText = "{0}/{1} Provider Types",
                                           dropupAuto = FALSE,
                                           size = 10),
                                         selected = provider_type_choices)),
                           box(
                             title = "Select Provider:",
                             width = 3,
                             height = "100px",
                             solidHeader = FALSE,
                             pickerInput("selectedProvider2",label=NULL,
                                         choices = default_provider_unique_exam,
                                         multiple=TRUE,
                                         options = pickerOptions(
                                           liveSearch = TRUE,
                                           actionsBox = TRUE,
                                           selectedTextFormat = "count > 1",
                                           countSelectedText = "{0}/{1} Providers",
                                           dropupAuto = FALSE,
                                           size = 10),
                                         selected = default_provider_unique_exam)),
                           column(3,
                                  br(),
                                  br(),
                                  br(),
                                  actionButton("update_filters2", "CLICK TO UPDATE", width = "75%"),
                                  br(),
                                  br()
                           )
                         )
                       ),
                       boxPlus(
                         title = "Unique Patients by Provider and Month", width = 12, status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         tableOutput(outputId ="uniqueProvMonthExam_tb") %>%
                           withSpinner(type = 5, color = "#d80b8c"))
                )
                
        ), #Close Unique Patients by Provider - Exam Visits tab
        tabItem(tabName = "no_show",
                div("No Show", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                column(11,
                       boxPlus(
                         title = "Metric Definition", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         column(3),
                         column(7,
                                box(
                                  title = p("No Show", style = "font-size:28px; font-weight:bold"), width = 12,  height = "125px", status = "warning", solidHeader = TRUE,
                                  p("No Show % = (No Show + Same-day Canceled/Rescheduled) / (Arrived + No Show + Same-day Canceled/Reshceduled)", style = "font-size:22px")
                                )
                         )
                       ),
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
                                    pickerInput("selectedVisitType_no_show",label=NULL,
                                                choices=default_visitType,
                                                multiple=TRUE,
                                                options = pickerOptions(
                                                  liveSearch = TRUE,
                                                  actionsBox = TRUE,
                                                  selectedTextFormat = "count > 1",
                                                  countSelectedText = "{0}/{1} Visit Types",
                                                  dropupAuto = FALSE),
                                                selected = default_visitType)),
                                  # box(
                                  #   title = "Select Disease Group:",
                                  #   width = 4,
                                  #   height = "100px",
                                  #   solidHeader = FALSE,
                                  #   pickerInput("selectedDiseaseGroup_no_show",label=NULL,
                                  #               choices=default_disease_group,
                                  #               multiple=TRUE,
                                  #               options = pickerOptions(
                                  #                 liveSearch = TRUE,
                                  #                 actionsBox = TRUE,
                                  #                 selectedTextFormat = "count > 1",
                                  #                 countSelectedText = "{0}/{1} Disease Groups",
                                  #                 dropupAuto = FALSE),
                                  #               selected = default_disease_group)),
                                  # box(
                                  #   title = "Select Disease Group Detail:",
                                  #   width = 4,
                                  #   height = "100px",
                                  #   solidHeader = FALSE,
                                  #   pickerInput("selectedDiseaseGroupDetail_no_show",label=NULL,
                                  #               choices=default_disease_group_detail,
                                  #               multiple=TRUE,
                                  #               options = pickerOptions(
                                  #                 liveSearch = TRUE,
                                  #                 actionsBox = TRUE,
                                  #                 selectedTextFormat = "count > 1",
                                  #                 countSelectedText = "{0}/{1} Disease Group Details",
                                  #                 dropupAuto = FALSE),
                                  #               selected = default_disease_group_detail)),
                                  # 
                                column(5,
                                       br(),
                                       br(),
                                       br(),
                                       actionButton("update_filters_no_show", "CLICK TO UPDATE", width = "75%"),
                                       br(),
                                       br()
                                )))),
                       boxPlus(
                         title = "No Show Summary", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           fluidRow(
                             column(2),
                              column(4, valueBoxOutput("avg_daily_no_show", width = 12) %>%
                                       withSpinner(type = 5, color = "#d80b8c")),
                              column(4, valueBoxOutput("daily_no_show_percent", width = 12) %>%
                                       withSpinner(type = 5, color = "#d80b8c"))
                           )),
                       boxPlus(
                         title = "Monthly No Show", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                          plotlyOutput("monthly_no_show_percent") %>%
                           withSpinner(type = 5, color = "#d80b8c"),
                         tableOutput("no_show_provider_breakdown") %>%
                           withSpinner(type = 5, color = "#d80b8c")
                         )
                         )
                
        ),
        
        tabItem(
          tabName = "ethnicity_and_race",
            div("Ethnicity and Race Capture", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
              column(11,
                     # boxPlus(
                     #   title = "Analysis Customization", width = 12, status = "primary",
                     #   solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                     #   fluidRow(
                     #     box(
                     #       title = "Select Race Grouper",
                     #       width = 4,
                     #       height = "100px",
                     #       solidHeader = FALSE,
                     #       pickerInput("race_grouper",
                     #                   choices = race_grouper_choices,
                     #                   multiple = TRUE,
                     #                   options = pickerOptions(
                     #                     liveSearch = TRUE,
                     #                     actionsBox = TRUE,
                     #                     selectedTextFormat = "count > 1",
                     #                     countSelectedText = "{0}/{1} Selected",
                     #                     dropupAuto = FALSE),
                     #                   selected = race_grouper_choices)
                     #     ),
                     #     br(),
                     #     br(),
                     #     column(3,
                     #            actionButton("update_filters_race", "CLICK TO UPDATE", width = "75%"))
                     #   )
                     # ),
                      boxPlus(
                        title = "Race/Ethnicity Capture Analysis", width = 12, status = "primary",
                        solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                        plotlyOutput("system_ethnicity_and_race_unknown") %>%
                          withSpinner(type = 5, color = "#d80b8c"), hr(),
                          #uiOutput("ethnicity_and_race_unknown") %>%
                          plotlyOutput("ethnicity_and_race_unknown_plots_single") %>%
                          withSpinner(type = 5, color = "#d80b8c"), hr(),
                        plotlyOutput("race_heatmap") %>%
                          withSpinner(type = 5, color = "#d80b8c"), hr(),
                        plotlyOutput("ethnicity_heatmap") %>%
                          withSpinner(type = 5, color = "#d80b8c")
                        
                      ),
                     boxPlus(
                       title = "Race/Ethnicity Breakdown", width = 12, status = "primary",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       plotlyOutput("system_race_unknown_breakdown") %>%
                         withSpinner(type = 5, color = "#d80b8c"), hr(),
                       plotlyOutput("system_ethnicity_unknown_breakdown") %>%
                         withSpinner(type = 5, color = "#d80b8c"), hr(),
                       plotlyOutput("site_race_unknown_breakdown") %>%
                         withSpinner(type = 5, color = "#d80b8c"), hr(),
                       plotlyOutput("site_ethnicity_unknown_breakdown") %>%
                         withSpinner(type = 5, color = "#d80b8c")
                     )
                     )
          
        ),
        
        tabItem(
          tabName = "my_chart_activation",
          div("MyChart Activation", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
          column(11,
                 boxPlus(
                   title = "Analysis Customization", width = 12, status = "primary",
                   solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                   fluidRow(
                     box(
                       title = "Select Race Grouper",
                       width = 4,
                       height = "100px",
                       solidHeader = FALSE,
                       pickerInput("race_grouper_mychart",
                                   choices = race_grouper_choices,
                                   multiple = TRUE,
                                   options = pickerOptions(
                                     liveSearch = TRUE,
                                     actionsBox = TRUE,
                                     selectedTextFormat = "count > 1",
                                     countSelectedText = "{0}/{1} Selected",
                                     dropupAuto = FALSE),
                                   selected = race_grouper_choices)
                     ),
                     
                     box(
                       title = "Select Ethnicity Grouper",
                       width = 4,
                       height = "100px",
                       solidHeader = FALSE,
                       pickerInput("ethnicity_grouper_mychart",
                                   choices = ethnicity_grouper_choices,
                                   multiple = TRUE,
                                   options = pickerOptions(
                                     liveSearch = TRUE,
                                     actionsBox = TRUE,
                                     selectedTextFormat = "count > 1",
                                     countSelectedText = "{0}/{1} Selected",
                                     dropupAuto = FALSE),
                                   selected = ethnicity_grouper_choices)
                     ),
                     
                     br(),
                     br(),
                     column(3,
                            actionButton("update_filters_mychart", "CLICK TO UPDATE", width = "75%"))
                   )
                 ),
                 boxPlus(
                   title = "Metirc Definition", width = 12, status = "primary",
                   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                   br(),
                   column(3),
                   column(6,
                          box(
                            title = p("Overall Grouper", style = "font-size:28px; font-weight:bold"), width = 12,  height = "150px", status = "warning", solidHeader = TRUE,
                            p("Overall is a rollup that contains all patients within the race or ethinicity groupers.", style = "font-size:22px")
                          ))
                 ),

                 boxPlus(
                   title = "System MyChart Activation Analysis", width = 12, status = "primary",
                   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                   plotlyOutput("system_my_chart_activation") %>%
                     withSpinner(type = 5, color = "#d80b8c"),
                   plotlyOutput("system_my_chart_activation_ethnicity") %>%
                     withSpinner(type = 5, color = "#d80b8c")
                   
                 ),
                 boxPlus(
                   title = "Site MyChart Activation Analysis", width = 12, status = "primary",
                   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                   plotlyOutput("site_my_chart_activation") %>%
                     withSpinner(type = 5, color = "#d80b8c"),
                   plotlyOutput("site_my_chart_activation_ethnicity") %>%
                     withSpinner(type = 5, color = "#d80b8c")
                   # plotlyOutput("site_my_chart_activation_white") %>%
                   #   withSpinner(type = 5, color = "#d80b8c") , hr(),
                   #   plotlyOutput("site_my_chart_activation_african_american") %>%
                   #   withSpinner(type = 5, color = "#d80b8c"), hr(),
                   #   plotlyOutput("site_my_chart_activation_asian") %>%
                   #   withSpinner(type = 5, color = "#d80b8c")
                   
                 )
          )
          
        ),

        # Utilization Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "utilization",
                       div("Utilization", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       textOutput("practiceName_utilization"),
                       tags$head(tags$style("#practiceName_utilization{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                       column(11,
                              boxPlus(
                                title = "Utilization Overview", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                br(),
                                column(12,
                                       column(3,
                                              #util_choices <- list("SCHEDULED time and duration" = "scheduled", "ACTUAL time and duration" = "arrived"),
                                              box(title = NULL, width = 12, solidHeader = FALSE,
                                                  radioGroupButtons(
                                                    inputId = "utilType",
                                                    label = h4("Analysis based on:"),
                                                    size = "lg",
                                                    choices = list("SCHEDULED time and duration" = "scheduled", "ACTUAL time and duration" = "actual"),
                                                    # choices = util_choices,
                                                    # selected = util_choices[1],
                                                    checkIcon = list(
                                                      yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                                      no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))),
                                                  hr(),
                                                  h5("SCHEDULED: Utilization of all arrived appointments based on scheduled appointment start and end time."),
                                                  h5("ACTUAL: Utilization of all arrived appointments based on actual appointment start and end time."))),
                                       column(3,
                                              box(title = NULL, width = 12, solidHeader = FALSE,
                                                  sliderInput("setRooms", label = h4("Set Rooms Available:"), min = 1, max = 65, value = 30)),
                                              box(title = NULL, width = 12, solidHeader = FALSE,
                                                  sliderInput("setHours", label = h4("Set Daily Open Hours:"), min = 1, max = 24, value = 8))),
                                       column(6,
                                              valueBoxOutput("roomStat1", width=12) %>%
                                                withSpinner(type = 5, color = "#d80b8c"),
                                              valueBoxOutput("avgUtilization", width=12),
                                              valueBoxOutput("maxUtilization", width=12),
                                              valueBoxOutput("maxRoomsRequired", width=12))
                                )),
                              boxPlus(
                                title = "Space Utilization", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                tabBox(
                                title = NULL,
                                id = "tabset1", width = "100%", height = "auto",
                                tabPanel("Average",
                                           plotlyOutput("spaceUtil", height = "auto") %>% 
                                             withSpinner(type = 5, color = "#d80b8c")
                                  ),
                                  tabPanel("Percentiles",
                                  plotlyOutput("spaceUtilPerc", height = "auto") %>%
                                    withSpinner(type = 5, color = "#d80b8c")
                                  )
                                  )
                                ),
                              boxPlus(
                                title = "Space Required", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                tabBox(
                                  title = NULL,
                                  id = "tabset2", width = "100%", height = "auto",
                                  tabPanel("Average",
                                           plotlyOutput("spaceUsed", height = "auto") %>% 
                                             withSpinner(type = 5, color = "#d80b8c")),
                                  tabPanel("Percentiles",
                                           plotlyOutput("spaceUsedPerc", height = "auto") %>% 
                                             withSpinner(type = 5, color = "#d80b8c"))))
                              
                              
                       )
        ),
        
        tabItem(tabName = "prov_util",
                  div("Provider Utilization", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                  textOutput("practiceName_utilization_provider"),
                  tags$head(tags$style("#practiceName_utilization_provider{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                  column(11,
                         boxPlus(
                           title = "Utilization Overview", width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           br(),
                           column(12,
                                  column(3,
                                         #util_choices <- list("SCHEDULED time and duration" = "scheduled", "ACTUAL time and duration" = "arrived"),
                                         box(title = NULL, width = 12, solidHeader = FALSE,
                                         radioGroupButtons(
                                           inputId = "utilType1",
                                           label = h4("Analysis based on:"),
                                           size = "lg",
                                           choices = list("SCHEDULED time and duration" = "scheduled", "ACTUAL time and duration" = "actual"),
                                           # choices = util_choices,
                                           # selected = util_choices[1],
                                           checkIcon = list(
                                             yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                             no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))),
                                         hr(),
                                         h5("SCHEDULED: Utilization of all arrived appointments based on scheduled appointment start and end time."),
                                         h5("ACTUAL: Utilization of all arrived appointments based on actual appointment start and end time."),
                                         ),
                                  ),
                                  column(3,
                                         box(title = NULL, width = 12, solidHeader = FALSE,
                                             sliderInput("setHours_provider", label = h4("Set Daily Open Hours:"), min = 1, max = 24, value = 8))),
                                  column(6,
                                         # valueBoxOutput("roomStat1", width=12) %>%
                                         #   withSpinner(type = 5, color = "#d80b8c"),
                                         valueBoxOutput("avgUtilization_provider", width=12),
                                         valueBoxOutput("maxUtilization_provider", width=12),
                                         # valueBoxOutput("maxRoomsRequired", width=12))
                                  )
                           
                           )
                         )
                         )
                ),
        
        tabItem(tabName = "treat_util",
                  div("Treatment Utilization", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                  textOutput("practiceName_utilization_treatment"),
                  tags$head(tags$style("#practiceName_utilization_treatment{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                column(11,
                       boxPlus(
                         title = "Utilization Overview", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                       column(3,
                              box(title = NULL, width = 12, solidHeader = FALSE,
                                sliderInput("setRooms_treatment", label = h4("Treatment Spaces Available:"), min = 1, max = 65, value = 30),
                                #sliderInput("setNurse", label = h4("Treatment Spaces per Nurse:"), min = 1, max = 10, value = 5),
                                sliderInput("setHours_treatment", label = h4("Set Daily Open Hours:"), min = 1, max = 24, value = 8, step = 0.5),
                               # selectInput("daysOfWeek_treatment",label = NULL,
                               #             choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), selected = daysOfWeek.options,
                               #             multiple=TRUE, selectize=TRUE),
                               pickerInput("operating_hours_start", label = NULL, 
                                           choices = operating_hours_choices),
                               pickerInput("operating_hours_end", label = NULL, 
                                           choices = operating_hours_choices,
                                           selected = "6:00PM")
                              )
                       ),
                       column(2,
                              rHandsontableOutput("treatment_input_table")
                              ),
                       column(7,
                              box(
                                title = p("How to Use", style = "font-size:34px; font-weight:bold"), width = 12,  height = "300px", status = "warning", solidHeader = TRUE,
                                p(strong("Step 1."),"Please select the treatment spaces available and the number of open daily hours", style = "font-size:22px"),
                                p(strong("Step 2."), "Set the open and close times", style = "font-size:22px"),
                                p(strong("Step 3."), "Enter the number of nurses available for each hour in the table", style = "font-size:22px"),
                                p(strong("Step 4."), "Use the filters on the right to enter the campus, department, and time frame the data should reflect, then click on the Click to Update button", style = "font-size:22px")
                              ))
                       ),
                       boxPlus(
                         title = "Treatment Space Utilization", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         column(3),
                         column(6,
                                box(
                                  title = p("Metric Definition", style = "font-size:28px; font-weight:bold"), width = 12,  height = "150px", status = "warning", solidHeader = TRUE,
                                  p("Total Duration of arrived Tx volume in minutes over Time Available (# of days in month patients were treated x Infusion Treatment", style = "font-size:22px")
                                )),
                         column(6,
                            tableOutput("treatment_space_util_month")
                         ),
                         column(6,
                                tableOutput("treatment_space_util_dayofweek")
                         )
                       ),
                       boxPlus(
                         title = "Treatment Nurse Capacity Utilization", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         column(3),
                         column(6,
                                box(
                                  title = p("Metric Definition", style = "font-size:28px; font-weight:bold"), width = 12,  height = "175px", status = "warning", solidHeader = TRUE,
                                  p("Total Duration of arrived Tx volume in minutes over Nurse Capacity (Nurses staffed by hour of the day x Chairs per Nurse x # of days in month patients were treated", style = "font-size:22px")
                                )),
                         column(6,
                                tableOutput("treatment_nurse_util_month")
                         ),
                         column(6,
                                tableOutput("treatment_nurse_util_dayofweek")
                         )
                       ),
                       boxPlus(
                         title = "Effective Infusion Capacity Utilization", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         column(3),
                         column(6,
                                box(
                                  title = p("Metric Definition", style = "font-size:28px; font-weight:bold"), width = 12,  height = "200px", status = "warning", solidHeader = TRUE,
                                  p("Calculates our 2 limitations of Nurses vs chairs available by hours of the day for total daily count, then x # of days in month patients were treated for the Effective Infusion Capacity, then divded over the Total Duration of arrived Tx volume in minutes.", style = "font-size:22px")
                                )),
                         column(6,
                                tableOutput("infusion_util_month")
                         ),
                         column(6,
                                tableOutput("infusion_util_dayofweek")
                         )
                       )
                     )
                       
              )
        
        
        # Slot Usage Tab ---------------------------------------------------------------------------------------------------------
        
      ), # Close Main tab Items
      
      
      # Formatting Dropdown Buttons

      tags$head(tags$style(HTML(".ht_master .wtHolder {
                                overflow: hidden;
                           }"))),
      tags$head(tags$style(HTML("#dropdownbutton {color: #212070;}"))),
      tags$head(tags$style(HTML("#dropdownbutton1 {color: #212070;}"))),
      tags$head(tags$style(HTML("#dropdownheight {color: #212070;}"))),
      tags$head(tags$style(HTML("#dropdownheight {color: #212070;}"))),
      tags$head(tags$style(HTML("#dropdownUnique {color: #d80b8c;}"))),
      tags$head(tags$style(HTML("#dropdown_treatment_utilization {color: #d80b8c;}"))),
      tags$head(tags$style(HTML("#dropdownUnique1 {color: #d80b8c;}"))),
      tags$head(tags$style(HTML("#dropdownZipCode {color: #d80b8c;}"))),
      tags$head(tags$style(HTML("#update_filters {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute;
                                                left: 25px;
                                                top: 53px;
                                                height 85%}"))),
      tags$head(tags$style(HTML("#update_filters1 {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}"))),
      tags$head(tags$style(HTML("#update_filters_exam_prov {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}"))),
      tags$head(tags$style(HTML("#update_filters_treatment {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}"))),
      tags$head(tags$style(HTML("#update_filters_no_show {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}"))),
      tags$head(tags$style(HTML("#update_filters_access {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}"))),
      tags$head(tags$style(HTML("#update_filters2 {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}"))),
      tags$head(tags$style(HTML("#update_filters3 {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}"))),
      tags$head(tags$style(HTML("#update_filters4 {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}"))),
      tags$head(tags$style(HTML("#update_filters5 {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}"))),
      tags$head(tags$style(HTML("#update_filters6 {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}}"))),
      tags$head(tags$style(HTML("#update_filters7 {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}}"))),
      tags$head(tags$style(HTML("#update_filters_mychart {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}}"))),
      
      tags$head(tags$style(HTML("#update_filters_race {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px;
                                                position: absolute}}"))),
      
      # Conditional Filters ------------------------------------------------------------------------------------------------------  
      
      conditionalPanel(
        condition = "input.sbm == 'volumetrend' | input.sbm == 'volumebreakdown' | input.sbm == 'volumecomparison' | 
        input.sbm == `provvolbreakdown` |
        input.sbm == `bookedFilled` | 
        input.sbm == 'uniqueAll' | input.sbm == 'uniqueOffice' | input.sbm == 'uniqueTreatment' | input.sbm == 'provUniqueExam' |
        input.sbm == 'systemuniqueOffice' | input.sbm == 'systemuniqueTreatment' |
        input.sbm == 'zipCode' | input.sbm == 'utilization' | input.sbm == 'treat_util' | input.sbm == 'prov_util' | input.sbm == 'download' | input.sbm == 'ethnicity_and_race' | input.sbm == 'my_chart_activation' | input.sbm == 'provvoltreatment' |
        input.sbm == 'no_show' | input.sbm == 'access'",
        column(1,
          dropdown(
            br(),
            actionButton("update_filters", "CLICK TO UPDATE", width = "80%"),
            br(),
            br(),
            br(),
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
            conditionalPanel(
              condition = "input.sbm == 'volumetrend' | input.sbm == 'volumebreakdown' | input.sbm == 'volumecomparison' | 
                            input.sbm == 'provvolbreakdown' | input.sbm == 'systemuniqueOffice' | input.sbm == 'systemuniqueTreatment' |
                   input.sbm == 'uniqueAll' | input.sbm == 'uniqueOffice' | input.sbm == 'uniqueTreatment' | input.sbm == 'provUniqueExam' | input.sbm == 'donwload' | input.sbm == 'ethnicity_and_race' | input.sbm == 'my_chart_activation' | input.sbm == 'provvoltreatment' |
                  input.sbm == 'no_show' | input.sbm == 'access'",
              box(
                title = "Select Diagnosis Grouper:",
                width = 12,
                height = "100px",
                solidHeader = FALSE,
                pickerInput("diag_grouper",label=NULL,
                            choices = default_diag_grouper,
                            multiple=TRUE,
                            options = pickerOptions(
                              liveSearch = TRUE,
                              actionsBox = TRUE,
                              selectedTextFormat = "count > 1",
                              countSelectedText = "{0}/{1} Groups",
                              dropupAuto = FALSE,
                              size = 10),
                            selected = default_diag_grouper
                )
              ),
            ),
            conditionalPanel(
                condition = "input.sbm == 'utilization' | input.sbm == 'prov_util'",
                box(
                  title = "Select Provider:",
                  width = 12,
                  height = "100px",
                  solidHeader = FALSE,
                  pickerInput("selectedProviderUtil",label=NULL,
                              choices=default_provider_utilization,
                              multiple=TRUE,
                              options = pickerOptions(
                                liveSearch = TRUE,
                                actionsBox = TRUE,
                                selectedTextFormat = "count > 1",
                                countSelectedText = "{0}/{1} Providers",
                                dropupAuto = FALSE,
                                size = 10),
                              selected = default_provider_utilization
                  )
                )
            ),
            conditionalPanel(
              condition = "input.sbm == 'volumebreakdown' | input.sbm == 'volumecomparison' | 
          input.sbm == `provvolbreakdown` |
          input.sbm == `bookedFilled` | input.sbm == 'provUniqueExam' |
          input.sbm == 'zipCode' | input.sbm == 'volumetrend' | input.sbm == 'systemuniqueOffice' | input.sbm == 'systemuniqueTreatment' |
                input.sbm == 'uniqueAll' | input.sbm == 'uniqueOffice' | input.sbm == 'uniqueTreatment' | input.sbm == 'download' | input.sbm == 'ethnicity_and_race' | input.sbm == 'my_chart_activation' | input.sbm == 'provvoltreatment' |
                input.sbm == 'no_show' | input.sbm == 'access'" ,                box(
                  title = "Select Date Range:", 
                  width = 12, 
                  height = "100px",
                  solidHeader = FALSE, 
                  dateRangeInput("dateRange", label = NULL,
                                 start = dateRangetrend_start, end = dateRange_max,
                                 min = dateRangetrend_min, max = dateRange_max
                  )
              )
            ),
            conditionalPanel(
              condition = "input.sbm == 'treat_util'",
              box(
                title = "Select Date Range:",
                width = 12, 
                height = "100px",
                solidHeader = FALSE, 
                dateRangeInput("dateRangetreat_util", label = NULL,
                               start = dateRangetrend_start, end = dateRange_max,
                               min = dateRangetrend_min, max = dateRange_max
                )
              )
            ),
            
            # conditionalPanel(
            #   condition = "input.sbm=='utilization' | input.sbm == 'prov_util'",
            #   box(
            #     title = "Select Date Range:",
            #     width = 12, 
            #     solidHeader = FALSE, 
            #     dateRangeInput("dateRangeUtil", label = NULL,
            #                    start = util_date_start, end = util_date_end,
            #                    min = util_date_min, max = util_date_end)),
            # ),
            
            # conditionalPanel(
            #   condition = "input.sbm == 'systemuniqueOffice' | input.sbm == 'systemuniqueTreatment' |
            #        input.sbm == 'uniqueAll' | input.sbm == 'uniqueOffice' | input.sbm == 'uniqueTreatment'",
            #   box(
            #     title = "Select Date Range:",
            #     width = 12,
            #     height = "100px",
            #     solidHeader = FALSE,
            #     dateRangeInput("dateRangeunique", label = NULL,
            #                    start = dateRange_min_default_unique, end = unique_max,
            #                    min = unique_min, max = unique_max
            #     )
            #   )
            # ),
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
                          selected = NULL
              )
            ),
            
            style = "material-circle", size = "lg", right = TRUE, status = "default",
            icon = icon("filter"), width = "300px",
            tooltip = tooltipOptions(title = "Set filters for graphs/tables."),
            inputId = "dropdownbutton"
            
          ) # Close Drop Down Button
      ),
      br(),
      br(),
      br()
      ),# Close conditional Panel
      
      conditionalPanel(
        condition = "input.sbm == 'volumetrend' | input.sbm == 'volumebreakdown' | input.sbm == 'volumecomparison' |
         input.sbm == `provvolbreakdown` | 
         input.sbm == `bookedFilled` |  
         input.sbm == 'uniqueAll' | input.sbm == 'uniqueOffice' | input.sbm == 'uniqueTreatment' | input.sbm == 'provUniqueExam' |
         input.sbm == 'systemuniqueOffice' | input.sbm == 'systemuniqueTreatment' |
         input.sbm == 'zipCode'",
        br(),
        # dropdown(
        # conditionalPanel(condition = "input.sbm == 'uniqueAll'",
        #   box(
        #     title = "Change Width:",
        #     width = 12,
        #     height = "150px",
        #     solidHeader = FALSE,
        #     sliderInput(
        #       inputId = "plotWidth",
        #       label = NULL, 
        #       value = 1000, min = 200, max = 2000,
        #       ticks = FALSE
        #     ),
        #     fluidRow(
        #       column(2, offset = 4,
        #              actionButton("resetwidth", "Reset")
        #       )
        #     )
        #   )
        # ),
        #   box(
        #     title = "Change Height:",
        #     width = 12,
        #     height = "150px",
        #     solidHeader = FALSE,
        #     sliderInput(
        #       inputId = "plotHeight",
        #       label = NULL, 
        #       value = 650, min = 450, max = 2000,
        #       ticks = FALSE
        #     ),
        #     fluidRow(
        #       column(2, offset = 4,
        #              actionButton("resetheight", "Reset")
        #       )
        #     )
        #   ),
        #   
        #   
        #   
        #   # numericInput("height", "height", 300),
        #   
        #   style = "material-circle", size = "lg", right = TRUE, status = "default",
        #   icon = icon("gear"), width = "300px",
        #   
        #   tooltip = tooltipOptions(title = "Format graphs."),
        #   inputId = "dropdownheight"
        #   
        # ), # Close Drop Down Button
        br(),
        
        tags$head(
          tags$style(HTML("
                  #download10 {
                    background: #212070;
                    color: #fff;
                    padding: 8px 15px;
                    font-size: 24px;
                    font-family: inherit;
                    height: 54px;
                    width: 54px;
                    line-height: 44px;
                    outline: none;
                    box-shadow: none;
                    border-radius: 50%;
                    border-color: transparent;}"))),
        
        # bsTooltip("dropdownbutton3", "Load previously saved bookmarks.",
        #           "left", options = list(container = "body")
        # ),
        # bsTooltip("dropdownbutton4", "Bookmark global filters.",
        #           "left", options = list(container = "body")
        # ),
        
        
        bsTooltip("download10", "Download (PNG) current tab.",
                  "right", options = list(container = "body")
        ),
        
        tags$head(tags$style(HTML("#save_filters19 {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px}")))
        
      ), # Close Conditional Panel
      
      
      # Info Button for Site Unique Patients Tab ----------
      conditionalPanel(
        condition = "input.sbm == 'uniqueAll' | input.sbm == 'uniqueOffice' | input.sbm == 'uniqueTreatment'",
        br(),
        dropdown(
          box(
            title = NULL,
            width = 20,
            height = "600px",
            solidHeader = FALSE,
            h3("TOTAL UNIQUE PATIENTS BY SITE:"),h4("Total count of unique patients who had >= 1 visit(s) at respective site (unique MRN by site)."),
            h3("UNIQUE PATIENTS BY SITE OVER TIME:"),h4("TOTAL UNIQUE PATIENTS BY SITE by month."),
            h3("UNIQUE PATIENTS BY SITE AND MONTH:"),h4("Total count of unique patients who had >= 1 visit(s) at respective site and month (unique MRN by site and month)."),
            h3("UNIQUE PATIENTS BY SITE, MONTH, AND PROVIDER/REFERRING PROVIDER:"),h4("Total count of unique patients who had >= 1 visit(s) at respective site and month with respective provider/referring provider (unique MRN by site, month, and provider/referring provider).")
          ),
          
          style = "material-circle", size = "lg", right = TRUE, status = "default",
          icon = icon("info"), width = "600px",
          
          tooltip = tooltipOptions(title = "Click for additional info on the site unique patient analysis."),
          inputId = "dropdownUnique"
          
        ) # Close Drop Down Button
      ), # Close Conditional Panel
      
      
      # Info Button for System Unique Patients Tab ----------
      # conditionalPanel(
      #   condition = "input.sbm == 'systemuniqueOffice' | input.sbm == 'systemuniqueTreatment'",
      #   br(),
      #   dropdown(
      #     box(
      #       title = NULL,
      #       width = 20,
      #       height = "600px",
      #       solidHeader = FALSE,
      #       h3("TOTAL SYSTEM UNIQUE PATIENTS:"),h4("Total count of unique patients who had >= 1 visit(s) at any MSHS site (unique MRN by system)."),
      #       # h3("SYSTEM UNIQUE PATIENTS OVER TIME:"),h4("TOTAL SYSTEM UNIQUE PATIENTS by month."),
      #       h3("SYSTEM UNIQUE PATIENTS BY MONTH:"),h4("Total count of unique patients who had >= 1 visit(s) at any MSHS site within respective month (unique MRN by system and month).")
      #     ),
      #     
      #     style = "material-circle", size = "lg", right = TRUE, status = "default",
      #     icon = icon("info"), width = "600px",
      #     
      #     tooltip = tooltipOptions(title = "Click for additional info on the system unique patient analysis."),
      #     inputId = "dropdownUnique1"
      #     
      #   ) # Close Drop Down Button
      # ), # Close Conditional Panel
      
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
          inputId = "Calculates our 2 limitations of Nurses vs chairs available by hours of the day for total daily  count, then x # of days in month pateints were treated  for the Effective Infusion Capacity, then divded over the Total Duration of arrived Tx volume in minutes."
          
        ) # Close Drop Down Button
      ), # Close Conditional Panel
      
      # conditionalPanel(
      #   condition = "input.sbm == 'treat_util'",
      #   br(),
      #     column(1,
      #     dropdown(
      #       box(
      #         title = NULL,
      #         width = 20,
      #         height = "600px",
      #         solidHeader = FALSE,
      #         h3("Treatment Space Utilization:"),h4("Total Duration of arrived Tx volume in minutes over Time Available (# of days in month patients were treated x Infusion Treatment"),
      #         h3("Nurse Capacity Utilization:"),h4("Total Duration of arrived Tx volume in minutes over Nurse Capacity  (Nurses staffed by hour of the day x Chairs per Nurse x # of days in month patients were treated"),
      #         h3("Effective Infusion Capacity Utilization:"),h4("Calculates our 2 limitations of Nurses vs chairs available by hours of the day for total daily  count, then x # of days in month patients were treated  for the Effective Infusion Capacity, then divded over the Total Duration of arrived Tx volume in minutes.")),
      #       
      #       style = "material-circle", size = "lg", right = TRUE, status = "default",
      #       icon = icon("info"), width = "600px",
      #       
      #       tooltip = tooltipOptions(title = "Click for additional info on the utilization analysis."),
      #       inputId = "dropdown_treatment_utilization"
      #       
      #     ) # Close Drop Down Button
      #   )
      # ), # Close Conditional Panel
      conditionalPanel(
        condition = "input.sbm == 'volumetrend'",
        br(),
        #download_btn_ui("volumetrend_download")
        uiOutput("volumetrend_download"),
        # downloadButton("volumetrend_download1")
      ), # Close Conditional Panel
      conditionalPanel(
        condition = "input.sbm == 'treat_util'",
        br(),
        #download_btn_ui("volumetrend_download")
        uiOutput("treat_util_download"),
        # downloadButton("volumetrend_download1")
      ) # Close Conditional Panel
      
    ) #Close Fluid
  ) # Close Dashboard Body
)# Close Dashboard Page

#shinyApp(ui, server)
