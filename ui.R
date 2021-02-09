ui <- dashboardPage(
  dashboardHeader(title = "Oncology Analytics Tool",
                  titleWidth = 250),
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
                         menuSubItem("Comparison", tabName = "volumecomparison"))
      
    ) # Close sidebarMenu
  ), # Close Dashboard Sidebar
  dashboardBody(
    conditionalPanel(
      condition = "input.sbm == 'volumetrend' | input.sbm == 'volumebreakdown' |
        input.sbm == 'volumecomparison'", 
      column(2,
             box(
               title = "Select Campus:",
               width = 12,
               height = "100px",
               solidHeader = FALSE,
               pickerInput("selectedCampus",label=NULL,
                           choices=sort(unique(historical.data$Campus)),
                           multiple=TRUE,
                           options = pickerOptions(
                             liveSearch = TRUE,
                             actionsBox = TRUE,
                             selectedTextFormat = "count > 1", 
                             countSelectedText = "{0}/{1} Campuses", 
                             dropupAuto = FALSE),
                           selected = "MSUS")),
             )# Close column
      
    )# Close conditional Panel
    
  ) # Close Dashboard Body
)# Close Dashboard Page