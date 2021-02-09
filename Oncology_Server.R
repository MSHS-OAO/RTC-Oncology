server <- function(input, output, session) {
# Observe Events ------------------------------------------------------------------------------------------------------    
  observeEvent(input$selectedCampus,{
    updatePickerInput(session,
                      inputId = "selectedSpecialty",
                      choices = NULL
    )},
    ignoreInit = TRUE)
  
  observeEvent(c(input$selectedCampus,input$selectedSpecialty),{
    updatePickerInput(session,
                      inputId = "selectedDepartment",
                      choices = NULL
    )},
    ignoreInit = TRUE)
  
  observeEvent(c(input$selectedCampus,input$selectedSpecialty, input$selectedDepartment),{
    updatePickerInput(session,
                      inputId = "selectedvisitype",
                      choices = NULL
    )},
    ignoreInit = TRUE)
  
  observeEvent(c(input$selectedCampus,input$selectedSpecialty, input$selectedDepartment,
                    input$selectedvisitype),{
    updatePickerInput(session,
                      inputId = "selectedprovider",
                      choices = NULL
    )},
    ignoreInit = TRUE)
  
  observeEvent(c(input$selectedCampus,input$selectedSpecialty, input$selectedDepartment,
                 input$selectedvisitype, input$selectedprovider),{
                   updatePickerInput(session,
                                     inputId = "selectedrefprovider",
                                     choices = NULL
                   )},
               ignoreInit = TRUE)
  
# Volume Trend Tab ------------------------------------------------------------------------------------------------------    
  output$trend_totalvisitsgraph <- renderPlot({
    
  })
  
  output$trend_totalvisitstable <- function() {
    
  }
  
  output$trend_examvisitsgraph <- renderPlot({
    
  })
  
  output$trend_examvisitstable <- function() {
    
  }
  
  output$trend_treatmentvisitsgraph <- renderPlot({
    
  })
  
  output$trend_treatmentvisitstable <- function() {
    
  }
  
  output$trend_labvisitsgraph <- renderPlot({
    
  })
  
  output$trend_labvisitstable <- function() {
    
  }
# Volume Breakdown Tab ------------------------------------------------------------------------------------------------------       
  
# Volume Comparison Tab ------------------------------------------------------------------------------------------------------       
  
  
}

shinyApp(ui, server)
