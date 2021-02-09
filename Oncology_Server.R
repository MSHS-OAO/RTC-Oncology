server <- function(input, output, session) {
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
  
}

shinyApp(ui, server)
