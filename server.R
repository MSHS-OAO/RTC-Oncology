server <- function(input, output, session) {
  # Date Range Header --------------------------------------------------------------------------
  output$practiceName_volumetrend <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
    })
  
  output$practiceName_volumebreakdown <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_volumecomparison <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_provvolbreakdown <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  
  output$practiceName_systemuniqueAll <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_systemuniqueOffice <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_systemuniqueTreatment <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_uniqueAll <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_uniqueOffice <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_uniqueTreatment <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_provUniqueExam <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2])})
  
  output$practiceName_zipCode <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_schedulingArrived <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_schedulingNoShows <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_schedulingBumps <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  # Observe Events ------------------------------------------------------------------------------------------------------    
  observeEvent(input$download1, {
    screenshot(filename = "Oncology Dashboard")
  })
  
  observeEvent(input$resetwidth, {
    updateSliderInput(session,"plotWidth",value = 1000)
    
  })
  
  observeEvent(input$resetheight, {
    updateSliderInput(session,"plotHeight",value = 650)
    
  })
  
  observeEvent(input$dateRangePreset, {
    if(input$dateRangePreset == "3M"){
      updateDateRangeInput(session,"dateRange",start = dateRange_max %m+% months(-3), end = dateRange_max)
    }
    
    if(input$dateRangePreset == "6M"){
      updateDateRangeInput(session,"dateRange",start = dateRange_max %m+% months(-6), end = dateRange_max)
    }
    if(input$dateRangePreset == "1Y"){
      updateDateRangeInput(session,"dateRange",start = dateRange_max %m+% months(-12), end = dateRange_max)
    }
    
  })
  
  observeEvent(input$selectedCampus,{
    department_choices <- sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus, "Department"]))
    updatePickerInput(session,
                      inputId = "selectedDepartment",
                      choices = department_choices,
                      selected = department_choices
    )
    
    visit_type_choices <- sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                        historical.data$Department %in% department_choices, "AssociationListA"]))
    updatePickerInput(session,
                      inputId = "selectedVisitType",
                      choices = visit_type_choices,
                      selected = visit_type_choices
    )
    appt_type_choices <- sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                       historical.data$Department %in% department_choices &
                                                       historical.data$AssociationListA %in% visit_type_choices, "AssociationListB"]))
    updatePickerInput(session,
                      inputId = "selectedApptType",
                      choices = appt_type_choices,
                      selected = appt_type_choices
    )
    treatment_choices <- sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                       historical.data$Department %in% department_choices &
                                                       historical.data$AssociationListA %in% visit_type_choices &
                                                       historical.data$AssociationListB %in% appt_type_choices, "AssociationListT"]))
    updatePickerInput(session,
                      inputId = "selectedTreatmentType",
                      choices = treatment_choices,
                      selected = treatment_choices
    )
    department_choices_disease <- sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% input$selectedCampus, "Department"]))
    disease_choices <- sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% input$selectedCampus &
                                                         arrivedDisease.data$Department %in% department_choices_disease, "Disease_Group"]))
    updatePickerInput(session,
                      inputId = "selectedDisease",
                      choices = disease_choices,
                      selected = disease_choices
    )
    updatePickerInput(session,
                      inputId = "selectedDisease2",
                      choices = disease_choices,
                      selected = disease_choices
    )
  
    provider_choices <- sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% input$selectedCampus &
                                                          arrivedDisease.data$Department %in% department_choices_disease &
                                                          arrivedDisease.data$Disease_Group %in% disease_choices, "Provider"]))
    
    updatePickerInput(session,
                      inputId = "selectedProvider",
                      choices = provider_choices,
                      selected = provider_choices
    )  
    updatePickerInput(session,
                      inputId = "selectedProvider2",
                      choices = provider_choices,
                      selected = provider_choices
    )  
    
    
    # prov3_choices <- sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
    #                                                    historical.data$Department %in% department_choices_disease , "Provider"]))
    # 
    # updatePickerInput(session,
    #                   inputId = "selectedProvider3",
    #                   choices = provider_choices,
    #                   selected = provider_choices
    # )  
    
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  observeEvent(input$selectedDepartment,{
    visit_type_choices <- sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                        historical.data$Department %in% input$selectedDepartment, "AssociationListA"]))
    updatePickerInput(session,
                      inputId = "selectedVisitType",
                      choices = visit_type_choices,
                      selected = visit_type_choices
    )
    appt_type_choices <- sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                       historical.data$Department %in% input$selectedDepartment &
                                                       historical.data$AssociationListA %in% visit_type_choices, "AssociationListB"]))
    updatePickerInput(session,
                      inputId = "selectedApptType",
                      choices = appt_type_choices,
                      selected = appt_type_choices
    )
    treatment_choices <- sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                       historical.data$Department %in% input$selectedDepartment &
                                                       historical.data$AssociationListA %in% visit_type_choices &
                                                       historical.data$AssociationListB %in% appt_type_choices, "AssociationListT"]))
    updatePickerInput(session,
                      inputId = "selectedTreatmentType",
                      choices = treatment_choices,
                      selected = treatment_choices
    )
    department_choices_disease <- sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% input$selectedCampus, "Department"]))    
    
    disease_choices <- sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% input$selectedCampus &
                                                         arrivedDisease.data$Department %in% department_choices_disease, "Disease_Group"]))
    updatePickerInput(session,
                      inputId = "selectedDisease",
                      choices = disease_choices,
                      selected = disease_choices
    )
    updatePickerInput(session,
                      inputId = "selectedDisease2",
                      choices = disease_choices,
                      selected = disease_choices
    )
    
    
    
    provider_choices <- sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% input$selectedCampus &
                                                          arrivedDisease.data$Department %in% department_choices_disease &
                                                          arrivedDisease.data$Disease_Group %in% disease_choices, "Provider"]))
    
    updatePickerInput(session,
                      inputId = "selectedProvider",
                      choices = provider_choices,
                      selected = provider_choices
    )  
    
    updatePickerInput(session,
                      inputId = "selectedProvider2",
                      choices = provider_choices,
                      selected = provider_choices
    )  
    
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  observeEvent(input$selectedVisitType,{
    appt_type_choices <- sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                       historical.data$Department %in% input$selectedDepartment &
                                                       historical.data$AssociationListA %in% input$selectedVisitType, "AssociationListB"]))
    updatePickerInput(session,
                      inputId = "selectedApptType",
                      choices = appt_type_choices,
                      selected = appt_type_choices
    )
    treatment_choices <- sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                       historical.data$Department %in% input$selectedDepartment &
                                                       historical.data$AssociationListA %in% input$selectedVisitType &
                                                       historical.data$AssociationListB %in% appt_type_choices, "AssociationListT"]))
    updatePickerInput(session,
                      inputId = "selectedTreatmentType",
                      choices = treatment_choices,
                      selected = treatment_choices
    )
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  observeEvent(input$selectedApptType,{
    treatment_choices <- sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                       historical.data$Department %in% input$selectedDepartment &
                                                       historical.data$AssociationListA %in% input$selectedVisitType &
                                                       historical.data$AssociationListB %in% input$selectedApptType, "AssociationListT"]))
    updatePickerInput(session,
                      inputId = "selectedTreatmentType",
                      choices = treatment_choices,
                      selected = treatment_choices
    )
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  observeEvent(c(input$selectedDisease),{
    provider_choices <-  sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% input$selectedCampus &
                                                           arrivedDisease.data$Department %in% input$selectedDepartment &
                                                           arrivedDisease.data$Disease_Group %in% input$selectedDisease, "Provider"]))
    updatePickerInput(session,
                      inputId = "selectedProvider",
                      choices = provider_choices,
                      selected = provider_choices
    )
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  observeEvent(c(input$selectedDisease2),{
    provider_choices <-  sort(unique(arrivedDisease.data[arrivedDisease.data$SITE %in% input$selectedCampus &
                                                           arrivedDisease.data$Department %in% input$selectedDepartment &
                                                           arrivedDisease.data$Disease_Group %in% input$selectedDisease2, "Provider"]))
    updatePickerInput(session,
                      inputId = "selectedProvider2",
                      choices = provider_choices,
                      selected = provider_choices
    )
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  observeEvent(list(input$selectedDepartment),{
    prov_choices <-  sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                           historical.data$Department %in% input$selectedDepartment, "Provider"]))
    updatePickerInput(session,
                      inputId = "selectedProvider1",
                      choices = prov_choices,
                      selected = prov_choices
    )
    
    updatePickerInput(session,
                      inputId = "selectedProvider4",
                      choices = prov_choices,
                      selected = prov_choices
    )
    updatePickerInput(session,
                      inputId = "selectedProvider5",
                      choices = prov_choices,
                      selected = prov_choices
    )
    
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  

  
  # Reactive Data -----------------------------------------------------------------------------------------------------------------------
  # All pre-processed data ============================================================================================================
  dataAll <- eventReactive(list(input$sbm, input$update_filters),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    groupByFilters(historical.data[all.data.rows,],
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # [2.2] Arrived + No Show data ============================================================================================================
  dataArrivedNoShow <- eventReactive(list(input$sbm, input$update_filters),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    groupByFilters(historical.data[arrivedNoShow.data.rows,],
                   input$selectedCampus, input$selectedDepartment, 
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # [2.3] Arrived data ============================================================================================================
  dataArrived <- eventReactive(list(input$sbm, input$update_filters),{
     validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
     )
    groupByFilters(historical.data[arrived.data.rows,],
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Canceled data ============================================================================================================
  dataCanceled<- eventReactive(list(input$sbm, input$update_filters),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    groupByFilters(historical.data[canceled.data.rows,],
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Bumped data ============================================================================================================
  dataBumped<- eventReactive(list(input$sbm, input$update_filters),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    groupByFilters(historical.data[bumped.data.rows,],
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Arrived data filtered: visitType, apptType, treatmentType ===============================================================
  
  dataArrived_filtered<- eventReactive(list(input$sbm, input$update_filters,input$update_filters5),{
    validate(
     need(input$selectedVisitType != "", "Please select a visit type"),
     need(input$selectedApptType != "", "Please select a visit type detail"),
     need(input$selectedTreatmentType != "", "Please select a treatment type")
    )
    groupByFilters_2(dataArrived(),
                     input$selectedVisitType, input$selectedApptType, input$selectedTreatmentType)
  })
  
  
  # Arrived Disease Group data filtered: Disease Group, Provider ==============================================================
  
  dataArrived_disease <- eventReactive(list(input$sbm, input$update_filters1),{
    validate(
      need(input$selectedDisease != "", "Please select a provider group"),
      need(input$selectedProvider != "", "Please select a provider")
    )
    groupByFilters_3(dataArrived(),
                     input$selectedDisease, input$selectedProvider)
  })
  
  dataArrived_disease_2 <- eventReactive(list(input$sbm, input$update_filters2),{
    validate(
      need(input$selectedDisease2 != "", "Please select a provider group"),
      need(input$selectedProvider2 != "", "Please select a provider")
    )
    groupByFilters_3(dataArrived(),
                     input$selectedDisease2, input$selectedProvider2)
  })
  
  # Arrived population data ============================================================================================================
  dataArrivedPop <- eventReactive(list(input$sbm, input$update_filters, input$selectedProvider),{
    validate(
      need(input$selectedVisitType != "", "Please select a visit type"),
      need(input$selectedApptType != "", "Please select a visit type detail"),
      need(input$selectedTreatmentType != "", "Please select a treatment type")
    )
    groupByFilters(population.data_filtered,
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Scheduling  data ============================================================================================================
  
  dataAllsched <- eventReactive(list(input$sbm, input$update_filters, input$update_filters3, input$update_filters4, input$update_filters5),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    groupByFilters_4(historical.data[all.data.rows,],
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$selectedProvider1)
  })
  
  dataArrivedNoShowsched <- eventReactive(list(input$sbm, input$update_filters, input$update_filters3, input$update_filters4, input$update_filters5),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    groupByFilters_4(historical.data[arrivedNoShow.data.rows,],
                   input$selectedCampus, input$selectedDepartment, 
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$selectedProvider1)
  })
  
  dataArrivedsched <- eventReactive(list(input$sbm, input$update_filters, input$update_filters3, input$update_filters4, input$update_filters5),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    groupByFilters_4(historical.data[arrived.data.rows,],
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$selectedProvider1)
  })
  
  dataBumpedsched <- eventReactive(list(input$sbm, input$update_filters, input$update_filters3, input$update_filters4, input$update_filters5),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    groupByFilters_4(historical.data[bumped.data.rows,],
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$selectedProvider1)
  })
  
  dataCanceledsched <- eventReactive(list(input$sbm, input$update_filters, input$update_filters3, input$update_filters4, input$update_filters5),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    groupByFilters(historical.data[canceled.data.rows,],
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$selectedProvider1)
  })
  
  # Site Volume Tab ------------------------------------------------------------------------------------------------------0
  # Volume Trend Tab ------------------------------------------------------------------------------------------------------    
  output$trend_totalvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    
    total_visits <- data %>%
      filter(AssociationListA %in% c("Exam","Treatment","Labs")) %>%
      group_by(Appt.Year, Appt.Month) %>% summarise(total = n())
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    ggplot(total_visits, aes(x=factor(Appt.Month, levels = monthOptions), y=total, group=Appt.Year))+
      geom_line(aes(color=Appt.Year), size=1.1)+
      geom_point(aes(color=Appt.Year), size=3)+
      scale_color_MountSinai('dark')+
      labs(title = paste0(site," ","Annual All Visits"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      scale_y_continuous(limits=c(0,(max(total_visits$total))*1.3))+
      geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, fontface="bold",
                 nudge_x = 0.1, size=5)
    
  }, height = function(x) input$plotHeight)
  
  
  output$trend_examvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    total_visits <- data %>% filter(AssociationListA == "Exam") %>% 
      group_by(Appt.Year, Appt.Month) %>% summarise(total = n())
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    ggplot(total_visits, aes(x=factor(Appt.Month, levels = monthOptions), y=total, group=Appt.Year))+
      geom_line(aes(color=Appt.Year), size=1.1)+
      geom_point(aes(color=Appt.Year), size=3)+
      scale_color_MountSinai('dark')+
      labs(title = paste0(site," ","Annual Exam Visits"), 
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      scale_y_continuous(limits=c(0,(max(total_visits$total))*1.3))+
      geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, fontface="bold",
                 nudge_x = 0.1, size=5)
  }, height = function(x) input$plotHeight)
  
  
  output$trend_treatmentvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    total_visits <- data %>% filter(AssociationListA == "Treatment") %>% 
      group_by(Appt.Year, Appt.Month) %>% summarise(total = n())
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    ggplot(total_visits, aes(x=factor(Appt.Month, levels = monthOptions), y=total, group=Appt.Year))+
      geom_line(aes(color=Appt.Year), size=1.1)+
      geom_point(aes(color=Appt.Year), size=3)+
      scale_color_MountSinai('dark')+
      labs(title = paste0(site," ","Annual Treatment Visits"), 
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      scale_y_continuous(limits=c(0,(max(total_visits$total))*1.3))+
      geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, fontface="bold",
                 nudge_x = 0.1, size=5)
    
  }, height = function(x) input$plotHeight)
  
  
  output$trend_labvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    total_visits <- data %>% filter(AssociationListA == "Labs") %>% group_by(Appt.Year, Appt.Month) %>% summarise(total = n())
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    ggplot(total_visits, aes(x=factor(Appt.Month, levels = monthOptions), y=total, group=Appt.Year))+
      geom_line(aes(color=Appt.Year), size=1.1)+
      geom_point(aes(color=Appt.Year), size=3)+
      scale_color_MountSinai('dark')+
      labs(title = paste0(site," ","Annual Lab Visits"), 
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      scale_y_continuous(limits=c(0,(max(total_visits$total))*1.3))+
      geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, fontface="bold",
                 nudge_x = 0.1, size=5)
    
  }, height = function(x) input$plotHeight)
  
  
  output$trend_visitstable <- function(){
    
    #options(knitr.kable.NA = "-")
    data <- dataArrived()
    #data <- arrived.data
    #created an if statement to include another table for all of the visit types
    #to show the total volume and the variance per month per year.
    
    #get the total patients per year
    if(input$annualVolSummary == "Total"){
      visits_tb_yearly <- data %>%
        group_by(Appt.Year) %>% summarise(total = n()) %>%
        spread(Appt.Year, total)
      visits_tb_yearly$Appt.Month <- "Total Annual Comparison"
      visits_tb_yearly <- visits_tb_yearly %>% relocate(Appt.Month)
      
      #get the total patients per year per month
      visits_tb <- data %>%
        group_by(Appt.Year, Appt.Month) %>% summarise(total = n()) %>%
        spread(Appt.Year, total)
      
      
    } else {
      #get the total patients per year
      visits_tb_yearly <- data %>% 
        filter(AssociationListA %in% input$annualVolSummary) %>%
        group_by(Appt.Year) %>% summarise(total = n()) %>%
        spread(Appt.Year, total)
      visits_tb_yearly$Appt.Month <- paste0("Total ",input$annualVolSummary,"\nAnnual Comparison")
      visits_tb_yearly <- visits_tb_yearly %>% relocate(Appt.Month)
      
      #get the total patients per year per month
      visits_tb <- data %>% 
        filter(AssociationListA %in% input$annualVolSummary) %>%
        group_by(Appt.Year, Appt.Month) %>% summarise(total = n()) %>%
        spread(Appt.Year, total)
    }
    
    
    #include all the months needed
    visits_tb <- visits_tb[match(monthOptions, visits_tb$Appt.Month),]
    visits_tb$Appt.Month <- monthOptions
    
    visits_ytd <- visits_tb
    visits_ytd[is.na(visits_ytd)] = 0  
    visits_ytd$Appt.Month <- NULL
    visits_ytd <- visits_ytd[1:month(input$dateRange[2]),]
    visits_ytd <- as.data.frame(colSums(visits_ytd))
    visits_ytd <- as.data.frame(t(as.matrix(visits_ytd)))
    visits_ytd <- cbind(Appt.Month = "YTD Comparison",visits_ytd)
    
    #bind the total visits per month per year with the total yeraly visits 
    visits_tb_total <- rbind(visits_tb, visits_ytd,visits_tb_yearly)
    
    #created an if statement to change the table based on the different years
    #if the number of years provided is one then there will be no need to calculate any variance
    #and only the volume will be showing for that specific year
    #if the number of years are more than 1 and less than or equal 3 then we calculate variance
    #if the number of years are more than 3 the code will raise a user error
    
    if(length(visits_tb_total)-1 == 1){
      visits_tb_total <- visits_tb_total
      year1 <- colnames(visits_tb_total)[2]
      column_names <- c("Month", paste0(year1))
      header_above <- c("Total Visit Volume" = 2)
      names(header_above) <- paste(c(input$annualVolSummary), c("Visit Volume"))
      
      
      column_border <- c(1, 2)
      
    } else if(length(visits_tb_total)-1 == 2){
      
      visits_tb_total$variance <- visits_tb_total %>% select(length(visits_tb_total)) - visits_tb_total %>% select(length(visits_tb_total)-1)
      
      visits_tb_total$variance_percentage <- visits_tb_total %>% select(length(visits_tb_total)) / visits_tb_total %>% select(length(visits_tb_total)-2)
      
      #######
      
      visits_tb_total$variance_percentage <- formattable::percent(as.numeric(unlist(visits_tb_total$variance_percentage)))
      
      year1 <- colnames(visits_tb_total)[2]
      year2 <- colnames(visits_tb_total)[3]
      
      visits_variance_only <- as.data.frame(visits_tb_total[1:month(input$dateRange[2]),4])
      visits_variance_percentage <- as.data.frame(visits_tb_total[1:month(input$dateRange[2]),5])
      #visits_variance_only <- as.data.frame(visits_tb_total[1:3,4])
      #visits_variance_percentage <- as.data.frame(visits_tb_total[1:3,5])
      
      visits_variance_only[is.na(visits_variance_only),] <- 0
      visits_variance_percentage[is.na(visits_variance_percentage),] <- 0
      visits_variance_only <- as.data.frame(colSums(visits_variance_only))
      visits_variance_percentage <- as.data.frame(colSums(visits_variance_percentage))
      visits_tb_total <- as.data.frame(visits_tb_total)
      visits_tb_total[13,4] <- visits_variance_only[1,1]
      visits_tb_total[13,5] <- visits_variance_percentage[1,1]
      
      
      #######
      
      column_names <- c("Month", paste0(year1), paste0(year2), 
                        paste0("Variance"," ", "(", paste0(year1), "-", paste0(year2), ")"), 
                        paste0("% Variance", " ", "(", paste0(year1), "-", paste0(year2), ")"))
      
      header_above <- c("Total Visit Volume" = 3, "Volume Variance" = 2)
      names(header_above) <- paste(c(input$annualVolSummary,input$annualVolSummary), c("Visit Volume","Volume Variance"))
      
      
      column_border <- c(1, 3, 5)
      
    } else if (length(visits_tb_total)-1 == 3){
      
      visits_tb_total$variance_1 <- visits_tb_total %>% select(length(visits_tb_total)-1) - visits_tb_total %>% select(length(visits_tb_total)-2)
      
      visits_tb_total$variance_percentage_1 <- visits_tb_total %>% select(length(visits_tb_total)) / visits_tb_total %>% select(length(visits_tb_total)-3)
      
      #######
      
      visits_tb_total$variance_2 <- visits_tb_total %>% select(length(visits_tb_total)-3) - visits_tb_total %>% select(length(visits_tb_total)-4)
      
      visits_tb_total$variance_percentage_2 <- visits_tb_total %>% select(length(visits_tb_total)) / visits_tb_total %>% select(length(visits_tb_total)-5)
      
      #######
      
      visits_tb_total$variance_percentage_1 <- formattable::percent(as.numeric(unlist(visits_tb_total$variance_percentage_1)))
      visits_tb_total$variance_percentage_2 <- formattable::percent(as.numeric(unlist(visits_tb_total$variance_percentage_2)))
      
      year1 <- colnames(visits_tb_total)[2]
      year2 <- colnames(visits_tb_total)[3]
      year3 <- colnames(visits_tb_total)[4]
      
      
      visits_variance_only <- as.data.frame(visits_tb_total[1:month(input$dateRange[2]),4:5])
      visits_variance_percentage <- as.data.frame(visits_tb_total[1:month(input$dateRange[2]),6])
      #visits_variance_only <- as.data.frame(visits_tb_total[1:3,4:5])
      #visits_variance_percentage <- as.data.frame(visits_tb_total[1:3,6])
      
      
      visits_variance_only[is.na(visits_variance_only)] <- 0
      visits_variance_percentage[is.na(visits_variance_percentage)] <- 0
      visits_variance_only <- t(as.data.frame(colSums(visits_variance_only)))
      visits_variance_percentage <- as.data.frame(colSums(visits_variance_percentage))
      visits_tb_total <- as.data.frame(visits_tb_total)
      visits_tb_total[13,4:5] <- visits_variance_only[1,1:2]
      visits_tb_total[13,6] <- visits_variance_percentage[1,1]
      
      #######
      
      column_names <- c("Month", paste0(year1), paste0(year2), paste0(year3), 
                        paste0("Variance"," ", "(", paste0(year1), "-", paste0(year2), ")"),
                        paste0("% Variance", " ", "(", paste0(year1), "-", paste0(year2), ")"),
                        paste0("Variance"," ", "(", paste0(year2), "-", paste0(year3), ")"), 
                        paste0("% Variance", " ", "(", paste0(year2), "-", paste0(year3), ")"))
      
      header_above <- c("Total Visit Volume" = 4, "Volume Variance" = 4) 
      names(header_above) <- paste(c(input$annualVolSummary,input$annualVolSummary), c("Visit Volume","Volume Variance"))
      
      
      column_border <- c(1, 4, 8)
      
    } else {print("Please select <= 3 years")}
    
    visits_tb_total %>%
      kable(escape = F, align = "c",
            col.names = column_names) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      add_header_above(header_above,  background = "#d80b8c", color = "white", font_size = 20, align = "center") %>%
      column_spec(column = column_border, border_right = "thin solid lightgray", width_min = "125px") %>%
      column_spec(column = 1, bold = T) %>%
      row_spec(row = 0, font_size = 18, bold=TRUE, background = "#d80b8c", color = "white") %>%
      row_spec(row = 14, bold = TRUE, background = "#d80b8c", color = "white") %>%
      row_spec(row = 13, bold = TRUE, background = "#dddedd")
    
  }
  
  # Volume Breakdown Tab ------------------------------------------------------------------------------------------------------       
  output$break_totalvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- historical.data[arrived.data.rows,]
    
    total_visits_break <- data %>% filter(AssociationListA %in% c("Labs","Treatment","Exam")) %>%
      group_by(Appt.MonthYear, AssociationListA) %>% summarise(total = n())
    
    total_visits_break$AssociationListA <- factor(total_visits_break$AssociationListA, levels = c("Labs","Treatment","Exam"))
    
    max <- total_visits_break %>% group_by(Appt.MonthYear) %>% summarise(max = sum(total))
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    g1 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y=total, group=AssociationListA, fill=AssociationListA))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,(max(max$max))*1.2))+
      labs(title = paste0(site," ","All Visit Volume Composition"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           x = NULL, y = "Patient Volume\n", fill = NULL)+
      theme_new_line()+
      theme(axis.title.y = element_text(size = 12, angle = 90),  plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    
    n <- length(unique(total_visits_break$AssociationListA)) - 1
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }
    
    g2 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y= AssociationListA, label=total, color = AssociationListA)) +
      scale_color_MountSinai('dark' )+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface  = "bold")+
      geom_hline(yintercept = hline_y, colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL, fill = "AssociationListA")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g1 + g2 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(total_visits_break$AssociationListA))))
    
    
  }, height = function(x) input$plotHeight)
  
  
  output$break_examvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    total_visits_break <- data %>% filter(AssociationListA == "Exam") %>%
      group_by(Appt.MonthYear, AssociationListB) %>% summarise(total = n())
    
    max <- total_visits_break %>% group_by(Appt.MonthYear) %>% summarise(max = sum(total))
    total_visits_break$AssociationListB <- factor(total_visits_break$AssociationListB, levels = c("Telehealth Visit","New Visit","Established Visit"))
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    total_visits_break <- total_visits_break %>% filter(!is.na(AssociationListB))
    
    g3 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y=total, group=AssociationListB, fill=AssociationListB))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,(max(max$max))*1.2))+
      labs(title = paste0(site," ","Exam Visit Volume Composition"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = "Patient Volume\n", x = NULL, fill = NULL)+
      theme_new_line()+
      theme(axis.title.y = element_text(size = 12, angle = 90), plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    
    n <- length(unique(total_visits_break$AssociationListB)) - 1
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }
    
    g4 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y= AssociationListB, label=total, color = AssociationListB)) +
      scale_color_MountSinai('dark' )+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = hline_y, colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "AssociationListB")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g3 + g4 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(total_visits_break$AssociationListB))))
    
  }, height = function(x) input$plotHeight)
  
  
  output$break_treatmentvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    
    #data <- historical.data[arrived.data.rows,] %>% filter(SITE == "MSW", Appt.MonthYear == "2020-12")
    #data <- historical.data[arrived.data.rows,] %>% filter(SITE == "MSW")
    # nrow(data)
    
    total_visits_break <- data %>% filter(AssociationListA == "Treatment") %>%
      group_by(Appt.MonthYear, AssociationListT) %>% summarise(total = n())
    
    max <- total_visits_break %>% group_by(Appt.MonthYear) %>% summarise(max = sum(total))
    
    factor_levels = c("Pump Disconnect", "Port Flush", "Transfusion", "Phlebotomy", "Hydration", "Injection", "Therapeutic Infusion", "Infusion")
    
    total_visits_break$AssociationListT <- factor(total_visits_break$AssociationListT, levels = factor_levels)
    
    total_visits_break <- total_visits_break %>% filter(!is.na(AssociationListT))
    
    sum <- total_visits_break %>% summarise(sum = sum(total))
    
    total_visits_break <- inner_join(total_visits_break,sum, by = c("Appt.MonthYear"))
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    g5 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y=total, group=AssociationListT, fill=AssociationListT))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,(max(max$max))*1.2))+
      labs(title = paste0(site," ","Treatment Visit Volume Composition"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = "Patient Volume\n", x = NULL, fill = NULL)+
      theme_new_line()+
      theme(axis.title.y = element_text(size = 12, angle = 90), plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(data=subset(total_visits_break, total/sum > .05),aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    
    n <- length(unique(total_visits_break$AssociationListT)) - 1
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }
    
    g6 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y= AssociationListT, label=total, color = AssociationListT)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = 'bold')+
      geom_hline(yintercept = hline_y, colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "AssociationListT")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g5 + g6 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(total_visits_break$AssociationListT))))
    
  }, height = function(x) input$plotHeight)
  
  
  # Volume Comparison Tab ------------------------------------------------------------------------------------------------------
  #Volume Comparison Tab - Total Breakdown
  output$volumeCompTotal_grh <- renderPlot({
    
    data <- dataArrived_filtered()
    # data <- arrived.data
    
    if(length(unique(data$AssociationListA)) == 1){
      visitType <- unique(data$AssociationListA)
      apptType <-  paste(sort(unique(data$AssociationListB)), sep="", collapse=", ")
      # apptType <-  paste(paste(sort(unique(data$AssociationListB)), sep="", collapse=", "),", ",
      #                     paste(sort(unique(data$AssociationListT)), sep="", collapse=", "))
    } else{
      visitType <- paste(sort(unique(data$AssociationListA)),sep="", collapse=", ")
      apptType <-  paste(sort(unique(data$AssociationListB)), sep="", collapse=", ")
    }
    
    
    if(input$comp_choices == "All"){
      
      if(input$analysis_type == "Monthly"){
        # Comparison by site
        visit_comp_all <- data %>%
          group_by(Appt.MonthYear) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_all, aes(x=Appt.MonthYear, y=total))+
          geom_bar(stat="identity", width=0.7, fill="#212070")+
          scale_y_continuous(limits=c(0,(max(visit_comp_all$total))*1.2))+
          geom_text(aes(label=total), vjust =-1, color="black", fontface="bold", size=5)
      } else{
        # Comparison by site
        visit_comp_all <- data %>%
          group_by(Appt.Week) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_all, aes(x=Appt.Week, y=total))+
          geom_bar(stat="identity", fill="#212070")+
          scale_y_continuous(limits=c(0,(max(visit_comp_all$total))*1.2))+
          geom_text(aes(label=total), vjust =-1, color="black", fontface="bold", size=5)+
          scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
        
      }
      
    } else{
      
      if(input$analysis_type == "Monthly"){
        # Comparison by site
        visit_comp_site <- data %>%
          group_by(Appt.MonthYear, SITE) %>% summarise(total = n())
        
        max <- visit_comp_site %>% group_by(Appt.MonthYear) %>% summarise(total = sum(total))
        
        graph <- ggplot(visit_comp_site, aes(x=Appt.MonthYear, y=total, group=SITE, fill=SITE))+
          geom_bar(position="stack", stat="identity", width=0.7)+
          scale_y_continuous(limits=c(0,(max(max$total))*1.2))+
          stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                       size=5, fontface="bold.italic")
        
      } else{
        # Comparison by site
        visit_comp_site <- data %>%
          group_by(Appt.Week, SITE) %>% summarise(total = n())
        
        max <- visit_comp_site %>% group_by(Appt.Week) %>% summarise(total = sum(total))
        
        graph <- ggplot(visit_comp_site, aes(x=Appt.Week, y=total, group=SITE, fill=SITE))+
          geom_bar(position="stack", stat="identity")+
          scale_y_continuous(limits=c(0,(max(max$total))*1.2))+
          stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.Week), geom="text", color="black", 
                       size=5, fontface="bold.italic")+
          scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
      }
    }
    
    
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    
    graph + 
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      scale_fill_MountSinai('dark')+
      labs(title = paste0(site, " Monthly ",visitType, " Volume Breakdown by ",input$comp_choices),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           caption = paste0("\n*Includes ",apptType),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(axis.text.x = element_text(angle = 40, hjust=1))
    
  }, height = function(x) input$plotHeight)
  
  
  # Volume Comparison Tab - Trend Graphs
  output$volumeCompTrend_grh <- renderPlot({
    
    data <- dataArrived_filtered()
    # data <- arrived.data
    
    
    if(length(unique(data$AssociationListA)) == 1){
      visitType <- unique(data$AssociationListA)
      apptType <-  paste(sort(unique(data$AssociationListB)), sep="", collapse=", ")
      # apptType <-  paste(paste(sort(unique(data$AssociationListB)), sep="", collapse=", "),", ",
      #                     paste(sort(unique(data$AssociationListT)), sep="", collapse=", "))
    } else{
      visitType <- paste(sort(unique(data$AssociationListA)),sep="", collapse=", ")
      apptType <-  paste(sort(unique(data$AssociationListB)), sep="", collapse=", ")
    }
    
    
    if(input$comp_choices == "All"){
      
      if(input$analysis_type == "Monthly"){
        # Comparison by site
        visit_comp_all <- data %>%
          group_by(Appt.MonthYear) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_all, aes(x=Appt.MonthYear, y=total, group=1))+
          geom_line(size=1.1)+
          geom_point(size=3)+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(visit_comp_all$total)*1.2))
        
      } else{
        # Comparison by site
        visit_comp_all <- data %>%
          group_by(Appt.Week) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_all, aes(x=Appt.Week, y=total, group=1))+
          geom_line(size=1.1)+
          geom_point(size=3)+
          scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(visit_comp_all$total)*1.2))
      }
      
    } else{
      
      if(input$analysis_type == "Monthly"){
        # Comparison by site
        visit_comp_site <- data %>%
          group_by(Appt.MonthYear, SITE) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_site, aes(x=Appt.MonthYear, y=total, group=SITE))+
          geom_line(aes(color=SITE), size=1.1)+
          geom_point(aes(color=SITE), size=3)+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(visit_comp_site$total)*1.2))
        
      } else{
        # Comparison by site
        visit_comp_site <- data %>%
          group_by(Appt.Week, SITE) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_site, aes(x=Appt.Week, y=total, group=SITE))+
          geom_line(aes(color=SITE), size=1.1)+
          geom_point(aes(color=SITE), size=3)+
          scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(visit_comp_site$total)*1.2))
      }
    }
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    
    
    graph + 
      scale_color_MountSinai('dark')+
      labs(title = paste0(site, " Monthly ",visitType, " Volume Trend by ",input$comp_choices),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           caption = paste0("\n*Includes ",apptType),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(axis.text.x = element_text(angle = 40, hjust=1))+
      geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
                 nudge_x = 0.1, size=5)
    
  }, height = function(x) input$plotHeight)
  
  
  # Provider Volume Tab ------------------------------------------------------------------------------------------------------
  ## Physician Visits Breakdown Tab =========================================================================================
  
  output$provVolumeExam_tb <- function(){
    
    data <- dataArrived_disease() %>%
      filter(AssociationListA == "Exam")
    
    tele_data <- dataArrived_disease() %>%
      filter(AssociationListB == "Telehealth Visit")
    # 
    # data <- historical.data[arrived.data.rows,] %>%
    #   filter(AssociationListA == "Exam", SITE %in% c("DBC"))
    # 
    # tele_data <- historical.data[arrived.data.rows,] %>%
    #   filter(AssociationListB == "Telehealth Visit", SITE %in% c("DBC"))

    prov_tb <- data %>% 
      group_by(Disease_Group, Provider, AssociationListB,  Appt.MonthYear) %>%
      summarise(total = n())  %>%
      `colnames<-` (c("Disease", "Provider", "Appointment Type", "Appt.MonthYear", "Total")) %>%
      pivot_wider(names_from = Appt.MonthYear,
                  values_from = Total,
                  values_fill = 0) %>%
      split(.[,"Provider"]) %>%  
      map_df(., janitor::adorn_totals, name = paste0("Total")) %>%
      mutate(Disease = ifelse(Disease == "Total", lag(Disease, 1), Disease),
             Provider = ifelse(Provider == "-", lag(Provider, 1), Provider),
             `Appointment Type` = ifelse(`Appointment Type` == "-", "Exam Total", `Appointment Type`)) %>%
      arrange(-desc(Disease))

    tele_tb <- tele_data %>% 
      group_by(Disease_Group, Provider, Appt.Type, Appt.MonthYear) %>%
      summarise(total = n()) %>%
      `colnames<-` (c("Disease", "Provider", "Appointment Type", "Appt.MonthYear", "Total")) %>%
      pivot_wider(names_from = Appt.MonthYear,
                  values_from = Total,
                  values_fill = 0)
    
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "all sites"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    
    appt_order <- c("Exam Total",c("Established Visit", "New Visit", "Telehealth Visit"), as.vector(unique(tele_tb$`Appointment Type`)))
    
    final_df <- bind_rows(prov_tb, tele_tb)
    final_df <- final_df[order(match(final_df$`Appointment Type`, appt_order)), ]
    final_df <- final_df %>%
      arrange(Disease, Provider) %>%
      adorn_totals("col", fill = "-", na.rm = TRUE, name = "YTD Total") 
    
    indent_rows <- which(final_df$`Appointment Type` %in% unique(tele_tb$`Appointment Type`))
    
    header_above <- c("Subtitle" = 8)
    names(header_above) <- paste0(c("Based on data from "),c(site))
    
    final_df %>%
      mutate(`Appointment Type` = cell_spec(`Appointment Type`, italic = ifelse(row_number() %in% indent_rows, T, F), 
                                            font_size = ifelse(row_number() %in% indent_rows, 14, 16))) %>%
      kable(booktabs = T, escape = F) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      add_header_above(header_above, color = "black", font_size = 16, align = "center", italic = TRUE) %>%
      add_header_above(c("Physician Visits Breakdown" = length(final_df)),
                       color = "black", font_size = 20, align = "center", line = FALSE) %>% 
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      row_spec(which(final_df$`Appointment Type` == "Exam Total"), bold = T) %>%
      column_spec(length(final_df), background = "#d80b8c", color = "white", bold = T) %>%
      column_spec(1, bold = T) %>%
      collapse_rows(c(1,2), valign = "top") %>%
      add_indent(indent_rows, level_of_indent = 2) %>%
      gsub("NA", " ", .)
  }
  
  
  ## Telehealth Visits Breakdown Tab =========================================================================================
  
  # output$provVolumeTele_tb <- function(){
  #   
  #   data <- dataArrived_disease() %>%
  #     filter(AssociationListB == "Telehealth Visit")
  #   
  #   data <- arrivedDisease.data %>%
  #     filter(AssociationListB == "Telehealth Visit")
  #   
  #   tele_tb <- data %>% 
  #     group_by(Disease_Group, Provider, Appt.Type, Appt.MonthYear) %>%
  #     summarise(total = n()) %>%
  #     `colnames<-` (c("Disease", "Provider", "Appointment Type", "Appt.MonthYear", "Total")) %>%
  #     pivot_wider(names_from = Appt.MonthYear,
  #                 values_from = Total,
  #                 values_fill = 0) %>%
  #     adorn_totals("col", fill = "-", na.rm = TRUE, name = "YTD Total") %>%
  #     arrange(Provider, `Appointment Type`, Disease)
  #   
  #   row_total <- tele_tb %>% 
  #     split(.[,"Provider"]) %>%  
  #     map_df(., janitor::adorn_totals, name = paste0("Telehealth Total"))
  #   
  #   
  #   kable(tele_tb[,2:length(tele_tb)] %>%
  #           split(.[,"Provider"]) %>%  
  #           map_df(., janitor::adorn_totals, name = paste0("Telehealth Total"))) %>%
  #     pack_rows(index = table(tele_tb$Disease), label_row_css = "background-color: #c9f0ff;") %>%
  #     kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
  #     add_header_above(c("Telehealth Visits Breakdown" = (length(tele_tb)-1)),
  #                      color = "black", font_size = 20, align = "center", line = FALSE) %>%
  #     row_spec(0, background = "#00aeef", color = "white", bold = T) %>%
  #     row_spec(which(row_total$Disease == "Telehealth Total"), bold = T, background = "#e6e6e6") %>%
  #     column_spec(1, bold = T) %>%
  #     column_spec(length(tele_tb)-1, background = "#00aeef", color = "white", bold = T) %>%
  #     collapse_rows(1, valign = "top")
  #   
  # }
  # 
  
  # Unique Patients by System and Site - All Tab ---------------------------------------------------------------------------------
  ## Unique MRN by System
  output$uniqueAllSystem <- renderValueBox({
    
    data <- uniquePts_df_system(dataArrived(), c("Exam","Labs","Treatment"))
    # data <- uniquePts.all.data
    
    valueBoxSpark(
      value =  prettyNum(nrow(data), big.mark = ','),
      title = toupper("Total System Unique Patients"),
      sparkobj = NULL,
      info = paste0("Total count of unique patients visited MSHS from..."), 
      icon = icon("hospital-user"),
      subtitle = "Based on exam, treatment, and lab visits.",
      color = "yellow",
      href = NULL
    )
  })
  
  
  ## Unique MRN  over Time (Months)
  output$uniqueAllTrendSystem <- renderPlot({
    
    data <- uniquePts_df_system(dataArrived(), c("Exam","Labs","Treatment"))
    # data <- uniquePts.all.data
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>%
      group_by(Appt.MonthYear) %>%
      summarise(total = n())
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    g9 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))+
      geom_line(size=1.1)+
      geom_point(size=3)+
      scale_color_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("System Unique Patients over Time"),
           subtitle = paste0("Based on ", site," data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
                 nudge_x = 0.1, size=5)
    
    g10 <- ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL)+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g9 + g10 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Month
  output$uniqueAllMonthSystem <- renderPlot({
    
    data <- uniquePts_df_systemMonth(dataArrived(), c("Exam","Labs","Treatment"))
    # data <- uniquePts.all.data
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear) %>%
      summarise(total = n())
    
    g11 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))+
      geom_line(size=1.1)+
      geom_point(size=3)+
      scale_color_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("System Unique Patients by Month"),
           subtitle = paste0("Based on ", site, "data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
                 nudge_x = 0.1, size=5)
    
    g12 <- ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL)+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g11 + g12 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Site
  output$uniqueAllSite <- renderPlot({
    
    data <- uniquePts_df_site(dataArrived(), c("Exam","Labs","Treatment"))
    # data <- uniquePts.all.data
    
    unique <- data %>%
      group_by(SITE) %>%
      summarise(total = n())
    
    g7 <- ggplot(unique, aes(x=SITE, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Total Unique Patients by Site"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      #theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", vjust = 1.2, position = position_dodge(0.9))
    
    g8 <- ggplot(unique, aes(x=SITE, y= "Site", label=total, color = SITE)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g7 + g8 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
    
  },  width = function(x) input$plotWidth, height = function(x) input$plotHeight)
  
  
  ## Unique MRN  over Time (Months)
  output$uniqueAllTrendSite <- renderPlot({
    
    data <- uniquePts_df_site(dataArrived(), c("Exam","Labs","Treatment"))
    # data <- uniquePts.all.data
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear, SITE) %>%
      summarise(total = n())
    
    g9 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE, color=SITE))+
      geom_line(size=1.1)+
      geom_point(size=3)+
      scale_color_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Unique Patients by Site over Time"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))
    
    # geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
    #            nudge_x = 0.1, size=5)
    
    g10 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g9 + g10 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(unique$SITE))))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Month
  output$uniqueAllMonthSite <- renderPlot({
    
    data <- uniquePts_df_siteMonth(dataArrived(), c("Exam","Labs","Treatment"))
    # data <- uniquePts.all.data
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear, SITE) %>%
      summarise(total = n())
    
    
    sum <- unique %>% summarise(sum = sum(total))
    unique <- inner_join(unique,sum, by = c("Appt.MonthYear"))
    
    #to get the upper limit for the y_continuous
    unique_ <- unique %>% spread(SITE, total)
    unique_$Appt.MonthYear <- NULL
    max_col <- function(data) sapply(data, max, na.rm = TRUE)
    max_tot_site <- max_col(unique_)
    
    
    
    
    g11 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,sum(max_tot_site)*1.2))+
      labs(title = paste0("Unique Patients by Site and Month"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(data = subset(unique, total/sum > 0.1),aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    g12 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g11 + g12 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(unique$SITE))))
    
  }, height = function(x) input$plotHeight)
  
  
  # Unique Patients by System and Site - Exam Tab --------------------------------------------------------------------------------------------
  ## Unique MRN by System
  output$uniqueOfficeSystem <- renderValueBox({
    
    data <- uniquePts_df_system(dataArrived(), c("Exam"))
    # data <- uniquePts.office.data
    
    valueBoxSpark(
      value =  prettyNum(nrow(data), big.mark = ','),
      title = toupper("Total System Unique Patients - Exam Visits"),
      sparkobj = NULL,
      info = paste0("Total count of unique patients visited MSHS from..."), 
      icon = icon("hospital-user"),
      subtitle = "Based on exam visits.",
      color = "fuchsia",
      href = NULL
    )
  })
  
  ## Unique MRN  over Time (Months)
  output$uniqueOfficeTrendSystem <- renderPlot({
    
    data <- uniquePts_df_system(dataArrived(), c("Exam"))
    # data <- uniquePts.office.data
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear) %>%
      summarise(total = n())
    
    g15 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))+
      geom_line(size=1.1)+
      geom_point(size=3)+
      scale_color_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("System Unique Patients over Time - Exam Visits"),
           subtitle = paste0("Based on ", site,  "data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
                 nudge_x = 0.1, size=5)
    
    g16 <- ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL)+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g15 + g16 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Month
  output$uniqueOfficeMonthSystem <- renderPlot({
    
    data <- uniquePts_df_systemMonth(dataArrived(), c("Exam"))
    # data <- uniquePts.office.data
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear) %>%
      summarise(total = n())
    
    g17 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))+
      geom_line(size=1.1)+
      geom_point(size=3)+
      scale_color_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("System Unique Patients by Month - Exam Visits"),
           subtitle = paste0("Based on ", site, " data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
                 nudge_x = 0.1, size=5)
    
    g18 <- ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL)+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g17 + g18 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Site
  output$uniqueOfficeSite <- renderPlot({
    
    data <- uniquePts_df_site(dataArrived(), c("Exam"))
    # data <- uniquePts.office.data
    
    unique <- data %>% 
      group_by(SITE) %>%
      summarise(total = n())
    
    g13 <- ggplot(unique, aes(x=SITE, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Total Unique Patients by Site - Exam Visits"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", vjust = 1.2, position = position_dodge(0.9))
    
    g14 <- ggplot(unique, aes(x=SITE, y= "Site", label=total, color = SITE)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g13 + g14 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
  }, height = function(x) input$plotHeight)
  
  ## Unique MRN  over Time (Months)
  output$uniqueOfficeTrendSite <- renderPlot({
    
    data <- uniquePts_df_site(dataArrived(), c("Exam"))
    # data <- uniquePts.office.data
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear, SITE) %>%
      summarise(total = n())
    
    g15 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE, color=SITE))+
      geom_line(size=1.1)+
      geom_point(size=3)+
      scale_color_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Unique Patients by Site over Time - Exam Visits"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))
    # geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
    #            nudge_x = 0.1, size=5)
    
    g16 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g15 + g16 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(unique$SITE))))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Month
  output$uniqueOfficeMonthSite <- renderPlot({
    
    data <- uniquePts_df_siteMonth(dataArrived(), c("Exam"))
    # data <- uniquePts.office.data
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear, SITE) %>%
      summarise(total = n())
    
    #to get the upper limit for the y_continuous
    unique_ <- unique %>% spread(SITE, total)
    unique_$Appt.MonthYear <- NULL
    max_col <- function(data) sapply(data, max, na.rm = TRUE)
    max_tot_site <- max_col(unique_)
    
    g17 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,sum(max_tot_site)*1.2))+
      
      labs(title = paste0("Unique Patients by Site and Month - Exam Visits"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    g18 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g17 + g18 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(unique$SITE))))
    
  }, height = function(x) input$plotHeight)
  
  # Unique Patients by System and Site - Treatment Tab ------------------------------------------------------------------
  ## Unique MRN by System
  output$uniqueTreatmentSystem <- renderValueBox({
    
    data <- uniquePts_df_system(dataArrived(), c("Treatment Visit"))
    # data <- uniquePts_df(arrived.data, c("Treatment"))
    
    valueBoxSpark(
      value =  prettyNum(nrow(data), big.mark = ','),
      title = toupper("Total System Unique Patients - Treatment Visits"),
      sparkobj = NULL,
      info = paste0("Total count of unique patients visited MSHS from..."), 
      icon = icon("hospital-user"),
      subtitle = "Based on treatment visits.",
      color = "aqua",
      href = NULL
    )
  })
  
  ## Unique MRN  over Time (Months)
  output$uniqueTreatmentTrendSystem <- renderPlot({
    
    data <- uniquePts_df_system(dataArrived(), c("Treatment Visit"))
    # data <- uniquePts_df_system(arrived.data, c("Treatment Visit"))
    
    if(length(unique(data$SITE)) == length(default_campus)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear) %>%
      summarise(total = n())
    
    g21 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))+
      geom_line(size=1.1)+
      geom_point(size=3)+
      scale_color_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("System Unique Patients over Time - Treatment Visits"),
           subtitle = paste0("Based on ", site, " data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
                 nudge_x = 0.1, size=5)
    
    g22 <- ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL)+
      theme_minimal() +
      table_theme()
    
    g21 + g22 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Month
  output$uniqueTreatmentMonthSystem <- renderPlot({
    
    data <- uniquePts_df_systemMonth(dataArrived(), c("Treatment Visit"))
    # data <- uniquePts_df(arrived.data, c("Treatment Visit"))
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear) %>%
      summarise(total = n())
    
    g23 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))+
      geom_line(size=1.1)+
      geom_point(size=3)+
      scale_color_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("System Unique Patients by Month  - Treatment Visits"),
           subtitle = paste0("Based on ",site, " data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
                 nudge_x = 0.1, size=5)
    
    g24 <- ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL)+
      theme_minimal() +
      table_theme()
    
    g23 + g24 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Site
  output$uniqueTreatmentSite <- renderPlot({
    
    data <- uniquePts_df_site(dataArrived(), c("Treatment Visit"))
    # data <- uniquePts_df(arrived.data, c("Treatment Visit"))
    
    unique <- data %>% 
      group_by(SITE) %>%
      summarise(total = n())
    
    g19 <- ggplot(unique, aes(x=SITE, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Total Unique Patients by Site - Treatment Visits"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", vjust = 1.2, position = position_dodge(0.9))
    
    g20 <- ggplot(unique, aes(x=SITE, y= "Site", label=total, color = SITE)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE")+
      theme_minimal() +
      table_theme()
    
    g19 + g20 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN  over Time (Months)
  output$uniqueTreatmentTrendSite <- renderPlot({
    
    data <- uniquePts_df_site(dataArrived(), c("Treatment Visit"))
    # data <- uniquePts_df(arrived.data, c("Treatment Visit"))
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>%
      group_by(Appt.MonthYear, SITE) %>%
      summarise(total = n())
    
    g21 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE, color=SITE))+
      geom_line(size=1.1)+
      geom_point(size=3)+
      scale_color_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Unique Patients by Site over Time - Treatment Visit"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))
    # geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
    #            nudge_x = 0.1, size=5)
    
    
    g22 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE")+
      theme_minimal() +
      table_theme()
    
    g21 + g22 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(unique$SITE))))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Month
  output$uniqueTreatmentMonthSite <- renderPlot({
    
    data <- uniquePts_df_siteMonth(dataArrived(), c("Treatment Visit"))
    # data <- uniquePts_df(arrived.data, c("Treatment Visit"))
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear, SITE) %>%
      summarise(total = n())
    
    #to get the upper limit for the y_continuous
    unique_ <- unique %>% spread(SITE, total)
    unique_$Appt.MonthYear <- NULL
    max_col <- function(data) sapply(data, max, na.rm = TRUE)
    max_tot_site <- max_col(unique_)
    
    g23 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,sum(max_tot_site)*1.2))+
      
      labs(title = paste0("Unique Patients by Site and Month - Treatment Visits"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    g24 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE")+
      theme_minimal() +
      table_theme()
    
    g23 + g24 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(unique$SITE))))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique Patients by Provider - Breakdown Tab -----------------------------------------------------------------------------------
  ## Unique Patients by Provider
  output$uniqueProvExam_tb <- function(){
    
    data <- uniquePts_df_siteProv(dataArrived_disease_2(), c("Exam")) 
    
    uniquePts_tb <- data %>%
      group_by(SITE, Provider, Appt.MonthYear) %>%
      summarise(total = n()) %>%
      arrange(Appt.MonthYear, SITE) %>%
      `colnames<-` (c("Site", "Provider", "Appt.MonthYear", "Total")) %>%
      pivot_wider(names_from = Appt.MonthYear, values_from = Total, values_fill = 0) %>%
      adorn_totals(where = "col", fill = "-", na.rm = TRUE, name = "YTD Total")
    
    row_total <- uniquePts_tb %>% 
      split(.[,"Provider"]) %>%  
      map_df(., janitor::adorn_totals, name = paste0("Provider Month Total"))
    
    kable(uniquePts_tb[,2:length(uniquePts_tb)]) %>%
      pack_rows(index = table(uniquePts_tb$Site), label_row_css = "background-color: #fcc9e9;") %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      add_header_above(c("Unique Patients Seen per Provider - Exam Visits" = (length(uniquePts_tb)-1)),
                       color = "black", font_size = 20, align = "center", line = FALSE) %>%
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      column_spec(1, bold = T) %>%
      column_spec(length(uniquePts_tb)-1, background = "#d80b8c", color = "white", bold = T) 
    
  }
  
  ## Unique Patients by Provider and Month
  output$uniqueProvMonthExam_tb <- function(){
    
    data <- uniquePts_df_siteProvMonth(dataArrived_disease_2(), c("Exam")) 
    
    # data <- uniquePts_df_siteProvMonth(arrivedDisease.data, c("Exam")) %>%
    #   filter(uniqueSiteProvMonth == 1)
    
    uniquePts_tb <- data %>%
      group_by(SITE, Provider, Appt.MonthYear) %>%
      summarise(total = n()) %>%
      arrange(Appt.MonthYear, SITE) %>%
      `colnames<-` (c("Site", "Provider", "Appt.MonthYear", "Total")) %>%
      pivot_wider(names_from = Appt.MonthYear, values_from = Total, values_fill = 0) %>%
      adorn_totals(where = "col", fill = "-", na.rm = TRUE, name = "YTD Total")
    
    row_total <- uniquePts_tb %>% 
      split(.[,"Provider"]) %>%  
      map_df(., janitor::adorn_totals, name = paste0("Provider Month Total"))
    
    
    kable(uniquePts_tb[,2:length(uniquePts_tb)]) %>%
      pack_rows(index = table(uniquePts_tb$Site), label_row_css = "background-color: #c9f0ff;") %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      add_header_above(c("Unique Patients Seen per Provider and Month - Exam Visits" = (length(uniquePts_tb)-1)),
                       color = "black", font_size = 20, align = "center", line = FALSE) %>%
      row_spec(0, background = "#00aeef", color = "white", bold = T) %>%
      column_spec(1, bold = T) %>%
      column_spec(length(uniquePts_tb)-1, background = "#00aeef", color = "white", bold = T) 
    
  }
  
  # Population - Zip Code Analysis Tab --------------------------------------------------------------------------------------------
  ## Bubble Map by Zip Code
  output$zipCode_map <- renderLeaflet({
    
    data <- dataArrivedPop()
    # data <- population.data_filtered
    
    # Set icons for each MSHS hospital
    icons <- awesomeIcons(
      icon = 'hospital-o',
      lib = 'fa',
      iconColor = "white",
      markerColor = "lightgray")
    
    
    if(input$selectedZipCodeMap == "System"){
      
      if(input$selectedZipCodeMap2 == "All"){
        
        newdata <-
          uniquePts_df_system(data, c("Exam","Labs","Treatment")) %>% 
          group_by(latitude, longitude) %>% 
          dplyr::summarise(total = n())
      } else if(input$selectedZipCodeMap2 == "Exam") {
        
        newdata <-
          uniquePts_df_system(data, c("Exam")) %>% 
          group_by(latitude, longitude) %>% 
          dplyr::summarise(total = n())
      } else{
        
        newdata <-
          uniquePts_df_system(data, c("Treatment Visit")) %>% 
          group_by(latitude, longitude) %>% 
          dplyr::summarise(total = n())
      }
      
      # Create a color palette with handmade bins.
      mybins <- round(seq(min(newdata$total), max(newdata$total), length.out=5),0)
      mypalette <- colorBin(palette=MountSinai_palettes$pinkBlue, domain=quakes$mag, na.color="transparent", bins=mybins)
      
      # Prepare the text for the tooltip:
      mytext <- paste(
        "Total Unique Patients: ", newdata$total, "<br/>") %>%
        lapply(htmltools::HTML)
      
      map <-
        leaflet(data = newdata) %>%
        addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
        setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                         fillColor = ~mypalette(total), fillOpacity = 0.9, color="white", stroke=FALSE,
                         # color = ~groupColors(SITE),
                         # group = ~SITE, 
                         radius= ~total^(1/2),
                         label = mytext,
                         labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "18px", direction = "auto")) %>%
        addLegend(pal=mypalette, values=~total, opacity=0.9, title = "Total Unique Patients", position = "topright") 
      
    } else{
      
      if(input$selectedZipCodeMap2 == "All"){
        
        newdata <-
          uniquePts_df_site(data, c("Exam","Labs","Treatment")) %>%
          group_by(SITE, latitude, longitude) %>%
          dplyr::summarise(total = n()) %>%
          filter(!is.na(SITE))
      } else if(input$selectedZipCodeMap2 == "Exam") {
        
        newdata <-
          uniquePts_df_site(data, c("Exam")) %>%
          group_by(SITE, latitude, longitude) %>%
          dplyr::summarise(total = n()) %>%
          filter(!is.na(SITE))
      } else{
        
        newdata <-
          uniquePts_df_site(data, c("Treatment Visit")) %>%
          group_by(SITE, latitude, longitude) %>%
          dplyr::summarise(total = n()) %>%
          filter(!is.na(SITE))
      }
      
      # Create a color palette with handmade bins.
      mybins <- round(seq(min(newdata$total), max(newdata$total), length.out=5),0)
      mypalette <- colorBin(palette=MountSinai_palettes$pinkBlue, domain=quakes$mag, na.color="transparent", bins=mybins)
      
      # List of all variables
      layer_names <- unique(newdata$SITE)
      # Set color scheme based on comparing variable
      groupColors = colorFactor(palette = MountSinai_colors, domain = newdata$SITE)
      # Prepare the text for the tooltip:
      mytext <- paste(
        paste(newdata$SITE," Total Visits: "), newdata$total, "<br/>") %>%
        lapply(htmltools::HTML)
      
      map <-
        leaflet(data = newdata) %>%
        addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
        setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude, color = ~groupColors(SITE),
                         group = ~SITE, radius= ~total^(1/2),
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "18px", direction = "auto")) %>%
        addLegend(position = "bottomright", pal = groupColors, values = ~SITE) %>%
        addLayersControl(overlayGroups = c(newdata$SITE, layer_names))
      
    } 
    
    map %>%
      addAwesomeMarkers(
        lng=-73.943324, lat=40.79171,
        label='Mount Sinai Hospital',
        icon = icons,
        labelOptions = labelOptions(noHide = T, textsize='15px', textOnly = TRUE,
                                    style=list('font-weight'= 'bold'))) %>%
      addAwesomeMarkers(
        lng=-73.92606, lat=40.77084,
        label='Mount Sinai Queens',
        icon = icons,
        labelOptions = labelOptions(noHide = T, textsize='15px', textOnly = TRUE,
                                    style=list('font-weight'= 'bold'))) %>%
      addAwesomeMarkers(
        lng=-73.98840, lat=40.73139,
        label='Mount Sinai Union Square',
        icon = icons,
        labelOptions = labelOptions(noHide = T, textsize='15px', textOnly = TRUE,
                                    style=list('font-weight'= 'bold'))) %>%
      addAwesomeMarkers(
        lng=-73.99181, lat=40.76719,
        label='Mount Sinai West',
        icon = icons,
        labelOptions = labelOptions(noHide = T, textsize='15px', textOnly = TRUE,
                                    style=list('font-weight'= 'bold'))) %>%
      addAwesomeMarkers(
        lng=-73.96316, lat=40.79834,
        label="Mount Sinai Morningside",
        icon = icons,
        labelOptions = labelOptions(noHide = T, textsize='15px', textOnly = TRUE,
                                    style=list('font-weight'= 'bold')))
    
  })
  
  ## Breakdown of Arrived Visits by Zip Code
  output$zipCode_tb <- function(){
    
    data <- dataArrivedPop()
    data <- population.data_filtered
    
    if(input$selectedZipCodeMap == "System"){
      if(input$selectedZipCodeMap2 == "All"){
        
        newdata <- uniquePts_df_system(data, c("Exam","Labs","Treatment")) 
        
      } else if(input$selectedZipCodeMap2 == "Exam") {
        
        newdata <- uniquePts_df_system(data, c("Exam"))  
        
      } else{
        
        newdata <- uniquePts_df_system(data, c("Treatment Visit")) 
        
      }
      
    } else{
      if(input$selectedZipCodeMap2 == "All"){
        
        newdata <- uniquePts_df_site(data, c("Exam","Labs","Treatment")) %>% filter(!is.na(SITE))
        
      } else if(input$selectedZipCodeMap2 == "Exam") {
        
        newdata <- uniquePts_df_site(data, c("Exam")) %>% filter(!is.na(SITE))
        
      } else{
        
        newdata <- uniquePts_df_site(data, c("Treatment Visit")) %>% filter(!is.na(SITE))
        
      }
    } 
    
    a_table <- newdata %>% 
      filter(`Zip Code Layer: A` != "EXCLUDE") %>%
      group_by(`Zip Code Layer: A`) %>% summarise(total = n()) %>%
      arrange(-total) %>%
      mutate(perc = round(total/sum(total),2)*100) %>%
      adorn_totals("row") %>%
      mutate(perc = paste0(perc,"%")) %>%
      `colnames<-` (c("Zip Code Layer", "total", "perc")) %>%
      mutate(Layer = `Zip Code Layer`)
    
    b_table <- newdata %>% 
      filter(`Zip Code Layer: A` != "EXCLUDE") %>%
      group_by(`Zip Code Layer: A`, `Zip Code Layer: B`) %>% summarise(total = n()) %>%
      arrange(-total)
    
    b_table <- b_table %>%
      mutate(perc = round(total/sum(b_table$total),2)*100) %>%
      mutate(perc = paste0(perc,"%")) %>%
      `colnames<-` (c("Layer", "Zip Code Layer", "total", "perc")) %>%
      filter(`Layer` %in% c("Manhattan", "Out of NYS", "Long Island", "Northern New York")) 
    
    zip_table <- bind_rows(a_table, b_table)
    zip_table <- zip_table[order(factor(zip_table$Layer, levels = unique(a_table$Layer))),]
    zip_table$Layer <- NULL
    
    
    # Table subtitle based on date range filter
    manhattan_ref <- which(zip_table$`Zip Code Layer` == "Manhattan")
    out_state_ref <-  which(zip_table$`Zip Code Layer` == "Out of NYS")
    long_is_ref <-  which(zip_table$`Zip Code Layer` == "Long Island")
    northern_ny_ref <-  which(zip_table$`Zip Code Layer` == "Northern New York")
    header_above <- c("Subtitle" = 3)
    names(header_above) <- paste0(c("Based on data from "),c(input$dateRange[1]),c(" to "),c(input$dateRange[2]))
    
    zip_table %>%
      kable(escape = F, 
            col.names = c("Zip Code Layer", "Total Unique Patients", "Percent of Total")) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      add_header_above(header_above, color = "black", font_size = 16, align = "center", italic = TRUE) %>%
      add_header_above(c("Total Unique Patients by Zipcode" = length(zip_table)),
                       color = "black", font_size = 20, align = "center", line = FALSE) %>%
      add_indent(c(manhattan_ref+1, manhattan_ref+2, manhattan_ref+3,
                   out_state_ref+1, out_state_ref+2, out_state_ref+3, out_state_ref+4, out_state_ref+5,
                   long_is_ref+1, long_is_ref+2, 
                   northern_ny_ref+1, northern_ny_ref+2, northern_ny_ref+3, northern_ny_ref+4, northern_ny_ref+5),
                 level_of_indent = 2) %>%
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      row_spec(c(manhattan_ref+1, manhattan_ref+2, manhattan_ref+3, out_state_ref+1, out_state_ref+2, 
                 out_state_ref+3, out_state_ref+4, out_state_ref+5, long_is_ref+1, long_is_ref+2, 
                 northern_ny_ref+1, northern_ny_ref+2, northern_ny_ref+3, northern_ny_ref+4, northern_ny_ref+5), font_size = 14) %>%
      row_spec(nrow(zip_table), background = "#d80b8c", color = "white", bold = T) 
  }
  
  output$zipCode_ref_tb1 <- function(){
    
    section <- c("Upper-Manhattan","Middle-Manhattan","Lower-Manhattan")
    geo <- c("72nd street and above","34th to 72nd street","34th street and below")
    
    data.frame(section,geo) %>%
      kable(escape = F, 
            col.names = c("Manhattan Zip Code Layer: B", "Geography")) %>%
      kable_styling(bootstrap_options = c("bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      row_spec(0, background = "#7f7f7f", color = "white", bold = T) 
  }
  
  output$zipCode_ref_tb2 <- function(){
    
    upstate <- zipcode_ref %>% filter(`Zip Code Layer: A` %in% c("Upstate New York","Out of NYS"))
    upstate <- as.data.frame(unique(upstate[,c("Zip Code Layer: A","Zip Code Layer: C")]))
    upstate <- upstate[order(desc(upstate$`Zip Code Layer: C`)),] 
    upstate <- upstate[order(desc(upstate$`Zip Code Layer: A`)),] 
    rownames(upstate) <- NULL
    
    upstate %>%
      kable(escape = F, 
            col.names = c("Zip Code Layer: A", "State/County")) %>%
      kable_styling(bootstrap_options = c("bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      row_spec(0, background = "#7f7f7f", color = "white", bold = T) %>%
      collapse_rows(1)
  }
  
  # Scheduling - Scheduled/Arrived Tab --------------------------------------------------------------------------
  ## Total Visits Scheduled 
  # output$totalScheduled <- renderValueBox({
  #   
  #   data <- dataAll()
  #   # data <- all.data
  #   
  #   valueBox(
  #     prettyNum(nrow(data), big.mark = ','),
  #     subtitle = tags$p("TOTAL VISITS SCHEDULED", style = "font-size: 130%;"), icon = NULL, color = "yellow"
  #   )
  # })
  # 
  ## Average Daily Visits Scheduled 
  output$avgScheduled <- renderValueBox({
    
    data <- dataAllsched()
    # data <- all.data
    
    valueBox(
      prettyNum(round(nrow(data)/length(unique(data$Appt.DateYear))), big.mark = ','),
      subtitle = tags$p("AVG APOINTMENTS SCHEDULED PER DAY", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  # Average Daily Incomplete Appts
  output$incompleteAppts <- renderValueBox({
    
    valueBox(
      #prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / length(unique(dataArrived()$Appt.DateYear)),0), big.mark = ","),
      paste0(prettyNum(round(nrow(dataArrivedNoShowsched() %>% filter(Appt.Status != "Arrived")) / 
                        nrow(dataArrivedNoShow()), 2)*100, big.mark = ","), "%"),
      subtitle = tags$p("% of INCOMPLETE APPOINTMENTS", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  output$schedulingStatusSummary <- renderPlot({
    
    data <- dataArrivedNoShowsched()
    # data <- kpi.all.data[arrivedNoShow.data.rows,]
    
    # sameDay <- data %>%
    #   group_by(Appt.Status) %>%
    #   summarise(value = round(n()/length(unique(kpi.all.data[arrived.data.rows,]$Appt.DateYear)))) %>%
    #   arrange(desc(value)) 
    
    
    sameDay <- data %>%
      group_by(Appt.Status) %>%
      summarise(value = round(n()/length(unique(dataArrived()$Appt.DateYear)))) %>%
      arrange(desc(value)) 
    
    sameDay$Appt.Status <- as.character(sameDay$Appt.Status)
    
    sameDay$Appt.Status[which(sameDay$Appt.Status == "Bumped")] <- "Same-day Bumped"
    sameDay$Appt.Status[which(sameDay$Appt.Status == "Canceled")] <- "Same-day Canceled"
    sameDay$Appt.Status[which(sameDay$Appt.Status == "Rescheduled")] <- "Same-day Rescheduled"
    
    ggplot(sameDay, aes(reorder(Appt.Status, -value), value, fill=Appt.Status)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(limits=c(0,(max(sameDay$value))*1.3))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      labs(x=NULL, y=NULL,
           title = "Avg Daily No Shows and Same-day \nBumped/Canceled/Rescheduled Appointments",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+ theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label=prettyNum(value, big.mark = ',')), hjust = 0.5, vjust = -1, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  
  

  
  # ## Average Daily Visits Arrived
  # output$avgArrived <- renderValueBox({
  #   
  #   data <- dataAll()
  #   # data <- all.data
  #   
  #   valueBox(
  #     prettyNum(round(nrow(data %>% filter(Appt.Status == "Arrived"))/length(unique(data$Appt.DateYear))), big.mark = ','),
  #     subtitle = tags$p("AVG DAILY ARRIVED VISITS", style = "font-size: 130%;"), icon = NULL, color = "yellow"
  #   )
  # })
  # 
  # ## Average Daily Visits No Showed 
  # output$avgNoShows <- renderValueBox({
  #   
  #   data <- dataArrivedNoShow() %>% filter(Appt.Status == "No Show")
  #   # data <- all.data
  #   
  #   valueBox(
  #     prettyNum(round(nrow(data %>% filter(Appt.Status != "Arrived"))/length(unique(dataArrived()$Appt.DateYear))), big.mark = ','),
  #     subtitle = tags$p("AVG DAILY NO SHOW VISITS", style = "font-size: 130%;"), icon = NULL, color = "yellow"
  #   )
  # })
  
  ## Average Scheduled vs. Arrived Patients Pattern
  output$avgPtArrival <- renderPlot({

    data <- dataArrivedNoShowsched()
    # data <- arrivedNoShow.data
    
    ptsScheduled <- data %>%
      group_by(Appt.TM.Hr) %>%
      summarise(Total = n()) %>%
      mutate(Scheduled = round(Total / length(unique(data$Appt.DateYear))))
    
    ptsArrived <- data %>%
      filter(Appt.Status == "Arrived") %>%
      group_by(Appt.TM.Hr) %>%
      summarise(Total = n()) %>%
      mutate(Arrived = round(Total / length(unique(data$Appt.DateYear))))
    
    ptsByHour <- as.data.frame(timeOptionsHr) 
    ptsByHour <- merge(ptsByHour,ptsScheduled, by.x = "timeOptionsHr", by.y = "Appt.TM.Hr", all.x = TRUE)  
    ptsByHour <- merge(ptsByHour, ptsArrived, by.x = "timeOptionsHr", by.y = "Appt.TM.Hr", all.x = TRUE)
    ptsByHour[is.na(ptsByHour)] <- 0
    
    names(ptsByHour) <- c("Time","Total Scheduled","Scheduled","Total Arrived","Arrived")
    avgPtsByHour <- ptsByHour[,c("Time","Scheduled","Arrived")]
    
    avgPtsByHour <- reshape2::melt(avgPtsByHour, id="Time", measure = c("Scheduled","Arrived"))
    
    avgPtsByHour <- avgPtsByHour %>% filter(Time %in% timeOptionsHr_filter)
    
    # Scheduled vs. Actual Arrival in Hour Interval 
    ggplot(avgPtsByHour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(aes(linetype=variable), size=1.2)+
      scale_linetype_manual(values=c("dashed","solid"))+
      scale_color_manual(values=c("maroon1","midnightblue"))+
      labs(x=NULL, y=NULL,
           title = "Average Scheduled* vs. Arrived Patients",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "*Scheduled = Arrived + No Show Patients")+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+ theme(legend.title = element_blank())
    
  })
  
  
  # Patient Arrivals by Day of Week 
  output$ptArrivalPattern <- renderPlot({
    
    data <- dataArrivedsched()
    # data <- arrived.data
    
    arrived <- data %>%
      group_by(Appt.DateYear, Appt.Day, Appt.TM.Hr) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.Day, Appt.TM.Hr) %>%
      dplyr::summarise(avg = round(mean(total),0))
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(arrived$Day)),]
    
    arrived <- as.data.frame(merge(byDayTime.df,arrived, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE))
    arrived[is.na(arrived)] <- 0
    
    arrived <- arrived %>% filter(Time %in% timeOptionsHr_filter)
    
    graph <- 
      ggplot(arrived, aes(x=Time, y=avg, col=factor(Day,level = daysOfWeek.options), group=Day))+
      geom_line(size=1.2)+
      labs(x=NULL, y=NULL,
           title = "Average Patients Arrived* by Time of Day and Day of Week",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "*Based on scehduled appointment time.")+
      scale_color_MountSinai("main")+
      guides(colour = guide_legend(nrow = 1))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+ theme(legend.title = element_blank())
    
    table <- 
      ggplot(arrived, aes(x=factor(Day, levels = rev(daysOfWeek.options)), y=Time))+
      labs(x=NULL, y=NULL)+
      geom_tile(aes(fill=avg), colour = "black", size=0.5)+
      coord_flip()+
      scale_fill_gradient2(midpoint = median(unique(arrived$avg)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Patients\nArrived")+
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
            plot.margin = margin(10,30,30,30))+
      geom_text(aes(label= ifelse(is.na(avg),"", round(avg,1))), color="black", size=5, fontface="bold")
    
    grid.arrange(graph, table, ncol = 1, heights = c(5,3))
    
  })
  
  # Scheduling - No Shows/Overbooks Tab --------------------------------------------------------------------------
  ## Average Daily No Show
  output$avgNoShows2 <- renderValueBox({
    
    data <- dataArrivedNoShowsched() %>% filter(Appt.Status %in% c("No Show"))
    # data <- all.data
    
    valueBox(
      prettyNum(round(nrow(data)/length(unique(dataArrived()$Appt.DateYear))), big.mark = ','),
      subtitle = tags$p("AVG DAILY NO SHOW VISITS", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  ## Average Daily No Show %
  output$avgNoShowsPerc <- renderValueBox({
    
    data <- dataArrivedNoShowsched() 
    # data <- arrivedNoShow.data
    
    valueBox(
      paste0(round((nrow(data %>% filter(Appt.Status == "No Show"))/nrow(data %>% filter(Appt.Status %in% c("Arrived","No Show"))))*100), "%"),
      subtitle = tags$p("AVG DAILY NO SHOW %", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  ## Average Daily Overbooks
  output$avgOverbooks <- renderValueBox({
    
    data <- dataArrivedNoShowsched()
    overbooks <- data %>%
      group_by(Provider, Appt.DateYear, Time) %>%
      dplyr::summarise(total = n()-1) %>%
      filter(total > 1) 
    
    valueBox(
      prettyNum(round(sum(overbooks$total)/length(unique(data$Appt.DateYear))), big.mark = ','),
      subtitle = tags$p("AVG DAILY OVERBOOKS", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  ## Average Daily Overbooks per Provider
  output$avgOverbooksProv <- renderValueBox({
    
    data <- dataArrivedNoShowsched()
    # data <- arrivedNoShow.data
    
    overbooks <- data %>%
      group_by(Provider, Appt.DateYear, Time) %>%
      dplyr::summarise(total = n()-1) %>%
      filter(total > 1) 

    valueBox(
      round(sum(overbooks$total)/length(unique(data$Appt.DateYear))/length(unique(data$Provider)), 1),
      subtitle = tags$p("AVG DAILY OVERBOOKS PER PROVIDER", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  ## No Show Breakdown 
  output$noShowBreakdown <- renderPlot({

    data <- dataArrivedNoShowsched()
    # data <- arrivedNoShow.data

    data$Appt.Status[which(data$Appt.Status == "Bumped")] <- "Same-day Bumped"
    data$Appt.Status[which(data$Appt.Status == "Canceled")] <- "Same-day Canceled"
    data$Appt.Status[which(data$Appt.Status == "Rescheduled")] <- "Same-day Rescheduled"
    
    breakdown <- data %>%
      filter(Appt.Status != "Arrived") %>%
      group_by(Appt.Status) %>%
      summarise(Total = n()) %>%
      mutate(Appt.Status = fct_reorder(Appt.Status, Total),
        avg = round(Total/length(unique(data$Appt.DateYear))),
             prop = (avg / sum(avg))*100) %>%
      arrange(-prop) %>%
      mutate(ypos = cumsum(prop))

    ggplot(breakdown, aes(x=1, y=prop, fill=Appt.Status)) +
      geom_col() +
      geom_text(aes(x=1,y = ypos-prop/2, label=paste0(avg,"\n(",round(prop),"%)"))) +
      coord_polar("y", start=0) +
      theme_void()+
      scale_fill_MountSinai('blue')+
      labs(x=NULL, y=NULL,
           title = "No Show Breakdown",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
      theme(plot.title = element_text(face = "bold", size = 20, hjust=0.5),
            plot.subtitle = element_text(size = 15, face = "italic",hjust=0.5))

  })

  
  ## No Shows by Time of Day and Day of Week 
  output$avgNoShowsDist <- renderPlot({

    data <- dataArrivedNoShowsched() %>% filter(Appt.Status %in% c("Arrived","No Show"))
    # data <- arrivedNoShow.data
    
    if(input$percent1 == FALSE){
      
      noShow_count <- data %>%
        filter(Appt.Status %in% "No Show") %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        dplyr::summarise(Total = n()) 
      
      days <- unique(data[,c("Appt.Day","Appt.DateYear")])
      days <- days %>% group_by(Appt.Day) %>% dplyr::summarise(days = n())
      
      noShow_count$days <- days$days[match(noShow_count$Appt.Day, days$Appt.Day)]
      noShow_count$avgNoShows <- round(noShow_count$Total/noShow_count$days,0)
      
      noShow_count.df <- byDayTime.df %>% filter(Day %in% unique(noShow_count$Appt.Day))
      noShow_count.df <- merge(noShow_count.df, noShow_count, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE)
      
      noShow_count.df <- noShow_count.df %>% filter(Time %in% timeOptionsHr_filter)
      
      noShowDist <-
        ggplot(noShow_count.df, aes(x=factor(Day, levels = daysOfWeek.options), y=Time))+
        scale_y_discrete(limits = rev(unique(sort(noShow_count.df$Time))))+
        labs(x=NULL, y=NULL,
             title = "Average Daily No Shows*",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
             caption = "*No Show Rate = No Show / (Arrived + No Show)")+
        geom_tile(aes(fill=avgNoShows), colour = "black", size=0.5)+
        geom_text(aes(label= ifelse(is.na(avgNoShows),"",avgNoShows)), color="black", size=5, fontface="bold")
        
    }else{
      
      noShow_perc <- data %>%
        group_by(Appt.DateYear, Appt.Day, Appt.TM.Hr, Appt.Status) %>%
        dplyr::summarise(Total = n())
      
      noShow_perc <- reshape2::dcast(noShow_perc, Appt.Day +  Appt.TM.Hr ~ Appt.Status, sum) 
      noShow_perc <- mutate(noShow_perc, percentage = round((`No Show` / (Arrived + `No Show`))*100,0))
      
      noShow_perc.df <- byDayTime.df %>% filter(Day %in% unique(noShow_perc$Appt.Day))
      noShow_perc.df <- merge(noShow_perc.df, noShow_perc, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE)
      
      noShow_perc.df <- noShow_perc.df %>% filter(Time %in% timeOptionsHr_filter)
      
      noShowDist <-
        ggplot(noShow_perc.df, aes(x=factor(Day, levels = daysOfWeek.options), y=Time))+
        scale_y_discrete(limits = rev(unique(sort(noShow_perc.df$Time))))+
        labs(x=NULL, y=NULL,
             title = "Average Daily Percent of No Show*",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
             caption = "*No Show Rate = No Show / (Arrived + No Show)")+
        geom_tile(aes(fill=percentage), colour = "black", size=0.5)+
        geom_text(aes(label= ifelse(is.na(percentage),"",paste0(percentage,"%"))), color="black", size=5, fontface="bold")
     
    }
    
    noShowDist + 
      scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="No Shows")+
      scale_x_discrete(position = "top")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14),
            plot.caption = element_text(size = 12, face = "italic"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(1,"cm"),
            legend.text = element_text(size="12"),
            axis.title.x = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_text(color="black", vjust=0.5, hjust = 0.5, margin = margin(b=15, t=100)),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
  })
  
  ## Overbooks by Time of Day and Day of Week 
  output$avgOverbooksDist <- renderPlot({
    
    data <- dataArrivedNoShowsched()
    # data <- arrivedNoShow.data
    
    daily.overbooks <- data %>%
      group_by(Provider, Appt.DateYear, Time, Appt.Day, Appt.TM.Hr) %>%
      dplyr::summarise(total = n()-1) %>%
      filter(total > 1) %>%
      group_by(Appt.Day, Appt.TM.Hr) %>%
      dplyr::summarise(total = sum(total))

    days <- unique(data[,c("Appt.Day","Appt.DateYear")])
    days <- days %>% group_by(Appt.Day) %>% dplyr::summarise(days = n())
    
    daily.overbooks$days <- days$days[match(daily.overbooks$Appt.Day, days$Appt.Day)]
    daily.overbooks$avgOverbooks <- round(daily.overbooks$total/daily.overbooks$days,0)
    
    daily.overbooks.df <- byDayTime.df %>% filter(Day %in% unique(daily.overbooks$Appt.Day))
    daily.overbooks.df <- merge(daily.overbooks.df, daily.overbooks, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE)
    
    daily.overbooks.df$avgOverbooks[is.na(daily.overbooks.df$avgOverbooks)] <- 0
    
    daily.overbooks.df <- daily.overbooks.df %>% filter(Time %in% timeOptionsHr_filter)
    
    ggplot(daily.overbooks.df, aes(x=factor(Day, levels = daysOfWeek.options), y=factor(Time, levels = rev(timeOptionsHr))))+
      geom_tile(aes(fill=avgOverbooks), colour = "black", size=0.5)+
      scale_fill_gradient(low = "white", high = "#63be7b", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="Overbooks* ")+
      scale_x_discrete(position = "top")+
      labs(x=NULL, y=NULL,
           title = "Average Daily Overbooks*",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "*Overbooks are defined as all Arrived/No Show Appointments \nscheduled in the same time slots.")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14),
            plot.caption = element_text(size = 12, face = "italic"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(1,"cm"),
            legend.text = element_text(size="12"),
            axis.title.x = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_text(color="black", vjust=0.5, hjust = 0.5, margin = margin(b=15, t=100)),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())+
      geom_text(aes(label= ifelse(is.na(avgOverbooks),"",avgOverbooks)), color="black", size=6)
  })
  
  # Distribution of No Shows (%) by Lead Days 
  output$noShowLeadDays <- renderPlot({
    
    data <- dataArrivedNoShowsched() %>% filter(Appt.Status %in% c("Arrived", "No Show")) %>%
      mutate(apptLeadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Made.DTTM,  units = "days"),2))) 

    # data <- historical.data[arrivedNoShow.data.rows,] %>% filter(Appt.Status %in% c("Arrived", "No Show")) %>%
    #   mutate(apptLeadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Made.DTTM,  units = "days"),2))) 
    # 
    data$Appt.Status <- ifelse(data$Appt.Status == "Arrived","Arrived","No Show")
    
    noShows <- 
      data %>%
      mutate(apptLeadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Made.DTTM,  units = "days"),2))) %>%
      mutate(apptLeadDays = ifelse(is.na(apptLeadDays),0, apptLeadDays)) %>%
      mutate(apptLeadDays = ifelse(apptLeadDays > 14, "> 14 days",
                                   ifelse(apptLeadDays <= 14 & apptLeadDays>= 8, "8-14 days",
                                          ifelse(apptLeadDays <= 7 & apptLeadDays >= 1, "1-7 days",
                                                 ifelse(apptLeadDays < 0, "0 day","0 day")))))
    
    noShows <- reshape2::dcast(noShows, apptLeadDays + Appt.DateYear ~ Appt.Status)
    noShows$noShow_perc <- round(noShows$`No Show`/ (noShows$`No Show` + noShows$Arrived),2)
    noShows$noShow_perc[!is.finite(noShows$noShow_perc)] <- 0
    
    status <- c('0 day','1-7 days','8-14 days','> 14 days')
    
    noShows_bar_tb <-
      noShows %>%
      group_by(apptLeadDays) %>%
      dplyr::summarise(Arrived = sum(Arrived),
                       `No Show` = sum(`No Show`)) %>%
      mutate(Average = round(`No Show`/(Arrived + `No Show`),2)) %>%
      select(apptLeadDays, Average)
    
    noShows_bar_tb <-
      reshape2::melt(noShows_bar_tb, id.vars = c("apptLeadDays"))
    
    ggplot(noShows_bar_tb, aes(x=factor(apptLeadDays, levels = status), y=value,fill=variable)) +
      geom_bar(stat="identity", position=position_dodge(), width = 0.8, fill="#f9878a", color="red") +
      labs(x=NULL, y = "% of Total Bumps",
           # caption = "*No Show includes no show and same-day bumped,
           # canceled, and rescheduled appointments.",
           title = "Average No Show Rate by \nLead Days to Appointment*",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
      scale_y_continuous(labels=scales::percent_format(accuracy=1),limits = c(0,max(noShows_bar_tb$value)*1.2))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      theme(plot.caption = element_text(size=12, face="italic"),
            axis.text.x = element_text(size = 16, angle=0, hjust=.50))+
      geom_text(aes(label=paste0(value*100,"%")), vjust = -1, hjust = .5, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  # Bumps/Cancellations Tab -----------------------------------------------------------------
  ## AVerage Daily Bumped Appts
  output$avgDailyBumps <- renderValueBox({
    
    data <- dataAllsched() 
    # data <- all.data %>% filter(Appt.Status %in% c("Bumped","Canceled","Rescheduled"))
    
    valueBox(
      prettyNum(round(nrow(data %>% filter(Appt.Status == "Bumped"))/length(unique(data$Appt.DateYear))),big.mark=","), 
      subtitle = tags$p("Avg Daily Bumped Appointments", style = "font-size: 160%;"), icon = NULL, color = "yellow"
    )
  })
  
  ## AVerage Daily Canceled Appts
  output$avgDailyCanc <- renderValueBox({
    
    data <- dataAllsched() 
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(round(nrow(data %>% filter(Appt.Status == "Canceled"))/length(unique(data$Appt.DateYear))),big.mark=","), 
      subtitle = tags$p("Avg Daily Canceled Appointments", style = "font-size: 160%;"), icon = NULL, color = "yellow"
    )
  })
  
  ## Average Daily Rescheduled Appts
  output$avgDailyResc <- renderValueBox({
    
    data <- dataAllsched() 
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(round(nrow(data %>% filter(Appt.Status == "Rescheduled"))/length(unique(data$Appt.DateYear))),big.mark=","), 
      subtitle = tags$p("Avg Daily Rescheduled Appointments", style = "font-size: 160%;"), icon = NULL, color = "yellow"
    )
  })
  
  ## Average Bumps/Canc/Resc Rate 
  output$avgBumpsCancRescRate <- renderPlot({
    
    data <- dataAllsched()
    # data <- all.data
    
    apptsCanceled <- data %>%
      group_by(Appt.Status) %>%
      summarise(value = n()) %>%
      arrange(desc(value)) %>%
      mutate(percent = round((value/sum(value)), 2)) %>%
      select(Appt.Status, percent) %>%
      filter(Appt.Status %in% c("Bumped", "Canceled", "Rescheduled"))
    
    ggplot(apptsCanceled, aes(reorder(Appt.Status, -percent), percent, fill=Appt.Status)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,(max(apptsCanceled$percent))*1.5))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      labs(x=NULL, y=NULL,
           title = "Bumped/Canceled/Rescheduled Rate*",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "*% of Total Appointments Scheduled.")+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+ theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label=paste0(round(percent*100),"%")), hjust = 0.5, vjust = -1, color="black", fontface="bold",
                position = position_dodge(1), size=5)
      
    
  })
  
  ## Lead Days to Bumps/Canc/Resc 
  output$leadDaysBumpsCancResc <- renderPlot({
    
    data <- dataAllsched() %>% filter(Appt.Status %in% c("Bumped","Canceled","Rescheduled"))
    # data <- canceled.bumped.rescheduled.data
    
    lead.days.df <- data %>%
      filter(Lead.Days >= 0) %>%
      mutate(leadDays = ifelse(Lead.Days> 14, "> 14 days", 
                               ifelse(Lead.Days <= 14 & Lead.Days >= 8, "8-14 days",
                                      ifelse(Lead.Days < 8 & Lead.Days >= 1, "1-7 days",
                                             ifelse(Lead.Days < 0, "0 day","0 day"))))) %>%
      group_by(leadDays,Appt.Status) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.Status) %>%
      mutate(perc = total/sum(total))
    
    ggplot(lead.days.df, aes(x=Appt.Status, y=perc, fill=factor(leadDays, levels=c("0 day","1-7 days","8-14 days","> 14 days"))))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_manual(values=c("grey","#00aeef","#d80b8c","midnightblue"))+
      labs(x=NULL, y=NULL,
           title = "% of Bumped/Canceled/Rescheduled \nAppointments by Lead Days*",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "*Time from appointment scheduled to status changed.")+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      guides(colour = guide_legend(nrow = 1))+
      geom_text(aes(label=paste0(round(perc*100),"%")), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
        plot.caption = element_text(size = 12, face = "italic"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = "14"),
        strip.text = element_text(size=14),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle=0, hjust=0.5),
        axis.text.y = element_text(size = 14),
        axis.line.x = element_blank())+
      coord_flip()
    
  })
  
  
  ## Average Daily Same-day Bumps/Canc/Resc Rate 
  output$sameDayBumpedCanceledRescheduled <- renderPlot({
    
    data <- dataArrivedNoShowsched() %>% filter(Appt.Status %in% c("Bumped","Canceled","Rescheduled"))
    # data <- historical.data[arrivedNoShow.data.rows, ] %>% filter(Appt.Status %in% c("Bumped","Canceled","Rescheduled"))
    
    sameDay <- data %>%
      group_by(Appt.Status) %>%
      summarise(total = n()) %>%
      mutate(avg = round(total/length(unique(dataAll()$Appt.DateYear))))
    
    ggplot(sameDay, aes(reorder(Appt.Status, -avg), avg, fill=Appt.Status)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(limits=c(0,(max(sameDay$avg))*1.5))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      labs(x=NULL, y=NULL,
           title = "Average Daily Same-day \nBumped/Canceled/Rescheduled Appointments",
           # subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "*Appointment status changed on the day of appoinment.")+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+ theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label=avg), hjust = 0.5, vjust = -1, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  
  ## Bumped Reasons by Lead Days
  output$reasonsBumps <- renderPlot({
    
    data <- dataBumpedsched()
    # data <- historical.data[bumped.data.rows,]
    
    total <- nrow(data)
    
    bumps <- 
      data %>%
      mutate(leadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Cancel.DTTM,  units = "days"),2))) %>% #(REMOVE)
      mutate(leadDays = ifelse(is.na(leadDays),0,leadDays)) %>%
      mutate(leadDays = ifelse(leadDays > 14, "> 14 days", 
                               ifelse(leadDays <= 14 & leadDays>= 8, "8-14 days",
                                      ifelse(leadDays < 8 & leadDays >= 1, "1-7 days",
                                             ifelse(leadDays < 0, "0 day","0 day"))))) %>%
      group_by(leadDays,Cancel.Reason) %>%
      dplyr::summarise(total = n()) %>%
      arrange(leadDays,desc(total))
    
    bumps$percent <- round(bumps$total/total,2)*100
    
    top10 <- bumps %>% 
      group_by(Cancel.Reason) %>% 
      dplyr::summarise(total = sum(total)) %>% 
      arrange(desc(total)) %>%
      head(10)
    
    top10 <- as.vector(top10$Cancel.Reason)
    
    if(input$percent2 == FALSE){
      
      graph <- 
        ggplot(bumps %>% filter(Cancel.Reason %in% top10), 
               aes(Cancel.Reason, factor(leadDays, levels=c("0 day","1-7 days","8-14 days","> 14 days")), fill = total))+
        geom_tile(color = "black")+
        coord_flip()+
        labs(x=NULL, y=NULL,
             title = "Top 10 Bumped Reasons by Lead Days",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        scale_fill_gradient(low = "white", high = "#d80b8c", space = "Lab", na.value = "#dddedd", guide = "colourbar", 
                            name="Total Bumped Appointments\n by Lead Days")+
        geom_text(aes(label= ifelse(is.na(total),"",total)), color="black", size=5)
      
    }else{
      
      graph <- 
        ggplot(bumps %>% filter(Cancel.Reason %in% top10), 
               aes(Cancel.Reason, factor(leadDays, levels=c("0 day","1-7 days","8-14 days","> 14 days")), fill = percent))+
        geom_tile(color = "black")+
        coord_flip()+
        labs(x=NULL, y=NULL,
             title = "Top 10 Bumped Reasons by Lead Days",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        scale_fill_gradient(low = "white", high = "#d80b8c", space = "Lab", na.value = "#dddedd", guide = "colourbar", 
                           name="% of Total Bumped \nAppointments by Lead Days")+
        geom_text(aes(label= ifelse(is.na(percent),"",paste0(percent,"%"))), color="black", size=5, fontface="bold")
      
    }
    
    graph +
      scale_x_discrete(labels = wrap_format(25))+
      theme_minimal()+ # minimal theme
      theme(plot.title = element_text(face = "bold", size = 20, hjust=0.5),
            plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
            legend.position = "top",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, colour = "black"),
            axis.text.y = element_text(size = 14, colour = "black"))
    
  })
  
  
  ## Canceled Reasons by Lead Days
  output$reasonsCanc <- renderPlot({
    
    data <- dataCanceledsched()
    # data <- canceled.data
    
    total <- nrow(data)
    
    cancellations <- 
      data %>%
      mutate(leadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Cancel.DTTM,  units = "days"),2))) %>% #(REMOVE)
      mutate(leadDays = ifelse(is.na(leadDays),0,leadDays)) %>%
      mutate(leadDays = ifelse(leadDays > 14, "> 14 days", 
                               ifelse(leadDays <= 14 & leadDays>= 8, "8-14 days",
                                      ifelse(leadDays < 8 & leadDays >= 1, "1-7 days",
                                             ifelse(leadDays < 0, "0 day","0 day"))))) %>%
      group_by(leadDays,Cancel.Reason) %>%
      dplyr::summarise(total = n()) %>%
      arrange(leadDays,desc(total))
    
    cancellations$percent <- round(cancellations$total/total,2)*100
    cancellations$Cancel.Reason[which(is.na(cancellations$Cancel.Reason))] <- "No Reasons Recorded"
    
    top10 <- cancellations %>% 
      group_by(Cancel.Reason) %>% 
      dplyr::summarise(total = sum(total)) %>% 
      arrange(desc(total)) %>%
      head(10)
    
    top10 <- as.vector(top10$Cancel.Reason)
    
    if(input$percent2 == FALSE){
      
      graph <- 
        ggplot(cancellations %>% filter(Cancel.Reason %in% top10), 
               aes(Cancel.Reason, factor(leadDays, levels=c("0 day","1-7 days","8-14 days","> 14 days")), fill = total))+
        geom_tile(color = "black")+
        coord_flip()+
        labs(x=NULL, y=NULL,
             title = "Top 10 Canceled Reasons by Lead Days",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        scale_fill_gradient(low = "white", high = "#00aeef", space = "Lab", na.value = "#dddedd", guide = "colourbar", 
                            name="Total Canceled Appointments\n by Lead Days")+
        geom_text(aes(label= ifelse(is.na(total),"",total)), color="black", size=5)
      
    }else{
      
      graph <- 
        ggplot(cancellations %>% filter(Cancel.Reason %in% top10), 
               aes(Cancel.Reason, factor(leadDays, levels=c("0 day","1-7 days","8-14 days","> 14 days")), fill = percent))+
        geom_tile(color = "black")+
        coord_flip()+
        labs(x=NULL, y=NULL,
             title = "Top 10 Canceled Reasons by Lead Days",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        scale_fill_gradient(low = "white", high = "#00aeef", space = "Lab", na.value = "#dddedd", guide = "colourbar", 
                            name="% of Total Canceled \nAppointments by Lead Days")+
        geom_text(aes(label= ifelse(is.na(percent),"",paste0(percent,"%"))), color="black", size=5)
    }
    
    graph +
      scale_x_discrete(labels = wrap_format(25))+
      theme_minimal()+ # minimal theme
      theme(plot.title = element_text(face = "bold", size = 20, hjust=0.5),
            plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
            legend.position = "top",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, colour = "black"),
            axis.text.y = element_text(size = 14, colour = "black"))
    
  })
  
  # Access Tab -----------------------------------------------------------------------------------
  ## Booked and Filled Rate
  
  
  
  
  
 
  
  
  
  
} # Close Server





