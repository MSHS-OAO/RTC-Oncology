server <- function(input, output, session) {
  
  output$data_download <- renderUI({
    if(user() %in% download_list){
      menuItem("Data Download", tabName = "download", icon = icon("download"))
    }
  })
  
  user <- reactive({
    if(!is.null(session$user)){
    name <- session$user
    } else {
      name <- "user"
    }
    #"villea04"
  })
  

  observeEvent(input$remove_filters,{
    if(is.null(input$filter_list)){
      shinyalert("No preset selected.", type = "error")
      #showNotification("Please provider a name", duration = 5, type = "error")
    }else{
      
      user <- user()
      filter_path_full <- paste0(filter_path, "/", user,"/",input$filter_list,".csv")
      filter_path_half <- paste0(filter_path, "/", user)
      if (file.exists(filter_path_full)) {
        #Delete file if it exists
        file.remove(filter_path_full)
      }
      
      filter_choices <- file_path_sans_ext(list.files(path = filter_path_half, pattern = "*.csv"))
      updatePickerInput(session, "filter_list", choices = filter_choices)
    }
    
    print(input$filter_list)
    
    
    
  })

  observeEvent(input$save_filters,{
    user <- user()
    
    print("collect")
    choices <- oncology_filters_tbl %>% summarise(choices_unique = unique(FILTER_NAME)) %>% collect()
    choices <- sort(choices$choices_unique, na.last = T)
    print("after collect")
    
    filter_name <- input$filter_name
    if(filter_name == ""){
      shinyalert("Please provide a name.", type = "error")
      #showNotification("Please provider a name", duration = 5, type = "error")
    } else if (filter_name %in% choices){
      shinyalert("The current name already exists, please provide a new one.", type = "error")
    } else{
      updateTextInput(session, "filter_name", value = "")
      print(filter_name)


      campus <- input$selectedCampus
      department <- input$selectedDepartment
      days <- input$daysOfWeek
      holidays <- input$excludeHolidays
      diagnosis <- input$diag_grouper
      
      dept_title_text <- input$dept_text
      print(dept_title_text)
      dept_title_text_selected <- gsub("/.*", "\\1", dept_title_text)
      dept_title_text_total <-  gsub(".*/(.+) .*", "\\1", dept_title_text)
      
      
      if(dept_title_text_selected == dept_title_text_total && length(department) > 1) {
        department <- "All"
      }
      
      diagnosis_title_text <- input$diagnosis_text
      print(diagnosis_title_text)
      diagnosis_title_text_selected <- gsub("/.*", "\\1", diagnosis_title_text)
      diagnosis_title_text_total <-  gsub(".*/(.+) .*", "\\1", diagnosis_title_text)
      
      
      if(diagnosis_title_text_selected == diagnosis_title_text_total && length(diagnosis) > 1) {
        diagnosis <- "All"
      }
      
      
      if (length(holidays) == 0) {
        holidays <- "none"
      }
      
      
      filter_df <- mapply(c, filter_name, campus,
                          department,days, holidays,
                          diagnosis,
                          SIMPLIFY = TRUE)
      filter_df <- as.data.frame(t(filter_df), row.names = FALSE)
      colnames(filter_df) <- c("Name", "Campus","Department", "Days", "Holiday", "Diagnosis_Grouper")
      write_filters_db(filter_df)
      
      filter_list_choices <- oncology_filters_tbl %>% summarise(choices = unique(FILTER_NAME)) %>% collect()
      filter_list_choices <- sort(filter_list_choices$choices, na.last = T)
      
      print("after collect")
      
      updatePickerInput(session,
                        inputId = "filter_list",
                        choices = filter_list_choices
      )
      
      filter_df
    }
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  
  observeEvent(input$filter_list, {
    filter_name <- input$filter_list
    # filter_name <- "test_not_all"
    filter_saved_all <- oncology_filters_tbl %>% filter(FILTER_NAME == filter_name) %>% collect()
    
    campus_selected <- unique(filter_saved_all$CAMPUS)
    updatePickerInput(session, "selectedCampus", selected = campus_selected)
    
    
    date_1 <- input$dateRange[1]
    date_2 <- input$dateRange[2]
    
    departments_selected <- unique(filter_saved_all$DEPARTMENT)
    department_choices <- oncology_tbl %>% filter(SITE %in% campus_selected) %>%
                          # filter(TO_DATE(date_1, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR ,
                          #          TO_DATE(date_2, "YYYY-MM-DD HH24:MI:SS") >= APPT_DATE_YEAR)%>% 
                            select(DEPARTMENT_NAME) %>% 
                            mutate(DEPARTMENT_NAME = unique(DEPARTMENT_NAME)) %>%
                            collect()
    department_choices <- sort(department_choices$DEPARTMENT_NAME, na.last = T)
    
    if(c("All") %in% departments_selected) {
      departments_selected <- department_choices
      print("TRUE")
    }

    
    updatePickerInput(session,
                      inputId = "selectedDepartment",
                      choices = department_choices,
                      selected = departments_selected
    )
    
    diag_grouper_selected <-  unique(filter_saved_all$DIAGNOSIS_GROUPER)
    if(c("All") %in% diag_grouper_selected) {
      diag_grouper_selected <- c(default_diag_grouper, "NA")
      print("TRUE")
    }
    
    if(c("NA") %in% diag_grouper_selected) {
      diag_grouper_selected <- c(diag_grouper_selected, "NA")
      print("TRUE")
    }
    updatePickerInput(session,
                      inputId = "diag_grouper",
                      selected = diag_grouper_selected
    )
    
    days_selected <- unique(filter_saved_all$DAYS)
    updatePickerInput(session,
                      inputId = "daysOfWeek",
                      selected = days_selected
    )
    
    
  })
  
  observeEvent(input$update_filters1,{
    
    updatePickerInput(session, "filter_list", selected = NA)
    
  })
  
  
  
  # observeEvent(input$filter_list, {
  #   user <- user()
  #   
  #   filter_path <- paste0(filter_path, "/", user, "/", input$filter_list, ".csv")
  #   filter_df <- read_csv(filter_path)
  #   
  #   campus_selected <- unique(filter_df$Campus)
  #   department_selected <- unique(filter_df$Department)
  #   day_selected <- unique(filter_df$Days)
  #   date_range1_trend <- unique(filter_df$Daterange_1_trend)
  #   date_range2_trend <- unique(filter_df$Daterange_2_trend)
  #   
  #   date_range1 <- unique(filter_df$Daterange_1)
  #   date_range2 <- unique(filter_df$Daterange_2)
  #   
  # 
  #   department_choices <- sort(unique(historical.data[historical.data$SITE %in% default_campus, "Department"]), na.last = TRUE) 
  #   
  #  
  #   
  #   
  #  updateDateRangeInput(session,
  #                       inputId = "dateRangetrend",
  #                       start = date_range1_trend,
  #                       end = date_range2_trend)
  #  
  #  updateDateRangeInput(session,
  #                       inputId = "dateRange",
  #                       start = date_range1,
  #                       end = date_range2)
  #  
  #   updatePickerInput(session,
  #                     inputId = "selectedCampus",
  #                     selected = campus_selected
  #   )
  #   
  # 
  #   updatePickerInput(session,
  #                     inputId = "selectedDepartment",
  #                     choices = department_choices,
  #                     selected = department_selected
  #   )
  # 
  #   updatePickerInput(session,
  #                     inputId = "daysOfWeek",
  #                     choices = daysOfWeek.options,
  #                     selected = day_selected
  #   )
  #   
  #   
  # })
  # 
  # observeEvent(input$update_filters0,{
  #   user <- user()
  #   filter_path_full <- paste0(filter_path, "/", user)
  #   dir.create(file.path(filter_path, user), showWarnings = FALSE)
  #   filter_choices <- file_path_sans_ext(list.files(path = filter_path_full, pattern = "*.csv"))
  #   updatePickerInput(session, "filter_list", choices = filter_choices)
  #   
  # })
  
  
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
  
  output$practiceName_utilization <- renderText({
    paste0("Based on data from ", input$dateRangeUtil[1]," to ", input$dateRangeUtil[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_utilization_treatment <- renderText({
    paste0("Based on data from ", input$dateRangetreat_util[1]," to ", input$dateRangetreat_util[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_utilization_provider <- renderText({
    paste0("Based on data from ", input$dateRangeUtil[1]," to ", input$dateRangeUtil[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  # Observe Events ------------------------------------------------------------------------------------------------------    
  observeEvent(input$download10, {
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
  
  
  observeEvent(c(input$selectedCampus#, input$dateRange[1], input$dateRange[2]
                 ),{
    if(!is.null(input$selectedCampus)) {
      select_campus <- input$selectedCampus
      first_date <- input$dateRange[1]  
      second_date <- input$dateRange[2]  
      
      #select_campus <- "MSW"
      # department_choices <- sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus, "Department"]))
      
      department_choices <- oncology_tbl %>% filter(SITE %in% select_campus) %>% 
                            # filter(TO_DATE(first_date, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR,
                            #        TO_DATE(second_date, "YYYY-MM-DD HH24:MI:SS") >= APPT_DATE_YEAR,) %>%
                            select(DEPARTMENT_NAME) %>% 
                            mutate(DEPARTMENT_NAME = unique(DEPARTMENT_NAME)) %>%
                            collect()
      department_choices <- sort(department_choices$DEPARTMENT_NAME, na.last = T)
      
      
      updatePickerInput(session,
                        inputId = "selectedDepartment",
                        choices = department_choices,
                        selected = department_choices
      )
      
      # 
      # visit_type_choices <- oncology_tbl %>% filter(SITE %in% select_campus & DEPARTMENT_NAME %in% department_choices) %>%
      #                       select(ASSOCIATIONLISTA) %>%
      #                       mutate(ASSOCIATIONLISTA = unique(ASSOCIATIONLISTA)) %>%
      #                       collect()
      # visit_type_choices <- sort(visit_type_choices$ASSOCIATIONLISTA, na.last = T)
      # 
      # updatePickerInput(session,
      #                   inputId = "selectedVisitType",
      #                   choices = visit_type_choices,
      #                   selected = visit_type_choices
      # )
      # 
      # 
      # appt_type_choices <- oncology_tbl %>% filter(SITE %in% select_campus & DEPARTMENT_NAME %in% department_choices &
      #                                               ASSOCIATIONLISTA %in%  visit_type_choices) %>%
      #                                       select(ASSOCIATIONLISTB) %>% 
      #                                       mutate(ASSOCIATIONLISTB = unique(ASSOCIATIONLISTB)) %>% 
      #                                       collect()
      # appt_type_choices <- sort(appt_type_choices$ASSOCIATIONLISTB, na.last = T)
      # updatePickerInput(session,
      #                   inputId = "selectedApptType",
      #                   choices = appt_type_choices,
      #                   selected = appt_type_choices
      # )
      # 
      # 
      # 
      # treatment_choices <- oncology_tbl %>% filter(SITE %in% select_campus & DEPARTMENT_NAME %in% department_choices &
      #                                                    ASSOCIATIONLISTA %in% visit_type_choices &
      #                                                    ASSOCIATIONLISTB %in% appt_type_choices) %>%
      #                                                     select(ASSOCIATIONLISTT) %>% 
      #                                                     mutate(ASSOCIATIONLISTT = unique(ASSOCIATIONLISTT)) %>% 
      #                                                     collect()
      # treatment_choices <- sort(treatment_choices$ASSOCIATIONLISTT, na.last = T)
      # 
      # updatePickerInput(session,
      #                   inputId = "selectedTreatmentType",
      #                   choices = treatment_choices,
      #                   selected = treatment_choices
      # )
      # 
      # 
      # department_choices_disease <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived")) %>%
      #                                 select(DEPARTMENT_NAME) %>%
      #                                 mutate(DEPARTMENT_NAME = unique(DEPARTMENT_NAME)) %>%
      #                                 collect()
      # department_choices_disease <- sort(department_choices_disease$DEPARTMENT_NAME, na.last = T)
      # 
      # 
      # 
      # disease_choices <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
      #                                                    DEPARTMENT_NAME %in% department_choices_disease) %>%
      #                                       select(DISEASE_GROUP) %>%
      #                                       mutate(DISEASE_GROUP = unique(DISEASE_GROUP)) %>%
      #                                       collect()
      # disease_choices <- sort(disease_choices$DISEASE_GROUP, na.last = T)
      # 
      # updatePickerInput(session,
      #                   inputId = "selectedDisease",
      #                   choices = disease_choices,
      #                   selected = disease_choices
      # )
      # updatePickerInput(session,
      #                   inputId = "selectedDisease2",
      #                   choices = disease_choices,
      #                   selected = disease_choices
      # )
      # 
      # 
      # 
      # provider_choices <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
      #                                               DEPARTMENT_NAME %in% department_choices_disease & 
      #                                               DISEASE_GROUP %in% disease_choices) %>%
      #                                             select(PROVIDER) %>%
      #                                             mutate(PROVIDER = unique(PROVIDER)) %>%
      #                                             collect()
      # provider_choices <- sort(provider_choices$PROVIDER, na.last = T)
      # 
      # updatePickerInput(session,
      #                   inputId = "selectedProvider",
      #                   choices = provider_choices,
      #                   selected = provider_choices
      # )  
      # updatePickerInput(session,
      #                   inputId = "selectedProvider2",
      #                   choices = provider_choices,
      #                   selected = provider_choices
      # )
  }
    
    
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  observeEvent(c(input$selectedDepartment#, input$dateRange[1], input$dateRange[2]
                 ),{
    if(!is.null(input$selectedDepartment)) {
    
      select_campus <- input$selectedCampus
      first_date <- input$dateRange[1]  
      second_date <- input$dateRange[2]  
      
      department_choices <- oncology_tbl %>% filter(SITE %in% select_campus) %>% 
        # filter(TO_DATE(first_date, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR,
        #        TO_DATE(second_date, "YYYY-MM-DD HH24:MI:SS") >= APPT_DATE_YEAR,) %>%
        select(DEPARTMENT_NAME) %>% 
        mutate(DEPARTMENT_NAME = unique(DEPARTMENT_NAME)) %>%
        collect()
      department_choices <- sort(department_choices$DEPARTMENT_NAME, na.last = T)
      
      
      visit_type_choices <- oncology_tbl %>% filter(SITE %in% select_campus & DEPARTMENT_NAME %in% department_choices) %>%
                                              select(ASSOCIATIONLISTA) %>%
                                              mutate(ASSOCIATIONLISTA = unique(ASSOCIATIONLISTA)) %>%
                                              collect()
      visit_type_choices <- sort(visit_type_choices$ASSOCIATIONLISTA, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedVisitType",
                        choices = visit_type_choices,
                        selected = visit_type_choices
      )
     
      
      
      appt_type_choices <- oncology_tbl %>% filter(SITE %in% select_campus & DEPARTMENT_NAME %in% department_choices &
                                                     ASSOCIATIONLISTA %in%  visit_type_choices) %>%
                                                  select(ASSOCIATIONLISTB) %>% 
                                                  mutate(ASSOCIATIONLISTB = unique(ASSOCIATIONLISTB)) %>% 
                                                  collect()
      appt_type_choices <- sort(appt_type_choices$ASSOCIATIONLISTB, na.last = T)
      updatePickerInput(session,
                        inputId = "selectedApptType",
                        choices = appt_type_choices,
                        selected = appt_type_choices
      )
     
      
      treatment_choices <- oncology_tbl %>% filter(SITE %in% select_campus & DEPARTMENT_NAME %in% department_choices &
                                                     ASSOCIATIONLISTA %in% visit_type_choices &
                                                     ASSOCIATIONLISTB %in% appt_type_choices) %>%
                                                    select(ASSOCIATIONLISTT) %>% 
                                                    mutate(ASSOCIATIONLISTT = unique(ASSOCIATIONLISTT)) %>% 
                                                    collect()
      treatment_choices <- sort(treatment_choices$ASSOCIATIONLISTT, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedTreatmentType",
                        choices = treatment_choices,
                        selected = treatment_choices
      )
      
      
      department_choices_disease <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived")) %>%
                                                      # filter(TO_DATE(first_date, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR,
                                                      #        TO_DATE(second_date, "YYYY-MM-DD HH24:MI:SS") >= APPT_DATE_YEAR,) %>%
                                                            select(DEPARTMENT_NAME) %>%
                                                            mutate(DEPARTMENT_NAME = unique(DEPARTMENT_NAME)) %>%
                                                            collect()
      department_choices_disease <- sort(department_choices_disease$DEPARTMENT_NAME, na.last = T)
      
    
      
      disease_choices <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
                                                   DEPARTMENT_NAME %in% department_choices_disease) %>%
                                                select(DISEASE_GROUP) %>%
                                                mutate(DISEASE_GROUP = unique(DISEASE_GROUP)) %>%
                                                collect()
      disease_choices <- sort(disease_choices$DISEASE_GROUP)
      
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
      
      disease_detail_choices <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
                                                   DEPARTMENT_NAME %in% department_choices_disease &
                                                   DISEASE_GROUP %in% disease_choices) %>%
        # filter(TO_DATE(first_date, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR,
        #        TO_DATE(second_date, "YYYY-MM-DD HH24:MI:SS") >= APPT_DATE_YEAR,) %>%
        select(DISEASE_GROUP_DETAIL) %>%
        mutate(DISEASE_GROUP_DETAIL = unique(DISEASE_GROUP_DETAIL)) %>%
        collect()
      disease_detail_choices <- sort(disease_detail_choices$DISEASE_GROUP_DETAIL, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedDiseaseDetail",
                        choices = disease_detail_choices,
                        selected = disease_detail_choices
      )
      
      
      
     
      selected_dept <- input$selectedDepartment
      provider_choices <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
                                                    DEPARTMENT_NAME %in% selected_dept & 
                                                    DISEASE_GROUP %in% disease_choices) %>%
                                            # filter(TO_DATE(first_date, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR,
                                            #        TO_DATE(second_date, "YYYY-MM-DD HH24:MI:SS") >= APPT_DATE_YEAR,) %>%
                                                    select(PROVIDER) %>%
                                                    mutate(PROVIDER = unique(PROVIDER)) %>%
                                                    collect()
      provider_choices <- sort(provider_choices$PROVIDER, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedProvider",
                        choices = provider_choices,
                        selected = provider_choices
      )
      
      
      
      provider_choices_volume_treatment <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
                                                    DEPARTMENT_NAME %in% selected_dept &
                                                      ASSOCIATIONLISTA %in% c("Treatment") &
                                                      ASSOCIATIONLISTB %in% c("Treatment Visit")) %>%
        select(REFERRING_PROVIDER, REFERRING_PROV_ID) %>%
        distinct(REFERRING_PROVIDER, REFERRING_PROV_ID) %>%
        collect()
      
      select_campus_referring <- paste(sort(unique(select_campus)),sep="", collapse="|")
      
      provider_choices_volume_treatment <- inner_join(provider_choices_volume_treatment, referring_provider_site)
      provider_choices_volume_treatment <- provider_choices_volume_treatment %>% filter(grepl(select_campus_referring,SITE_REFERRING))
      
      
      
      provider_choices_volume_treatment <- sort(provider_choices_volume_treatment$REFERRING_PROVIDER, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selected_referring_provider_treatment",
                        choices = provider_choices_volume_treatment,
                        selected = provider_choices_volume_treatment
      )  
      
      date_1 <- input$dateRange[1]
      date_2 <- input$dateRange[2]
      
      selected_dept <- input$selectedDepartment
      provider_unique_exam_choices <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
                                                                DEPARTMENT_NAME %in% selected_dept &
                                                                ASSOCIATIONLISTA %in% c("Exam")) %>%
        # filter(TO_DATE(date_1, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR, 
        #        TO_DATE(date_2, "YYYY-MM-DD HH24:MI:SS") >= APPT_DATE_YEAR) %>%
        select(PROVIDER) %>%
        mutate(PROVIDER = unique(PROVIDER)) %>%
        collect()
      provider_unique_exam_choices <- sort(provider_unique_exam_choices$PROVIDER, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedProvider2",
                        choices = provider_unique_exam_choices,
                        selected = provider_unique_exam_choices
      ) 
      
      
      # provider_utlization_choices <- data.frame(Provider = sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
      #                                                                            historical.data$Department %in% input$selectedDepartment, "Provider"])),
      #                                   stringsAsFactors=FALSE
      # )
      
      depts <- input$selectedDepartment
      
      date_util1 <- input$dateRangetreat_util[1]
      date_util2 <- input$dateRangetreat_util[2]
      provider_utlization_choices <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
                                                                DEPARTMENT_NAME %in% depts) %>%
        # filter(TO_DATE(date_util1, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR,
        #        TO_DATE(date_util2, "YYYY-MM-DD HH24:MI:SS") >= APPT_DATE_YEAR) %>%
        select(PROVIDER) %>%
        mutate(PROVIDER = unique(PROVIDER)) %>%
        collect()
      provider_utlization_choices <- data.frame(Provider = sort(provider_utlization_choices$PROVIDER, na.last = T), stringsAsFactors=FALSE)
      
      
      provider_utlization_choices <- as.character(t(inner_join(provider_utlization_choices, all_provider)))
      
      updatePickerInput(session,
                        inputId = "selectedProviderUtil",
                        choices = provider_utlization_choices,
                        selected = provider_utlization_choices
      )
    }
    
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  observeEvent(input$selectedVisitType,{
    if(!is.null(input$selectedVisitType)) {
      select_campus <- input$selectedCampus
      select_dept <- input$selectedDepartment
      select_visit_type <- input$selectedVisitType
     
      
      appt_type_choices <- oncology_tbl %>% filter(SITE %in% select_campus & DEPARTMENT_NAME %in% select_dept &
                                                     ASSOCIATIONLISTA %in%  select_visit_type) %>%
                                                  select(ASSOCIATIONLISTB) %>% 
                                                  mutate(ASSOCIATIONLISTB = unique(ASSOCIATIONLISTB)) %>% 
                                                  collect()
      appt_type_choices <- sort(appt_type_choices$ASSOCIATIONLISTB, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedApptType",
                        choices = appt_type_choices,
                        selected = appt_type_choices
      )
      
      
      
      treatment_choices <- oncology_tbl %>% filter(SITE %in% select_campus & DEPARTMENT_NAME %in% select_dept &
                                                     ASSOCIATIONLISTA %in% select_visit_type &
                                                     ASSOCIATIONLISTB %in% appt_type_choices) %>%
        select(ASSOCIATIONLISTT) %>% 
        mutate(ASSOCIATIONLISTT = unique(ASSOCIATIONLISTT)) %>% 
        collect()
      treatment_choices <- sort(treatment_choices$ASSOCIATIONLISTT, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedTreatmentType",
                        choices = treatment_choices,
                        selected = treatment_choices
      )
    }
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  observeEvent(input$selectedApptType,{
    if(!is.null(input$selectedApptType)) {
      select_campus <- input$selectedCampus
      select_dept <- input$selectedDepartment
      select_visit_type <- input$selectedVisitType
      select_appt_type <- input$selectedApptType
    
      
      treatment_choices <- oncology_tbl %>% filter(SITE %in% select_campus & DEPARTMENT_NAME %in% select_dept &
                                                     ASSOCIATIONLISTA %in% select_visit_type &
                                                     ASSOCIATIONLISTB %in% select_appt_type) %>%
                                                  select(ASSOCIATIONLISTT) %>% 
                                                  mutate(ASSOCIATIONLISTT = unique(ASSOCIATIONLISTT)) %>% 
                                                  collect()
      treatment_choices <- sort(treatment_choices$ASSOCIATIONLISTT, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedTreatmentType",
                        choices = treatment_choices,
                        selected = treatment_choices
      )
    }
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  observeEvent(c(input$selectedDisease),{
    if(!is.null(input$selectedDisease)){
      select_campus <- input$selectedCampus
      select_dept <- input$selectedDepartment
      select_disease <- input$selectedDisease
      
      
      disease_detail_choices <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
                                                    DEPARTMENT_NAME %in% select_dept & 
                                                    DISEASE_GROUP %in% select_disease) %>%
                                                  select(DISEASE_GROUP_DETAIL) %>%
                                                  mutate(DISEASE_GROUP_DETAIL = unique(DISEASE_GROUP_DETAIL)) %>%
                                                  collect()
      disease_detail_choices <- sort(disease_detail_choices$DISEASE_GROUP_DETAIL, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedDiseaseDetail",
                        choices = disease_detail_choices,
                        selected = disease_detail_choices
      )
    }
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  observeEvent(c(input$selectedDiseaseDetail),{
    if(!is.null(input$selectedDiseaseDetail)){
      select_campus <- input$selectedCampus
      select_dept <- input$selectedDepartment
      select_disease <- input$selectedDisease
      select_disease_detail <- input$selectedDiseaseDetail
      first_date <- input$dateRange[1]
      second_date <- input$dateRange[2]
      
      if(c("NA") %in% input$selectedDiseaseDetail) {
        print("disease detail has NA")
        provider_choices_non_nan <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
                                                      DEPARTMENT_NAME %in% select_dept & 
                                                      DISEASE_GROUP %in% select_disease &
                                                      DISEASE_GROUP_DETAIL %in% select_disease_detail)
        
        provider_choices_na <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
                                                      DEPARTMENT_NAME %in% select_dept & 
                                                      DISEASE_GROUP %in% select_disease &
                                                      is.na(DISEASE_GROUP_DETAIL)) 
        provider_choices <- union_all(provider_choices_non_nan, provider_choices_na) %>% 
                                  select(PROVIDER) %>%
                                  mutate(PROVIDER = unique(PROVIDER)) %>%
                                  collect()
      } else{
      provider_choices <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
                                                    DEPARTMENT_NAME %in% select_dept & 
                                                    DISEASE_GROUP %in% select_disease &
                                                    DISEASE_GROUP_DETAIL %in% select_disease_detail) %>%
        # filter(TO_DATE(first_date, "YYYY-MM-DD HH24:MI:SS") <= APPT_DATE_YEAR,
        #        TO_DATE(second_date, "YYYY-MM-DD HH24:MI:SS") >= APPT_DATE_YEAR,) %>%
        select(PROVIDER) %>%
        mutate(PROVIDER = unique(PROVIDER)) %>%
        collect()
      }
      provider_choices <- sort(provider_choices$PROVIDER, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedProvider",
                        choices = provider_choices,
                        selected = provider_choices
      )
    }
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  # 
  # observeEvent(c(input$selectedDisease2),{
  #   
  #   if(!is.null(input$selectedDisease2)){
  #     select_campus <- input$selectedCampus
  #     select_dept <- input$selectedDepartment
  #     select_disease <- input$selectedDisease2
  #     
  #     
  #     provider_choices <- oncology_tbl %>% filter(SITE %in% select_campus & APPT_STATUS %in% c("Arrived") &
  #                                                   DEPARTMENT_NAME %in% select_dept & 
  #                                                   DISEASE_GROUP %in% select_disease) %>%
  #                                                   select(PROVIDER) %>%
  #                                                   mutate(PROVIDER = unique(PROVIDER)) %>%
  #                                                   collect()
  #     provider_choices <- sort(provider_choices$PROVIDER, na.last = T)
  #     
  #     updatePickerInput(session,
  #                       inputId = "selectedProvider2",
  #                       choices = provider_choices,
  #                       selected = provider_choices
  #     )
  #   }
  # },
  # ignoreNULL = FALSE,
  # ignoreInit = TRUE)
  # 
  
  observeEvent(list(input$selectedDepartment),{
    if(!is.null(input$selectedDepartment)) {
      # prov_choices <- historical.data %>% filter(!(Disease_Group %in% c("No Disease Group"))) %>% drop_na(Disease_Group)
      # 
      # prov_choices <-  sort(unique(prov_choices[prov_choices$SITE %in% input$selectedCampus &
      #                                             prov_choices$Department %in% input$selectedDepartment, "Provider"]))
      
      
      campus <- input$selectedCampus
      depts <- input$selectedDepartment
      prov_choices <- oncology_tbl %>% filter(!is.na(DISEASE_GROUP)) %>%
        filter(SITE %in% campus & APPT_STATUS %in% c("Arrived") &
                 DEPARTMENT_NAME %in% depts) %>%
        select(PROVIDER) %>%
        mutate(PROVIDER = unique(PROVIDER)) %>%
        collect()
      
      prov_choices <- sort(prov_choices$PROVIDER, na.last = T)
  
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
    }
    
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  
  
  
  # Reactive Data -----------------------------------------------------------------------------------------------------------------------
  # All pre-processed data ============================================================================================================
  dataAll <- eventReactive(list(input$update_filters, input$update_filters1),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    data <- groupByFilters(oncology_tbl,
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
    data_test <- data %>% head(n = 1L) %>% collect()
    validate(
      need(nrow(data_test) != 0, "There is no data for these filters.")
    )
    
    data
  })
  
  # [2.2] Arrived + No Show data ============================================================================================================
  # dataArrivedNoShow <- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  #   groupByFilters(historical.data[arrivedNoShow.data.rows,],
  #                  input$selectedCampus, input$selectedDepartment, 
  #                  input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  # })
  
  # [2.3] Arrived data ============================================================================================================
  dataArrived <- eventReactive(list(input$update_filters, input$update_filters1),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    
    data <- groupByFilters(arrived_data,
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
    data_test <- data %>% head(n = 1L) %>% collect()
    validate(
      need(nrow(data_test) != 0, "There is no arrived data for these filters.")
    )
    
    data
    

    
  })
  
  dataArrived_unique_trend <- eventReactive(list(input$update_filters, input$update_filters1),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    data <- groupByFilters_unique_trend(arrived_data,
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
    data_test <- data %>% head(n = 1L) %>% collect()
    validate(
      need(nrow(data_test) != 0, "There is no arrived data for these filters.")
    )
    
    data
  })
  
  dataArrived_Diag <- eventReactive(list(input$update_filters, input$update_filters1),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    #groupByFilters_Trend(historical.data[arrived.data.rows,],
    data <- groupByFilters_Trend(arrived_data,
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays,
                   input$diag_grouper
                   )
    
    data_test <- data %>% head(n = 1L) %>% collect()
    validate(
      need(nrow(data_test) != 0, "There is no arrived data for these filters.")
    )
    
    data

  })
  
  # Canceled data ============================================================================================================
  # dataCanceled<- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  #   groupByFilters(historical.data[canceled.data.rows,],
  #                  input$selectedCampus, input$selectedDepartment,
  #                  input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  # })
  
  # Bumped data ============================================================================================================
  # dataBumped<- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  #   groupByFilters(historical.data[bumped.data.rows,],
  #                  input$selectedCampus, input$selectedDepartment,
  #                  input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  # })
  
  # Arrived data filtered: visitType, apptType, treatmentType ===============================================================
  
  dataArrived_filtered <- eventReactive(list(input$update_filters,input$update_filters6, input$update_filters1),{
    validate(
      need(input$selectedVisitType != "", "Please select a visit type"),
      need(input$selectedApptType != "", "Please select a visit type detail"),
      need(input$selectedTreatmentType != "", "Please select a treatment type")
    )
    data <- groupByFilters_2(dataArrived(),
                      input$selectedVisitType, input$selectedApptType, input$selectedTreatmentType, input$diag_grouper)
    
    data_test <- data %>% head(n = 1L) %>% collect()
    validate(
      need(nrow(data_test) != 0, "There is no arrived data for these filters.")
    )
    
    data
    
  })
  
  
  # Arrived Disease Group data filtered: Disease Group, Provider ==============================================================
  
  dataArrived_disease <- eventReactive(list(input$update_filters, input$update_filters1),{
    validate(
      need(input$selectedDisease != "", "Please select a disease group"),
      need(input$selectedDiseaseDetail != "", "Please select a disease group detail"),
      need(input$selectedProvider != "", "Please select a provider")
    )
    #groupByFilters_3(dataArrived(),
    groupByFilters_3_detail(dataArrived_Diag(),
                     input$selectedDisease, input$selectedProvider, input$diag_grouper, input$selectedDiseaseDetail
                     )
    

  })
  
  dataArrived_disease_2 <- eventReactive(list(input$update_filters,input$update_filters2, input$update_filters1),{
    validate(
      need(input$selectedDisease2 != "", "Please select a provider group"),
      need(input$selectedProvider2 != "", "Please select a provider")
    )
    groupByFilters_3(dataArrived(),
                     input$selectedDisease2, input$selectedProvider2, input$diag_grouper)
  })
  
  # Arrived population data ============================================================================================================
  dataArrivedPop <- eventReactive(list(input$update_filters, input$update_filters7, input$update_filters1),{
    validate(
      need(input$selectedVisitType != "", "Please select a visit type"),
      need(input$selectedApptType != "", "Please select a visit type detail"),
      need(input$selectedTreatmentType != "", "Please select a treatment type")
    )
    data <- groupByFilters_pop(population.data_filtered,
                       input$selectedCampus, input$selectedDepartment,
                       input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$selectedProvider7,
                       input$dx_grouper_zip)
    
    data_test <- data %>% head(n = 1L) %>% collect()
    validate(
      need(nrow(data_test) != 0, "There is no data for these filters.")
    )
    
    data
  })
  
  # Scheduling  data ============================================================================================================
  
  # dataAllsched <- eventReactive(list(input$update_filters, input$update_filters3, input$update_filters4, input$update_filters5),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  #   groupByFilters_4(historical.data[all.data.rows,],
  #                    input$selectedCampus, input$selectedDepartment,
  #                    input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$selectedProvider1)
  # })
  # 
  # dataArrivedNoShowsched <- eventReactive(list(input$update_filters,input$update_filters3, input$update_filters4, input$update_filters5),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  #   groupByFilters_4(historical.data[arrivedNoShow.data.rows,],
  #                    input$selectedCampus, input$selectedDepartment, 
  #                    input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$selectedProvider1)
  # })
  
  # dataArrivedsched <- eventReactive(list(input$update_filters,input$update_filters3, input$update_filters4, input$update_filters5),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  #   groupByFilters_4(historical.data[arrived.data.rows,],
  #                    input$selectedCampus, input$selectedDepartment,
  #                    input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$selectedProvider1)
  # })
  
  # dataBumpedsched <- eventReactive(list(input$update_filters,input$update_filters3, input$update_filters4, input$update_filters5),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  #   groupByFilters_4(historical.data[bumped.data.rows,],
  #                    input$selectedCampus, input$selectedDepartment,
  #                    input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$selectedProvider1)
  # })
  
  # dataCanceledsched <- eventReactive(list(input$update_filters,input$update_filters3, input$update_filters4, input$update_filters5),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  #   groupByFilters_4(historical.data[canceled.data.rows,],
  #                    input$selectedCampus, input$selectedDepartment,
  #                    input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$selectedProvider1)
  # })
  
  # Trend  data ============================================================================================================
  # dataArrivedTrend <- reactive({
  #   
  #   input$update_filters
  #   
  #   isolate({
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  #   groupByFilters_Trend(historical.data[arrived.data.rows.trend,],
  #                        input$selectedCampus, input$selectedDepartment,
  #                        input$dateRangetrend[1], input$dateRangetrend[2], input$daysOfWeek, input$excludeHolidays
  #   )
  #   })
  # })

  
  dataArrivedTrend <- reactive({
    
    input$update_filters
    input$update_filters1
    
    isolate({
      validate(
        need(input$selectedCampus != "" , "Please select a Campus"),
        need(input$selectedDepartment != "", "Please select a Department")
      )
      data  <- groupByFilters_Trend(arrived_data,
                           input$selectedCampus, input$selectedDepartment,
                           input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays,
                           input$diag_grouper
      )
      
      
      data_test <- data %>% head(n = 1L) %>% collect()
      validate(
        need(nrow(data_test) != 0, "There is no arrived data for these filters.")
        )
      
      data

    })
  })
  
  
  dataArrivedTrend_download <- reactive({
    
    input$update_filters
    input$update_filters1
    
    isolate({
      data <- dataAll() %>% select(SITE, DEPARTMENT_NAME, PROVIDER, APPT_DTTM, APPT_TYPE, APPT_STATUS,
                                  ASSOCIATIONLISTA, ASSOCIATIONLISTB, ASSOCIATIONLISTT, EPIC_PROVIDER_ID, APPT_STATUS, LOS_CODE, MRN) %>% 
            rename(CAMPUS = SITE,
               DEPARTMENT = DEPARTMENT_NAME) %>% collect() %>% 
        relocate(CAMPUS, .before = DEPARTMENT)
      
      data_test <- data %>% head(n = 1L) %>% collect()
      validate(
        need(nrow(data_test) != 0, "There is no data for these filters.")
      )
      
      data
    })
  })
  
  # Unique Patients  data ============================================================================================================
  dataUniqueExam_system <- eventReactive(list(input$update_filters, input$update_filters1),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )

    data <- groupByFilters_unique(arrived_data,
                          input$selectedCampus, input$selectedDepartment,
                          input$dateRangeunique[1], input$dateRangeunique[2], input$daysOfWeek, input$excludeHolidays,
                          input$diag_grouper)
    
    data_test <- data %>% head(n = 1L) %>% collect()
    validate(
      need(nrow(data_test) != 0, "There is no arrived data for these filters.")
    )
    
    data

  })
  # 
  # dataUniqueExam_system_month <- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  # 
  #   groupByFilters_unique(historical.data.unique.exam.month,
  #                          input$selectedCampus, input$selectedDepartment,
  #                          input$dateRangeunique[1], input$dateRangeunique[2], input$daysOfWeek, input$excludeHolidays,
  #                          input$diag_grouper)
  # 
  # })
  
  # dataUniqueAll_system <- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  # 
  #   groupByFilters_unique(historical.data.unique.all,
  #                  input$selectedCampus, input$selectedDepartment,
  #                  input$dateRangeunique[1], input$dateRangeunique[2], input$daysOfWeek, input$excludeHolidays,
  #                  input$diag_grouper)
  #   
  # })
  
  # dataUniqueAll_system_month <- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  # 
  #   
  #   groupByFilters_unique(historical.data.unique.all.month,
  #                  input$selectedCampus, input$selectedDepartment,
  #                  input$dateRangeunique[1], input$dateRangeunique[2], input$daysOfWeek, input$excludeHolidays,
  #                  input$diag_grouper)
  #   
  # })
  
  # dataUniqueTreatment_system <- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  # 
  #   groupByFilters_unique(historical.data.unique.treatment,
  #                          input$selectedCampus, input$selectedDepartment,
  #                          input$dateRangeunique[1], input$dateRangeunique[2], input$daysOfWeek, input$excludeHolidays,
  #                          input$diag_grouper)
  #   
  # })
  
  # dataUniqueTreatment_system_month <- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  # 
  #   
  #   groupByFilters_unique(historical.data.unique.treatment.month,
  #                          input$selectedCampus, input$selectedDepartment,
  #                          input$dateRangeunique[1], input$dateRangeunique[2], input$daysOfWeek, input$excludeHolidays,
  #                          input$diag_grouper)
  #   
  # })
  
  
  
  
  
  # dataUniqueExam_site <- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  # 
  #   groupByFilters_unique(historical.data.site.exam,
  #                          input$selectedCampus, input$selectedDepartment,
  #                          input$dateRangeunique[1], input$dateRangeunique[2], input$daysOfWeek, input$excludeHolidays,
  #                          input$diag_grouper)
  #   
  # })
  
  # dataUniqueExam_site_month <- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  #   
  #   
  #   groupByFilters_unique(historical.data.site.exam.month,
  #                          input$selectedCampus, input$selectedDepartment,
  #                          input$dateRangeunique[1], input$dateRangeunique[2], input$daysOfWeek, input$excludeHolidays,
  #                          input$diag_grouper)
  #   
  # })
  
  # dataUniqueAll_site <- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  # 
  #   groupByFilters_unique(historical.data.site.all,
  #                          input$selectedCampus, input$selectedDepartment,
  #                          input$dateRangeunique[1], input$dateRangeunique[2], input$daysOfWeek, input$excludeHolidays,
  #                          input$diag_grouper)
  #   
  # })
  
  # dataUniqueAll_site_month <- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  # 
  #   
  #   groupByFilters_unique(historical.data.site.all.month,
  #                          input$selectedCampus, input$selectedDepartment,
  #                          input$dateRangeunique[1], input$dateRangeunique[2], input$daysOfWeek, input$excludeHolidays,
  #                          input$diag_grouper)
  #   
  # })
  
  # dataUniqueTreatment_site <- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  # 
  #   
  #   groupByFilters_unique(historical.data.site.treatment,
  #                          input$selectedCampus, input$selectedDepartment,
  #                          input$dateRangeunique[1], input$dateRangeunique[2], input$daysOfWeek, input$excludeHolidays,
  #                          input$diag_grouper)
  #   
  # })
  # 
  # dataUniqueTreatment_site_month <- eventReactive(list(input$update_filters),{
  #   validate(
  #     need(input$selectedCampus != "" , "Please select a Campus"),
  #     need(input$selectedDepartment != "", "Please select a Department")
  #   )
  # 
  #   
  #   groupByFilters_unique(historical.data.site.treatment.month,
  #                          input$selectedCampus, input$selectedDepartment,
  #                          input$dateRangeunique[1], input$dateRangeunique[2], input$daysOfWeek, input$excludeHolidays,
  #                          input$diag_grouper)
  #   
  # })
  
  
  # [2.2] All pre-processed data for utilization tabs --------------------------------------------------------------------------------------
  
  dataUtilization <- eventReactive(list(input$update_filters,input$utilType,input$update_filters1),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    data <- groupByFilters_util(utilization.data,
                     input$selectedCampus, input$selectedDepartment, input$selectedProviderUtil,
                     input$dateRangeUtil[1], input$dateRangeUtil[2], input$daysOfWeek, input$excludeHolidays, input$utilType)
    
    data_test <- data %>% head(n = 1L) %>% collect()
    validate(
      need(nrow(data_test) != 0, "There is no utilization data for these filters.")
    )
    
    data
  }) 
  
  
  dataUtilization_Treatment <- eventReactive(list(input$update_filters,input$utilType,input$update_filters1),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    # data <- groupByFilters_util_treatment(historical.data,
    data <- groupByFilters_util_treatment(oncology_tbl,
                                  input$selectedCampus, input$selectedDepartment, #input$selectedProviderUtil,
                                  input$dateRangetreat_util[1], input$dateRangetreat_util[2], input$daysOfWeek, input$excludeHolidays)
    data <- data %>% filter(APPT_STATUS %in% c("Arrived"))
    data <- data %>% filter(ASSOCIATIONLISTA %in% c("Treatment"))
    
    data_test <- data %>% head(n = 1L) %>% collect()
    validate(
      need(nrow(data_test) != 0, "There is no data for these filters.")
    )
    
    data
  }) 
  
  dataUtilization_Treatment_download <- eventReactive(list(input$update_filters,input$utilType,input$update_filters1),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    #data <- groupByFilters_util_treatment(historical.data,
    data <- groupByFilters_util_treatment(oncology_tbl,
                                          input$selectedCampus, input$selectedDepartment, #input$selectedProviderUtil,
                                          input$dateRangetrend[1], input$dateRangetrend[2], input$daysOfWeek, input$excludeHolidays)
    data <- data %>% filter(APPT_STATUS %in% c("Arrived"))
    data <- data %>% filter(ASSOCIATIONLISTA %in% c("Treatment"))
    
    data_test <- data %>% head(n = 1L) %>% collect()
    validate(
      need(nrow(data_test) != 0, "There is no data for these filters.")
    )
    
    data
  }) 
  
  dataUtilization_provider <- eventReactive(list(input$update_filters,input$utilType,input$update_filters1),{
    validate(
      need(input$selectedCampus != "" , "Please select a Campus"),
      need(input$selectedDepartment != "", "Please select a Department")
    )
    data <- groupByFilters_util(utilization.data,
                        input$selectedCampus, input$selectedDepartment, input$selectedProviderUtil,
                        input$dateRangeUtil[1], input$dateRangeUtil[2], input$daysOfWeek, input$excludeHolidays, input$utilType1)
    
    data_test <- data %>% head(n = 1L) %>% collect()
    validate(
      need(nrow(data_test) != 0, "There is no utilization data for these filters.")
    )
    
    data
  }) 
  
  # Site Volume Tab ------------------------------------------------------------------------------------------------------0
  # Volume Trend Tab ------------------------------------------------------------------------------------------------------    
  output$trend_totalvisitsgraph <- renderPlotly({
    data <- dataArrivedTrend()
    # data_testing <<- data
    
    # min_date <- min(data$Appt.DateYear)
    # max_date <- max(data$Appt.DateYear)
    
    total_visits <- data %>%
      filter(ASSOCIATIONLISTA %in% c("Exam","Treatment","Labs")) %>%
      group_by(APPT_YEAR, APPT_MONTH) %>% summarise(total = n()) %>% collect()

    data <- data %>% select(SITE) %>% mutate(SITE = unique(SITE)) %>% collect()
    
    if(length(unique(data$SITE)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }

    
    title <- paste0(site," ","Annual All Visits")
  
    
    
      n <- length(unique(total_visits$APPT_YEAR)) - 1
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }
  
    total_visits$APPT_YEAR <- as.character(total_visits$APPT_YEAR)
    total_visits$APPT_MONTH <- str_to_title(total_visits$APPT_MONTH)
    
    total_visits_yearly_total <- total_visits %>% group_by(APPT_YEAR) %>% 
      summarise(total = sum(total, na.rm = T)) %>% 
      mutate(APPT_MONTH = "Total")
    
    total_visits_yearly_total$APPT_YEAR_RENAME <- paste0(total_visits_yearly_total$APPT_YEAR, " (", total_visits_yearly_total$total, ")")
    total_visits_yearly_total <- total_visits_yearly_total %>% select(-total, -APPT_MONTH)
    
    total_visits_yearly_total <- left_join(total_visits, total_visits_yearly_total)
    total_visits_yearly_total <- total_visits_yearly_total %>% ungroup() %>% select(-APPT_YEAR) %>%
                          rename(APPT_YEAR = APPT_YEAR_RENAME)
    
    g1 <- ggplot_line_graph(total_visits_yearly_total, title)
    
    
    

    
    #total_visits <- bind_rows(total_visits, total_visits_yearly_total)
    g2 <- ggplot_table(total_visits, hline_y)
    

    subplot(g1, g2, nrows = 2, margin = 0.1, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Visits"), scene = list(aspectration=list(x=1,y=1)))
    
  }#, height = function(x) input$plotHeight
  )
  
  
  output$trend_examvisitsgraph <- renderPlotly({
    
    data <- dataArrivedTrend()
    # data <- historical.data[arrived.data.rows.trend,]
    
    total_visits <- data %>% filter(ASSOCIATIONLISTA == "Exam") %>% 
      group_by(APPT_YEAR, APPT_MONTH) %>% summarise(total = n()) %>% collect()
    
    data <- data %>% select(SITE) %>% mutate(SITE = unique(SITE)) %>% collect()
    
    if(length(unique(data$SITE)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }

    title <- paste0(site," ","Annual Exam Visits")
  
    n <- length(unique(total_visits$APPT_YEAR)) - 1
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }

    total_visits$APPT_YEAR <- as.character(total_visits$APPT_YEAR)
    total_visits$APPT_MONTH <- str_to_title(total_visits$APPT_MONTH)
    

    total_visits_yearly_total <- total_visits %>% group_by(APPT_YEAR) %>% 
      summarise(total = sum(total, na.rm = T)) %>% 
      mutate(APPT_MONTH = "Total")
    
    total_visits_yearly_total$APPT_YEAR_RENAME <- paste0(total_visits_yearly_total$APPT_YEAR, " (", total_visits_yearly_total$total, ")")
    total_visits_yearly_total <- total_visits_yearly_total %>% select(-total, -APPT_MONTH)
    
    total_visits_yearly_total <- left_join(total_visits, total_visits_yearly_total)
    total_visits_yearly_total <- total_visits_yearly_total %>% ungroup() %>% select(-APPT_YEAR) %>%
      rename(APPT_YEAR = APPT_YEAR_RENAME)
    
    g1 <- ggplot_line_graph(total_visits_yearly_total, title)
    g2 <- ggplot_table(total_visits, hline_y)
    
    
    subplot(g1, g2, nrows = 2, margin = 0.1, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Visits")#, legend = list(title = list(text = "Year"))
    )
    
  }#, height = function(x) input$plotHeight
  )
  
  
  output$trend_treatmentvisitsgraph <- renderPlotly({
    
    data <- dataArrivedTrend()
    # data <- arrived.data
    
    total_visits <- data %>% filter(ASSOCIATIONLISTA == "Treatment") %>% 
      group_by(APPT_YEAR, APPT_MONTH) %>% summarise(total = n()) %>% collect()
    
    data <- data %>% select(SITE) %>% mutate(SITE = unique(SITE)) %>% collect()
    
    if(length(unique(data$SITE)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }

    title <- paste0(site," ","Annual Treatment Visits")
    
    n <- length(unique(total_visits$APPT_YEAR)) - 1
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }
    
    total_visits$APPT_YEAR <- as.character(total_visits$APPT_YEAR)
    total_visits$APPT_MONTH <- str_to_title(total_visits$APPT_MONTH)
  
    total_visits_yearly_total <- total_visits %>% group_by(APPT_YEAR) %>% 
      summarise(total = sum(total, na.rm = T)) %>% 
      mutate(APPT_MONTH = "Total")
    
    total_visits_yearly_total$APPT_YEAR_RENAME <- paste0(total_visits_yearly_total$APPT_YEAR, " (", total_visits_yearly_total$total, ")")
    total_visits_yearly_total <- total_visits_yearly_total %>% select(-total, -APPT_MONTH)
    
    total_visits_yearly_total <- left_join(total_visits, total_visits_yearly_total)
    total_visits_yearly_total <- total_visits_yearly_total %>% ungroup() %>% select(-APPT_YEAR) %>%
      rename(APPT_YEAR = APPT_YEAR_RENAME)
    
    g1 <- ggplot_line_graph(total_visits_yearly_total, title)
    g2 <- ggplot_table(total_visits, hline_y)
    
    
    subplot(g1, g2, nrows = 2, margin = 0.1, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Visits")#, legend = list(title = list(text = "Year"))
    )
    
    }#, height = function(x) input$plotHeight
  )
  
  
  output$trend_labvisitsgraph <- renderPlotly({
    
    data <- dataArrivedTrend()
    # data <- arrived.data
    
    total_visits <- data %>% filter(ASSOCIATIONLISTA == "Labs") %>% group_by(APPT_YEAR, APPT_MONTH) %>% summarise(total = n()) %>% collect()
    
    data <- data %>% select(SITE) %>% mutate(SITE = unique(SITE)) %>% collect()
    
    if(length(unique(data$SITE)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }

    title <- paste0(site," ","Annual Lab Visits")
    
    n <- length(unique(total_visits$APPT_YEAR)) - 1
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }
  
    
    total_visits$APPT_YEAR <- as.character(total_visits$APPT_YEAR)
    total_visits$APPT_MONTH <- str_to_title(total_visits$APPT_MONTH)
    
    total_visits_yearly_total <- total_visits %>% group_by(APPT_YEAR) %>% 
      summarise(total = sum(total, na.rm = T)) %>% 
      mutate(APPT_MONTH = "Total")
    
    total_visits_yearly_total$APPT_YEAR_RENAME <- paste0(total_visits_yearly_total$APPT_YEAR, " (", total_visits_yearly_total$total, ")")
    total_visits_yearly_total <- total_visits_yearly_total %>% select(-total, -APPT_MONTH)
    
    total_visits_yearly_total <- left_join(total_visits, total_visits_yearly_total)
    total_visits_yearly_total <- total_visits_yearly_total %>% ungroup() %>% select(-APPT_YEAR) %>%
      rename(APPT_YEAR = APPT_YEAR_RENAME)
    
    g1 <- ggplot_line_graph(total_visits_yearly_total, title)
    g2 <- ggplot_table(total_visits, hline_y)
    
    
    subplot(g1, g2, nrows = 2, margin = 0.1, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Visits")#, legend = list(title = list(text = "Year"))
    )
  }#, height = function(x) input$plotHeight
  )
  
  
  output$trend_visitstable <- function(){
    
    options(knitr.kable.NA = "-")
     data <- dataArrivedTrend()
    # data <- historical.data[arrived.data.rows.trend,] %>% filter(Appt.DateYear >= "2019-04-01")
    #created an if statement to include another table for all of the visit types
    #to show the total volume and the variance per month per year.
     
     date_2 <- isolate(as.Date(input$dateRange[2]))
     #date_2 <- as.Date("2023-09-30")
     max_month_abb <- format(date_2, "%b")
     
     index <- which(max_month_abb == month.abb)
     month_vector <-toupper(month.abb[1:index])
     
     data <- data %>% filter(APPT_MONTH %in% month_vector)
     

    #get the total patients per year
    if(input$annualVolSummary == "Total"){
      visits_tb_yearly <- data %>%
        group_by(APPT_YEAR) %>% summarise(total = n()) %>% collect() %>%
        spread(APPT_YEAR, total)
      visits_tb_yearly$APPT_MONTH <- "Total YTD Comparison"
      visits_tb_yearly <- visits_tb_yearly %>% relocate(APPT_MONTH)
      
      #get the total patients per year per month
      visits_tb <- data %>%
        group_by(APPT_YEAR, APPT_MONTH) %>% summarise(total = n()) %>% collect() %>%
        spread(APPT_YEAR, total)
      
      
    } else {
      filter <- input$annualVolSummary
      #get the total patients per year
      visits_tb_yearly <- data %>% 
        filter(ASSOCIATIONLISTA %in% filter) %>%
        group_by(APPT_YEAR) %>% summarise(total = n()) %>% collect() %>%
        spread(APPT_YEAR, total)
      visits_tb_yearly$APPT_MONTH <- paste0("Total ",input$annualVolSummary,"\n YTD Comparison")
      visits_tb_yearly <- visits_tb_yearly %>% relocate(APPT_MONTH)
      
      #get the total patients per year per month
      visits_tb <- data %>% 
        filter(ASSOCIATIONLISTA %in% filter) %>%
        group_by(APPT_YEAR, APPT_MONTH) %>% summarise(total = n()) %>% collect() %>%
        spread(APPT_YEAR, total)
    }
    
     
     # visits_tb_yearly$APPT_YEAR <- as.character(visits_tb_yearly$APPT_YEAR)
     visits_tb_yearly$APPT_MONTH <- str_to_title(visits_tb_yearly$APPT_MONTH)

     # visits_tb$APPT_YEAR <- as.character(visits_tb$APPT_YEAR)
     visits_tb$APPT_MONTH <- str_to_title(visits_tb$APPT_MONTH)
    
    #include all the months needed
    visits_tb <- visits_tb[match(monthOptions, visits_tb$APPT_MONTH),]
    visits_tb$APPT_MONTH <- monthOptions
    
    visits_ytd <- visits_tb
    visits_ytd[is.na(visits_ytd)] = 0  
    visits_ytd$APPT_MONTH <- NULL
    #visits_ytd <- visits_ytd[1:month(input$dateRange[2]),]
    visits_ytd <- visits_ytd[1:2,]
    visits_ytd <- as.data.frame(colSums(visits_ytd))
    visits_ytd <- as.data.frame(t(as.matrix(visits_ytd)))
    visits_ytd <- cbind(Appt.Month = "YTD Comparison",visits_ytd)
    
    #bind the total visits per month per year with the total yeraly visits 
    visits_tb_total <- rbind(visits_tb,visits_tb_yearly)
    
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
      rownames(visits_tb_total) <- NULL
      
      column_border <- c(1, 2)
      
    } else if(length(visits_tb_total)-1 == 2){
      
      visits_tb_total$variance <- visits_tb_total %>% select(length(visits_tb_total)) - visits_tb_total %>% select(length(visits_tb_total)-1)
      
      visits_tb_total$variance_percentage <- visits_tb_total %>% select(length(visits_tb_total)) / visits_tb_total %>% select(length(visits_tb_total)-2)
      
      #######
      
      visits_tb_total$variance_percentage <- formattable::percent(as.numeric(unlist(visits_tb_total$variance_percentage)))
      
      year1 <- colnames(visits_tb_total)[2]
      year2 <- colnames(visits_tb_total)[3]
      
      # visits_variance_only <- as.data.frame(visits_tb_total[1:month(isolate(input$dateRange[2])),4])
      # visits_variance_percentage <- as.data.frame(visits_tb_total[1:month(isolate(input$dateRange[2])),5])
      # 
      # visits_variance_only[is.na(visits_variance_only),] <- 0
      # visits_variance_percentage[is.na(visits_variance_percentage),] <- 0
      # visits_variance_only <- as.data.frame(colSums(visits_variance_only))
      # visits_variance_percentage <- as.data.frame(colSums(visits_variance_percentage))
      # visits_tb_total <- as.data.frame(visits_tb_total)
      # visits_tb_total[13,4] <- visits_variance_only[1,1]
      # visits_tb_total[13,5] <- visits_variance_percentage[1,1]
      
      
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
      
      visits_tb_total$variance_2 <- visits_tb_total %>% select(length(visits_tb_total)-2) - visits_tb_total %>% select(length(visits_tb_total)-3)
      
      visits_tb_total$variance_percentage_2 <- visits_tb_total %>% select(length(visits_tb_total)) / visits_tb_total %>% select(length(visits_tb_total)-4)
      
      #######
      
      visits_tb_total$variance_percentage_1 <- formattable::percent(as.numeric(unlist(visits_tb_total$variance_percentage_1)))
      visits_tb_total$variance_percentage_2 <- formattable::percent(as.numeric(unlist(visits_tb_total$variance_percentage_2)))
      
      year1 <- colnames(visits_tb_total)[2]
      year2 <- colnames(visits_tb_total)[3]
      year3 <- colnames(visits_tb_total)[4]
      
      
      # visits_variance_only <- as.data.frame(visits_tb_total[1:month(isolate(input$dateRange[2])),4:5])
      # visits_variance_percentage <- as.data.frame(visits_tb_total[1:month(isolate(input$dateRange[2])),6])
      # 
      # visits_variance_only <- t(as.data.frame(colSums(visits_variance_only, na.rm = TRUE)))
      # visits_variance_percentage <- as.data.frame(colSums(visits_variance_percentage, na.rm = TRUE))
      # visits_tb_total <- as.data.frame(visits_tb_total)
      # visits_tb_total[13,4:5] <- visits_variance_only[1,1:2]
      # visits_tb_total[13,6] <- visits_variance_percentage[1,1]
      
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
    
    visits_tb_total <- visits_tb_total[!is.na(visits_tb_total[,2]),]
    
    total_row <- nrow(visits_tb_total)
    
    visits_tb_total$APPT_MONTH <- gsub("Ytd", "YTD", visits_tb_total$APPT_MONTH)
    
    visits_tb_total %>%
      kable(escape = F, align = "c",
            col.names = column_names) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "left", row_label_position = "l", font_size = 16) %>%
      add_header_above(header_above,  background = "#d80b8c", color = "white", font_size = 20, align = "center") %>%
      column_spec(column = column_border, border_right = "thin solid lightgray", width_min = "125px") %>%
      column_spec(column = 1, bold = T) %>%
      row_spec(row = 0, font_size = 18, bold=TRUE, background = "#d80b8c", color = "white") %>%
      row_spec(row = total_row, bold = TRUE, background = "#d80b8c", color = "white") #%>%
      #row_spec(row = 13, bold = TRUE, background = "#dddedd")
    
  }
  
  # Volume Breakdown Tab ------------------------------------------------------------------------------------------------------       
  output$break_totalvisitsgraph <- renderPlotly({
    
    data <- dataArrived_Diag()
    # data <- historical.data[arrived.data.rows,]

    total_visits_break <- data %>% filter(ASSOCIATIONLISTA %in% c("Labs","Treatment","Exam")) %>%
      group_by(APPT_MONTH_YEAR, ASSOCIATIONLISTA) %>% summarise(total = n()) %>% collect()
    
    total_visits_break$AssociationListA <- factor(total_visits_break$ASSOCIATIONLISTA, levels = c("Labs","Treatment","Exam"))
    
    max <- total_visits_break %>% group_by(APPT_MONTH_YEAR) %>% summarise(max = sum(total))
    
    if(length(isolate(input$selectedCampus)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(isolate(input$selectedCampus)),sep="", collapse=", ")
    }
    
    title <- paste0(site," ","All Visit Volume Composition")

    total_visits_yearly_total <- total_visits_break %>% group_by(AssociationListA) %>% 
      summarise(total = sum(total, na.rm = T)) %>% 
      mutate(APPT_MONTH = "Total")
    
    total_visits_yearly_total$APPT_YEAR_RENAME <- paste0(total_visits_yearly_total$AssociationListA, " (", total_visits_yearly_total$total, ")")
    total_visits_yearly_total <- total_visits_yearly_total %>% select(-total, -APPT_MONTH)
    
    total_visits_break_legend <- left_join(total_visits_break, total_visits_yearly_total)
    total_visits_break_legend <- total_visits_break_legend %>% ungroup() %>% select(-AssociationListA) %>%
      rename(AssociationListA = APPT_YEAR_RENAME)
    
    g1 <- ggplot_bar_graph(total_visits_break_legend, title, total_visits_break_legend$APPT_MONTH_YEAR, total_visits_break_legend$total, total_visits_break_legend$AssociationListA, max)

    Total <- total_visits_break %>% 
      group_by(APPT_MONTH_YEAR) %>%
      summarise(total = sum(total))
    Total$AssociationListA <- "Total"
    total_visits_break <- full_join(total_visits_break,Total)
    
    total_visits_break <- total_visits_break %>% select(-ASSOCIATIONLISTA) %>% rename(ASSOCIATIONLISTA = AssociationListA)

    
    n <- length(unique(total_visits_break$ASSOCIATIONLISTA)) - 1
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }
    

    total_visits_break$ASSOCIATIONLISTA <- factor(total_visits_break$ASSOCIATIONLISTA, levels = c("Total","Treatment","Labs","Exam"))

    # g2 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y= AssociationListA, label=total)) +
    #   #scale_color_MountSinai('dark' )+
    #   geom_text(size = 7, vjust = "center", hjust = "center", fontface  = "bold")+
    #   geom_hline(yintercept = hline_y, colour='black')+
    #   geom_vline(xintercept = 0, colour = 'black')+
    #   scale_x_discrete(position = "top") + 
    #   labs(y = NULL, x = NULL, fill = "AssociationListA")+
    #   theme_minimal() +
    #   table_theme()
    
    #g2 <- ggplot_bar_table(total_visits_break, total_visits_break$Appt.MonthYear, total_visits_break$AssociationListB, total_visits_break$total, hline_y)
    
    g2 <- ggplot(total_visits_break, aes(x=APPT_MONTH_YEAR, y= ASSOCIATIONLISTA, label=total)) +
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
      table_theme()
    

     g2 <- ggplotly(g2, tooltip = NULL)
    
    
    subplot(g1, g2, nrows = 2, margin = 0.1, heights = c(0.6, 0.4)) %>% layout(showlegend = T#, legend = list(title = list(text = "Visit Type"))
    )
    
  }#, height = function(x) input$plotHeight
  )
  
  
  output$break_examvisitsgraph <- renderPlotly({
    
    data <- dataArrived_Diag()
     #data_test <<- dataArrived_Diag()
    
    total_visits_break <- data %>% filter(ASSOCIATIONLISTA == "Exam") %>%
      group_by(APPT_MONTH_YEAR, ASSOCIATIONLISTB) %>% summarise(total = n()) %>% collect()
    
    max <- total_visits_break %>% group_by(APPT_MONTH_YEAR) %>% summarise(max = sum(total))
    total_visits_break$ASSOCIATIONLISTB <- factor(total_visits_break$ASSOCIATIONLISTB, levels = c("Telehealth Visit","New Visit","Established Visit"))
    
    if(length(isolate(input$selectedCampus)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(isolate(input$selectedCampus)),sep="", collapse=", ")
    }
    
    total_visits_break <- total_visits_break %>% filter(!is.na(ASSOCIATIONLISTB))
    
    # g3 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y=total, group=AssociationListB, fill=AssociationListB))+
    #   geom_bar(position="stack",stat="identity", width=0.7)+
    #   scale_fill_MountSinai('dark')+
    #   scale_y_continuous(limits=c(0,(max(max$max))*1.2))+
    #   labs(title = paste0(site," ","Exam Visit Volume Composition"),
    #        subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),"\n"),
    #        y = "Patient Volume\n", x = NULL, fill = NULL)+
    #   theme_new_line()+
    #   theme(axis.title.y = element_text(size = 12, angle = 90), plot.margin=unit(c(1,1,-0.5,1), "cm"))#+
    #   # geom_text(aes(label=total), color="white", 
    #   #           size=5, fontface="bold", position = position_stack(vjust = 0.5))+
    #   # stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
    #   #              size=5, fontface="bold.italic")
    
    title <- paste0(site," ","Exam Visit Volume Composition")

    
    total_visits_yearly_total <- total_visits_break %>% group_by(ASSOCIATIONLISTB) %>% 
      summarise(total = sum(total, na.rm = T)) %>% 
      mutate(APPT_MONTH = "Total")
    
    total_visits_yearly_total$APPT_YEAR_RENAME <- paste0(total_visits_yearly_total$ASSOCIATIONLISTB, " (", total_visits_yearly_total$total, ")")
    total_visits_yearly_total <- total_visits_yearly_total %>% select(-total, -APPT_MONTH)
    
    total_visits_break_legend <- left_join(total_visits_break, total_visits_yearly_total)
    total_visits_break_legend <- total_visits_break_legend %>% ungroup() %>% select(-ASSOCIATIONLISTB) %>%
      rename(ASSOCIATIONLISTB = APPT_YEAR_RENAME)
    
    
    g3 <- ggplot_bar_graph(total_visits_break_legend, title, total_visits_break_legend$APPT_MONTH_YEAR, total_visits_break_legend$total, total_visits_break_legend$ASSOCIATIONLISTB, max)

    
    Total <- total_visits_break %>% 
      group_by(APPT_MONTH_YEAR) %>%
      summarise(total = sum(total))
    Total$ASSOCIATIONLISTB <- "Total"
    total_visits_break <- full_join(total_visits_break,Total)
    
    n <- length(unique(total_visits_break$ASSOCIATIONLISTB)) - 1
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }
    

    total_visits_break$ASSOCIATIONLISTB <- factor(total_visits_break$ASSOCIATIONLISTB, levels = c("Total","Telehealth Visit","New Visit","Established Visit"))

    total_in_list <- length(unique(total_visits_break$ASSOCIATIONLISTB))-1
    colors <- all_pallete[total_in_list:1]
    
    # g4 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y= AssociationListB, label=total)) +
    #   #scale_color_MountSinai('dark')+
    #   geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
    #   geom_hline(yintercept = hline_y, colour='black')+
    #   geom_vline(xintercept = 0, colour = 'black')+
    #   scale_x_discrete(position = "top") + 
    #   labs( y = NULL, x = NULL, fill = "AssociationListB")+
    #   theme_minimal() +
    #   table_theme()
    
  g4 <- ggplot(total_visits_break, aes(x=APPT_MONTH_YEAR, y= ASSOCIATIONLISTB, label=total)) +
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
      table_theme()
    
  # g3 <- ggplotly(g3, tooltip = c("total")) 
  g4 <- ggplotly(g4, tooltip = NULL)
  
  
  subplot(g3, g4, nrows = 2, margin = 0.1, heights = c(0.6, 0.4)) %>% layout(showlegend = T#, legend = list(title = list(text = "Visit Type"))
  )
  
    
    
  }#, height = function(x) input$plotHeight
  )
  
  
  output$break_treatmentvisitsgraph <- renderPlotly({
    
    data <- dataArrived_Diag()
    
    #data <- historical.data[arrived.data.rows,] %>% filter(SITE == "MSW", Appt.MonthYear == "2020-12")
    #data <- historical.data[arrived.data.rows,] %>% filter(SITE == "MSW")
    # nrow(data)
    
    total_visits_break <- data %>% filter(ASSOCIATIONLISTA == "Treatment") %>%
      group_by(APPT_MONTH_YEAR, ASSOCIATIONLISTT) %>% summarise(total = n()) %>% collect()
    
    max <- total_visits_break %>% group_by(APPT_MONTH_YEAR) %>% summarise(max = sum(total))
    
    factor_levels = c("Pump Disconnect", "Port Flush", "Transfusion", "Phlebotomy", "Hydration", "Injection", "Therapeutic Infusion", "Infusion")
    
    total_visits_break$ASSOCIATIONLISTT <- factor(total_visits_break$ASSOCIATIONLISTT, levels = factor_levels)
    
    total_visits_break <- total_visits_break %>% filter(!is.na(ASSOCIATIONLISTT))
    
    sum <- total_visits_break %>% summarise(sum = sum(total))
    
    total_visits_break <- inner_join(total_visits_break,sum, by = c("APPT_MONTH_YEAR"))
    

    if(length(isolate(input$selectedCampus)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(isolate(input$selectedCampus)),sep="", collapse=", ")
    }
    
    # g5 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y=total, group=AssociationListT, fill=AssociationListT))+
    #   geom_bar(position="stack",stat="identity", width=0.7)+
    #   scale_fill_MountSinai('dark')+
    #   scale_y_continuous(limits=c(0,(max(max$max))*1.2))+
    #   labs(title = paste0(site," ","Treatment Visit Volume Composition"),
    #        subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),"\n"),
    #        y = "Patient Volume\n", x = NULL, fill = NULL)+
    #   theme_new_line()+
    #   theme(axis.title.y = element_text(size = 12, angle = 90), plot.margin=unit(c(1,1,-0.5,1), "cm"))#+
    #   # geom_text(data=subset(total_visits_break, total/sum > .05),aes(label=total), color="white", 
    #   #           size=5, fontface="bold", position = position_stack(vjust = 0.5))+
    #   # stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
    #   #              size=5, fontface="bold.italic")
    # 
    
    
    
    title <- paste0(site," ","Treatment Visit Volume Composition")

    
    total_visits_yearly_total <- total_visits_break %>% group_by(ASSOCIATIONLISTT) %>% 
      summarise(total = sum(total, na.rm = T)) %>% 
      mutate(APPT_MONTH = "Total")
    
    total_visits_yearly_total$APPT_YEAR_RENAME <- paste0(total_visits_yearly_total$ASSOCIATIONLISTT, " (", total_visits_yearly_total$total, ")")
    total_visits_yearly_total <- total_visits_yearly_total %>% select(-total, -APPT_MONTH)
    
    total_visits_break_legend <- left_join(total_visits_break, total_visits_yearly_total)
    total_visits_break_legend <- total_visits_break_legend %>% ungroup() %>% select(-ASSOCIATIONLISTT) %>%
      rename(ASSOCIATIONLISTT = APPT_YEAR_RENAME)
    
    
    g5 <- ggplot_bar_graph(total_visits_break_legend, title, total_visits_break_legend$APPT_MONTH_YEAR, total_visits_break_legend$total, total_visits_break_legend$ASSOCIATIONLISTT, max)
    # g5 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y=total, group=AssociationListT, fill=AssociationListT))+
    #   geom_bar(position="stack",stat="identity", width=0.7)+
    #   scale_fill_MountSinai('dark')+
    #   labs(title = paste0(site," ","Treatment Visit Volume Composition"),
    #        subtitle = paste0("Based on data from ",isolate(input$dateRangetrend[1])," to ",isolate(input$dateRangetrend[2]),"\n"),
    #        y = "Patient Volume", x = NULL, fill = NULL)+
    #   scale_y_continuous(limits=c(0,(max(max$max))*1.2))+
    #   theme(legend.position = 'top',
    #         legend.title=element_blank(),
    #         plot.title = element_text(hjust=0.5, face = "bold", size = 16),
    #         axis.title = element_text(size="12"),
    #         axis.text = element_text(size="12"),
    #         axis.title.x = element_blank(),
    #         axis.line = element_line(size = 0.3, colour = "black"),
    #         axis.title.y = element_text(size = 12, angle = 90)
    #         
    #   )
    
      
    Total <- total_visits_break %>% 
      group_by(APPT_MONTH_YEAR) %>%
      summarise(total = sum(total))
    Total$ASSOCIATIONLISTT <- "Total"
    total_visits_break <- full_join(total_visits_break,Total)
    
    
    n <- length(unique(total_visits_break$ASSOCIATIONLISTT)) - 1
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }
    
    
    factor_levels = c("Total",rev(c("Infusion", "Therapeutic Infusion", "Injection", "Hydration", "Phlebotomy", "Transfusion", "Port Flush", "Pump Disconnect")))

    total_visits_break$ASSOCIATIONLISTT <- factor(total_visits_break$ASSOCIATIONLISTT, levels = factor_levels)
    
    list_length <- length(unique(total_visits_break$ASSOCIATIONLISTT))
    
    
    # g6 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y= AssociationListT, label=total)) +
    #   #scale_color_MountSinai('dark')+
    #   geom_text(size = 7, vjust = "center", hjust = "center", fontface = 'bold')+
    #   geom_hline(yintercept = hline_y, colour='black')+
    #   geom_vline(xintercept = 0, colour = 'black')+
    #   scale_x_discrete(position = "top") + 
    #   labs( y = NULL, x = NULL, fill = "AssociationListT")+
    #   theme_minimal() +
    #   table_theme()
    # 
    
    
    g6 <- ggplot(total_visits_break, aes(x=APPT_MONTH_YEAR, y= ASSOCIATIONLISTT, label=total)) +
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
      geom_text(aes(label= ifelse(is.na(total),"",total)), color="black", size=4, fontface="bold") +
      geom_hline(yintercept = hline_y, colour='black')+
      geom_vline(xintercept = 0, colour = 'black') +
      table_theme()
    
    
    # library(patchwork)
    # g5 + g6 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(total_visits_break$AssociationListT))))
    # plotly_function(g5, c("total"))
    
    
    #g5 <- ggplotly(g5, tooltip = c("total")) 
    g6 <- ggplotly(g6, tooltip = NULL)
    
    
    subplot(g5, g6, nrows = 2, margin = 0.1, heights = c(0.4, 0.6)) %>% layout(showlegend = T#, legend = list(title = list(text = "Year"))
    )
    
    
  }#, height = function(x) input$plotHeight
  )
  
  
  # Volume Comparison Tab ------------------------------------------------------------------------------------------------------
  #Volume Comparison Tab - Total Breakdown
  output$volumeCompTotal_grh <- renderPlotly({
    
    data <- dataArrived_filtered() %>% select(ASSOCIATIONLISTA, ASSOCIATIONLISTB, SITE, APPT_MONTH_YEAR) %>% collect()

    flag <- 0
      

    if(length(unique(data$ASSOCIATIONLISTA)) == 1){
      visitType <- unique(data$ASSOCIATIONLISTA)
      apptType <-  paste(sort(unique(data$ASSOCIATIONLISTB)), sep="", collapse=", ")
      # apptType <-  paste(paste(sort(unique(data$ASSOCIATIONLISTB)), sep="", collapse=", "),", ",
      #                     paste(sort(unique(data$AssociationListT)), sep="", collapse=", "))
    } else{
      visitType <- paste(sort(unique(data$ASSOCIATIONLISTA)),sep="", collapse=", ")
      apptType <-  paste(sort(unique(data$ASSOCIATIONLISTB)), sep="", collapse=", ")
    }
    
    
    if(length(unique(data$SITE)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    
    
    if(input$comp_choices == "All"){
      flag <- 1
      
      #if(input$analysis_type == "Monthly"){
        # Comparison by site
        visit_comp_all <- data %>%
          group_by(APPT_MONTH_YEAR) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_all, aes(x=APPT_MONTH_YEAR, y=total))+
          geom_bar(stat="identity", width=0.7, fill="#212070")+
          scale_y_continuous(limits=c(0,(max(visit_comp_all$total))*1.2))+
          #geom_text(aes(label=total), vjust =-1, color="black", fontface="bold", size=5)+
          theme_new_line()+
          theme(axis.text.x = element_text(angle = 0, hjust=0.5))+
          labs(title = paste0(site, " Monthly ",visitType, " Volume Breakdown by ",input$comp_choices),
               subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),"\n"),
               caption = paste0("\n*Includes ",apptType),
               y = NULL, x = NULL, fill = NULL)

        
        visit_comp_all <- visit_comp_all %>% mutate(Total = "Total")
        table <- ggplot_table_comparison(visit_comp_all, 0)
      # } else{
      #   # Comparison by site
      #   visit_comp_all <- data %>%
      #     group_by(Appt.Week) %>% summarise(total = n())
      #   
      #   graph <- ggplot(visit_comp_all, aes(x=Appt.Week, y=total))+
      #     geom_bar(stat="identity", fill="#212070")+
      #     scale_y_continuous(limits=c(0,(max(visit_comp_all$total))*1.2))+
      #     #geom_text(aes(label=total), vjust =-1, color="black", fontface="bold", size=5)+
      #     scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))+
      #     theme_new_line()+
      #     theme(axis.text.x = element_text(angle = 0, hjust=0.5))+
      #     labs(title = paste0(site, " Weekly ",visitType, " Volume Breakdown by ",input$comp_choices),
      #          subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),"\n"),
      #          caption = paste0("\n*Includes ",apptType),
      #          y = NULL, x = NULL, fill = NULL)+
      #     theme(axis.text.x = element_text(size = 16, angle=50, hjust=1))
      #   
      # }
      
    } else{
      
      #if(input$analysis_type == "Monthly"){
        # Comparison by site
    
        visit_comp_site <- data %>%
          group_by(APPT_MONTH_YEAR, SITE) %>% summarise(total = n())
        
        max <- visit_comp_site %>% group_by(APPT_MONTH_YEAR) %>% summarise(total = sum(total))
        
        Total <- visit_comp_site %>%
                      group_by(APPT_MONTH_YEAR) %>%
                      summarise(Total = sum(total))
        
        visit_comp_site <- full_join(visit_comp_site,Total)
        
        
        graph <- ggplot(visit_comp_site, aes(x=APPT_MONTH_YEAR, y=total, group=SITE, fill=SITE))+
          geom_bar(position="stack", stat="identity", width=0.7)+
          scale_y_continuous(limits=c(0,(max(max$total))*1.2))+
          # stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
          #              size=5, fontface="bold.italic")+
          theme_new_line()+
          labs(title = paste0(site, " Monthly ",visitType, " Volume Breakdown by ",input$comp_choices),
               subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),"\n"),
               #caption = paste0("\n*Includes ",apptType),
               y = NULL, x = NULL, fill = NULL)#+
          #geom_text(data=subset(visit_comp_site, total/Total > .15),aes(label=total), color="white", 
                    #size=5, fontface="bold", position = position_stack(vjust = 0.5))
        
    
        if(length(isolate(input$selectedCampus)) > 1){
          Total$SITE <- " Total"
          visit_comp_site <- subset(visit_comp_site, select = -c(Total))
          Total <- Total %>%
                    rename(total = Total)
          visit_comp_site <- full_join(Total, visit_comp_site)
        }

        n <- length(unique(visit_comp_site$SITE)) - 1
        if(n==0){
          hline_y <- 0
        } else{
          hline_y <- seq(1.5, 0.5+n, by= 1)
        }
        
        g2 <- ggplot(visit_comp_site, aes(x=APPT_MONTH_YEAR, y= SITE, label=total)) +
          #scale_color_MountSinai('dark')+
          geom_text(size = 5, vjust = "center", hjust = "center", fontface = 'bold')+
          geom_hline(yintercept = hline_y, colour='black')+
          geom_vline(xintercept = 0, colour = 'black')+
          scale_x_discrete(position = "top") + 
          labs( y = NULL, x = NULL, fill = "SITE")+
          theme_minimal() +
          table_theme()+
          labs(caption = paste0("\n*Includes ",apptType))+
          theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
        

      # } else{
      #   # Comparison by site
      #   visit_comp_site <- data %>%
      #     group_by(Appt.Week, SITE) %>% summarise(total = n())
      #   
      #   max <- visit_comp_site %>% group_by(Appt.Week) %>% summarise(total = sum(total))
      #   
      #   Total <- visit_comp_site %>%
      #     group_by(Appt.Week) %>%
      #     summarise(Total = sum(total))
      #   
      #   visit_comp_site <- full_join(visit_comp_site,Total)
      #   
      #   graph <- ggplot(visit_comp_site, aes(x=Appt.Week, y=total, group=SITE, fill=SITE))+
      #     geom_bar(position="stack", stat="identity")+
      #     scale_y_continuous(limits=c(0,(max(max$total))*1.2))+
      #     # stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.Week), geom="text", color="black", 
      #     #              size=5, fontface="bold.italic")+
      #     scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))+
      #      theme_new_line()+
      #     labs(title = paste0(site, " Weekly ",visitType, " Volume Breakdown by ",input$comp_choices),
      #          subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),"\n"),
      #          #caption = paste0("\n*Includes ",apptType),
      #          y = NULL, x = NULL, fill = NULL)+
      #     # geom_text(data=subset(visit_comp_site, total/Total > .15),aes(label=total), color="white",
      #     #           size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      #     theme(axis.text.x = element_text(size = 16, angle=50, hjust=1))
      #   
      #   
      #   n <- length(unique(visit_comp_site$SITE)) - 1
      #   if(n==0){
      #     hline_y <- 0
      #   } else{
      #     hline_y <- seq(1.5, 0.5+n, by= 1)
      #   }
      #   
      #   
      #   g2 <- ggplot(visit_comp_site, aes(x=as.character(Appt.Week), y= SITE, label=total)) +
      #     #scale_color_MountSinai('dark')+
      #     geom_text(size = 5, vjust = "center", hjust = "center", fontface = 'bold')+
      #     geom_hline(yintercept = hline_y, colour='black')+
      #     geom_vline(xintercept = 0, colour = 'black')+
      #     scale_x_discrete(position = "top") + 
      #     labs( y = NULL, x = NULL, fill = "SITE")+
      #     theme_minimal() +
      #     table_theme()+
      #     labs(caption = paste0("\n*Includes ",apptType))+
      #     theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
      #   
      # }
    }
    
    

    g1 <- graph + 
      # geom_text(aes(label=total), color="white", 
      #           size=5, fontface="bold", position = position_stack(vjust = 0.5))+
          scale_fill_MountSinai('dark')

    
    
    if(flag == 0){
      library(patchwork)

      #ggplotly(g1, tooltip = c("total")) # + g2 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(visit_comp_site$SITE))))
      subplot(g1, g2, nrows = 2, margin = 0.1, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Visits"), scene = list(aspectration=list(x=1,y=1)))
      
    }else{
        #ggplotly(g1, tooltip = c("total"))
      subplot(g1, table, nrows = 2, margin = 0.1, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Visits"), scene = list(aspectration=list(x=1,y=1)))
    }
      
    
  }#, height = function(x) input$plotHeight
  )
  
  
  # Volume Comparison Tab - Trend Graphs
  output$volumeCompTrend_grh <- renderPlotly({
    
    data <- dataArrived_filtered() %>% select(APPT_MONTH_YEAR, SITE, ASSOCIATIONLISTA, ASSOCIATIONLISTB,ASSOCIATIONLISTT) %>% collect()
    # data <- historical.data[arrived.data.rows,]
    
    flag <- 0

    
    if(length(unique(data$ASSOCIATIONLISTA)) == 1){
      visitType <- unique(data$ASSOCIATIONLISTA)
      apptType <-  paste(sort(unique(data$ASSOCIATIONLISTB)), sep="", collapse=", ")
      # apptType <-  paste(paste(sort(unique(data$AssociationListB)), sep="", collapse=", "),", ",
      #                     paste(sort(unique(data$AssociationListT)), sep="", collapse=", "))
    } else{
      visitType <- paste(sort(unique(data$ASSOCIATIONLISTA)),sep="", collapse=", ")
      apptType <-  paste(sort(unique(data$ASSOCIATIONLISTB)), sep="", collapse=", ")
    }
    
    if(length(unique(data$SITE)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    
    if(input$comp_choices == "All"){
      
      #if(input$analysis_type == "Monthly"){
        flag <- 0
        # Comparison by site
        visit_comp_all <- data %>%
          group_by(APPT_MONTH_YEAR) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_all, aes(x=APPT_MONTH_YEAR, y=total, group=1))+
          geom_line(size=1.1)+
          geom_point(size=3)+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(visit_comp_all$total)*1.2))+
          labs(title = paste0(site, " Monthly ",visitType, " Volume Trend by ",input$comp_choices),
               subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),"\n"),
               #caption = paste0("\n*Includes ",apptType),
               y = NULL, x = NULL, fill = NULL)+
          theme_new_line()+
          theme(axis.text.x = element_text(angle = 0, hjust=0.5))

        g2 <- ggplot(visit_comp_all, aes(x=APPT_MONTH_YEAR, y= "Total", label=total)) +
          scale_color_MountSinai('dark')+
          geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
          geom_hline(yintercept = c(2.5), colour='black')+
          geom_vline(xintercept = 0, colour = 'black')+
          scale_x_discrete(position = "top") +
          labs(y = NULL, x = NULL)+
          theme_minimal() +
          table_theme()+
          labs(caption = paste0("\n*Includes ",apptType))+
          theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
        
      # } else{
      #   # Comparison by site
      #   visit_comp_all <- data %>%
      #     group_by(Appt.Week) %>% summarise(total = n())
      #   
      #   graph <- ggplot(visit_comp_all, aes(x=Appt.Week, y=total, group=1))+
      #     geom_line(size=1.1)+
      #     geom_point(size=3)+
      #     scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))+
      #     scale_y_continuous(expand = c(0,0), limits = c(0,max(visit_comp_all$total)*1.2))+
      #     labs(title = paste0(site, " Weekly ",visitType, " Volume Trend by ",input$comp_choices),
      #          subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),"\n"),
      #          #caption = paste0("\n*Includes ",apptType),
      #          y = NULL, x = NULL, fill = NULL)+
      #     theme_new_line()+
      #     theme(axis.text.x = element_text(size = 16, angle=50, hjust=1))
      #   
      #   
      #   g2 <- ggplot(visit_comp_all, aes(x=as.character(Appt.Week), y= "Total", label=total)) +
      #     scale_color_MountSinai('dark')+
      #     geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
      #     geom_hline(yintercept = c(2.5), colour='black')+
      #     geom_vline(xintercept = 0, colour = 'black')+
      #     scale_x_discrete(position = "top") +
      #     labs(y = NULL, x = NULL)+
      #     theme_minimal() +
      #     table_theme()+
      #     labs(caption = paste0("\n*Includes ",apptType))+
      #     theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
      #   
      # }
      
    } else{
      
      #if(input$analysis_type == "Monthly"){
        flag <- 1
        # Comparison by site
        visit_comp_site <- data %>%
          group_by(APPT_MONTH_YEAR, SITE) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_site, aes(x=APPT_MONTH_YEAR, y=total, group=SITE))+
          geom_line(aes(color=SITE), size=1.1)+
          geom_point(aes(color=SITE), size=3)+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(visit_comp_site$total)*1.2))+
          labs(title = paste0(site, " Monthly ",visitType, " Volume Trend by ",input$comp_choices),
               subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),"\n"),
               #caption = paste0("\n*Includes ",apptType),
               y = NULL, x = NULL, fill = NULL)+
          theme_new_line()+
          theme(axis.text.x = element_text(angle = 0, hjust=0.5))

        if(length(isolate(input$selectedCampus)) > 1) {
          Total <- visit_comp_site %>%
            group_by(APPT_MONTH_YEAR) %>%
            summarise(total = sum(total))
          
          Total$SITE <- " Total"
          visit_comp_site <- full_join(Total, visit_comp_site)
        }

        
        n <- length(unique(visit_comp_site$SITE)) - 1
        if(n==0){
          hline_y <- 0
        } else{
          hline_y <- seq(1.5, 0.5+n, by= 1)
        }
        
        g2 <- ggplot(visit_comp_site, aes(x=APPT_MONTH_YEAR, y= SITE, label=total)) +
          #scale_color_MountSinai('dark')+
          geom_text(size = 5, vjust = "center", hjust = "center", fontface = 'bold')+
          geom_hline(yintercept = hline_y, colour='black')+
          geom_vline(xintercept = 0, colour = 'black')+
          scale_x_discrete(position = "top") + 
          labs( y = NULL, x = NULL, fill = "SITE")+
          theme_minimal() +
          table_theme()+
          labs(caption = paste0("\n*Includes ",apptType))+
          theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
        
        
      # } else{
      #   # Comparison by site
      #   
      # 
      #   
      #   
      #   flag <- 1
      #   visit_comp_site <- data %>%
      #     group_by(Appt.Week, SITE) %>% summarise(total = n())
      #   
      #   n <- length(unique(visit_comp_site$SITE)) - 1
      #   if(n==0){
      #     hline_y <- 0
      #   } else{
      #     hline_y <- seq(1.5, 0.5+n, by= 1)
      #   }
      #   
      #   graph <- ggplot(visit_comp_site, aes(x=Appt.Week, y=total, group=SITE))+
      #     geom_line(aes(color=SITE), size=1.1)+
      #     geom_point(aes(color=SITE), size=3)+
      #     scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))+
      #     scale_y_continuous(expand = c(0,0), limits = c(0,max(visit_comp_site$total)*1.2))+
      #     labs(title = paste0(site, " Weekly ",visitType, " Volume Trend by ",input$comp_choices),
      #          subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),"\n"),
      #          #caption = paste0("\n*Includes ",apptType),
      #          y = NULL, x = NULL, fill = NULL)+
      #     theme_new_line()+
      #     theme(axis.text.x = element_text(size = 16, angle=50, hjust=1))
      #   
      #   g2 <- ggplot(visit_comp_site, aes(x=as.character(Appt.Week), y= SITE , label=total)) +
      #     scale_color_MountSinai('dark')+
      #     geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
      #     geom_hline(yintercept = hline_y, colour='black')+
      #     geom_vline(xintercept = 0, colour = 'black')+
      #     scale_x_discrete(position = "top") +
      #     labs(y = NULL, x = NULL)+
      #     theme_minimal() +
      #     table_theme()+
      #     labs(caption = paste0("\n*Includes ",apptType))+
      #     theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
      #   
      #   
      # }
    }
    
    if(length(unique(data$SITE)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    
    
    g1 <- graph + 
          scale_color_MountSinai('dark')

    
    

    if(flag == 1){
      library(patchwork)

      # ggplotly(g1, tooltip = c("total"))# + g2 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(visit_comp_site$SITE))))
      subplot(g1, g2, nrows = 2, margin = 0.1, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Visits"), scene = list(aspectration=list(x=1,y=1)))
      }else if(flag == 0){
        library(patchwork)
        # ggplotly(g1,  tooltip = c("total"))# + g2 + plot_layout(ncol = 1, heights = c(7, 0.67))
        subplot(g1, g2, nrows = 2, margin = 0.1, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Visits"), scene = list(aspectration=list(x=1,y=1)))
      }else{
        ggplotly(g1,  tooltip = c("total"))
        }
    
  } #, height = function(x) input$plotHeight
  )
  
  
  # Provider Volume Tab ------------------------------------------------------------------------------------------------------
  ## Physician Visits Breakdown Tab =========================================================================================
  
  output$provVolumeExam_tb <- function(){
    
    data <- dataArrived_disease() %>%
      filter(ASSOCIATIONLISTA %in% c("Exam"))

    tele_data <- dataArrived_disease() %>%
      filter(ASSOCIATIONLISTB %in% c("Telehealth Visit"))
    
    tele_data_test <<- tele_data
    data_test <<- data

    # 
    # data <- historical.data[arrived.data.rows,] %>%
    #   filter(AssociationListA == "Exam", SITE %in% c("DBC"))
    # 
    # tele_data <- historical.data[arrived.data.rows,] %>%
    #   filter(AssociationListB == "Telehealth Visit", SITE %in% c("DBC"))
    
    prov_tb <- data %>% 
      group_by(DISEASE_GROUP, DISEASE_GROUP_DETAIL, PROVIDER, ASSOCIATIONLISTB,  APPT_MONTH_YEAR) %>%
      summarise(total = n())  %>% collect() %>%
      `colnames<-` (c("Disease", "Disease Detail", "Provider", "Appointment Type", "Appt.MonthYear", "Total")) %>%
      pivot_wider(names_from = Appt.MonthYear,
                  values_from = Total,
                  values_fill = 0) %>%
      split(.[,"Provider"]) %>%  
      map_df(., janitor::adorn_totals, name = paste0("Total")) %>%
      mutate(Disease = ifelse(Disease == "Total", lag(Disease, 1), Disease),
             Provider = ifelse(Provider == "-", lag(Provider, 1), Provider),
             `Appointment Type` = ifelse(`Appointment Type` == "-", "Exam Total", `Appointment Type`)) %>%
      arrange(-desc(Disease)) %>%
      mutate(`Disease Detail` = na_if(`Disease Detail`, "-")) %>%
      fill(`Disease Detail`, .direction = "down")
    
    tele_tb <- tele_data %>% 
      group_by(DISEASE_GROUP,DISEASE_GROUP_DETAIL,  PROVIDER, APPT_TYPE, APPT_MONTH_YEAR) %>%
      summarise(total = n()) %>% collect() %>%
      `colnames<-` (c("Disease", "Disease Detail","Provider", "Appointment Type", "Appt.MonthYear", "Total")) %>%
      pivot_wider(names_from = Appt.MonthYear,
                  values_from = Total,
                  values_fill = 0)
    
    
    site_selected <- isolate(input$selectedCampus)
    
    if(length(unique(site_selected)) == length(campus_choices)){
      site <- "all sites"
    } else{
      site <- paste(sort(unique(site_selected)),sep="", collapse=", ")
    }
    
    
    appt_order <- c("Exam Total",c("Established Visit", "New Visit", "Telehealth Visit"), as.vector(unique(tele_tb$`Appointment Type`)))
    
    final_df <- bind_rows(prov_tb, tele_tb)
    final_df <- final_df[order(match(final_df$`Appointment Type`, appt_order)), ]
    final_df <- final_df %>%
      arrange(Disease, `Disease Detail`, Provider) %>%
      adorn_totals("col", fill = "-", na.rm = TRUE, name = "Total") 
    
    indent_rows <- which(final_df$`Appointment Type` %in% unique(tele_tb$`Appointment Type`))
    
    header_above <- c("Subtitle" = ncol(final_df))
    names(header_above) <- paste0(c("Based on data from "),c(site))
    
    
    months_sorted <- sort(colnames(final_df)[5:(length(final_df)-1)])
    col_order <- c(colnames(final_df)[1:4], months_sorted, colnames(final_df)[length(final_df)])
    final_df <- final_df[, col_order]
    final_df$Disease = ifelse(duplicated(final_df$Disease),"",final_df$Disease)
    final_df$Provider = ifelse(duplicated(final_df$Provider),"",final_df$Provider)
    
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
      collapse_rows(c(1,2,3), valign = "top") %>%
      add_indent(indent_rows, level_of_indent = 2) %>%
      gsub("NA", " ", .)
  }
  
  
  output$provVolumeTreatment_grph <- renderPlotly({
    data <- dataArrivedTrend()
  })
  
  output$provVolumeTreatment_tb <- function(){
  
    data <- dataArrivedTrend()

    treatment_data <-  data %>% filter(ASSOCIATIONLISTA == "Treatment") %>% 
                      group_by(PROVIDER, APPT_MONTH_YEAR) %>%
                     summarise(total = n())  %>% collect() %>%
      `colnames<-` (c("Provider", "Appt.MonthYear", "Total")) %>%
      pivot_wider(names_from = Appt.MonthYear,
                  values_from = Total,
                  values_fill = 0)
    treatment_data$Total <- rowSums(treatment_data[2:length(treatment_data)])
    
    months_sorted <- sort(colnames(treatment_data)[2:(length(treatment_data)- 1)])
    col_order <- c(colnames(treatment_data)[1], months_sorted, colnames(treatment_data)[length(treatment_data)])
    treatment_data <- treatment_data[, col_order]
    
    treatment_data <- treatment_data[order(treatment_data$Provider),]
    
    site_selected <- isolate(input$selectedCampus)
    
    if(length(unique(site_selected)) == length(campus_choices)){
      site <- "all sites"
    } else{
      site <- paste(sort(unique(site_selected)),sep="", collapse=", ")
    }
    header_above <- c("Subtitle" = ncol(treatment_data))
    names(header_above) <- paste0(c("Based on data from "),c(site))
    
    
    treatment_data %>% 
      kable(booktabs = T, escape = F) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      add_header_above(header_above, color = "black", font_size = 16, align = "center", italic = TRUE) %>%
      add_header_above(c("Physician Treatment Visits Breakdown" = length(treatment_data)),
                       color = "black", font_size = 20, align = "center", line = FALSE) %>% 
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      column_spec(length(treatment_data), background = "#d80b8c", color = "white", bold = T) %>%
      column_spec(1, bold = T) %>%
      # add_indent(indent_rows, level_of_indent = 2) %>%
      gsub("NA", " ", .)
  }
  
  dataArrivedTrend_provider_volume <- reactive({
    input$update_filters_treatment
    selected_referring_provider <- isolate(input$selected_referring_provider_treatment)
    
    if(length(selected_referring_provider) > 1000) {
      data <- dataArrivedTrend()
    } else {
      data <- dataArrivedTrend() %>% filter(REFERRING_PROVIDER %in% selected_referring_provider)
      
    }
  })
  
  
  output$provVolumeTreatmentReferring_grph <- renderPlotly({
    data <- dataArrivedTrend_provider_volume()

    treatment_data <-  data %>% filter(ASSOCIATIONLISTA == "Treatment") %>% 
      filter(ASSOCIATIONLISTB == "Treatment Visit") %>% 
      group_by(APPT_MONTH_YEAR) %>%
      summarise(total = n())  %>% collect()
    
    site <- paste(sort(unique(isolate(input$selectedCampus))),sep="", collapse=", ")
    
    plot_ly(treatment_data, x=~APPT_MONTH_YEAR, y = ~total, type = 'bar', name = "Treatment Visits",
              marker = list(color = "#212070")) %>%
    layout(xaxis = list(title = "Month"),
           yaxis = list(title = "Treatment Visits"),
           title = paste0(site, " Referring Physician Treatment Visits"))
    
  })
  
  
  output$provVolumeTreatmentReferring_tb <- function(){
    
    data <- dataArrivedTrend_provider_volume()
    
    treatment_data <-  data %>% filter(ASSOCIATIONLISTA == "Treatment") %>% 
      filter(ASSOCIATIONLISTB == "Treatment Visit") %>% 
      group_by(REFERRING_PROVIDER, APPT_MONTH_YEAR) %>%
      summarise(total = n())  %>% collect() %>%
      `colnames<-` (c("Referring Provider", "Appt.MonthYear", "Total")) %>%
      pivot_wider(names_from = Appt.MonthYear,
                  values_from = Total,
                  values_fill = 0)
    treatment_data$Total <- rowSums(treatment_data[2:length(treatment_data)])
    
    months_sorted <- sort(colnames(treatment_data)[2:(length(treatment_data)- 1)])
    col_order <- c(colnames(treatment_data)[1], months_sorted, colnames(treatment_data)[length(treatment_data)])
    treatment_data <- treatment_data[, col_order]
    
    treatment_data <- treatment_data[order(treatment_data$`Referring Provider`),]
    
    site_selected <- isolate(input$selectedCampus)
    
    if(length(unique(site_selected)) == length(campus_choices)){
      site <- "all sites"
    } else{
      site <- paste(sort(unique(site_selected)),sep="", collapse=", ")
    }
    header_above <- c("Subtitle" = ncol(treatment_data))
    names(header_above) <- paste0(c("Based on data from "),c(site))
    
    
    treatment_data %>% 
      kable(booktabs = T, escape = F) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      add_header_above(header_above, color = "black", font_size = 16, align = "center", italic = TRUE) %>%
      add_header_above(c("Referring Physician Treatment Visits" = length(treatment_data)),
                       color = "black", font_size = 20, align = "center", line = FALSE) %>% 
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      column_spec(length(treatment_data), background = "#d80b8c", color = "white", bold = T) %>%
      column_spec(1, bold = T) %>%
      # add_indent(indent_rows, level_of_indent = 2) %>%
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
    
    #data <- uniquePts_df_system(dataArrived(), c("Exam","Labs","Treatment"))
    data <- historical.data.unique.all 
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
    
    # if(input$sort_unique == FALSE){
    #   data <- uniquePts_df_system(dataArrivedTrend(), c("Exam","Labs","Treatment"))
    # 
    # }else{
    # 
    #   data <- uniquePts_df_system_rev(dataArrivedTrend(), c("Exam","Labs","Treatment"))
    # 
    # }
    
    #data <- uniquePts_df_system(dataArrivedTrend(), c("Exam","Labs","Treatment"))
    data <- dataUniqueAll_system()
    #data <- historical.data.unique.all
    

    
    
    #data <- uniquePts_df_system(historical.data[arrived.data.rows,], c("Exam","Labs","Treatment"))
    
    unique <- data %>%
      group_by(Appt.MonthYear) %>%
      summarise(total = n())

    unique$Appt.MonthYear <- as.factor(unique$Appt.MonthYear)
    
    # unique <- data %>%
    #   group_by(Appt.Year, Appt.Month) %>%
    #   summarise(total = n())
    
     
    if(length(unique(data$SITE)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    # if(input$sort_unique == FALSE){
    #   g9 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))
    #   g10 <-  ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total))
    #   
    # }else{
    #   
    #   g9 <- ggplot(unique, aes(x=factor(Appt.MonthYear, levels = rev(as.vector(Appt.MonthYear))), y=total, group=1))
    #   g10 <-  ggplot(unique, aes(x=factor(Appt.MonthYear, levels = rev(as.vector(Appt.MonthYear))), y= "System", label= total))
    #   
    # }
    # 
    

      g9 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))
      g10 <-  ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total))
    
    # g9 <- ggplot(unique, aes(x= factor(Appt.Month, levels = monthOptions), y= total, fill = Appt.Year))
    # g10 <-  ggplot(unique, aes(x= factor(Appt.Month, levels = monthOptions), y= Appt.Year, label=total))
    
    
    g9 <- g9+
      # geom_line(size=1.1)+
      # geom_point(size=3)+
       #geom_bar(position="dodge", stat="identity")+
      geom_bar(stat="identity", fill = "#221f72")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("System Unique Patients over Time*"),
           subtitle = paste0("Based on arrived visits from ", unique_min, " to ",unique_max, "\n" ),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))
      # geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
      #            nudge_x = 0.1, size=5)
    
    
    # n <- length(unique(unique$Appt.Year)) - 1
    # if(n==0){
    #   hline_y <- 0
    # } else{
    #   hline_y <- seq(1.5, 0.5+n, by= 1)
    # }
    # 
    
    g10 <- g10+
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      #geom_hline(yintercept = c(2.5), colour='black')+
      #geom_hline(yintercept = hline_y, colour='black')+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") +
      labs(y = NULL, x = NULL,
           caption = paste0("*Total count of unique patients who had at least one visit at any MSHS site over the past 3 years (",unique_min, " to ", unique_max,")" ))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))

    library(patchwork)
    g9 + g10 + plot_layout(ncol = 1, heights = c(7, 0.67))#*length(unique(unique$Appt.Year))))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Month
  output$uniqueAllMonthSystem <- renderPlot({
    
    data <- dataUniqueAll_system_month()
    
    
    # if(input$sort_unique == FALSE){
    #   data <- uniquePts_df_systemMonth(dataArrived(), c("Exam","Labs","Treatment"))
    # }else{
    #   data <- uniquePts_df_systemMonth_rev(dataArrived(), c("Exam","Labs","Treatment"))
    # 
    # }
    
    
    # data <- uniquePts.all.data
    
    if(length(unique(data$SITE)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>%
      group_by(Appt.MonthYear) %>%
      summarise(total = n())

    unique$Appt.MonthYear <- as.factor(unique$Appt.MonthYear)
    
    # unique <- data %>% 
    #   group_by(Appt.Year, Appt.Month) %>%
    #   summarise(total = n())
    
    # if(input$sort_unique == FALSE){
    #   g11 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))
    #   g12 <-  ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total))
    # }else{
    #   g11 <- ggplot(unique, aes(x=factor(Appt.MonthYear, levels = rev(as.vector(Appt.MonthYear))), y=total, group=1))
    #   g12 <-  ggplot(unique, aes(x=factor(Appt.MonthYear, levels = rev(as.vector(Appt.MonthYear))), y= "System", label= total))
    #   
    # }
    
      g11 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))
      g12 <-  ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total))
    
    
    # g11 <- ggplot(unique, aes(x=factor(Appt.Month, levels = monthOptions), y=total, fill=Appt.Year))
    # g12 <-  ggplot(unique, aes(x= factor(Appt.Month, levels = monthOptions), y= Appt.Year, label=total))
    
    g11 <- g11+
      # geom_line(size=1.1)+
      # geom_point(size=3)+
      # geom_bar(position="dodge", stat="identity")+
      geom_bar(stat="identity", fill = "#221f72")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("System Unique Patients by Month*"),
           subtitle = paste0("Based on arrived visits from ",isolate(input$dateRangeunique[1])," to ",isolate(input$dateRangeunique[2]),"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))
      # geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
      #            nudge_x = 0.1, size=5)
    
    g12 <- g12+
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL,
           caption = paste0("*Total count of unique patients who had at least one visit at any MSHS site within the respective month (",isolate(input$dateRange[1]), " to ", isolate(input$dateRange[2]),")"))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    library(patchwork)
    g11 + g12 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Site
  output$uniqueAllSite <- renderPlot({
    
    #data <- uniquePts_df_site(dataArrived(), c("Exam","Labs","Treatment"))
    # data <- uniquePts.all.data
    data <- historical.data.site.all
    
    unique <- data %>%
      group_by(SITE) %>%
      summarise(total = n())
    
    g7 <- ggplot(unique, aes(x=SITE, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Total Unique Patients by Site*"),
           subtitle = paste0("Based on arrived visits from ", unique_min, " to ",unique_max, "\n" ),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      #theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      # geom_text(aes(label=total), color="white", 
      #           size=5, fontface="bold", vjust = 1.2, position = position_dodge(0.9))
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = SITE), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    g8 <- ggplot(unique, aes(x=SITE, y= "Site", label=total)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE",
            caption = paste0("*Total count of unique patients who had at least one visit within a respecctive MSHS site over the past 3 years (",unique_min, " to ", unique_max,")" ))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    library(patchwork)
    g7 + g8 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN  over Time (Months)
  output$uniqueAllTrendSite <- renderPlot({
    
    #data <- uniquePts_df_site(dataArrived(), c("Exam","Labs","Treatment"))
    # data <- uniquePts_df_site(historical.data[arrived.data.rows,], c("Exam","Labs","Treatment"))
    data <- dataUniqueAll_site()
    
    
    
    if(length(unique(data$SITE)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear, SITE) %>%
      summarise(total = n())
    
    
    sum <- unique %>% group_by(Appt.MonthYear) %>% summarise(sum = sum(total))
    unique <- inner_join(unique,sum, by = c("Appt.MonthYear"))
    
    unique$Appt.MonthYear <- as.factor(unique$Appt.MonthYear)
    
    # if(input$sort_unique3 == TRUE){
    #   unique$Appt.MonthYear <- factor(unique$Appt.MonthYear, levels = rev(unique(unique$Appt.MonthYear)), ordered = TRUE)
    # }
    
    
    Total <- unique %>% 
      group_by(Appt.MonthYear) %>%
      summarise(total = sum(total))
    Total$SITE <- " Total"
    
    
    
    g9 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE))+
      # geom_line(size=1.1)+
      # geom_point(size=3)+
      # scale_color_MountSinai('dark')+
      geom_bar(position="stack", stat="identity", width =0.7)+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$sum)*1.5))+
      labs(title = paste0("Unique Patients by Site over Time*"),
           subtitle = paste0("Based on arrived visits from ", unique_min, " to ",unique_max, "\n" ),
           y = NULL, x = NULL)+
      theme_new_line()+
      #theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      # geom_text(data = subset(unique, total/sum > 0.35),aes(label=total), color="white", 
      #           size=5, fontface="bold", position = position_stack(vjust = 0.5))+
     geom_text(data = Total, aes(x = Appt.MonthYear, total, label = total, fill = NULL), color = "black", size = 5, fontface = "bold.italic", vjust = -1)
    
    
    unique <- full_join(Total, unique)
    
    pallete <- types_pallete[1:length(unique(unique$SITE))-1]
    
    
    g10 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = rev(SITE)))+
      #scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") +
      labs( y = NULL, x = NULL, fill = "SITE",
            caption = paste0("*Total count of unique patients who had at least one visit within a respective MSHS site over the past 3 years (",unique_min, " to ", unique_max,")" ))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    

    
    # total_visits_break <- full_join(total_visits_break,Total)
    # g11 <- ggplot(Total, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE))+
    #   #scale_color_MountSinai('dark')+
    #   scale_color_manual(values = "#000000")+
    #   geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
    #   geom_hline(yintercept = seq(0.5, length(unique(Total$SITE)), by= 1)[-1], colour='black')+
    #   geom_vline(xintercept = 0, colour = 'black')+
    #   scale_x_discrete(position = "top") + 
    #   labs( y = NULL, x = NULL, fill = "SITE")+
    #   theme_minimal() +
    #   table_theme()
    
    library(patchwork)
    g9 + g10 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(unique$SITE))))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Month
  output$uniqueAllMonthSite <- renderPlot({
    
    data <- dataUniqueAll_site_month()
    #data <- historical.data.unique.all.month
    
    
    #data <- uniquePts_df_siteMonth(dataArrived(), c("Exam","Labs","Treatment"))
    # data <- uniquePts.all.data
    
    if(length(unique(data$SITE)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear, SITE) %>%
      summarise(total = n())
    
    sum <- unique %>% group_by(Appt.MonthYear) %>% summarise(sum = sum(total))
    unique <- inner_join(unique,sum, by = c("Appt.MonthYear"))
    
    #to get the upper limit for the y_continuous
    unique_ <- unique %>% spread(SITE, total)
    unique_$Appt.MonthYear <- NULL
    max_col <- function(data) sapply(data, max, na.rm = TRUE)
    max_tot_site <- max_col(unique_)
    
    unique$Appt.MonthYear <- as.factor(unique$Appt.MonthYear)
    
    Total <- unique %>% 
      group_by(Appt.MonthYear) %>%
      summarise(total = sum(total))
    Total$SITE <- " Total"
    
    g11 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,sum(max_tot_site)*1.2))+
      labs(title = paste0("Unique Patients by Site and Month*"),
           subtitle = paste0("Based on arrived visits from ",isolate(input$dateRangeunique[1])," to ",isolate(input$dateRangeunique[2]),"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      # geom_text(data = subset(unique, total/sum > 0.35),aes(label=total), color="white", 
      #           size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      # stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
      #              size=5, fontface="bold.italic")
      geom_text(data = Total, aes(x = Appt.MonthYear, total, label = total, fill = NULL), color = "black", size = 5, fontface = "bold.italic", vjust = -1)
    

    
    unique <- full_join(Total, unique)
    
    pallete <- types_pallete[1:length(unique(unique$SITE))-1]
    
    
    g12 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE)) +
      #scale_color_MountSinai('dark')+
      scale_color_manual(values = c("#000000",pallete))+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE",
            caption = paste0("*Total count of unique patients who had at least one visit within a respective MSHS site and month over the past 3 years (",isolate(input$dateRange[1]), " to ", isolate(input$dateRange[2]),")" ))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    library(patchwork)
    g11 + g12 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(unique$SITE))))
    
  }, height = function(x) input$plotHeight)
  
  
  # Unique Patients by System and Site - Exam Tab --------------------------------------------------------------------------------------------
  ## Unique MRN by System
  output$uniqueOfficeSystem <- renderValueBox({
    data <- dataArrived_unique_trend()

    data <- uniquePts_df_system(data, c("Exam")) %>% summarise(total = n()) %>% collect()
    # data <- uniquePts.office.data
    #data <- historical.data.unique.exam
    
    valueBoxSpark(
      value =  prettyNum(data$total, big.mark = ','),
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
    print("1")
    
    data <- dataArrived_unique_trend()
    data_trend_test <<- data

    unique <- uniquePts_df_system(data, c("Exam")) %>% group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) %>% collect()
    unique_test <<- unique
    
    min_date <- as.Date(paste0(format(as.Date(isolate(input$dateRange[1]), format = "%Y-%m-%d"), "%Y-%m"), "-01"))
    unique$APPT_MONTH_YEAR <- as.Date(paste0(unique$APPT_MONTH_YEAR,"-01"), format = "%Y-%m-%d")
    unique <- unique %>% filter(APPT_MONTH_YEAR >= min_date)
    unique$APPT_MONTH_YEAR <- format(unique$APPT_MONTH_YEAR, "%Y-%m")

print("2")
    
    # input$update_filters
    # 
    # data <- uniquePts_df_system(historical.data[arrived.data.rows.unique,], c("Exam"))
    # 
    # unique_min <- min(data$Appt.DateYear)
    # unique_max <- max(data$Appt.DateYear)
    # 
    # data <- data %>% filter(SITE %in% input$selectedCampus, Department %in% input$selectedDepartment, 
    #                         input$dateRangeunique[1] <= Appt.DateYear, input$dateRangeunique[2] >= Appt.DateYear, Appt.Day %in% input$daysOfWeek, !holiday %in% input$excludeHolidays)
    # 

    
    
    

    # data <- uniquePts.office.data
    
    # if(length(unique(data$SITE)) == length(campus_choices)){
    #   site <- "System"
    # } else{
    #   site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    # }
    
    # unique <- data %>%
    #   group_by(APPT_MONTH_YEAR) %>%
    #   summarise(total = n())

    unique$APPT_MONTH_YEAR <- as.factor(unique$APPT_MONTH_YEAR)
    
    
    # unique <- data %>%
    #   group_by(Appt.Year, Appt.Month) %>%
    #   summarise(total = n())
    # 
    # 
    # 
    # n <- length(unique(unique$Appt.Year)) - 1
    # if(n==0){
    #   hline_y <- 0
    # } else{
    #   hline_y <- seq(1.5, 0.5+n, by= 1)
    # }
    
    g15 <- ggplot(unique, aes(x=APPT_MONTH_YEAR, y=total, group=1))
    g16 <-  ggplot(unique, aes(x=APPT_MONTH_YEAR, y= "System", label= total))
    
      # g15 <- ggplot(unique, aes(x=factor(Appt.Month, levels = monthOptions), y=total, fill=Appt.Year))
      # g16 <-  ggplot(unique, aes(x= factor(Appt.Month, levels = monthOptions), y= Appt.Year, label=total))
    
    g15 <- g15+
      # geom_line(aes(color = Appt.Year),size=1.1)+
      # geom_point(aes(color = Appt.Year),size=3)+
      #geom_bar(position="dodge", stat="identity")+
      geom_bar(stat="identity", fill = "#221f72")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("System Unique Patients over Time - Exam Visits*"),
           subtitle = paste0("Based on arrived visits from ", unique_min, " to ",isolate(input$dateRange[2]), "\n" ),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))
    
    g16 <- g16 +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      #geom_hline(yintercept = hline_y, colour='black')+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL,
           caption = paste0("*Total count of unique patients who had at least one exam visit at any MSHS site over the past 3 years (",unique_min, " to ", isolate(input$dateRange[2]),")" ))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    library(patchwork)
    g15 + g16 + plot_layout(ncol = 1, heights = c(7, 0.67))#* length(unique(unique$Appt.Year))))
    
  })
  
  
  ## Unique MRN by Month
  output$uniqueOfficeMonthSystem <- renderPlotly({
    data <- dataArrived()
     unique <- uniquePts_df_systemMonth(data, c("Exam")) %>% group_by(SITE, APPT_MONTH_YEAR) %>% summarise(total = n()) %>% collect()


  
    # data <- uniquePts.office.data
    
    # if(length(unique(data$SITE)) == length(campus_choices)){
    #   site <- "System"
    # } else{
    #   site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    # }

    unique$APPT_MONTH_YEAR <- as.factor(unique$APPT_MONTH_YEAR)
    
      # g17 <- ggplot(unique, aes(x=factor(Appt.Month, levels = monthOptions), y=total, fill=Appt.Year))
      # g18 <-  ggplot(unique, aes(x= factor(Appt.Month, levels = monthOptions), y= Appt.Year, label=total))
    g17 <- ggplot(unique, aes(x=APPT_MONTH_YEAR, y=total, group=SITE, fill=SITE))
    # g18 <-  ggplot(unique, aes(x=APPT_MONTH_YEAR, y= "System", label= total))
    
    g17 <- g17+
      # geom_bar(position="dodge", stat="identity")+
      geom_bar(position="stack", stat="identity", width=0.7)+
      scale_fill_MountSinai('dark')+
      # scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Site Unique Patients by Month - Exam Visits*"),
           subtitle = paste0("Based on arrived visits from ", isolate(input$dateRange[1]), " to ",isolate(input$dateRange[2]), "\n" ),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))#+
      # geom_text(aes(label=total), color="white", 
      #           size=5, fontface="bold", position = position_stack(vjust = 0.5))
    
    # g18 <- g18+
    #   scale_color_MountSinai('dark')+
    #   geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
    #   # geom_hline(yintercept = hline_y, colour='black')+
    #   geom_hline(yintercept = c(2.5), colour='black')+
    #   geom_vline(xintercept = 0, colour = 'black')+
    #   scale_x_discrete(position = "top") + 
    #   labs(y = NULL, x = NULL,
    #        caption = paste0("*Total count of unique patients who had at least one exam visit at any MSHS site within the respective month (",isolate(input$dateRange[1]), " to ", isolate(input$dateRange[2]),")" )
    #        )+
    #   theme_minimal() +
    #   table_theme()+
    #   theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    
    if(length(unique(unique$SITE)) == 1) {
      unique <- unique %>% mutate(Total = "Total")
      g18 <- ggplot_table_comparison(unique, 0)
    } else {
      n <- length(unique(unique$SITE)) - 1
      hline_y <- seq(1.5, 0.5+n, by= 1)
      
      g18 <- ggplot(unique, aes(x=APPT_MONTH_YEAR, y= SITE, label=total)) +
        #scale_color_MountSinai('dark')+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = 'bold')+
        geom_hline(yintercept = hline_y, colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs( y = NULL, x = NULL, fill = "SITE")+
        theme_minimal() +
        table_theme()+
        theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    }
    
    library(patchwork)
    # g17 + g18 + plot_layout(ncol = 1, heights = c(7, 0.67))
    subplot(g17, g18, nrows = 2, margin = 0.1, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Visits"), scene = list(aspectration=list(x=1,y=1))) %>%
      style(hoverinfo = 'none')
  })
  
  
  ## Unique MRN by Site
  output$uniqueOfficeSite <- renderPlot({
    
    data <- uniquePts_df_site(dataArrived(), c("Exam"))
    # data <- uniquePts.office.data
    #data <- historical.data.site.exam
    
    unique <- data %>% 
      group_by(SITE) %>%
      summarise(total = n()) %>% collect()
    
    g13 <- ggplot(unique, aes(x=SITE, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Total Unique Patients by Site - Exam Visits*"),
           subtitle = paste0("Based on arrived vistis from ", unique_min, " to ",unique_max, "\n" ),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      # geom_text(aes(label=total), color="white", 
      #           size=5, fontface="bold", vjust = 1.2, position = position_dodge(0.9))
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = SITE), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    g14 <- ggplot(unique, aes(x=SITE, y= "Site", label=total)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE",
            caption = paste0("*Total count of unique patients who had at least one exam visit within a respecctive MSHS site over the past 3 years (",unique_min, " to ", unique_max,")" ))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    library(patchwork)
    g13 + g14 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
  })
  
  ## Unique MRN  over Time (Months)
  output$uniqueOfficeTrendSite <- renderPlot({
    
    #data <- uniquePts_df_site(dataArrived(), c("Exam"))
    # data <- uniquePts.office.data
    
    data <- dataUniqueExam_site()
      
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% 
      group_by(Appt.MonthYear, SITE) %>%
      summarise(total = n())
    
    unique$Appt.MonthYear <- as.factor(unique$Appt.MonthYear)
    
    
    
    sum <- unique %>% group_by(Appt.MonthYear) %>% summarise(sum = sum(total))
    unique <- inner_join(unique,sum, by = c("Appt.MonthYear"))
    
    Total <- unique %>% 
      group_by(Appt.MonthYear) %>%
      summarise(total = sum(total))
    Total$SITE <- " Total"
    

    g15 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE)) +
      # geom_line(size=1.1)+
      # geom_point(size=3)+
      # scale_color_MountSinai('dark')+
      geom_bar(position="stack", stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$sum)*1.2))+
      labs(title = paste0("Unique Patients by Site over Time - Exam Visits*"),
           subtitle = paste0("Based on arrived vistis from ", unique_min, " to ",unique_max, "\n" ),
           y = NULL, x = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      # geom_text(data = subset(unique, total/sum > 0.30),aes(label=total), color="white", 
      #           size=5, fontface="bold", position = position_stack(vjust = 0.5))
    # geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
    #            nudge_x = 0.1, size=5)
      geom_text(data = Total, aes(x = Appt.MonthYear, total, label = total, fill = NULL), color = "black", size = 5, fontface = "bold.italic", vjust = -1)
    

    unique <- full_join(Total, unique)
    
    pallete <- types_pallete[1:length(unique(unique$SITE))-1]
    
    
    g16 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE))+
      scale_color_manual(values = c("#000000",pallete))+      
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE",
            caption = paste0("**Total count of unique patients who had at least one exam visit within a respecctive MSHS site over the past 3 years (",unique_min, " to ", unique_max,")" ))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    library(patchwork)
    g15 + g16 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(unique$SITE))))
    
  })
  
  
  ## Unique MRN by Month
  output$uniqueOfficeMonthSite <- renderPlot({
    
    #data <- uniquePts_df_siteMonth(dataArrived(), c("Exam"))
    #data <- uniquePts_df_siteMonth(historical.data[arrived.data.rows,], c("Exam"))
    
    data <- dataUniqueExam_site_month()
    
    if(length(unique(data$SITE)) == 9){
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
    
    unique$Appt.MonthYear <- as.factor(unique$Appt.MonthYear)
    
    Total <- unique %>% 
      group_by(Appt.MonthYear) %>%
      summarise(total = sum(total))
    Total$SITE <- " Total"
    

    g17 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,sum(max_tot_site)*1.2))+
      
      labs(title = paste0("Unique Patients by Site and Month - Exam Visits*"),
           subtitle = paste0("Based on arrived vistis from ",isolate(input$dateRangeunique[1])," to ",isolate(input$dateRangeunique[2]),"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      # geom_text(data=subset(unique, total/sum > .35),aes(label=total), color="white", 
      #           size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      # stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
      #              size=5, fontface="bold.italic")
      geom_text(data = Total, aes(x = Appt.MonthYear, total, label = total, fill = NULL), color = "black", size = 5, fontface = "bold.italic", vjust = -1)

    
    unique <- full_join(Total, unique)
    
    pallete <- types_pallete[1:length(unique(unique$SITE))-1]
    
    g18 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE))+
      scale_color_manual(values = c("#000000",pallete))+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE",
            caption = paste0("*Total count of unique patients who had at least one exam visit within a respective MSHS site and month over the past 3 years (",isolate(input$dateRange[1]), " to ", isolate(input$dateRange[2]),")" ))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    library(patchwork)
    g17 + g18 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(unique$SITE))))
    
  })
  
  # Unique Patients by System and Site - Treatment Tab ------------------------------------------------------------------
  ## Unique MRN by System
  output$uniqueTreatmentSystem <- renderValueBox({
    
    #data <- uniquePts_df_system(dataArrived(), c("Treatment Visit"))
    # data <- uniquePts_df(arrived.data, c("Treatment"))
    # data <- historical.data.unique.treatment
    data <- dataArrived_unique_trend()
    
    data <- uniquePts_df_system(data, c("Treatment Visit")) %>% summarise(total = n()) %>% collect()
    
    valueBoxSpark(
      value =  prettyNum(data$total, big.mark = ','),
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
    
    # if(input$sort_unique5 == FALSE){
    #   data <- uniquePts_df_system(dataArrived(), c("Treatment Visit"))
    #   
    # }else{
    #   data <- uniquePts_df_system_rev(dataArrived(), c("Treatment Visit"))
    # }
    # 
    print("1")
    # data <- dataUniqueTreatment_system()
    # data <- dataArrived_unique_trend()
    data <- dataArrived()
    data_test <<- data
    # data <- uniquePts_df_system(arrived.data, c("Treatment Visit"))
    
    if(length(isolate(input$selectedCampus)) == length(campus_choices)){
      site <- "System"
    } else{
      site <- paste(sort(isolate(input$selectedCampus)),sep="", collapse=", ")
    }
    # 
    
    unique <- uniquePts_df_system(data, c("Treatment Visit")) %>% group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) %>% collect()
    # unique <- data %>%
    #   group_by(Appt.MonthYear) %>%
    #   summarise(total = n())

    unique$APPT_MONTH_YEAR <- as.factor(unique$APPT_MONTH_YEAR)
    
    # unique <- data %>% 
    #   group_by(Appt.Year, Appt.Month) %>%
    #   summarise(total = n())
    

    

      g21 <- ggplot(unique, aes(x=APPT_MONTH_YEAR, y=total, group=1))
      g22 <-  ggplot(unique, aes(x=APPT_MONTH_YEAR, y= "System", label= total))
    
    # g21 <- ggplot(unique, aes(x=factor(Appt.Month, levels = monthOptions), y=total, fill=Appt.Year))
    # g22 <-  ggplot(unique, aes(x= factor(Appt.Month, levels = monthOptions), y= Appt.Year, label=total))
    
    
    g21 <- g21+
      # geom_line(size=1.1)+
      # geom_point(size=3)+
      #scale_color_MountSinai('dark')+
      # geom_bar(position="dodge", stat="identity")+
      geom_bar(stat="identity", fill = "#221f72")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("System Unique Patients over Time - Treatment Visits*"),
           subtitle = paste0("Based on arrived visits from ",unique_min," to ",isolate(input$dateRange[2]),"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))
      # geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
      #            nudge_x = 0.1, size=5)
    
    g22 <- g22+
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL,
           caption = paste0("*Total count of unique patients who had at least one treatment visit at any MSHS site over the past 3 years (",unique_min, " to ", isolate(input$dateRange[2]),")" ))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    g21 + g22 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
  })
  
  
  ## Unique MRN by Month
  output$uniqueTreatmentMonthSystem <- renderPlotly({

    
    # data <- dataUniqueTreatment_system_month()
    

    # data <- uniquePts_df(arrived.data, c("Treatment Visit"))
    
    
    data <- dataArrived()
    unique <- uniquePts_df_systemMonth(data, c("Treatment Visit")) %>% group_by(SITE, APPT_MONTH_YEAR) %>% summarise(total = n()) %>% collect()
    
    # if(length(unique(data$SITE)) == 9){
    #   site <- "System"
    # } else{
    #   site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    # }
    
    # unique <- data %>%
    #   group_by(APPT_MONTH_YEAR) %>%
    #   summarise(total = n())

    unique$APPT_MONTH_YEAR <- as.factor(unique$APPT_MONTH_YEAR)
    
    # unique <- data %>% 
    #   group_by(Appt.Year, Appt.Month) %>%
    #   summarise(total = n())
    
    
    # g23 <- ggplot(unique, aes(x=factor(Appt.Month, levels = monthOptions), y=total, fill=Appt.Year))
    # g24 <- ggplot(unique, aes(x= factor(Appt.Month, levels = monthOptions), y= Appt.Year, label=total))
    
      g23 <- ggplot(unique, aes(x=APPT_MONTH_YEAR, y=total, group=SITE, fill = SITE))
      # g24 <- ggplot(unique, aes(x=APPT_MONTH_YEAR, y= "System", label= total))

    
    
    g23 <- g23+
      # geom_line(size=1.1)+
      # geom_point(size=3)+
      # scale_color_MountSinai('dark')+
      # geom_bar(position="dodge", stat="identity")+
      geom_bar(position="stack", stat="identity", width=0.7)+
      scale_fill_MountSinai('dark')+
      labs(title = paste0("System Unique Patients by Month  - Treatment Visits*"),
           subtitle = paste0("Based on arrived vistis from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))#+
      # geom_text(aes(label=total), color="white", 
      #           size=5, fontface="bold", position = position_stack(vjust = 0.5))
      # geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
      #            nudge_x = 0.1, size=5)

    # g24 <- g24+
    #   scale_color_MountSinai('dark')+
    #   geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
    #   geom_hline(yintercept = c(2.5), colour='black')+
    #   geom_vline(xintercept = 0, colour = 'black')+
    #   scale_x_discrete(position = "top") + 
    #   labs(y = NULL, x = NULL,
    #        caption = paste0("*Total count of unique patients who had at least one treatment visit at any MSHS site within the respective month (",isolate(input$dateRange[1]), " to ", isolate(input$dateRange[2]),")" ))+
    #   theme_minimal() +
    #   table_theme()+
    #   theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    
    if(length(unique(unique$SITE)) == 1) {
      unique <- unique %>% mutate(Total = "Total")
      g24 <- ggplot_table_comparison(unique, 0)
    } else {
      n <- length(unique(unique$SITE)) - 1
      hline_y <- seq(1.5, 0.5+n, by= 1)
      
      g24 <- ggplot(unique, aes(x=APPT_MONTH_YEAR, y= SITE, label=total)) +
        #scale_color_MountSinai('dark')+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = 'bold')+
        geom_hline(yintercept = hline_y, colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs( y = NULL, x = NULL, fill = "SITE")+
        theme_minimal() +
        table_theme()+
        theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    }
    
    library(patchwork)
    # g17 + g18 + plot_layout(ncol = 1, heights = c(7, 0.67))
    subplot(g23, g24, nrows = 2, margin = 0.1, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Visits"), scene = list(aspectration=list(x=1,y=1))) %>%
      style(hoverinfo = 'none')
    
  })
  
  
  ## Unique MRN by Site
  output$uniqueTreatmentSite <- renderPlot({
    
    #data <- uniquePts_df_site(dataArrived(), c("Treatment Visit"))
    # data <- uniquePts_df(arrived.data, c("Treatment Visit"))
    data <- historical.data.site.treatment
    
    unique <- data %>% 
      group_by(SITE) %>%
      summarise(total = n())
    
    g19 <- ggplot(unique, aes(x=SITE, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Total Unique Patients by Site - Treatment Visits*"),
           subtitle = paste0("Based on arrived  visits from ", unique_min, " to ",unique_max, "\n" ),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      # geom_text(aes(label=total), color="white", 
      #           size=5, fontface="bold", vjust = 1.2, position = position_dodge(0.9))
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = SITE), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    g20 <- ggplot(unique, aes(x=SITE, y= "Site", label=total)) +
      scale_color_MountSinai('dark')+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE",
            caption = paste0("*Total count of unique patients who had at least one treatment visit within a respecctive MSHS site over the past 3 years (",unique_min, " to ", unique_max,")" ))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    g19 + g20 + plot_layout(ncol = 1, heights = c(7, 0.67))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN  over Time (Months)
  output$uniqueTreatmentTrendSite <- renderPlot({
    
    #data <- uniquePts_df_site(dataArrived(), c("Treatment Visit"))
    # data <- uniquePts_df(arrived.data, c("Treatment Visit"))
    data <- dataUniqueTreatment_site()
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>%
      group_by(Appt.MonthYear, SITE) %>%
      summarise(total = n())
    
    Total <- unique %>% 
      group_by(Appt.MonthYear) %>%
      summarise(total = sum(total))
    Total$SITE <- " Total"
    
    
    sum <- unique %>% group_by(Appt.MonthYear) %>% summarise(sum = sum(total))
    unique <- inner_join(unique,sum, by = c("Appt.MonthYear"))
    
    unique$Appt.MonthYear <- as.factor(unique$Appt.MonthYear)
    
    
    g21 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE))+
      # geom_line(size=1.1)+
      # geom_point(size=3)+
      #scale_color_MountSinai('dark')+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$sum)*1.2))+
      labs(title = paste0("Unique Patients by Site over Time - Treatment Visit*"),
           subtitle = paste0("Based on arrived vistis from ", unique_min, " to ",unique_max, "\n" ),
           y = NULL, x = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      # geom_text(data = subset(unique, total/sum > 0.30),aes(label=total), color="white", 
      #           size=5, fontface="bold", position = position_stack(vjust = 0.5))
    # geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
    #            nudge_x = 0.1, size=5)
      geom_text(data = Total, aes(x = Appt.MonthYear, total, label = total, fill = NULL), color = "black", size = 5, fontface = "bold.italic", vjust = -1)
    
    

    
    unique <- full_join(Total, unique)
    
    pallete <- types_pallete[1:length(unique(unique$SITE))-1]
    
    
    
    g22 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE))+
      scale_color_manual(values = c("#000000",pallete))+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE",
            caption = paste0("*Total count of unique patients who had at least one treatment visit within a respecctive MSHS site over the past 3 years (",unique_min, " to ", unique_max,")" ))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
    
    g21 + g22 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(unique$SITE))))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique MRN by Month
  output$uniqueTreatmentMonthSite <- renderPlot({
    
    #data <- uniquePts_df_siteMonth(dataArrived(), c("Treatment Visit"))
    # data <- uniquePts_df_siteMonth(historical.data[arrived.data.rows,], c("Treatment Visit"))
    
    data <- dataUniqueTreatment_site_month()
    
    if(length(unique(data$SITE)) == 9){
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
    
    unique$Appt.MonthYear <- as.factor(unique$Appt.MonthYear)
    
    Total <- unique %>% 
      group_by(Appt.MonthYear) %>%
      summarise(total = sum(total))
    Total$SITE <- " Total"
    
    
    g23 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,sum(max_tot_site)*1.2))+
      labs(title = paste0("Unique Patients by Site and Month - Treatment Visits*"),
           subtitle = paste0("Based on arrived visits from ",isolate(input$dateRangeunique[1])," to ",isolate(input$dateRangeunique[2]),"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      
      # geom_text(data=subset(unique, total/sum > 0.35),aes(label=total), color="white", 
      #           size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      # stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
      #              size=5, fontface="bold.italic")
      geom_text(data = Total, aes(x = Appt.MonthYear, total, label = total, fill = NULL), color = "black", size = 5, fontface = "bold.italic", vjust = -1)
    
    

    
    unique <- full_join(Total, unique)
    
    pallete <- types_pallete[1:length(unique(unique$SITE))-1]
    
    
    g24 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE)) +
      scale_color_manual(values = c("#000000",pallete))+
      geom_text(size = 7, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE",
            caption = paste0("*Total count of unique patients who had at least one treatment visit within a respective MSHS site and month over the past 3 years (",isolate(input$dateRange[1]), " to ", isolate(input$dateRange[2]),")" ))+
      theme_minimal() +
      table_theme()+
      theme(plot.caption = element_text(hjust = 0, size = 18, face = "italic"))
      
    
    g23 + g24 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(unique$SITE))))
    
  }, height = function(x) input$plotHeight)
  
  
  ## Unique Patients by Provider - Breakdown Tab -----------------------------------------------------------------------------------
  ## Unique Patients by Provider
  
  ## Unique Patients by Provider and Month
  output$uniqueProvMonthExam_tb <- function(){

    # data <- uniquePts_df_siteProvMonth(dataArrived_disease_2(), c("Exam")) 
    data <- uniquePts_df_siteProvMonth(dataArrived(), c("Exam")) 
    
    # data <- historical.data[arrived.data.rows,] %>% filter(SITE == "DBC", Provider %in% default_provider, Disease_Group %in% default_disease_group)
    # data <- uniquePts_df_siteProvMonth(data, c("Exam"))

    uniquePts_tb <- data %>%
      group_by(SITE, PROVIDER, APPT_MONTH_YEAR) %>%
      summarise(total = n()) %>% collect() %>%
      rename(Appt.MonthYear = APPT_MONTH_YEAR,
             Provider = PROVIDER) %>%
      arrange(Appt.MonthYear, SITE) %>%
      `colnames<-` (c("Site", "Provider", "Appt.MonthYear", "Total")) %>%
      pivot_wider(names_from = Appt.MonthYear, values_from = Total, values_fill = 0) #%>%
    #adorn_totals(where = "col", fill = "-", na.rm = TRUE, name = "YTD Total")
    
    data_over_time <- uniquePts_df_siteProv(dataArrived(), c("Exam")) 
    
    # data_over_time <- uniquePts_df_siteProv(dataArrived_disease_2(), c("Exam")) 
    # data_over_time <- historical.data[arrived.data.rows,] %>% filter(SITE == "DBC", Provider %in% default_provider, Disease_Group %in% default_disease_group)
    # data_over_time <- uniquePts_df_siteProv(data_over_time, c("Exam"))
    
    uniquePts_tb_over_time <- data_over_time %>%
      group_by(SITE, PROVIDER, APPT_MONTH_YEAR) %>%
      summarise(total = n()) %>% collect() %>%
      rename(Appt.MonthYear = APPT_MONTH_YEAR,
             Provider = PROVIDER) %>%
      arrange(Appt.MonthYear, SITE) %>%
      `colnames<-` (c("Site", "Provider", "Appt.MonthYear", "Total")) %>%
      pivot_wider(names_from = Appt.MonthYear, values_from = Total, values_fill = 0) %>%
      adorn_totals(where = "col", fill = "-", na.rm = TRUE, name = "Total Unique Patients Over Time")
    
    uniquePts_tb_over_time <- uniquePts_tb_over_time[,c(1:2,ncol(uniquePts_tb_over_time))]
    
    uniquePts_tb <- merge(uniquePts_tb, uniquePts_tb_over_time, by = c("Site", "Provider"))
    
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
          uniquePts_df_system_zip(data, c("Exam","Labs","Treatment")) %>% 
          group_by(latitude, longitude) %>% 
          dplyr::summarise(total = n())
      } else if(input$selectedZipCodeMap2 == "Exam") {
        
        newdata <-
          uniquePts_df_system_zip(data, c("Exam")) %>% 
          group_by(latitude, longitude) %>% 
          dplyr::summarise(total = n())
      } else{
        
        newdata <-
          uniquePts_df_system_zip(data, c("Treatment Visit")) %>% 
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
          uniquePts_df_site_zip(data, c("Exam","Labs","Treatment")) %>%
          group_by(SITE, latitude, longitude) %>%
          dplyr::summarise(total = n()) %>%
          filter(!is.na(SITE))
      } else if(input$selectedZipCodeMap2 == "Exam") {
        
        newdata <-
          uniquePts_df_site_zip(data, c("Exam")) %>%
          group_by(SITE, latitude, longitude) %>%
          dplyr::summarise(total = n()) %>%
          filter(!is.na(SITE))
      } else{
        
        newdata <-
          uniquePts_df_site_zip(data, c("Treatment Visit")) %>%
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
    # data <- population.data_filtered
    
    if(input$selectedZipCodeMap == "System"){
      if(input$selectedZipCodeMap2 == "All"){
        
        newdata <- uniquePts_df_system_zip(data, c("Exam","Labs","Treatment")) 
        
      } else if(input$selectedZipCodeMap2 == "Exam") {
        
        newdata <- uniquePts_df_system_zip(data, c("Exam"))  
        
      } else{
        
        newdata <- uniquePts_df_system_zip(data, c("Treatment Visit")) 
        
      }
      
    } else{
      if(input$selectedZipCodeMap2 == "All"){
        
        newdata <- uniquePts_df_site_zip(data, c("Exam","Labs","Treatment")) %>% filter(!is.na(SITE))
        
      } else if(input$selectedZipCodeMap2 == "Exam") {
        
        newdata <- uniquePts_df_site_zip(data, c("Exam")) %>% filter(!is.na(SITE))
        
      } else{
        
        newdata <- uniquePts_df_site_zip(data, c("Treatment Visit")) %>% filter(!is.na(SITE))
        
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
    names(header_above) <- paste0(c("Based on data from "),c(isolate(input$dateRange[1])),c(" to "),c(isolate(input$dateRange[2])))
    
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
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
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
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
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
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
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
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
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
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
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
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
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
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
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
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
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
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
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
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
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
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
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
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
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
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
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
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
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
  
  ### Utilization Tab -----------------------------------------------------------------------------------------------------------------
  # Average Rooms Required --------------------------------------------------------------------------------------------
  output$roomStat1 <- renderValueBox({
    valueBox(NULL,
             # paste0(input$setRooms," rooms available\n throughout",input$setHours," hours"),
             subtitle = tags$p(paste0("Analysis based on ",input$setRooms," rooms available\n throughout ",input$setHours," hours"), style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "yellow"
    )
  })
  
  output$maxRoomsRequired <- renderValueBox({
    
    valueBox(NULL,
             subtitle = tags$p(paste0("Max # of rooms required during the day: ",
                                      max((dataUtilization() %>%
                                             filter(comparison == 0) %>%
                                             select(Appt.DateYear, timeOptionsHr_filter) %>%
                                             gather(Time, sum, 2:15) %>%
                                             group_by(Time) %>%
                                             summarise(avg = ceiling((sum(sum)/length(unique(Appt.DateYear)))/60)))$avg)), 
                               style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "aqua"
    )
  })
  
  output$avgUtilization <- renderValueBox({
    
    valueBox(NULL,
             subtitle = tags$p(paste0("Avg utilization per day: ",
                                      paste0(round((sum((dataUtilization() %>% filter(comparison == 0))$sum))/
                                                     (length(unique(dataUtilization()$Appt.DateYear))*(60*input$setHours*input$setRooms))*100),"%")), 
                               style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "fuchsia"
    )
  })
  
  output$maxUtilization <- renderValueBox({
    
    valueBox(NULL,
             subtitle = tags$p(paste0("Peak utilization during the day: ",
                                      max((dataUtilization() %>%
                                             filter(comparison == 0) %>%
                                             select(Appt.DateYear, timeOptionsHr_filter) %>%
                                             gather(Time, sum, 2:15) %>%
                                             group_by(Time) %>%
                                             summarise(avg = round((sum(sum)/ 
                                                                      (length(unique(dataUtilization()$Appt.DateYear))*(60*input$setRooms)))*100)))$avg),"%"), 
                               style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "fuchsia"
    )
  })
  
  
  output$avgUtilization_provider <- renderValueBox({
    
    valueBox(NULL,
             subtitle = tags$p(paste0("Avg utilization per day: ",
                                      paste0(round((sum((dataUtilization_provider() %>% filter(comparison == 0))$sum))/
                                                     (length(unique(dataUtilization_provider()$Appt.DateYear))*(60*input$setHours_provider))*100),"%")), 
                               style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "fuchsia"
    )
  })
  
  output$maxUtilization_provider <- renderValueBox({
    
    valueBox(NULL,
             subtitle = tags$p(paste0("Peak utilization during the day: ",
                                      max((dataUtilization_provider() %>%
                                             filter(comparison == 0) %>%
                                             select(Appt.DateYear, timeOptionsHr_filter) %>%
                                             gather(Time, sum, 2:15) %>%
                                             group_by(Time) %>%
                                             summarise(avg = round((sum(sum)/ 
                                                                      (length(unique(dataUtilization_provider()$Appt.DateYear))*(60)))*100)))$avg),"%"), 
                               style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "fuchsia"
    )
  })
  
  # # Scheduled and Avg Utilization --------------------------------------------------------------------------------------------------------
  # output$avgScheduledUtilization <- renderValueBox({
  #   
  #   data <- dataUtilization() 
  #   # data <- utilization.data[scheduled.utilization.data.rows,]
  #   
  #   paste0(round((sum(data$sum))/(length(unique(data$Appt.DateYear))*(60*input$setHours*input$setRooms))*100),"%") %>%
  #     valueBox(
  #       subtitle = tags$p("Average Daily Booked Utilization", style = "font-size: 160%;"), icon = NULL, color = "aqua")
  # })
  # 
  # output$avgUtilization <- renderValueBox({
  #   
  #   data <- dataUtilization() %>% filter(Appt.Status == "Arrived")
  #   # data <- utilization.data %>% filter(util.type == "actual") %>% filter(Appt.Status == "Arrived")
  #   
  #   paste0(round((sum(data$sum))/(length(unique(data$Appt.DateYear))*(60*input$setHours*input$setRooms))*100),"%") %>%
  #     valueBox(
  #       subtitle = tags$p("Average Daily Filled Utilization", style = "font-size: 160%;"), icon = NULL, color = "aqua")
  # })
  # Average Utilization by Time of Day
  output$spaceUtil <- renderPlotly({
    data <- dataUtilization() %>% filter(comparison == 0)
    # data <- utilization.data %>% filter(Appt.Status == "Arrived")
    
    # Days of Week Table
    daysOfWeek.Table <- 
      data %>%
      group_by(Appt.Day, Appt.DateYear) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.Day) %>%
      dplyr::summarise(count = n())
    
    c.start <- which(colnames(data)=="07:00")
    c.end <- which(colnames(data)=="20:00")
    
    space.hour.day <- aggregate(data[c(c.start:c.end)], list(data$Appt.Day),FUN = sum)
    space.hour.day <- reshape2::melt(space.hour.day, id=c("Group.1"))
    space.hour.day$days <- daysOfWeek.Table$count[match(daysOfWeek.Table$Appt.Day,space.hour.day$Group.1)]
    
    space.hour.day$utilization <- round(space.hour.day$value/(space.hour.day$days*60*input$setRooms), 1)
    #space.hour.day$utilization <- round(space.hour.day$value/(space.hour.day$days*60*8), 1)
    
    names(space.hour.day) <- c("Day","Time","Total_Dur","Days","Average_Util")
    #space.hour.day$Average_Util <- space.hour.day$Average_Util*100
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(space.hour.day$Day)),]
    
    space.hour.day <- as.data.frame(merge(byDayTime.df,space.hour.day, by.x = c("Day","Time"), by.y = c("Day","Time"), all = TRUE))
    space.hour.day[is.na(space.hour.day)] <- 0
    
    space.hour.day <- space.hour.day %>% filter(Time %in% timeOptionsHr_filter)
    #space.hour.day$target <- 80
    space.hour.day$target <- 0.8
    
    space.hour.day <- space.hour.day %>% rename(`Average Utilization` = Average_Util)
    graph <- ggplot(space.hour.day, aes(x=Time, col=factor(Day,level = daysOfWeek.options), y=`Average Utilization`, group=Day))+
      geom_line(size=1.2)+
      labs(x=NULL, y="Utilization (%)", 
           title = "Average Space Utilization (%) by Time of Day and Day of Week",
           subtitle = paste0("Based on scheduled appointment time and duration from ",isolate(input$dateRangeUtil[1])," to ",isolate(input$dateRangeUtil[2]))
           )+
      scale_color_MountSinai("main")+
      #geom_hline(yintercept = .8, color = "red", linetype="dashed")+
      geom_hline(aes(yintercept = .8), color = "red", linetype="dashed")+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,max(space.hour.day$`Average Utilization`)*1.2))+
      theme(legend.position = 'top',
            legend.title=element_blank(),
            plot.title = element_text(hjust=0.5, face = "bold", size = 16),
            axis.title = element_text(size="12"),
            axis.text = element_text(size="12"),
            axis.title.x = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            axis.title.y = element_text(size = 12, angle = 90)
            
      )
    
    space.hour.day$Average_Util <- space.hour.day$`Average Utilization`*100
    
    table <- ggplot(space.hour.day, aes(x=factor(Day, levels = rev(daysOfWeek.options)), y=Time))+
      labs(x=NULL, y=NULL)+
      #geom_tile(aes(fill=Average_Util), colour = "black", size=0.5)+
      coord_flip()+
      scale_fill_gradient2(midpoint = median(unique(space.hour.day$`Average Utilization`)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Space Utilization")+
      #scale_fill_gradient2(midpoint = median(unique(space.hour.day$Average_Util_tble)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Space Utilization %", labels = scales::percent)+
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
      geom_text(aes(label= ifelse(is.na(`Average Utilization`),"",paste0(round(`Average Utilization`,2),"%"))), color="black", size=4, fontface="bold")
    #geom_text(aes(label= ifelse(is.na(Average_Util),"",paste0(round(Average_Util*100,2)*100,"%"))), color="black", size=5, fontface="bold")
    
    
    p1 <- ggplotly(graph, tooltip = c("Average Utilization")
             ) %>% layout(yaxis = list(mirror = T), xaxis = list(mirror = T))
    
    #p1 <- ggplotly(graph)
    p2 <- ggplotly(table, tooltip = NULL)

    subplot(p1, p2, nrows = 2, margin = 0.03, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Utilization (%)"), margin  = list(l = 80, r = 80, t = 40, b = 0, pad = 0))
    
    #grid.arrange(graph, table, ncol = 1, heights = c(5,3))
    
  })
  
  # Utilization by Percentile
  output$spaceUtilPerc <- renderPlotly({
    data <- dataUtilization() %>% filter(comparison == 0)
    #data <- utilization.data %>% filter(comparison == 0)
    
    c.start <- which(colnames(data)=="07:00")
    c.end <- which(colnames(data)=="20:00")
    
    space.hour <- aggregate(data[c(c.start:c.end)], list(data$Appt.DateYear),FUN = sum)
    space.hour <- reshape2::melt(space.hour, id=c("Group.1"))
    
    space.hour <- space.hour %>%
      group_by(variable) %>%
      dplyr::summarise( 
        Median = quantile(value, probs=0.5)/(60*input$setRooms),
        `70th Percentile`= quantile(value, probs=0.75)/(60*input$setRooms),
        `90th Percentile`= quantile(value, probs=0.90)/(60*input$setRooms))
    
    # space.hour <- space.hour %>%
    #   group_by(variable) %>%
    #   dplyr::summarise( 
    #     Median = quantile(value, probs=0.5)/(60*8),
    #     `70th Percentile`= quantile(value, probs=0.75)/(60*8),
    #     `90th Percentile`= quantile(value, probs=0.90)/(60*8))
    
    
    colnames(space.hour)[1] <- "Time"
    space.hour <- as.data.frame(reshape2::melt(space.hour, id=c("Time")))
    
    space.hour <- space.hour %>% filter(Time %in% timeOptionsHr_filter)
    
    space.hour <- space.hour %>% rename(Utilization = value)
    graph <- ggplot(space.hour, aes(x=Time, y=Utilization, col=variable, group=variable))+
      geom_line(size=1.2)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,max(space.hour$Utilization)*1.2))+
      labs(x=NULL, y="Number of Rooms\n", 
           title = "Space Utilization (%) by Percentile by Time of Day",
           #subtitle = paste0("Based on scheduled appointment time and duration from ",isolate(input$dateRangeUtil[1])," to ",isolate(input$dateRangeUtil[2]))
      )+
      scale_color_MountSinai("main")+
      #theme_new_line()+
      #theme_bw()+
      #graph_theme("top") + theme(legend.title = element_blank(), legend.direction = "horizontal", legend.key.size = unit(1.0,"cm"))
      theme(legend.position = 'top',
            legend.title=element_blank(),
            plot.title = element_text(hjust=0.5, face = "bold", size = 16),
            axis.title = element_text(size="12"),
            axis.text = element_text(size="12"),
            axis.title.x = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            axis.title.y = element_text(size = 12, angle = 90)
            
      )
    guides(colour = guide_legend(nrow = 1))
    
    space.hour$value <- space.hour$Utilization*100
    
    table <- ggplot(space.hour, aes(x=variable, y=Time))+
      labs(x=NULL, y=NULL)+
      #geom_tile(aes(fill=value), colour = "black", size=0.5)+
      coord_flip()+
      scale_fill_gradient2(midpoint = median(unique(space.hour$value)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Space Utilization ")+
      #scale_y_discrete(limits = unique(sort(space.hour$Time)), position = "bottom")+
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
      geom_text(aes(label= ifelse(is.na(Utilization),"",paste0(round(Utilization,1),"%"))), color="black", size=4, fontface="bold")
    
    
    #grid.arrange(graph, table, ncol = 1, heights = c(5,2))
    
    p1 <- ggplotly(graph, tooltip = c("Utilization")
    ) %>% layout(yaxis = list(mirror = T), xaxis = list(mirror = T))
    
    #p1 <- ggplotly(graph)
    p2 <- ggplotly(table, tooltip = NULL)
    
    subplot(p1, p2, nrows = 2, margin = 0.03, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Utilization (%)"), margin  = list(l = 80, r = 80, t = 40, b = 0, pad = 0))
    
    
  })
  
  # Average Number of Rooms Required -----------------------------------------------
  output$spaceUsed <- renderPlotly({
    data <- dataUtilization() %>% filter(comparison == 0)
    
    # data <- as.data.frame(utilization.data[arrived.utilization.data.rows,])
    
    # Days of Week Table
    daysOfWeek.Table <- 
      data %>%
      group_by(Appt.Day, Appt.DateYear) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.Day) %>%
      dplyr::summarise(count = n())
    
    c.start <- which(colnames(data)=="07:00")
    c.end <- which(colnames(data)=="20:00")
    
    space.hour.day <- aggregate(data[c(c.start:c.end)], list(data$Appt.Day),FUN = sum)
    space.hour.day <- reshape2::melt(space.hour.day, id=c("Group.1"))
    space.hour.day$days <- daysOfWeek.Table$count[match(daysOfWeek.Table$Appt.Day,space.hour.day$Group.1)]
    
    space.hour.day$average <- round(space.hour.day$value/(space.hour.day$days*60), 1)
    names(space.hour.day) <- c("Day","Time","Total_Dur","Days","Average_Req")
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(space.hour.day$Day)),]
    
    space.hour.day <- as.data.frame(merge(byDayTime.df,space.hour.day, by.x = c("Day","Time"), by.y = c("Day","Time"), all = TRUE))
    space.hour.day[is.na(space.hour.day)] <- 0
    
    space.hour.day <- space.hour.day %>% filter(Time %in% timeOptionsHr_filter)
    
    space.hour.day <- space.hour.day %>% rename(`Average Space Requiered` = Average_Req)
    
    graph <- ggplot(space.hour.day, aes(x=Time, y=`Average Space Requiered`, col=factor(Day,level = daysOfWeek.options), group=Day))+
      geom_line(size=1.2)+
      labs(x=NULL, y="Number of Rooms\n",
           title = "Average Space Required by Time of Day and Day of Week",
           subtitle = paste0("Based on scheduled appointment time and duration from ",isolate(input$dateRangeUtil[1])," to ",isolate(input$dateRangeUtil[2])))+
      scale_color_MountSinai("main")+
      #theme_new_line()+
      #theme_bw()+
      #graph_theme("top") + 
      theme(legend.position = 'top',
            legend.title=element_blank(),
            plot.title = element_text(hjust=0.5, face = "bold", size = 16),
            axis.title = element_text(size="12"),
            axis.text = element_text(size="12"),
            axis.title.x = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            axis.title.y = element_text(size = 12, angle = 90)
            
      )+
      guides(colour = guide_legend(nrow = 1))
    
    table <- ggplot(space.hour.day, aes(x=factor(Day, levels = rev(daysOfWeek.options)), y=Time))+
      labs(x=NULL, y=NULL)+
      #geom_tile(aes(fill=Average_Req), colour = "black", size=0.5)+
      coord_flip()+
      scale_fill_gradient2(midpoint = median(unique(space.hour.day$`Average Space Requiered`)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Space Required ")+
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
      geom_text(aes(label= ifelse(is.na(`Average Space Requiered`),"", round(`Average Space Requiered`))), color="black", size=4, fontface="bold")
    
    #grid.arrange(graph, table, ncol = 1, heights = c(5,3))
    
    p1 <- ggplotly(graph, tooltip = c("Average Space Requiered")
    ) %>% layout(yaxis = list(mirror = T), xaxis = list(mirror = T))
    
    #p1 <- ggplotly(graph)
    p2 <- ggplotly(table, tooltip = NULL)
    
    subplot(p1, p2, nrows = 2, margin = 0.04, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Number of Rooms"), margin  = list(l = 80, r = 80, t = 40, b = 0, pad = 0))
    
  })
  # Rooms Required by Percentile 
  output$spaceUsedPerc <- renderPlotly({
    data <- dataUtilization() %>% filter(comparison == 0)
    
    c.start <- which(colnames(data)=="07:00")
    c.end <- which(colnames(data)=="20:00")
    
    space.hour <- aggregate(data[c(c.start:c.end)], list(data$Appt.DateYear),FUN = sum)
    space.hour <- reshape2::melt(space.hour, id=c("Group.1"))
    
    space.hour <- space.hour %>%
      group_by(variable) %>%
      dplyr::summarise( 
        Median = round(quantile(value, probs=0.5)/60,1),
        `70th Percentile`= round(quantile(value, probs=0.75)/60,1),
        `90th Percentile`= round(quantile(value, probs=0.90)/60,1))
    
    colnames(space.hour)[1] <- "Time"
    
    space.hour <- as.data.frame(reshape2::melt(space.hour, id=c("Time")))
    
    space.hour <- space.hour %>% filter(Time %in% timeOptionsHr_filter)
    
    space.hour <- space.hour %>% rename(`Space Required` = value)
    graph <- ggplot(space.hour, aes(x=Time, y=`Space Required`, col=variable, group=variable))+
      geom_line(size=1.2)+
      scale_y_continuous(limits=c(0, max(space.hour$`Space Required`)*1.2))+
      labs(x=NULL, y="Number of Rooms\n",
           title = "Space Required by Percentile by Time of Day",
           subtitle = paste0("Based on scheduled appointment time and duration from ",isolate(input$dateRangeUtil[1])," to ",isolate(input$dateRangeUtil[2])))+
      scale_color_MountSinai("main")+
      #theme_new_line()+
      #theme_bw()+
      #graph_theme("top") + theme(legend.title = element_blank(), legend.direction = "horizontal", legend.key.size = unit(1.0,"cm"))
      theme(legend.position = 'top',
            legend.title=element_blank(),
            plot.title = element_text(hjust=0.5, face = "bold", size = 16),
            axis.title = element_text(size="12"),
            axis.text = element_text(size="12"),
            axis.title.x = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            axis.title.y = element_text(size = 12, angle = 90)
            
      )+
    guides(colour = guide_legend(nrow = 1))
    
    table <- ggplot(space.hour, aes(x=variable, y=Time))+
      labs(x=NULL, y=NULL)+
      #geom_tile(aes(fill=value), colour = "black", size=0.5)+
      coord_flip()+
      scale_fill_gradient2(midpoint = median(unique(space.hour$`Space Required`)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Space Required ")+
      #scale_y_discrete(limits = unique(sort(space.hour$Time)), position = "bottom")+
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
      geom_text(aes(label= ifelse(is.na(`Space Required`),"", round(`Space Required`,1))), color="black", size=4, fontface="bold")
    
    
    p1 <- ggplotly(graph, tooltip = c("Space Required")
    ) %>% layout(yaxis = list(mirror = T), xaxis = list(mirror = T))
    
    #p1 <- ggplotly(graph)
    p2 <- ggplotly(table, tooltip = NULL)
    
    subplot(p1, p2, nrows = 2, margin = 0.03, heights = c(0.6, 0.4)) %>% layout(showlegend = T, yaxis = list(title = "Number of Rooms"), margin  = list(l = 80, r = 80, t = 40, b = 0, pad = 0))
    
    
    
    
    #grid.arrange(graph, table, ncol = 1, heights = c(5,2))
    
  })
  
 
  treatment_space_util_month_data <- reactive({
    num_rooms <- input$setRooms_treatment
    num_hours <- input$setHours_treatment
    data <- dataUtilization_Treatment()
    #data_test <<- dataUtilization_Treatment()
    
    # num_rooms <- 3
    # num_hours <- 9

    data <-  data %>%
              #historical.data %>%
              #filter(is.na(holiday)) %>%
              select(APPT_MONTH_YEAR, APPT_DUR, APPT_DATE_YEAR) %>% collect() %>%
              group_by(APPT_MONTH_YEAR) %>%
              summarise(`Total Duration (hr)` = round(sum(APPT_DUR)/60,0),
                        `Time Available (hr)` = round(length(unique(APPT_DATE_YEAR))*num_rooms*num_hours,0),
                        `Utilization %` = round(`Total Duration (hr)`/`Time Available (hr)`*100,0)) %>%
              #arrange(match(Appt.Month, month.abb)) %>%
              rename(Month = APPT_MONTH_YEAR) #%>%
              #select(Appt.MonthYear,`Total Duration (hr)`,`Time Available (hr)`, `Utilization %`)
    
    data_total <- data.frame(t(colSums(data[,c(2,3)])))
    col_names <- colnames(data[,c(2,3)])
    colnames(data_total) <- col_names
    
    data_total$`Utilization %` <- round((data_total[1,1]/data_total[1,2])*100,0)
    
    data$Month <- paste0(data$Month, "-01")
    data$Month <- format(as.Date(data$Month), "%Y-%b")
    data_total$Month <- "Total"
    
    data <- bind_rows(data,data_total)
    
    data
  })
  

  output$treatment_space_util_month <- function(){
    kable(treatment_space_util_month_data(),booktabs = T, escape = F) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      row_spec(nrow(treatment_space_util_month_data()), background = "#d80b8c", color = "white", bold = T) 
  }
  
  
  
  treatment_space_util_dayofweek_data <- reactive({
    data <- dataUtilization_Treatment() %>%
      #historical.data %>%
      # filter(is.na(holiday)) %>%
      select(APPT_DAY, APPT_DUR, APPT_DATE_YEAR) %>% collect() %>%
      group_by(APPT_DAY) %>%
      summarise(`Total Duration (hr)` = round(sum(APPT_DUR)/60,0),
                `Time Available (hr)` = round(length(unique(APPT_DATE_YEAR))*input$setRooms_treatment*input$setHours_treatment,0),
                `Utilization %` = round(`Total Duration (hr)`/`Time Available (hr)`*100,0)) %>%
      rename(DayofWeek = APPT_DAY) 
    
    data$DayofWeek <- factor(data$DayofWeek, levels= toupper(c("Sun", "Mon", 
                                                     "Tue", "Wed", "Thu", "Fri", "Sat")))
    
    data <-  data[order(data$DayofWeek), ]
    
    data_total <- data.frame(t(colSums(data[,c(2,3)])))
    col_names <- colnames(data[,c(2,3)])
    colnames(data_total) <- col_names
    
    data_total$`Utilization %` <- round((data_total[1,1]/data_total[1,2])*100,0)
    data_total$DayofWeek <- "Total"
    
    data <- bind_rows(data,data_total)
    
      
    
    data
  })
  
  
  output$treatment_space_util_dayofweek <- function(){
    kable(treatment_space_util_dayofweek_data(),booktabs = T, escape = F) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      row_spec(nrow(treatment_space_util_dayofweek_data()), background = "#d80b8c", color = "white", bold = T) 
    
  }
  
  treatment_input_table_data <- reactive({
    remainder <- as.data.frame(round(input$setHours_treatment,0))
    operating_hours_start <- input$operating_hours_start
    operating_hours_end <- input$operating_hours_end
    set_rooms <- input$setRooms_treatment
    
    # operating_hours_start <- "7:00AM"
    # operating_hours_end <- "6:00PM"
    # set_rooms <- "16"
    
    if(grepl(":30", operating_hours_start, fixed = TRUE)){
      start_time <- parse_date_time(operating_hours_start, "%H:%M%p")
      if(grepl(":30", operating_hours_end, fixed = TRUE)) {
        end_time <- floor_date(parse_date_time(operating_hours_end, "%H:%M%p"), unit = 'hours')
      }else {
        end_time <- parse_date_time(operating_hours_end, "%H:%M%p") - 3600
      }
      start_time_hour <- start_time + 1800
      
      time_df <- data.frame(Time = as.POSIXct(c(start_time_hour, end_time)))
      time_df <- seq(min(time_df$Time), max(time_df$Time), by = "1 hour")
      time_df <- as.data.frame(time_df)
      time_df <- rename(time_df, Time = time_df)
      time_df$Time <- format(time_df$Time, format = "%H:%M")
      
      start_time <- format(start_time, format = "%H:%M")
      
      time_df <- rbind(start_time, time_df)
    }else{
      start_time <- parse_date_time(operating_hours_start, "%H:%M%p")
      if(grepl(":30", operating_hours_end, fixed = TRUE)) {
        end_time <- floor_date(parse_date_time(operating_hours_end, "%H:%M%p"), unit = 'hours')
      }else {
        end_time <- parse_date_time(operating_hours_end, "%H:%M%p") - 3600
      }      
      start_time_hour <- start_time
      
      time_df <- data.frame(Time = as.POSIXct(c(start_time_hour, end_time)))
      time_df <- seq(min(time_df$Time), max(time_df$Time), by = "1 hour")
      time_df <- as.data.frame(time_df)
      time_df <- rename(time_df, Time = time_df)
      time_df$Time <- format(time_df$Time, format = "%H:%M")
      
      start_time <- format(start_time, format = "%H:%M")
      
      time_df <- rbind(start_time, time_df)
      
      time_df <- time_df %>% distinct()
    }
    
    time_df <- time_df %>%
                add_column(`# of Nurses` = as.character(NA))
    #time_df$`Space Capacity` <- set_rooms
    
    time_df
  })
  
  
  output$treatment_input_table <- renderRHandsontable({
    data <- treatment_input_table_data()
    
    rhandsontable(data, overflow= 'hidden', rowHeaders = FALSE, readOnly = FALSE) %>% hot_table() %>%
      hot_col(1, readOnly = T) %>%
      hot_col(2, readOnly = F)
    
  })
  
  

  treatment_nurse_util_month_data <- reactive({

    nurse_total <- hot_to_r(input$treatment_input_table) 
    
    nurse_total <- nurse_total %>%
                       summarise(sum(as.numeric(`# of Nurses`)))
    #nurse_test <<- nurse_total
    data <- dataUtilization_Treatment()
    #data_test <<-  dataUtilization_Treatment()
    
    data <-  data %>%
      #historical.data %>%
      # filter(is.na(holiday)) %>%
      select(APPT_MONTH_YEAR, APPT_DUR, APPT_DATE_YEAR) %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>%
      summarise(`Total Duration (hr)` = round(sum(APPT_DUR)/60,0),
                `Time Available (hr)` = round(length(unique(APPT_DATE_YEAR))*3*as.numeric(nurse_total),0),
                `Utilization %` = round(`Total Duration (hr)`/`Time Available (hr)`*100,0)) %>%
      #arrange(match(Appt.Month, month.abb)) %>%
      rename(Month = APPT_MONTH_YEAR)
    
    data_total <- data.frame(t(colSums(data[,c(2,3)])))
    col_names <- colnames(data[,c(2,3)])
    colnames(data_total) <- col_names
    
    data_total$`Utilization %` <- round((data_total[1,1]/data_total[1,2])*100,0)
    
    data$Month <- paste0(data$Month, "-01")
    data$Month <- format(as.Date(data$Month), "%Y-%b")
    
    data_total$Month <- "Total"
    
    data <- bind_rows(data,data_total)

    data
  })


  output$treatment_nurse_util_month <- function(){
    kable(treatment_nurse_util_month_data(),booktabs = T, escape = F) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      row_spec(nrow(treatment_nurse_util_month_data()), background = "#d80b8c", color = "white", bold = T) 
  }



  treatment_nurse_util_dayofweek_data <- reactive({
    
    nurse_total <- hot_to_r(input$treatment_input_table) 
    
    nurse_total <- nurse_total %>%
      summarise(sum(as.numeric(`# of Nurses`)))
    
    data <- dataUtilization_Treatment() %>%
      #historical.data %>%
      # filter(is.na(holiday)) %>%
      select(APPT_DAY, APPT_DUR, APPT_DATE_YEAR) %>% collect() %>%
      group_by(APPT_DAY) %>%
      summarise(`Total Duration (hr)` = round(sum(APPT_DUR)/60,0),
                `Time Available (hr)` = length(unique(APPT_DATE_YEAR))*3*as.numeric(nurse_total),
                `Utilization %` = round(`Total Duration (hr)`/`Time Available (hr)`*100,0)) %>%
      rename(DayofWeek = APPT_DAY)
      
    data$DayofWeek <- factor(data$DayofWeek, levels= toupper(c("Sun", "Mon", 
                                             "Tue", "Wed", "Thu", "Fri", "Sat")))
    
   data <-  data[order(data$DayofWeek), ]
   
   data_total <- data.frame(t(colSums(data[,c(2,3)])))
   col_names <- colnames(data[,c(2,3)])
   colnames(data_total) <- col_names
   
   data_total$`Utilization %` <- round((data_total[1,1]/data_total[1,2])*100,0)
   data_total$DayofWeek <- "Total"
   
   data <- bind_rows(data,data_total)


    data
  })


  output$treatment_nurse_util_dayofweek <- function(){
    options(knitr.kable.NA = '-')
    kable(treatment_nurse_util_dayofweek_data(),booktabs = T, escape = F) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      row_spec(nrow(treatment_nurse_util_dayofweek_data()), background = "#d80b8c", color = "white", bold = T)
    
  }

  
  infusion_util_month_data <- reactive({
    rooms_set <- input$setRooms_treatment
    #rooms_set <- "16"
    
    effective_capacity <<- hot_to_r(input$treatment_input_table) 
    
    effective_capacity <- effective_capacity %>% 
                                              mutate(`Nursing Capacity` = as.numeric(`# of Nurses`) * 3)
    
    room_set_df <- data.frame(Time = effective_capacity$Time)
    room_set_df$rooms <- as.character(rooms_set)
    
    effective_capacity <- merge(effective_capacity,room_set_df)
    effective_capacity$rooms <- as.numeric(effective_capacity$rooms)
    effective_capacity$`Nursing Capacity` <- as.numeric(effective_capacity$`Nursing Capacity`)
    
    effective_capacity <- data.frame(Effective.Capacity = pmin(effective_capacity$`Nursing Capacity`,effective_capacity$rooms))
    

    effective_capacity <- effective_capacity %>% summarise(`Total Effective Capacity` = sum(as.numeric(Effective.Capacity)))
    
    data <- dataUtilization_Treatment() %>%
      #historical.data %>%
      # filter(is.na(holiday)) %>%
      select(APPT_MONTH_YEAR, APPT_DUR, APPT_DATE_YEAR) %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>%
      summarise(`Total Duration (hr)` = round(sum(APPT_DUR)/60,0),
                `Time Available (hr)` = length(unique(APPT_DATE_YEAR))*as.numeric(effective_capacity),
                `Utilization %` = round(`Total Duration (hr)`/`Time Available (hr)`*100,0)) %>%
      #arrange(match(Appt.Month, month.abb)) %>%
      rename(Month = APPT_MONTH_YEAR)
    
    data_total <- data.frame(t(colSums(data[,c(2,3)])))
    col_names <- colnames(data[,c(2,3)])
    colnames(data_total) <- col_names
    
    data_total$`Utilization %` <- round((data_total[1,1]/data_total[1,2])*100,0)
    
    data$Month <- paste0(data$Month, "-01")
    data$Month <- format(as.Date(data$Month), "%Y-%b")
    
    data_total$Month <- "Total"
    
    data <- bind_rows(data,data_total)

    data
  })
  
  
  output$infusion_util_month <- function(){
    kable(infusion_util_month_data(),booktabs = T, escape = F) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      row_spec(nrow(infusion_util_month_data()), background = "#d80b8c", color = "white", bold = T) 
    
  }
  
  
  infusion_util_dayofweek_data <- reactive({
    effective_capacity <- hot_to_r(input$treatment_input_table) 
    
    effective_capacity <- effective_capacity %>% 
      mutate(`Nursing Capacity` = as.numeric(`# of Nurses`) * 3)
    
    effective_capacity <- transform(effective_capacity, `Effective Capacity` = pmin(`Nursing Capacity`, input$setRooms_treatment))
    
    effective_capacity <- effective_capacity %>% summarise(`Total Effective Capacity` = sum(as.numeric(Effective.Capacity)))
    
    data <- dataUtilization_Treatment() %>%
      #historical.data %>%
      #filter(is.na(holiday)) %>%
      select(APPT_DAY, APPT_DUR, APPT_DATE_YEAR) %>% collect() %>%
      group_by(APPT_DAY) %>%
      summarise(`Total Duration (hr)` = round(sum(APPT_DUR)/60,0),
                `Time Available (hr)` = length(unique(APPT_DATE_YEAR))*as.numeric(effective_capacity),
                `Utilization %` = round(`Total Duration (hr)`/`Time Available (hr)`*100,0)) %>%
      rename(DayofWeek = APPT_DAY)
    
    data$DayofWeek <- factor(data$DayofWeek, levels= toupper(c("Sun", "Mon", 
                                                     "Tue", "Wed", "Thu", "Fri", "Sat")))
    
    data <-  data[order(data$DayofWeek), ]
    
    data_total <- data.frame(t(colSums(data[,c(2,3)])))
    col_names <- colnames(data[,c(2,3)])
    colnames(data_total) <- col_names
    
    data_total$`Utilization %` <- round((data_total[1,1]/data_total[1,2])*100,0)
    data_total$DayofWeek <- "Total"
    
    data <- bind_rows(data,data_total)
    
    
    data
  })
  
  
  output$infusion_util_dayofweek <- function(){
    kable(infusion_util_dayofweek_data(),booktabs = T, escape = F) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      row_spec(nrow(infusion_util_dayofweek_data()), background = "#d80b8c", color = "white", bold = T) 
    
    
  }
  
  #download_btn_server("volumetrend_download")
  output$volumetrend_download <- renderUI({
    #ns <- session$ns
    
    if(is.null(session$user) || !(session$user %in% c("nevink01"))) return()
    downloadButton("volumetrend_download1")
  })
  
  # observeEvent(input$volumetrend_download, {
  #   downloadHandler(
  #     filename = "filename.csv",
  #     content = write.csv(dataArrivedTrend(), "filename.csv")
  #   )
  # })
  
  output$volumetrend_download1 <- downloadHandler(
    filename = function() {
      paste("data", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(dataArrivedTrend(), file)
      },
    contentType = "text/csv"
  )
  
  
  
  
  output$treat_util_download <- renderUI({
    #ns <- session$ns
    
    if(is.null(session$user) || !(session$user %in% c("nevink01"))) return()
    downloadButton("treat_util_download1")
  })
  
  # observeEvent(input$volumetrend_download, {
  #   downloadHandler(
  #     filename = "filename.csv",
  #     content = write.csv(dataArrivedTrend(), "filename.csv")
  #   )
  # })
  
  output$treat_util_download1 <- downloadHandler(
    filename = function() {
      paste("data", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(dataUtilization_Treatment(), file)
    },
    contentType = "text/csv"
  )
  
  # Data Download--------------------------------------------------------------------------------------------
  output$volume_data_tbl = renderDT(
    dataAll() %>% select(SITE, DEPARTMENT_NAME, PROVIDER, APPT_DTTM, APPT_TYPE, APPT_STATUS,
                         ASSOCIATIONLISTA, ASSOCIATIONLISTB, ASSOCIATIONLISTT, EPIC_PROVIDER_ID, APPT_STATUS, LOS_CODE, MRN) %>% 
      rename(CAMPUS = SITE,
             DEPARTMENT = DEPARTMENT_NAME) %>% collect() %>% 
      relocate(CAMPUS, .before = DEPARTMENT),
    #callback = JS("$('div.dwnld').append($('#download1'));"),
    callback = callback,
    #filter = "top",
    server = TRUE,
    #extensions = 'Buttons',
    options = list(
      dom = 'B<"dwnld">frtip',
      #buttons = c('csv','excel'),
      scrollX = TRUE,
      autowidth = TRUE,
      columnDefs = list(list(width = '2000px', targets = c(2))),
      rownames = FALSE
    )
  )
  
  
  # output$utilization_data_tbl = renderDT(
  #   dataUtilization_Treatment_download(), #%>% select(-Campus.Specialty, -Sex, -uniqueId, -New.PT2, -New.PT) %>% 
  #     #rename(New.PT = New.PT3,
  #            #Campus = SITE) %>%
  #     #relocate(Campus, .before = Department),
  #   filter = "top",
  #   server = TRUE,
  #   extensions = 'Buttons',
  #   options = list(
  #     dom = 'Bfrtip',
  #     #buttons = c('csv','excel'),
  #     scrollX = TRUE
  #   )
  # )
  
  
  output$download1 <- downloadHandler(
    filename = function(){
      paste0("oncology-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dataArrivedTrend_download(),file, row.names = F)
    }
  )
  
  # system_date_range <- eventReactive(input$update_filters, {
  #   date_1 <- input$dateRange[1]
  #   date_2 <- input$dateRange[2]
  # 
  #   date_list <- list(date_1, date_2)
  #   
  #   date_list
  # },
  # ignoreNULL = FALSE,
  # ignoreInit = TRUE)
  
  system_date_range <- reactive({
    input$update_filters

      date_1 <- isolate(input$dateRange[1])
      date_2 <- isolate(input$dateRange[2])

      date_list <- list(date_1, date_2)
      
      date_list
  })
  
  my_chart_race_grouper_selected <- reactive({
    input$update_filters_race
    selected <- isolate(input$race_grouper)
  })
  
  
  output$system_ethnicity_and_race_unknown <- renderPlotly({
    date_range <- system_date_range()
    format <- "YYYY-MM-DD HH24:MI:SS"
    date_1 <- date_range[[1]]
    date_2 <- as.Date(date_range[[2]]) + 1
    data <- oncology_tbl %>% filter(APPT_STATUS == "Arrived",
                                    TO_DATE(date_1, format) <= APPT_DTTM, 
                                    TO_DATE(date_2, format) > APPT_DTTM)
    

    unique_patients <- data %>% select(MRN,APPT_MONTH_YEAR) %>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n())
    
    unknown_race_count <- data %>% select(MRN,APPT_MONTH_YEAR, RACE_GROUPER) %>% filter(RACE_GROUPER == 'BLANK/UNKNOWN')%>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    null_race_count <- data %>% select(MRN,APPT_MONTH_YEAR, RACE_GROUPER) %>% filter(is.null(RACE_GROUPER)) %>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    unknown_and_null_race_count <- bind_rows(unknown_race_count, null_race_count) %>% group_by(APPT_MONTH_YEAR) %>% summarise(race_unkown_total = sum(total))
    
    unknown_ethicity_count <- data %>% select(MRN,APPT_MONTH_YEAR, ETHNICITY_GROUPER) %>% filter(ETHNICITY_GROUPER == 'BLANK/UNKNOWN')%>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    null_ethnicity_count <- data %>% select(MRN,APPT_MONTH_YEAR, ETHNICITY_GROUPER) %>% filter(is.null(ETHNICITY_GROUPER)) %>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    unknown_and_null_ethnicity_count <- bind_rows(unknown_ethicity_count, null_ethnicity_count) %>% group_by(APPT_MONTH_YEAR) %>% summarise(ethnicity_unkown_total = sum(total))
    
    unique_patients_combined <- inner_join(unique_patients, unknown_and_null_race_count)
    unique_patients_combined <- inner_join(unique_patients_combined, unknown_and_null_ethnicity_count)
    unique_patients_combined <- unique_patients_combined %>% group_by(APPT_MONTH_YEAR) %>% mutate(perc_race_unknown = (race_unkown_total/total),
                                                                                                  ethnicity_race_unknown = (ethnicity_unkown_total/total)) 
    
    
    plot_ly(unique_patients_combined, x = ~APPT_MONTH_YEAR, y = ~total, type = 'bar', name = "Unique MRN",
            marker = list(color = "#212070")) %>%
      add_trace(unique_patients_combined, x=~APPT_MONTH_YEAR, y = ~perc_race_unknown, yaxis = "y2",type = "scatter", mode = 'line', name = "Race % Blank/Unk",line = list(color = "#00aeef"), marker = list(color = "#00aeef")) %>%
      add_trace(unique_patients_combined, x=~APPT_MONTH_YEAR, y = ~ethnicity_race_unknown, yaxis = "y2",type = "scatter", mode = 'line', name = "Ethnicity % Blank/Unk",line = list(color = "#d80b8c"), marker = list(color = "#d80b8c")) %>%
      layout(yaxis2 = list(overlaying = "y", side = "right", tickformat = ".2%", range = c(0,round_any(2.5*max(unique_patients_combined$ethnicity_race_unknown),0.05, ceiling)), 
                           automargin = T)) %>%
      layout(legend = list(x = 1.10, y = 1), title = "System Race and Ethnicity % Blank/Unknown") %>%
      layout(xaxis = list(title = NA),
             yaxis = list(title = NA))
    
  })
  
  plotCount <- reactive({
    input$update_filters
    as.numeric(length(isolate(input$selectedCampus)))
  })
  
  plotHeight <- reactive(350 * plotCount())    
  output$ethnicity_and_race_unknown_plots <- renderPlotly({
    data <- dataArrivedTrend()

    # unknow_ethnicity_count <- data %>% select(MRN,APPT_MONTH_YEAR, ETHNICITY_GROUPER) %>% filter(ETHNICITY_GROUPER == 'UNKNOWN')%>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
    #                             group_by(APPT_MONTH_YEAR) %>% summarise(total = n())
    # 
    # null_ethnicity_count <- data %>% select(MRN,APPT_MONTH_YEAR, ETHNICITY_GROUPER) %>% filter(is.null(ETHNICITY_GROUPER)) %>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
    #                           group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    # 
    # unknown_and_null_ethnicity_count <- bind_rows(unknow_ethnicity_count, null_ethnicity_count) %>% group_by(APPT_MONTH_YEAR) %>% summarise(ethnicity_unknown_total = sum(total))
    # 
    # uknown_counts_combined <- inner_join(unknown_and_null_race_count, unknown_and_null_ethnicity_count)
  
    unique_patients <- data %>% select(SITE, MRN,APPT_MONTH_YEAR) %>% group_by(SITE, MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
                        group_by(SITE, APPT_MONTH_YEAR) %>% summarise(total = n())
    
    unknown_race_count <- data %>% select(SITE, MRN,APPT_MONTH_YEAR, RACE_GROUPER) %>% filter(RACE_GROUPER == 'BLANK/UNKNOWN')%>% group_by(SITE, MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
                            group_by(SITE, APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    null_race_count <- data %>% select(SITE,MRN,APPT_MONTH_YEAR, RACE_GROUPER) %>% filter(is.null(RACE_GROUPER)) %>% group_by(SITE,MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(SITE, APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    unknown_and_null_race_count <- bind_rows(unknown_race_count, null_race_count) %>% group_by(SITE, APPT_MONTH_YEAR) %>% summarise(race_unkown_total = sum(total))
    
    # unknow_ethnicity_count <- data %>% select(MRN,APPT_MONTH_YEAR, ETHNICITY_GROUPER) %>% filter(ETHNICITY_GROUPER == 'UNKNOWN')%>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
    #                             group_by(APPT_MONTH_YEAR) %>% summarise(total = n())
    # 
    # null_ethnicity_count <- data %>% select(MRN,APPT_MONTH_YEAR, ETHNICITY_GROUPER) %>% filter(is.null(ETHNICITY_GROUPER)) %>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
    #                           group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    # 
    # unknown_and_null_ethnicity_count <- bind_rows(unknow_ethnicity_count, null_ethnicity_count) %>% group_by(APPT_MONTH_YEAR) %>% summarise(ethnicity_unknown_total = sum(total))
    # 
    # uknown_counts_combined <- inner_join(unknown_and_null_race_count, unknown_and_null_ethnicity_count)
    
    unique_patients_combined <- inner_join(unique_patients, unknown_and_null_race_count)
    unique_patients_combined <- unique_patients_combined %>% group_by(SITE, APPT_MONTH_YEAR) %>% mutate(perc_race_unknown = (race_unkown_total/total)) 

    unique_patients_combined_test <<- unique_patients_combined
    site_unique_plotly_graph(unique_patients_combined)
    
  
    
                          
    
  })
  
  output$ethnicity_and_race_unknown <- renderUI({
    plotlyOutput("ethnicity_and_race_unknown_plots", height = plotHeight()) %>%
      withSpinner(type = 5, color = "#d80b8c")
  })
  
  output$ethnicity_and_race_unknown_plots_single <- renderPlotly({
    data <- dataArrivedTrend()

    unique_patients <- data %>% select(MRN,APPT_MONTH_YEAR) %>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n())
    
    unknown_race_count <- data %>% select(MRN,APPT_MONTH_YEAR, RACE_GROUPER) %>% filter(RACE_GROUPER == 'BLANK/UNKNOWN')%>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by( APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    null_race_count <- data %>% select(MRN,APPT_MONTH_YEAR, RACE_GROUPER) %>% filter(is.null(RACE_GROUPER)) %>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    unknown_and_null_race_count <- bind_rows(unknown_race_count, null_race_count) %>% group_by(APPT_MONTH_YEAR) %>% summarise(race_unkown_total = sum(total))
    
    unknown_ethicity_count <- data %>% select(MRN,APPT_MONTH_YEAR, ETHNICITY_GROUPER) %>% filter(ETHNICITY_GROUPER == 'BLANK/UNKNOWN')%>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    null_ethnicity_count <- data %>% select(MRN,APPT_MONTH_YEAR, ETHNICITY_GROUPER) %>% filter(is.null(ETHNICITY_GROUPER)) %>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    unknown_and_null_ethnicity_count <- bind_rows(unknown_ethicity_count, null_ethnicity_count) %>% group_by(APPT_MONTH_YEAR) %>% summarise(ethnicity_unkown_total = sum(total))
    
    unique_patients_combined <- inner_join(unique_patients, unknown_and_null_race_count)
    unique_patients_combined <- inner_join(unique_patients_combined, unknown_and_null_ethnicity_count)
    
    unique_patients_combined <- unique_patients_combined %>% group_by(APPT_MONTH_YEAR) %>% mutate(perc_race_unknown = (race_unkown_total/total),
                                                                                                  ethnicity_race_unknown = (ethnicity_unkown_total/total))
    
    title_plot <- isolate(input$selectedCampus)
    title_plot <- paste(unique(sort(title_plot)), collapse = ",")
    
    plot_ly(unique_patients_combined, x = ~APPT_MONTH_YEAR, y = ~total, type = 'bar', name = "Unique MRN",
            marker = list(color = "#212070")) %>%
      add_trace(unique_patients_combined, x=~APPT_MONTH_YEAR, y = ~perc_race_unknown, yaxis = "y2",type = "scatter", mode = 'line', name = "Race % Blank/Unk",line = list(color = "#00aeef"), marker = list(color = "#00aeef")) %>%
      add_trace(unique_patients_combined, x=~APPT_MONTH_YEAR, y = ~ethnicity_race_unknown, yaxis = "y2",type = "scatter", mode = 'line', name = "Ethnicity % Blank/Unk",line = list(color = "#d80b8c"), marker = list(color = "#d80b8c")) %>%
      layout(yaxis2 = list(overlaying = "y", side = "right", tickformat = ".2%", range = c(0,round_any(2.5*max(unique_patients_combined$ethnicity_race_unknown),0.05, ceiling)), 
                           automargin = T)) %>%
      layout(legend = list(x = 1.10, y = 1), title = paste0(title_plot, " Race and Ethnicity % Blank/Unknown")) %>%
      layout(xaxis = list(title = NA),
             yaxis = list(title = NA))
  })
  
  output$race_heatmap <- renderPlotly({
    date_range <- system_date_range()
    format <- "YYYY-MM-DD HH24:MI:SS"
    date_1 <- date_range[[1]]
    date_2 <- as.Date(date_range[[2]]) + 1
    data <- oncology_tbl %>% filter(APPT_STATUS == "Arrived",
                                    TO_DATE(date_1, format) <= APPT_DTTM, 
                                    TO_DATE(date_2, format) > APPT_DTTM)
    
    unique_patients <- data %>% select(MRN,APPT_MONTH_YEAR) %>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n())
    
    unknown_race_count <- data %>% select(MRN,APPT_MONTH_YEAR, RACE_GROUPER) %>% filter(RACE_GROUPER == 'BLANK/UNKNOWN')%>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    null_race_count <- data %>% select(MRN,APPT_MONTH_YEAR, RACE_GROUPER) %>% filter(is.null(RACE_GROUPER)) %>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    unknown_and_null_race_count <- bind_rows(unknown_race_count, null_race_count) %>% group_by(APPT_MONTH_YEAR) %>% summarise(race_unkown_total = sum(total))
    
    unique_patients_combined <- inner_join(unique_patients, unknown_and_null_race_count)
    unique_patients_combined <- unique_patients_combined %>% group_by(APPT_MONTH_YEAR) %>% mutate(perc_race_unknown = round(race_unkown_total/total*100, 1)) 
    unique_patients_combined_system <- unique_patients_combined %>% mutate(SITE = "System")
    data <- dataArrivedTrend()
    
    unique_patients <- data %>% select(SITE, MRN,APPT_MONTH_YEAR) %>% group_by(SITE, MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(SITE, APPT_MONTH_YEAR) %>% summarise(total = n())
    
    unknown_race_count <- data %>% select(SITE, MRN,APPT_MONTH_YEAR, RACE_GROUPER) %>% filter(RACE_GROUPER == 'BLANK/UNKNOWN')%>% group_by(SITE, MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(SITE, APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    null_race_count <- data %>% select(SITE,MRN,APPT_MONTH_YEAR, RACE_GROUPER) %>% filter(is.null(RACE_GROUPER)) %>% group_by(SITE,MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(SITE, APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    unknown_and_null_race_count <- bind_rows(unknown_race_count, null_race_count) %>% group_by(SITE, APPT_MONTH_YEAR) %>% summarise(race_unkown_total = sum(total))
    
    unique_patients_combined <- inner_join(unique_patients, unknown_and_null_race_count)
    unique_patients_combined <- unique_patients_combined %>% group_by(SITE, APPT_MONTH_YEAR) %>% mutate(perc_race_unknown = round((race_unkown_total/total) *100, 1))
    
    unique_patients_combined_all <- bind_rows(unique_patients_combined_system, unique_patients_combined)
    

    table <- ggplot(unique_patients_combined_all, aes(x= APPT_MONTH_YEAR, y = SITE)) +
              geom_tile(aes(fill=perc_race_unknown), colour = "black", size=0.5) +
              labs(x=NULL, y=NULL,
                   title = "Race % Blank/Unknown")+
      scale_fill_gradient(low = "#63BE7B", high = "#F8696B", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="% Blank/Unknown")+
      scale_y_discrete(limits = unique(sort(unique_patients_combined_all$SITE))) +
      geom_text(aes(label= ifelse(is.na(perc_race_unknown),"",paste0(perc_race_unknown,"%"))), color="black", size=5, fontface="bold")+
      scale_x_discrete(position = "top") +
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(0.7,"cm"),
            legend.text = element_text(size="12"),
            axis.title.x = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_text(color="black", vjust=0.5, hjust = 0.5, margin = margin(b=15, t=100)),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(30,30,30,30))
    
    
    ggplotly(table) %>%
      layout(xaxis = list(side ="top" )) 
  })
  
  
  output$ethnicity_heatmap <- renderPlotly({
    date_range <- system_date_range()
    format <- "YYYY-MM-DD HH24:MI:SS"
    date_1 <- date_range[[1]]
    date_2 <- as.Date(date_range[[2]]) + 1
    data <- oncology_tbl %>% filter(APPT_STATUS == "Arrived",
                                    TO_DATE(date_1, format) <= APPT_DTTM, 
                                    TO_DATE(date_2, format) > APPT_DTTM)
    
    unique_patients <- data %>% select(MRN,APPT_MONTH_YEAR) %>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n())
    
    unknown_ethnicity_count <- data %>% select(MRN,APPT_MONTH_YEAR, ETHNICITY_GROUPER) %>% filter(ETHNICITY_GROUPER == 'BLANK/UNKNOWN')%>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    null_ethnicity_count <- data %>% select(MRN,APPT_MONTH_YEAR, ETHNICITY_GROUPER) %>% filter(is.null(ETHNICITY_GROUPER)) %>% group_by(MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    unknown_and_null_ethnicity_count <- bind_rows(unknown_ethnicity_count, null_ethnicity_count) %>% group_by(APPT_MONTH_YEAR) %>% summarise(race_unkown_total = sum(total))
    
    unique_patients_combined <- inner_join(unique_patients, unknown_and_null_ethnicity_count)
    unique_patients_combined <- unique_patients_combined %>% group_by(APPT_MONTH_YEAR) %>% mutate(perc_race_unknown = round(race_unkown_total/total*100, 1)) 
    unique_patients_combined_system <- unique_patients_combined %>% mutate(SITE = "System")
    data <- dataArrivedTrend()
    
    unique_patients <- data %>% select(SITE, MRN,APPT_MONTH_YEAR) %>% group_by(SITE, MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(SITE, APPT_MONTH_YEAR) %>% summarise(total = n())
    
    unknown_ethnicity_count <- data %>% select(SITE, MRN,APPT_MONTH_YEAR, ETHNICITY_GROUPER) %>% filter(ETHNICITY_GROUPER == 'BLANK/UNKNOWN')%>% group_by(SITE, MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(SITE, APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    null_race_count <- data %>% select(SITE,MRN,APPT_MONTH_YEAR, ETHNICITY_GROUPER) %>% filter(is.null(ETHNICITY_GROUPER)) %>% group_by(SITE,MRN,APPT_MONTH_YEAR) %>% distinct() %>% collect() %>%
      group_by(SITE, APPT_MONTH_YEAR) %>% summarise(total = n()) 
    
    unknown_and_null_ethnicity_count <- bind_rows(unknown_ethnicity_count, null_race_count) %>% group_by(SITE, APPT_MONTH_YEAR) %>% summarise(race_unkown_total = sum(total))
    
    unique_patients_combined <- inner_join(unique_patients, unknown_and_null_ethnicity_count)
    unique_patients_combined <- unique_patients_combined %>% group_by(SITE, APPT_MONTH_YEAR) %>% mutate(perc_race_unknown = round((race_unkown_total/total) *100, 1))
    
    unique_patients_combined_all <- bind_rows(unique_patients_combined_system, unique_patients_combined)
    

    table <- ggplot(unique_patients_combined_all, aes(x= APPT_MONTH_YEAR, y = SITE)) +
      geom_tile(aes(fill=perc_race_unknown), colour = "black", size=0.5) +
      labs(x=NULL, y=NULL,
           title = "Ethnicity % Blank/Unknown")+
      scale_fill_gradient(low = "#63BE7B", high = "#F8696B", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="% Blank/Unknown")+
      scale_y_discrete(limits = unique(sort(unique_patients_combined_all$SITE))) +
      geom_text(aes(label= ifelse(is.na(perc_race_unknown),"",paste0(perc_race_unknown,"%"))), color="black", size=5, fontface="bold")+
      scale_x_discrete(position = "top") +
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(0.7,"cm"),
            legend.text = element_text(size="12"),
            axis.title.x = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_text(color="black", vjust=0.5, hjust = 0.5, margin = margin(b=15, t=100)),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(30,30,30,30))
    
    
    ggplotly(table) %>%
      layout(xaxis = list(side ="top" )) 
  })
  
  my_chart_race_grouper_selected <- reactive({
    input$update_filters_mychart
    selected <- isolate(input$race_grouper_mychart)

    
    
  })
  
  output$system_my_chart_activation <- renderPlotly({
    date_range <- system_date_range()
    date_range_test <<- date_range
    format <- "YYYY-MM-DD HH24:MI:SS"
    date_1 <- date_range[[1]]
    date_2 <- as.Date(date_range[[2]]) + 1
    data <- oncology_tbl %>% filter(APPT_STATUS == "Arrived",
                                    TO_DATE(date_1, format) <= APPT_DTTM, 
                                    TO_DATE(date_2, format) > APPT_DTTM)
    race_grouper <- my_chart_race_grouper_selected()

    
    activation_data <- data %>% filter(RACE_GROUPER %in% race_grouper) %>% 
                      #group_by(MRN,APPT_MONTH_YEAR, MYCHART_STATUS_GROUPER, RACE_GROUPER) %>% distinct() %>% collect() %>%
                      group_by(APPT_MONTH_YEAR, MYCHART_STATUS_GROUPER, RACE_GROUPER) %>% summarise(total = n()) %>% collect() %>%
                      group_by(APPT_MONTH_YEAR, RACE_GROUPER) %>% mutate(total_race_group = sum(total)) %>%
                      group_by(APPT_MONTH_YEAR, MYCHART_STATUS_GROUPER, RACE_GROUPER) %>% mutate(`Percent Activated` = round(total/total_race_group,2)) %>%
                      rename(`Appt Month` = APPT_MONTH_YEAR)
    
    activated_percent <- activation_data %>% filter(MYCHART_STATUS_GROUPER == "Activated")
    
    plot <-   ggplot(activated_percent, aes(y = `Percent Activated`, x= `Appt Month`, fill = RACE_GROUPER))+
              geom_bar(position='dodge', stat= "identity") +
              scale_fill_manual(values = c("#d80b8c", "#212070","#7f7f7f"))+
              scale_y_continuous(labels = scales::percent, limits = c(0,1))+
              labs(title = "System MyChart Activation by Race/Ethnicity", x=NULL)+
              theme(plot.title = element_text(hjust = 0.5),
                    legend.position = "top")+
              geom_text(aes(label=`Percent Activated`*100, y = `Percent Activated`*100 + 0.01), position=position_dodge(width=0.9),vjust=0)
    
    ggplotly(plot) %>%
      layout(legend = list(title = NA, orientation = "h",   # show entries horizontally
                           y = 1.05, x = 0.35))
      #                                   xanchor = "center",  # use center of legend as anchor
      #                                   x = 0.5))             # put legend
    
    
    
    
  })
  
  output$site_my_chart_activation <- renderPlotly({
    
    data <- dataArrivedTrend()

    race_grouper <- my_chart_race_grouper_selected()
    
    activation_data <- data %>% filter(RACE_GROUPER %in% race_grouper) %>%
      group_by(APPT_MONTH_YEAR, MYCHART_STATUS_GROUPER, RACE_GROUPER) %>% summarise(total = n()) %>% collect() %>%
      group_by(APPT_MONTH_YEAR, RACE_GROUPER) %>% mutate(total_race_group = sum(total)) %>%
      group_by(APPT_MONTH_YEAR, MYCHART_STATUS_GROUPER, RACE_GROUPER) %>% mutate(`Percent Activated` = round(total/total_race_group,2)) %>%
      rename(`Appt Month` = APPT_MONTH_YEAR,
             Race = RACE_GROUPER)
    
    activated_percent <- activation_data %>% filter(MYCHART_STATUS_GROUPER == "Activated")
    
    title <- paste(sort(unique(isolate(input$selectedCampus))),sep="", collapse=", ")
    
    plot <-   ggplot(activated_percent, aes(fill = Race, y = `Percent Activated`, x= `Appt Month`))+
      geom_bar(position='dodge', stat= "identity") +
      scale_fill_manual(values = c("#d80b8c", "#212070","#7f7f7f"))+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      labs(title = paste0(title," MyChart Activation by Race/Ethnicity"), x=NULL)+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "top")+
      geom_text(aes(label=`Percent Activated`*100, y = `Percent Activated`*100 + 0.01), position=position_dodge(width=0.9),vjust=0)
    
    ggplotly(plot) %>%
      layout(legend = list(title = NA, orientation = "h",   # show entries horizontally
                           y = 1.05, x = 0.35))
    
  })
  
  output$site_my_chart_activation_white <- renderPlotly({
    data <- dataArrivedTrend()
    
    
    activation_data <- data %>% filter(RACE_GROUPER %in% c("WHITE")) %>% 
      group_by(SITE, APPT_MONTH_YEAR, MYCHART_STATUS_GROUPER) %>% summarise(total = n()) %>% collect() %>%
      group_by(SITE, APPT_MONTH_YEAR) %>% mutate(total_race_group = sum(total)) %>%
      group_by(SITE, APPT_MONTH_YEAR, MYCHART_STATUS_GROUPER) %>% mutate(`Percent Activated` = round(total/total_race_group,2)) %>%
      rename(`Appt Month` = APPT_MONTH_YEAR)
    
    activated_percent <- activation_data %>% filter(MYCHART_STATUS_GROUPER == "Activated")
    manual_colours <- c("#212070","#7f7f7f", "#d80b8c", "#c9f0ff", "#ffc000", "#fcc9e9", "#dddedd", "#5cd3ff")
    
    plot <-   ggplot(activated_percent, aes(fill = SITE, y = `Percent Activated`, x= `Appt Month`))+
      geom_bar(position='dodge', stat= "identity") +
      scale_fill_manual(values = manual_colours[1:length(sort(unique(activated_percent$SITE)))])+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      labs(title = "Site MyChart Activation by Race/Ethnicity - White", x=NULL)+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "top")+
      geom_text(aes(label=`Percent Activated`*100, y = `Percent Activated`*100 + 0.01), position=position_dodge(width=0.9),vjust=0)
    
    ggplotly(plot) %>%
      layout(legend = list(title = NA, orientation = "h",   # show entries horizontally
                           y = 1.05, x = 0.35))
    #                                   xanchor = "center",  # use center of legend as anchor
    #                                   x = 0.5))             # put legend
    
    
    
    
  })
  
  output$site_my_chart_activation_african_american <- renderPlotly({
    data <- dataArrivedTrend()
    
    
    activation_data <- data %>% filter(RACE_GROUPER %in% c("AFRICAN-AMERICAN")) %>% 
      group_by(SITE, APPT_MONTH_YEAR, MYCHART_STATUS_GROUPER) %>% summarise(total = n()) %>% collect() %>%
      group_by(SITE, APPT_MONTH_YEAR) %>% mutate(total_race_group = sum(total)) %>%
      group_by(SITE, APPT_MONTH_YEAR, MYCHART_STATUS_GROUPER) %>% mutate(`Percent Arrived` = round(total/total_race_group,2)) %>%
      rename(`Appt Month` = APPT_MONTH_YEAR)
    
    activated_percent <- activation_data %>% filter(MYCHART_STATUS_GROUPER == "Activated")
    manual_colours <- c("#212070","#7f7f7f", "#d80b8c", "#c9f0ff", "#ffc000", "#fcc9e9", "#dddedd", "#5cd3ff")
    
    plot <-   ggplot(activated_percent, aes(fill = SITE, y = `Percent Arrived`, x= `Appt Month`))+
      geom_bar(position='dodge', stat= "identity") +
      scale_fill_manual(values = manual_colours[1:length(sort(unique(activated_percent$SITE)))])+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      labs(title = "Site MyChart Activation by Race/Ethnicity - African American", x=NULL)+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "top")+
      geom_text(aes(label=`Percent Arrived`*100, y = `Percent Arrived`*100 + 0.01), position=position_dodge(width=0.9),vjust=0)
    
    ggplotly(plot) %>%
      layout(legend = list(title = NA, orientation = "h",   # show entries horizontally
                           y = 1.05, x = 0.35))
    #                                   xanchor = "center",  # use center of legend as anchor
    #                                   x = 0.5))             # put legend
    
    
    
    
  })
  
  output$site_my_chart_activation_asian <- renderPlotly({
    data <- dataArrivedTrend()
    
    
    activation_data <- data %>% filter(RACE_GROUPER %in% c("ASIAN")) %>% 
      group_by(SITE, APPT_MONTH_YEAR, MYCHART_STATUS_GROUPER) %>% summarise(total = n()) %>% collect() %>%
      group_by(SITE, APPT_MONTH_YEAR) %>% mutate(total_race_group = sum(total)) %>%
      group_by(SITE, APPT_MONTH_YEAR, MYCHART_STATUS_GROUPER) %>% mutate(`Percent Arrived` = round(total/total_race_group,2)) %>%
      rename(`Appt Month` = APPT_MONTH_YEAR)
    
    activated_percent <- activation_data %>% filter(MYCHART_STATUS_GROUPER == "Activated")
    manual_colours <- c("#212070","#7f7f7f", "#d80b8c", "#c9f0ff", "#ffc000", "#fcc9e9", "#dddedd", "#5cd3ff")
    
    plot <-   ggplot(activated_percent, aes(fill = SITE, y = `Percent Arrived`, x= `Appt Month`))+
      geom_bar(position='dodge', stat= "identity") +
      scale_fill_manual(values = manual_colours[1:length(sort(unique(activated_percent$SITE)))])+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      labs(title = "Site MyChart Activation by Race/Ethnicity - Asian", x=NULL)+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "top")+
      geom_text(aes(label=`Percent Arrived`*100, y = `Percent Arrived`*100 + 0.01), position=position_dodge(width=0.9),vjust=0)
    
    ggplotly(plot) %>%
      layout(legend = list(title = NA, orientation = "h",   # show entries horizontally
                           y = 1.05, x = 0.35))
    #                                   xanchor = "center",  # use center of legend as anchor
    #                                   x = 0.5))             # put legend
    
    
    
    
  })


} # Close Server
