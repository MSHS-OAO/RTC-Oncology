server <- function(input, output, session) {
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
                      selected = disease_choices[1]
    )
    updatePickerInput(session,
                      inputId = "selectedDisease2",
                      choices = disease_choices,
                      selected = disease_choices[1]
    )
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
                      selected = disease_choices[1]
    )
    updatePickerInput(session,
                      inputId = "selectedDisease2",
                      choices = disease_choices,
                      selected = disease_choices[1]
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
                      selected = provider_choices[1]
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
                      selected = provider_choices[1]
    )
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  # Reactive Data -----------------------------------------------------------------------------------------------------------------------
  # All pre-processed data ============================================================================================================
  dataAll <- reactive({
    groupByFilters(all.data,
                   input$selectedCampus, input$selectedDepartment,
                   input$selectedVisitType, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # [2.2] Arrived + No Show data ============================================================================================================
  dataArrivedNoShow <- reactive({
    groupByFilters(arrivedNoShow.data,
                   input$selectedCampus, input$selectedDepartment, 
                   input$selectedVisitType, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # [2.3] Arrived data ============================================================================================================
  dataArrived <- reactive({
    groupByFilters(arrived.data,
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Canceled data ============================================================================================================
  dataCanceled<- reactive({
    groupByFilters(canceled.data,
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Bumped data ============================================================================================================
  dataBumped<- reactive({
    groupByFilters(bumped.data,
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Arrived data filtered: visitType, apptType, treatmentType ===============================================================
  
  dataArrived_filtered<- reactive({
    groupByFilters_2(dataArrived(),
                     input$selectedVisitType, input$selectedApptType, input$selectedTreatmentType)
  })
  
  
  # Arrived Disease Group data filtered: Disease Group, Provider ==============================================================
  
  dataArrived_disease <- reactive({
    groupByFilters_3(dataArrived(),
                     input$selectedDisease, input$selectedProvider)
  })
  
  dataArrived_disease_2 <- reactive({
    groupByFilters_3(dataArrived(),
                     input$selectedDisease2, input$selectedProvider2)
  })
  
  # Arrived population data ============================================================================================================
  dataArrivedPop <- reactive({
    groupByFilters(population.data_filtered,
                   input$selectedCampus, input$selectedDepartment,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  
  # Site Volume Tab ------------------------------------------------------------------------------------------------------0
  # Volume Trend Tab ------------------------------------------------------------------------------------------------------    
  output$trend_totalvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    
    total_visits <- data %>%
      filter(AssociationListA %in% c("Exam","Treatment","Labs")) %>%
      group_by(Appt.Year, Appt.Month) %>% summarise(total = n())
    
    if(length(unique(data$SITE)) == 1){
      site <- unique(data$SITE)
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
      scale_y_continuous(limits=c(0,(max(total_visits$total))*1.3))
    
  }, height = function(x) input$plotHeight)
  
  
  output$trend_examvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    total_visits <- data %>% filter(AssociationListA == "Exam") %>% 
      group_by(Appt.Year, Appt.Month) %>% summarise(total = n())
    
    if(length(unique(data$SITE)) == 1){
      site <- unique(data$SITE)
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
      scale_y_continuous(limits=c(0,(max(total_visits$total))*1.3))
    
  }, height = function(x) input$plotHeight)
  
  
  output$trend_treatmentvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    total_visits <- data %>% filter(AssociationListA == "Treatment") %>% 
      group_by(Appt.Year, Appt.Month) %>% summarise(total = n())
    
    if(length(unique(data$SITE)) == 1){
      site <- unique(data$SITE)
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
      scale_y_continuous(limits=c(0,(max(total_visits$total))*1.3))
    
  }, height = function(x) input$plotHeight)
  
  
  output$trend_labvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    total_visits <- data %>% filter(AssociationListA == "Labs") %>% group_by(Appt.Year, Appt.Month) %>% summarise(total = n())
    
    if(length(unique(data$SITE)) == 1){
      site <- unique(data$SITE)
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
      scale_y_continuous(limits=c(0,(max(total_visits$total))*1.3))
    
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
    # data <- arrived.data
    
    total_visits_break <- data %>% filter(AssociationListA %in% c("Exam","Treatment","Labs")) %>%
      group_by(Appt.MonthYear, AssociationListA) %>% summarise(total = n())
    
    max <- total_visits_break %>% group_by(Appt.MonthYear) %>% summarise(max = sum(total))
    
    if(length(unique(data$SITE)) == 1){
      site <- unique(data$SITE)
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
    
    if(length(unique(data$SITE)) == 1){
      site <- unique(data$SITE)
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
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
    
    #data <- arrived.data %>% filter(SITE == "MSW", Appt.MonthYear == "2020-12")
    # nrow(data)
    
    total_visits_break <- data %>% filter(AssociationListA == "Treatment") %>%
      group_by(Appt.MonthYear, AssociationListT) %>% summarise(total = n())
    
    max <- total_visits_break %>% group_by(Appt.MonthYear) %>% summarise(max = sum(total))
    
    if(length(unique(data$SITE)) == 1){
      site <- unique(data$SITE)
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
      geom_text(aes(label=total), color="white", 
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
    
    graph + 
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      scale_fill_MountSinai('dark')+
      labs(title = paste0("Monthly ",visitType, " Volume Breakdown by ",input$comp_choices),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           caption = paste0("\nIncludes ",apptType),
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
          geom_point(size=3)
        
      } else{
        # Comparison by site
        visit_comp_all <- data %>%
          group_by(Appt.Week) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_all, aes(x=Appt.Week, y=total, group=1))+
          geom_line(size=1.1)+
          geom_point(size=3)+
          scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
      }
      
    } else{
      
      if(input$analysis_type == "Monthly"){
        # Comparison by site
        visit_comp_site <- data %>%
          group_by(Appt.MonthYear, SITE) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_site, aes(x=Appt.MonthYear, y=total, group=SITE))+
          geom_line(aes(color=SITE), size=1.1)+
          geom_point(aes(color=SITE), size=3)
        
      } else{
        # Comparison by site
        visit_comp_site <- data %>%
          group_by(Appt.Week, SITE) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_site, aes(x=Appt.Week, y=total, group=SITE))+
          geom_line(aes(color=SITE), size=1.1)+
          geom_point(aes(color=SITE), size=3)+
          scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
      }
    }
    
    graph + 
      scale_color_MountSinai('dark')+
      labs(title = paste0("Monthly ",visitType, " Volume Trend by ",input$comp_choices),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           caption = paste0("\nIncludes ",apptType),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(axis.text.x = element_text(angle = 40, hjust=1))
    
  }, height = function(x) input$plotHeight)
  
  
  # Provider Volume Tab ------------------------------------------------------------------------------------------------------
  ## Physician Visits Breakdown Tab =========================================================================================
  
  output$provVolumeExam_tb <- function(){
    
    data <- dataArrived_disease() %>%
      filter(AssociationListA == "Exam")
    
    # data <- arrivedDisease.data %>%
    #   filter(AssociationListA == "Exam", SITE %in% c("DBC"))
    
    prov_tb <- data %>% 
      group_by(Disease_Group, Provider, AssociationListB, Appt.MonthYear) %>%
      summarise(total = n())  %>%
      `colnames<-` (c("Disease", "Provider", "Appointment Type", "Appt.MonthYear", "Total")) %>%
      pivot_wider(names_from = Appt.MonthYear,
                  values_from = Total,
                  values_fill = 0) %>%
      adorn_totals("col", fill = "-", na.rm = TRUE, name = "YTD Total") %>%
      arrange(Disease)
    
    row_total <- prov_tb %>% 
      split(.[,"Provider"]) %>%  
      map_df(., janitor::adorn_totals, name = paste0("Exam Total"))
    
    
    kable(prov_tb[,2:length(prov_tb)] %>%
            split(.[,"Provider"]) %>%  
            map_df(., janitor::adorn_totals, name = paste0("Exam Total"))) %>%
      pack_rows(index = table(prov_tb$Disease), label_row_css = "background-color: #fcc9e9;") %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      add_header_above(c("Physician Visits Breakdown" = (length(prov_tb)-1)),
                       color = "black", font_size = 20, align = "center", line = FALSE) %>%
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      row_spec(which(row_total$Disease == "Exam Total"), bold = T, background = "#e6e6e6") %>%
      column_spec(1, bold = T) %>%
      column_spec(length(prov_tb)-1, background = "#d80b8c", color = "white", bold = T) %>%
      collapse_rows(1, valign = "top")
    
  }
  
  
  ## Telehealth Visits Breakdown Tab =========================================================================================
  
  output$provVolumeTele_tb <- function(){
    
    data <- dataArrived_disease() %>%
      filter(AssociationListB == "Telehealth Visit")
    
    # data <- arrivedDisease.data %>%
    #   filter(AssociationListB == "Telehealth Visit")
    
    tele_tb <- data %>% 
      group_by(Disease_Group, Provider, Appt.Type, Appt.MonthYear) %>%
      summarise(total = n()) %>%
      `colnames<-` (c("Disease", "Provider", "Appointment Type", "Appt.MonthYear", "Total")) %>%
      pivot_wider(names_from = Appt.MonthYear,
                  values_from = Total,
                  values_fill = 0) %>%
      adorn_totals("col", fill = "-", na.rm = TRUE, name = "YTD Total") %>%
      arrange(Provider, `Appointment Type`, Disease)
    
    row_total <- tele_tb %>% 
      split(.[,"Provider"]) %>%  
      map_df(., janitor::adorn_totals, name = paste0("Telehealth Total"))
    
    
    kable(tele_tb[,2:length(tele_tb)] %>%
            split(.[,"Provider"]) %>%  
            map_df(., janitor::adorn_totals, name = paste0("Telehealth Total"))) %>%
      pack_rows(index = table(tele_tb$Disease), label_row_css = "background-color: #c9f0ff;") %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      add_header_above(c("Telehealth Visits Breakdown" = (length(tele_tb)-1)),
                       color = "black", font_size = 20, align = "center", line = FALSE) %>%
      row_spec(0, background = "#00aeef", color = "white", bold = T) %>%
      row_spec(which(row_total$Disease == "Telehealth Total"), bold = T, background = "#e6e6e6") %>%
      column_spec(1, bold = T) %>%
      column_spec(length(tele_tb)-1, background = "#00aeef", color = "white", bold = T) %>%
      collapse_rows(1, valign = "top")
    
  }
  
  
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
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>%
      group_by(Appt.MonthYear) %>%
      summarise(total = n())
    
    g9 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))+
      geom_line(size=1.1)+
      geom_point(size=3)+
      scale_color_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("System Unique Patients over Time"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
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
    
    if(length(unique(data$SITE)) == 9){
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
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
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
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
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
    
    if(length(unique(data$SITE)) == 9){
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
    
    g11 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,sum(max_tot_site)*1.2))+
      labs(title = paste0("Unique Patients by Site and Month"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
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
    
    if(length(unique(data$SITE)) == 9){
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
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
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
    
    if(length(unique(data$SITE)) == 9){
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
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
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
    
    data <- uniquePts_df(dataArrived(), c("Treatment Visit"))
    # data <- uniquePts_df(arrived.data, c("Treatment Visit"))
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    unique <- data %>% filter(uniqueSystem == FALSE) %>% 
      group_by(Appt.MonthYear) %>%
      summarise(total = n())
    
    g21 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))+
      geom_line(size=1.1)+
      geom_point(size=3)+
      scale_color_MountSinai('dark')+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      # labs(title = paste0("System Unique Patients over Time - Treatment Visits"),
      #      subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
      #      y = NULL, x = NULL, fill = NULL)+
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
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
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
  
  
  
} # Close Server





