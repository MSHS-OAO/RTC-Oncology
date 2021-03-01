server <- function(input, output, session) {
  # Observe Events ------------------------------------------------------------------------------------------------------    
  observeEvent(input$download1, {
    screenshot(filename = "Oncology Dashboard")
  })
  
  observeEvent(input$resetheight, {
    updateSliderInput(session,"plotHeight",value = 650)
    
  })
  
  observeEvent(input$dateRangePreset, {
    if(input$dateRangePreset == "1M"){
      updateDateRangeInput(session,"dateRange",start = dateRange_max %m+% months(-1), end = dateRange_max)
    }
    
    if(input$dateRangePreset == "2M"){
      updateDateRangeInput(session,"dateRange",start = dateRange_max %m+% months(-2), end = dateRange_max)
    }
    
    if(input$dateRangePreset == "4M"){
      updateDateRangeInput(session,"dateRange",start = dateRange_max %m+% months(-4), end = dateRange_max)
    }
    
  })
  
  observeEvent(input$selectedCampus,{
    updatePickerInput(session,
                      inputId = "selectedSpecialty",
                      choices = sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus, "Campus.Specialty"]))
    )},
    ignoreInit = TRUE)
  
  observeEvent(c(input$selectedCampus,input$selectedSpecialty),{
    updatePickerInput(session,
                      inputId = "selectedDepartment",
                      choices = sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                              historical.data$Campus.Specialty %in% input$selectedSpecialty, "Department"])) 
    )},
    ignoreInit = TRUE)
  
  observeEvent(c(input$selectedCampus,input$selectedSpecialty, input$selectedDepartment),{
    updatePickerInput(session,
                      inputId = "selectedProvider",
                      choices = sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                              historical.data$Campus.Specialty %in% input$selectedSpecialty &
                                                              historical.data$Department %in% input$selectedDepartment, "Provider"])) 
    )},
    ignoreInit = TRUE)
  
  observeEvent(c(input$selectedCampus,input$selectedSpecialty, input$selectedDepartment, input$selectedProvider),{
    updatePickerInput(session,
                      inputId = "selectedrefProvider",
                      choices = sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                              historical.data$Campus.Specialty %in% input$selectedSpecialty &
                                                              historical.data$Department %in% input$selectedDepartment &
                                                              historical.data$Provider %in% input$selectedProvider, "Ref.Provider"]))
    )},
    ignoreInit = TRUE)
  
  observeEvent(c(input$selectedCampus,input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider),{
    updatePickerInput(session,
                      inputId = "selectedVisitType",
                      choices = sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                              historical.data$Campus.Specialty %in% input$selectedSpecialty &
                                                              historical.data$Department %in% input$selectedDepartment &
                                                              historical.data$Provider %in% input$selectedProvider &
                                                              historical.data$Ref.Provider %in% input$selectedrefProvider, "AssociationListA"]))
    )},
    ignoreInit = TRUE)
  
  observeEvent(c(input$selectedCampus,input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider, input$selectedVisitType),{
    updatePickerInput(session,
                      inputId = "selectedApptType",
                      choices = sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                              historical.data$Campus.Specialty %in% input$selectedSpecialty &
                                                              historical.data$Department %in% input$selectedDepartment &
                                                              historical.data$Provider %in% input$selectedProvider &
                                                              historical.data$Ref.Provider %in% input$selectedrefProvider &
                                                              historical.data$AssociationListA %in% input$selectedVisitType, "AssociationListB"]))
    )},
    ignoreInit = TRUE)
  
  observeEvent(c(input$selectedCampus,input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider, input$selectedVisitType, input$selectedApptType),{
    updatePickerInput(session,
                      inputId = "selectedTreatmentType",
                      choices = sort(unique(historical.data[historical.data$SITE %in% input$selectedCampus &
                                                              historical.data$Campus.Specialty %in% input$selectedSpecialty &
                                                              historical.data$Department %in% input$selectedDepartment &
                                                              historical.data$Provider %in% input$selectedProvider &
                                                              historical.data$Ref.Provider %in% input$selectedrefProvider &
                                                              historical.data$AssociationListA %in% input$selectedVisitType &
                                                              historical.data$AssociationListB %in% input$selectedApptType, "AssociationListT"]))
    )},
    ignoreInit = TRUE)
  
  
  # Reactive Data -----------------------------------------------------------------------------------------------------------------------
  # All pre-processed data ============================================================================================================
  dataAll <- reactive({
    groupByFilters(all.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$selectedVisitType, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # [2.2] Arrived + No Show data ============================================================================================================
  dataArrivedNoShow <- reactive({
    groupByFilters(arrivedNoShow.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$selectedVisitType, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # [2.3] Arrived data ============================================================================================================
  dataArrived <- reactive({
    groupByFilters(arrived.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Canceled data ============================================================================================================
  dataCanceled<- reactive({
    groupByFilters(canceled.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Bumped data ============================================================================================================
  dataBumped<- reactive({
    groupByFilters(bumped.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Arrived data filtered: visitType, apptType, treatmentType ===============================================================
  dataArrived_filtered<- reactive({
    groupByFilters_2(dataArrived(),
                     input$selectedVisitType, input$selectedApptType, input$selectedTreatmentType)
  })
  
  # Arrived population data ============================================================================================================
  dataArrivedPop <- reactive({
    groupByFilters(population.data_filtered,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Arrived unique patients - all visits ================================================================================================
  dataUniqueAll <- reactive({
    groupByFilters(uniquePts.all.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Arrived unique patients - office visits =================================================================================================
  dataUniqueOffice <- reactive({
    groupByFilters(uniquePts.office.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  
  # Arrived unique patients - treatment visits ===================================================================================================
  dataUniqueTreatment <- reactive({
    groupByFilters(uniquePts.treatment.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  
  # Volume Trend Tab ------------------------------------------------------------------------------------------------------    
  output$trend_totalvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    # data <- arrived.data %>% filter(SITE == "MSW", Appt.MonthYear == "2021-02")
    # nrow(data)
    # data <- arrived.data %>% filter(SITE %in% c("DBC","RTC","MSW"))
    
    # nas <- data %>% filter(is.na(AssociationListA))
    # nrow(nas)
    # exc <- data %>% filter(AssociationListA == "Exclude")
    # nrow(exc)
    # exc[1,]
    # 
    # test <- data %>% filter(Appt.Type == "CHEMO FOLLOW UP")
    # unique(test[,c("SITE","Appt.Type","AssociationListA")])
    # unique(exc)
    
    
    total_visits <- data %>%
      filter(AssociationListA %in% c("Office","Treatment","Labs")) %>%
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
      theme_new_line()
    
  }, height = function(x) input$plotHeight)
  
  
  output$trend_examvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    total_visits <- data %>% filter(AssociationListA == "Office") %>% group_by(Appt.Year, Appt.Month) %>% summarise(total = n())
    
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
      theme_new_line()
    
  }, height = function(x) input$plotHeight)
  
  
  output$trend_treatmentvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    total_visits <- data %>% filter(AssociationListA == "Treatment") %>% group_by(Appt.Year, Appt.Month) %>% summarise(total = n())
    
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
      theme_new_line()
    
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
      theme_new_line()
    
  }, height = function(x) input$plotHeight)
  
  output$trend_visitstable <- function(){
    
    data <- dataArrived()
    # data <- arrived.data
    
    #get the total patients per year
    visits_tb_yearly <- data %>% 
      filter(AssociationListA %in% input$annualVolSummary) %>%
      group_by(Appt.Year) %>% summarise(total = n()) %>%
      spread(Appt.Year, total)
    visits_tb_yearly$Appt.Month <- "TOTAL Annual  Comparison"
    visits_tb_yearly <- visits_tb_yearly %>% relocate(Appt.Month)
    
    #get the total patients per year per month
    visits_tb <- data %>% 
      filter(AssociationListA %in% input$annualVolSummary) %>%
      group_by(Appt.Year, Appt.Month) %>% summarise(total = n()) %>%
      spread(Appt.Year, total)
    
    #include all the months needed
    visits_tb <- visits_tb[match(monthOptions, visits_tb$Appt.Month),]
    visits_tb$Appt.Month <- monthOptions
    
    #bind the total visits per month per year with the total yeraly visits 
    visits_tb_total <- rbind(visits_tb, visits_tb_yearly)
    
    #calculate the difference between the two years
    #to do: make itn more dynamic 
    
    visits_tb_total$variance <- visits_tb_total %>% select(length(visits_tb_total)) - visits_tb_total %>% select(length(visits_tb_total)-1)
    
    visits_tb_total$variance_percentage <- visits_tb_total %>% select(length(visits_tb_total)) / visits_tb_total %>% select(length(visits_tb_total)-2)
    
    visits_tb_total$variance_percentage <- formattable::percent(as.numeric(unlist(visits_tb_total$variance_percentage)))
    
    
    year1 <- colnames(visits_tb_total)[2]
    year2 <- colnames(visits_tb_total)[3]
    
    visits_tb_total %>%
      kable(escape = F, align = "c",
            col.names = c("Month", paste0(year1), paste0(year2), paste0("Variance"," ", "(", paste0(year1), "-", paste0(year2), ")"), paste0("% Variance", " ", "(", paste0(year1), "-", paste0(year2), ")"))) %>%
      kable_styling(bootstrap_options = "hover", full_width = FALSE, position = "center", row_label_position = "c", font_size = 24) %>%
      add_header_above(c("Total Visit Volume" = 3, "Volume Variance" = 2),  background = "#7f7f7f", color = "white", font_size = 22, align = "center") %>%
      column_spec(column = c(1, 3, 5), border_right = "thin solid lightgray") %>%
      row_spec(row = 0, font_size = 22, bold=TRUE, background = "#7f7f7f", color = "white") %>%
      row_spec(row = 13, bold = TRUE, background = "#a5a7a5", color = "white")
    
    # months <- append(unique(visits_tb$Appt.Month),"Total")
    # 
    # total_val <- colSums(visits_tb[,-1])
    # visits_tb <- rbind(visits_tb, total_val)
    # 
    # visits_tb$Appt.Month <- months
    # 
    # visits_tb <- variance
    # 
    # visits_tb$variance1 <- visits_tb[,3]-visits_tb[,2]
    # visits_tb$variance_perc <- visits_tb
    
  }
  
  
  # Volume Breakdown Tab ------------------------------------------------------------------------------------------------------       
  output$break_totalvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    total_visits_break <- data %>% filter(AssociationListA %in% c("Office","Treatment","Labs")) %>%
      group_by(Appt.MonthYear, AssociationListA) %>% summarise(total = n())
    
    max <- total_visits_break %>% group_by(Appt.MonthYear) %>% summarise(max = sum(total))
    
    if(length(unique(data$SITE)) == 1){
      site <- unique(data$SITE)
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    g1 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y=total, group=AssociationListA, fill=AssociationListA))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_MountSinai('dark', reverse = TRUE)+
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
      scale_color_MountSinai('dark', reverse = TRUE)+
      geom_text(size = 5, vjust = "center", hjust = "center", fontface  = "bold")+
      geom_hline(yintercept = hline_y, colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL, fill = "AssociationListA")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g1 + g2 + plot_layout(ncol = 1, heights = c(7, 2))
    
    
  }, height = function(x) input$plotHeight)
  
  
  output$break_examvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    total_visits_break <- data %>% filter(AssociationListA == "Office") %>%
      group_by(Appt.MonthYear, AssociationListB) %>% summarise(total = n())
    
    max <- total_visits_break %>% group_by(Appt.MonthYear) %>% summarise(max = sum(total))
    
    if(length(unique(data$SITE)) == 1){
      site <- unique(data$SITE)
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    g3 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y=total, group=AssociationListB, fill=AssociationListB))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_MountSinai('dark', reverse = TRUE)+
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
      scale_color_MountSinai('dark', reverse = TRUE)+
      geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = hline_y, colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "AssociationListB")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g3 + g4 + plot_layout(ncol = 1, heights = c(7, 2))
    
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
      scale_fill_MountSinai('dark', reverse = TRUE)+
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
      scale_color_MountSinai('dark', reverse = TRUE)+
      geom_text(size = 5, vjust = "center", hjust = "center", fontface = 'bold')+
      geom_hline(yintercept = hline_y, colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "AssociationListT")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g5 + g6 + plot_layout(ncol = 1, heights = c(7, 2))
    
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
            
    } else if(input$comp_choices == "Site"){

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
      
    } else if(input$comp_choices == "Specialty"){
      
      if(input$analysis_type == "Monthly"){
        # Comparison by specialty
        visit_comp_specialty <- data %>%
          group_by(Appt.MonthYear, SITE, Campus.Specialty) %>% summarise(total = n()) %>%
          mutate(specialty = paste0(SITE," - ",Campus.Specialty))
        
        max <- visit_comp_specialty %>% group_by(Appt.MonthYear) %>% summarise(total = sum(total))
        
        graph <- ggplot(visit_comp_specialty, aes(x=Appt.MonthYear, y=total, group=specialty, fill=specialty))+
          geom_bar(position="stack", stat="identity", width=0.7)+
          scale_y_continuous(limits=c(0,(max(max$total))*1.2))+
          stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                       size=5, fontface="bold.italic")
        
      } else{
        # Comparison by specialty
        visit_comp_specialty <- data %>%
          group_by(Appt.Week, SITE, Campus.Specialty) %>% summarise(total = n()) %>%
          mutate(specialty = paste0(SITE," - ",Campus.Specialty))
        
        max <- visit_comp_specialty %>% group_by(Appt.Week) %>% summarise(total = sum(total))
        
        graph <- ggplot(visit_comp_specialty, aes(x=Appt.Week, y=total, group=specialty, fill=specialty))+
          geom_bar(position="stack", stat="identity")+
          scale_y_continuous(limits=c(0,(max(max$total))*1.2))+
          stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.Week), geom="text", color="black", 
                       size=5, fontface="bold.italic")+
          scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
      }
      
    } else{
      
      
      if(input$analysis_type == "Monthly"){
        # Comparison by provider
        visit_comp_prov <- data %>%
          group_by(Appt.MonthYear, Provider) %>% summarise(total = n())
        
        max <- visit_comp_prov %>% group_by(Appt.MonthYear) %>% summarise(total = sum(total))
        
        graph <- ggplot(visit_comp_prov, aes(x=Appt.MonthYear, y=total, group=Provider, fill=Provider))+
          geom_bar(position="stack", stat="identity", width=0.7)+
          scale_y_continuous(limits=c(0,(max(max$total))*1.2))+
          stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                       size=5, fontface="bold.italic")
        
      } else{
        # Comparison by provider
        visit_comp_prov <- data %>%
          group_by(Appt.Week, Provider) %>% summarise(total = n())
        
        max <- visit_comp_prov %>% group_by(Appt.Week) %>% summarise(total = sum(total))
        
        graph <- ggplot(visit_comp_prov, aes(x=Appt.Week, y=total, group=Provider, fill=Provider))+
          geom_bar(position="stack", stat="identity", width=0.7)+
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
      
    } else if(input$comp_choices == "Site"){
      
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
      
    } else if(input$comp_choices == "Specialty"){
      
      if(input$analysis_type == "Monthly"){
        # Comparison by specialty
        visit_comp_specialty <- data %>%
          group_by(Appt.MonthYear, SITE, Campus.Specialty) %>% summarise(total = n()) %>%
          mutate(specialty = paste0(SITE," - ",Campus.Specialty))
        
        graph <- ggplot(visit_comp_specialty, aes(x=Appt.MonthYear, y=total, group=specialty))+
          geom_line(aes(color=specialty), size=1.1)+
          geom_point(aes(color=specialty), size=3)
        
      } else{
        # Comparison by specialty
        visit_comp_specialty <- data %>%
          group_by(Appt.Week, SITE, Campus.Specialty) %>% summarise(total = n()) %>%
          mutate(specialty = paste0(SITE," - ",Campus.Specialty))
        
        graph <- ggplot(visit_comp_specialty, aes(x=Appt.Week, y=total, group=specialty))+
          geom_line(aes(color=specialty), size=1.1)+
          geom_point(aes(color=specialty), size=3)+
          scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
      }
      
    } else{
      
      if(input$analysis_type == "Monthly"){
        # Comparison by provider
        visit_comp_prov <- data %>%
          group_by(Appt.MonthYear, Provider) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_prov, aes(x=Appt.MonthYear, y=total, group=Provider))+
          geom_line(aes(color=Provider), size=1.1)+
          geom_point(aes(color=Provider), size=3)
        
      } else{
        # Comparison by provider
        visit_comp_prov <- data %>%
          group_by(Appt.Week, Provider) %>% summarise(total = n())
        
        graph <- ggplot(visit_comp_prov, aes(x=Appt.Week, y=total, group=Provider))+
          geom_line(aes(color=Provider), size=1.1)+
          geom_point(aes(color=Provider), size=3)+
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
  
  
  # Unique Patients - All Tab --------------------------------------------------------------------------------------------
  ## Unique MRN by System
  output$uniqueAllSystem <- renderValueBox({
    
    data <- dataUniqueAll()
    # data <- uniquePts.all.data
    
    valueBoxSpark(
      value =  prettyNum(nrow(data %>% filter(uniqueSystem == FALSE)), big.mark = ','),
      title = toupper("Total System Unique Patients"),
      sparkobj = NULL,
      info = paste0("Total count of unique patients visited MSHS from..."), 
      icon = icon("hospital-user"),
      subtitle = "Based on office, treatment, and lab visits.",
      color = "yellow",
      href = NULL
    )
  })
  
  ## Unique MRN by Site
  output$uniqueAllSite <- renderPlot({
    
    data <- dataUniqueAll()
    # data <- uniquePts.all.data
    
    unique <- data %>% filter(uniqueSite == FALSE) %>% 
      group_by(SITE) %>%
      summarise(total = n())
    
    g7 <- ggplot(unique, aes(x=SITE, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark', reverse = TRUE)+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Total Unique Patients by Site"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", vjust = 1.2, position = position_dodge(0.9))
    
    g8 <- ggplot(unique, aes(x=SITE, y= "Site", label=total, color = SITE)) +
      scale_color_MountSinai('dark', reverse = TRUE)+
      geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g7 + g8 + plot_layout(ncol = 1, heights = c(7, 2))
    

  }, height = function(x) input$plotHeight)
  
  ## Unique MRN  over Time (Months)
  output$uniqueAllTrend <- renderPlot({
    
    data <- dataUniqueAll()
    # data <- uniquePts.all.data
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    if(input$selectedUniquePts == "System"){
      unique <- data %>% filter(uniqueSystem == FALSE) %>% 
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
        scale_color_MountSinai('dark', reverse = TRUE)+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
        geom_hline(yintercept = c(2.5), colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs(y = NULL, x = NULL)+
        theme_minimal() +
        table_theme()
      
      library(patchwork)
      g9 + g10 + plot_layout(ncol = 1, heights = c(7, 2))
      

    } else{
      unique <- data %>% filter(uniqueSite == FALSE) %>% 
        group_by(Appt.MonthYear, SITE) %>%
        summarise(total = n())
      
      g9 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE, color=SITE))+
        geom_line(size=1.1)+
        geom_point(size=3)+
        scale_color_MountSinai('dark', reverse = TRUE)+
        scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
        labs(title = paste0("Unique Patients by Site over Time"),
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
             y = NULL, x = NULL)+
        theme_new_line()+
        theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))
      
      # geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
      #            nudge_x = 0.1, size=5)
      
      g10 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE)) +
        scale_color_MountSinai('dark', reverse = TRUE)+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
        geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs( y = NULL, x = NULL, fill = "SITE")+
        theme_minimal() +
        table_theme()
      
      library(patchwork)
      g9 + g10 + plot_layout(ncol = 1, heights = c(7, 2))
      
    }
  }, height = function(x) input$plotHeight)
  
  ## Unique MRN by Month
  output$uniqueAllMonth <- renderPlot({
    
    data <- dataUniqueAll()
    # data <- uniquePts.all.data
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    if(input$selectedUniquePts == "System"){
      unique <- data %>% filter(uniqueSystemMonth == FALSE) %>% 
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
        scale_color_MountSinai('dark', reverse = TRUE)+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
        geom_hline(yintercept = c(2.5), colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs(y = NULL, x = NULL)+
        theme_minimal() +
        table_theme()
      
      library(patchwork)
      g11 + g12 + plot_layout(ncol = 1, heights = c(7, 2))
      
    } else{
      unique <- data %>% filter(uniqueSiteMonth == FALSE) %>% 
        group_by(Appt.MonthYear, SITE) %>%
        summarise(total = n())
      
      #to get the upper limit for the y_continuous
      unique_ <- unique %>% spread(SITE, total)
      unique_$Appt.MonthYear <- NULL
      max_col <- function(data) sapply(data, max, na.rm = TRUE)
      max_tot_site <- max_col(unique_)
      
      g11 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE))+
        geom_bar(position="stack",stat="identity")+
        scale_fill_MountSinai('dark', reverse = TRUE)+
        scale_y_continuous(limits=c(0,sum(max_tot_site)*1.2))+
        labs(title = paste0("Unique Patients by Site by Month"),
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
             y = NULL, x = NULL, fill = NULL)+
        theme_new_line()+
        theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
        geom_text(aes(label=total), color="white", 
                  size=5, fontface="bold", position = position_stack(vjust = 0.5))+
        stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                     size=5, fontface="bold.italic")
      
      g12 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE)) +
        scale_color_MountSinai('dark', reverse = TRUE)+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
        geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs( y = NULL, x = NULL, fill = "SITE")+
        theme_minimal() +
        table_theme()
      
      library(patchwork)
      g11 + g12 + plot_layout(ncol = 1, heights = c(7, 2))

    }
    
  }, height = function(x) input$plotHeight)
  
  ##----
  
  ## Unique MRN by System
  output$uniqueOfficeSystem <- renderValueBox({
    
    data <- dataUniqueOffice()
    # data <- uniquePts.office.data
    

    valueBoxSpark(
      value =  prettyNum(nrow(data %>% filter(uniqueSystem == FALSE)), big.mark = ','),
      title = toupper("Total System Unique Office Visit Patients"),
      sparkobj = NULL,
      info = paste0("Total count of unique patients visited MSHS from..."), 
      icon = icon("hospital-user"),
      subtitle = "Based on office visits.",
      color = "fuchsia",
      href = NULL
    )
  })
  
  ## Unique MRN by Site
  output$uniqueOfficeSite <- renderPlot({
    
    data <- dataUniqueOffice()
    # data <- uniquePts.office.data
    
    unique <- data %>% filter(uniqueSite == FALSE) %>% 
      group_by(SITE) %>%
      summarise(total = n())
    
    g13 <- ggplot(unique, aes(x=SITE, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark', reverse = TRUE)+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Total Unique Office Visit Patients by Site"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", vjust = 1.2, position = position_dodge(0.9))
    
    g14 <- ggplot(unique, aes(x=SITE, y= "Site", label=total, color = SITE)) +
      scale_color_MountSinai('dark', reverse = TRUE)+
      geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g13 + g14 + plot_layout(ncol = 1, heights = c(7, 2))

  }, height = function(x) input$plotHeight)
  
  ## Unique MRN  over Time (Months)
  output$uniqueOfficeTrend <- renderPlot({
    
    data <- dataUniqueOffice()
    # data <- uniquePts.office.data
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    if(input$selectedUniquePts2 == "System"){
      unique <- data %>% filter(uniqueSystem == FALSE) %>% 
        group_by(Appt.MonthYear) %>%
        summarise(total = n())
      
      g15 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))+
        geom_line(size=1.1)+
        geom_point(size=3)+
        scale_color_MountSinai('dark')+
        scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
        labs(title = paste0("System Unique Office Visit Patients over Time"),
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
             y = NULL, x = NULL, fill = NULL)+
        theme_new_line()+
        theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
        geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
                   nudge_x = 0.1, size=5)
      
      g16 <- ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total)) +
        scale_color_MountSinai('dark', reverse = TRUE)+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
        geom_hline(yintercept = c(2.5), colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs(y = NULL, x = NULL)+
        theme_minimal() +
        table_theme()
      
      library(patchwork)
      g15 + g16 + plot_layout(ncol = 1, heights = c(7, 2))

    } else{
      unique <- data %>% filter(uniqueSite == FALSE) %>% 
        group_by(Appt.MonthYear, SITE) %>%
        summarise(total = n())
      
      g15 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE, color=SITE))+
        geom_line(size=1.1)+
        geom_point(size=3)+
        scale_color_MountSinai('dark', reverse = TRUE)+
        scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
        labs(title = paste0("Unique Office Visit Patients by Site over Time"),
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
             y = NULL, x = NULL)+
        theme_new_line()+
        theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))
      # geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
      #            nudge_x = 0.1, size=5)
      
      g16 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE)) +
        scale_color_MountSinai('dark', reverse = TRUE)+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
        geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs( y = NULL, x = NULL, fill = "SITE")+
        theme_minimal() +
        table_theme()
      
      library(patchwork)
      g15 + g16 + plot_layout(ncol = 1, heights = c(7, 2))

    }
  }, height = function(x) input$plotHeight)
  
  ## Unique MRN by Month
  output$uniqueOfficeMonth <- renderPlot({
    
    data <- dataUniqueOffice()
    # data <- uniquePts.office.data
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    if(input$selectedUniquePts2 == "System"){
      unique <- data %>% filter(uniqueSystemMonth == FALSE) %>% 
        group_by(Appt.MonthYear) %>%
        summarise(total = n())
      
      g17 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))+
        geom_line(size=1.1)+
        geom_point(size=3)+
        scale_color_MountSinai('dark')+
        scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
        labs(title = paste0("System Unique Office Visit Patients by Month"),
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
             y = NULL, x = NULL, fill = NULL)+
        theme_new_line()+
        theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
        geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
                   nudge_x = 0.1, size=5)
      
      g18 <- ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total)) +
        scale_color_MountSinai('dark', reverse = TRUE)+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
        geom_hline(yintercept = c(2.5), colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs(y = NULL, x = NULL)+
        theme_minimal() +
        table_theme()
      
      library(patchwork)
      g17 + g18 + plot_layout(ncol = 1, heights = c(7, 2))
      
    } else{
      unique <- data %>% filter(uniqueSiteMonth == FALSE) %>% 
        group_by(Appt.MonthYear, SITE) %>%
        summarise(total = n())
      
      #to get the upper limit for the y_continuous
      unique_ <- unique %>% spread(SITE, total)
      unique_$Appt.MonthYear <- NULL
      max_col <- function(data) sapply(data, max, na.rm = TRUE)
      max_tot_site <- max_col(unique_)
      
      g17 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE))+
        geom_bar(position="stack",stat="identity")+
        scale_fill_MountSinai('dark', reverse = TRUE)+
        scale_y_continuous(limits=c(0,sum(max_tot_site)*1.2))+
        
        labs(title = paste0("Unique Office Visit Patients by Site by Month"),
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
             y = NULL, x = NULL, fill = NULL)+
        theme_new_line()+
        theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
        geom_text(aes(label=total), color="white", 
                  size=5, fontface="bold", position = position_stack(vjust = 0.5))+
        stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                     size=5, fontface="bold.italic")
      
      g18 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE)) +
        scale_color_MountSinai('dark', reverse = TRUE)+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
        geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs( y = NULL, x = NULL, fill = "SITE")+
        theme_minimal() +
        table_theme()
      
      library(patchwork)
      g17 + g18 + plot_layout(ncol = 1, heights = c(7, 2))

    }
    
  }, height = function(x) input$plotHeight)
  

  ##---- Treatment Visits   
  ## Unique MRN by System
  output$uniqueTreatmentSystem <- renderValueBox({
    
    data <- dataUniqueTreatment()
    # data <- uniquePts.treatment.data
    
    valueBoxSpark(
      value =  prettyNum(nrow(data %>% filter(uniqueSystem == FALSE)), big.mark = ','),
      title = toupper("Total System Treatment Visit Unique Patients"),
      sparkobj = NULL,
      info = paste0("Total count of unique patients visited MSHS from..."), 
      icon = icon("hospital-user"),
      subtitle = "Based on treatment visits.",
      color = "aqua",
      href = NULL
    )
  })
  
  ## Unique MRN by Site
  output$uniqueTreatmentSite <- renderPlot({
    
    data <- dataUniqueTreatment()
    # data <- uniquePts.treatment.data
    
    unique <- data %>% filter(uniqueSite == FALSE) %>% 
      group_by(SITE) %>%
      summarise(total = n())
    
    g19 <- ggplot(unique, aes(x=SITE, y=total, fill=SITE, group=SITE))+
      geom_bar(position="stack",stat="identity")+
      scale_fill_MountSinai('dark', reverse = TRUE)+
      scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
      labs(title = paste0("Total Unique Treatment Visit Patients by Site"),
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", vjust = 1.2, position = position_dodge(0.9))
    
    g20 <- ggplot(unique, aes(x=SITE, y= "Site", label=total, color = SITE)) +
      scale_color_MountSinai('dark', reverse = TRUE)+
      geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "SITE")+
      theme_minimal() +
      table_theme()
    
    library(patchwork)
    g19 + g20 + plot_layout(ncol = 1, heights = c(7, 2))

  }, height = function(x) input$plotHeight)
  
  ## Unique MRN  over Time (Months)
  output$uniqueTreatmentTrend <- renderPlot({
    
    data <- dataUniqueTreatment()
    # data <- uniquePts.treatment.data
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    if(input$selectedUniquePts3 == "System"){
      unique <- data %>% filter(uniqueSystem == FALSE) %>% 
        group_by(Appt.MonthYear) %>%
        summarise(total = n())
      
      g21 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))+

        geom_line(size=1.1)+
        geom_point(size=3)+
        scale_color_MountSinai('dark')+
        scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
        labs(title = paste0("System Unique Treatment Visit Patients over Time"),
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
             y = NULL, x = NULL, fill = NULL)+
        theme_new_line()+
        theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
        geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
                   nudge_x = 0.1, size=5)
      
      g22 <- ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total)) +
        scale_color_MountSinai('dark', reverse = TRUE)+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
        geom_hline(yintercept = c(2.5), colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs(y = NULL, x = NULL)+
        theme_minimal() +
        table_theme()
      
      library(patchwork)
      g21 + g22 + plot_layout(ncol = 1, heights = c(7, 2))
      
    } else{
      unique <- data %>% filter(uniqueSite == FALSE) %>% 
        group_by(Appt.MonthYear, SITE) %>%
        summarise(total = n())
      
      g21 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE, color=SITE))+
        geom_line(size=1.1)+
        geom_point(size=3)+
        scale_color_MountSinai('dark', reverse = TRUE)+
        scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
        labs(title = paste0("Unique Treatment Visit Patients by Site over Time"),
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
             y = NULL, x = NULL)+
        theme_new_line()+
        theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))
      # geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
      #            nudge_x = 0.1, size=5)
      
      
      g22 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE)) +
        scale_color_MountSinai('dark', reverse = TRUE)+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
        geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs( y = NULL, x = NULL, fill = "SITE")+
        theme_minimal() +
        table_theme()
      
      library(patchwork)
      g21 + g22 + plot_layout(ncol = 1, heights = c(7, 2))

    }
  }, height = function(x) input$plotHeight)
  
  ## Unique MRN by Month
  output$uniqueTreatmentMonth <- renderPlot({
    
    data <- dataUniqueTreatment()
    # data <- uniquePts.treatment.data
    
    if(length(unique(data$SITE)) == 9){
      site <- "System"
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    if(input$selectedUniquePts3 == "System"){
      unique <- data %>% filter(uniqueSystemMonth == FALSE) %>% 
        group_by(Appt.MonthYear) %>%
        summarise(total = n())
      
      g23 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, group=1))+
        geom_line(size=1.1)+
        geom_point(size=3)+
        scale_color_MountSinai('dark')+
        scale_y_continuous(limits=c(0,max(unique$total)*1.2))+
        labs(title = paste0("System Unique Treatment Visit Patients by Month"),
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
             y = NULL, x = NULL, fill = NULL)+
        theme_new_line()+
        theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
        geom_label(aes(label=prettyNum(total, big.mark = ',')), hjust = 1, color="black", fontface="bold",
                   nudge_x = 0.1, size=5)
      
      g24 <- ggplot(unique, aes(x=Appt.MonthYear, y= "System", label= total)) +
        scale_color_MountSinai('dark', reverse = TRUE)+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
        geom_hline(yintercept = c(2.5), colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs(y = NULL, x = NULL)+
        theme_minimal() +
        table_theme()
      
      library(patchwork)
      g23 + g24 + plot_layout(ncol = 1, heights = c(7, 2))
      
    } else{
      unique <- data %>% filter(uniqueSiteMonth == FALSE) %>% 
        group_by(Appt.MonthYear, SITE) %>%
        summarise(total = n())
      
      #to get the upper limit for the y_continuous
      unique_ <- unique %>% spread(SITE, total)
      unique_$Appt.MonthYear <- NULL
      max_col <- function(data) sapply(data, max, na.rm = TRUE)
      max_tot_site <- max_col(unique_)
      
      g23 <- ggplot(unique, aes(x=Appt.MonthYear, y=total, fill=SITE, group=SITE))+
        geom_bar(position="stack",stat="identity")+
        scale_fill_MountSinai('dark', reverse = TRUE)+
        scale_y_continuous(limits=c(0,sum(max_tot_site)*1.2))+
        
        labs(title = paste0("Unique Treatment Visit Patients by Site by Month"),
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2],"\n"),
             y = NULL, x = NULL, fill = NULL)+
        theme_new_line()+
        theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+

        geom_text(aes(label=total), color="white", 
                  size=5, fontface="bold", position = position_stack(vjust = 0.5))+
        stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                     size=5, fontface="bold.italic")
      
      g24 <- ggplot(unique, aes(x=Appt.MonthYear, y= SITE, label=total, group = SITE, color = SITE)) +
        scale_color_MountSinai('dark', reverse = TRUE)+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
        geom_hline(yintercept = seq(0.5, length(unique(unique$SITE)), by= 1)[-1], colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") + 
        labs( y = NULL, x = NULL, fill = "SITE")+
        theme_minimal() +
        table_theme()
      
      library(patchwork)
      g23 + g24 + plot_layout(ncol = 1, heights = c(7, 2))
      
    }
    
  }, height = function(x) input$plotHeight)
  
  
  
  
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
    
    if(input$selectedZipCodeMap == "Site"){
      
      newdata <- data %>% group_by(SITE, latitude, longitude) %>% dplyr::summarise(total = n())
      
      # List of all variables
      layer_names <- unique(newdata$SITE)
      # Set color scheme based on comparing variable
      groupColors = colorFactor(palette = MountSinai_colors, domain = newdata$SITE)
      # Prepare the text for the tooltip:
      mytext <- paste(
        "Total Visits: ", newdata$total, "<br/>") %>%
        lapply(htmltools::HTML)
      
      map <-  
        leaflet(data = newdata) %>%
        addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%      
        setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude, color = ~groupColors(SITE), 
                         group = ~SITE, radius= ~total^(1/2),
                         label = mytext,
                         labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "18px", direction = "auto")) %>%
        addLegend(position = "bottomright", pal = groupColors, values = ~SITE) %>%
        addLayersControl(overlayGroups = c(newdata$SITE, layer_names))
      
    } else if(input$selectedZipCodeMap == "Visit Type"){
      
      newdata <- data %>% group_by(AssociationListA, latitude, longitude) %>% dplyr::summarise(total = n())
      
      # List of all variables
      layer_names <- unique(newdata$AssociationListA)
      # Set color scheme based on comparing variable
      groupColors = colorFactor(palette = MountSinai_colors, domain = newdata$AssociationListA)
      # Prepare the text for the tooltip:
      mytext <- paste(
        "Total Visits: ", newdata$total, "<br/>") %>%
        lapply(htmltools::HTML)
      
      map <-  
        leaflet(data = newdata) %>%
        addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%      
        setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude, color = ~groupColors(AssociationListA),
                         group = ~AssociationListA, radius= ~total^(1/2),
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "18px", direction = "auto")) %>%
        addLegend(position = "bottomright", pal = groupColors, values = ~AssociationListA) %>%
        addLayersControl(overlayGroups = c(newdata$AssociationListA, layer_names))
      
    } else{
      
      newdata <- data %>% group_by(AssociationListB, latitude, longitude) %>% dplyr::summarise(total = n())
      
      
      # List of all variables
      layer_names <- unique(newdata$AssociationListB)
      # Set color scheme based on comparing variable
      groupColors = colorFactor(palette = MountSinai_colors, domain = newdata$AssociationListB)
      # Prepare the text for the tooltip:
      mytext <- paste(
        "Total Visits: ", newdata$total, "<br/>") %>%
        lapply(htmltools::HTML)
      
      map <-  
        leaflet(data = newdata) %>%
        addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%      
        setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude, color = ~groupColors(AssociationListB),
                         group = ~AssociationListB, radius= ~total^(1/2),
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "18px", direction = "auto")) %>%
        addLegend(position = "bottomright", pal = groupColors, values = ~AssociationListB) %>%
        addLayersControl(overlayGroups = c(newdata$AssociationListB, layer_names))
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
    
    zip_table <- data %>% group_by(`Zip Code Layer: A`) %>% summarise(total = n()) %>%
      arrange(-total) %>%
      mutate(perc = round(total/sum(total),2)*100) %>%
      adorn_totals("row") %>%
      mutate(perc = paste0(perc,"%"))
    
    manhattan <- data %>% group_by(`Zip Code Layer: B`) %>% summarise(total = n()) %>%
      mutate(perc = paste0(round(total/sum(total),2)*100, "%")) %>%
      filter(`Zip Code Layer: B` %in% c("Lower-Manhattan", "Middle-Manhattan", "Upper-Manhattan")) %>%
      `colnames<-` (c("Zip Code Layer: A","total","perc"))
    
    row_start <- which(zip_table$`Zip Code Layer: A` == "Manhattan") 
    
    table_1 <- zip_table[1:row_start,]
    table_2 <- zip_table[row_start+1:nrow(zip_table),]
    
    final_tb <- rbind(table_1, manhattan, table_2) %>% drop_na()
    
    final_tb %>%
      kable(escape = F, 
            col.names = c("Zip Code Layer A - B", "Total Arrived", "Percent of Total")) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 18) %>%
      add_header_above(c("Total Patients Arrived by Zipcode" = length(zip_table)),
                       color = "black", font_size = 20, align = "center") %>%
      add_indent(c(row_start+1, row_start+2, row_start+3), level_of_indent = 2) %>%
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      row_spec(1:nrow(final_tb), background = "	#e6e6e6", color = "black") %>%
      row_spec(c(row_start+1, row_start+2, row_start+3), background = "#f2f2f2") %>%
      row_spec(nrow(final_tb), background = "#fcc9e9", color = "black", bold = T) 

  }
  
  output$zipCode_ref_tb1 <- function(){
    
    section <- c("Upper-Manhattan","Middle-Manhattan","Lower-Manhattan")
    geo <- c("72nd street and above","34th to 72nd street","34th street and below")
    
    data.frame(section,geo) %>%
      kable(escape = F, 
            col.names = c("Manhattan Zip Code Layer: B", "Geography")) %>%
      kable_styling(bootstrap_options = c("bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 18) %>%
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
      kable_styling(bootstrap_options = c("bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 18) %>%
      row_spec(0, background = "#7f7f7f", color = "white", bold = T) %>%
      collapse_rows(1)
  }
  
  
  
} # Close Server

#shinyApp(ui, server)


