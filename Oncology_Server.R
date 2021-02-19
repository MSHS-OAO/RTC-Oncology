server <- function(input, output, session) {
# Observe Events ------------------------------------------------------------------------------------------------------    
  observeEvent(input$download1, {
    screenshot(filename = "Oncology Dashboard")
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
  
  # Canceled data -----------------------------------------------------------------------------------------------------------------------
  dataCanceled<- reactive({
    groupByFilters(canceled.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })

  # Bumped data -----------------------------------------------------------------------------------------------------------------------
  dataBumped<- reactive({
    groupByFilters(bumped.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Arrived data filtered: visitType, apptType, treatmentType ---------------------------------------------------------------------------
  dataArrived_filtered<- reactive({
    groupByFilters_2(dataArrived(),
                     input$selectedVisitType, input$selectedApptType, input$selectedTreatmentType)
  })
  
  # Arrived population data -------------------------------------------------------------------------------------------------------------
  dataArrivedPop <- reactive({
    groupByFilters(population.data_filtered,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
# Volume Trend Tab ------------------------------------------------------------------------------------------------------    
  output$trend_totalvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    # data <- arrived.data %>% filter(SITE == "MSW", Appt.MonthYear == "2020-12")
    # nrow(data)
    # # data <- arrived.data %>% filter(SITE %in% c("DBC","RTC","MSW"))
    # 
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
      labs(title = paste0(site," ","Annual All Visits\n"),
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
      labs(title = paste0(site," ","Annual Exam Visits\n"), 
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
      labs(title = paste0(site," ","Annual Treatment Visits\n"), 
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
      labs(title = paste0(site," ","Annual Lab Visits\n"), 
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()
    
  }, height = function(x) input$plotHeight)
  
  output$trend_visitstable <- function(){

    data <- dataArrived()
    # data <- arrived.data

    visits_tb <- data %>% 
      filter(AssociationListA %in% input$annualVolSummary) %>%
      group_by(Appt.Year, Appt.Month) %>% summarise(total = n()) %>%
      spread(Appt.Year, total)
    
    visits_tb <- visits_tb[match(monthOptions, visits_tb$Appt.Month),]
    visits_tb$Appt.Month <- monthOptions
    
    visits_tb %>%
      kable(escape = F, align = "c",
            col.names = c("Month", "2020", "2021")) %>%
      kable_styling(bootstrap_options = "hover", full_width = FALSE, position = "center", row_label_position = "c", font_size = 24) %>%
      add_header_above(c("Total Visit Volume" = 3),
                      color = "black", font_size = 22, align = "center") %>%
      row_spec(row = 0, font_size = 22, bold=TRUE, background = "#212070", color = "white")
    
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
      labs(title = paste0(site," ","All Visit Volume Composition\n"),x = NULL, y = "Patient Volume\n", fill = NULL)+
      theme_new_line()+
      theme(axis.title.y = element_text(size = 12, angle = 90),  plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    g2 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y= AssociationListA, label=total, color = AssociationListA)) +
      scale_color_MountSinai('dark', reverse = TRUE)+
      geom_text(size = 5, vjust = "center", hjust = "center", fontface  = "bold")+
      geom_hline(yintercept = c(1.5, 2.5), colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs( y = NULL, x = NULL, fill = "AssociationListA")+
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
      labs(title = paste0(site," ","Exam Visit Volume Composition\n"), 
           y = "Patient Volume\n", x = NULL, fill = NULL)+
      theme_new_line()+
      theme(axis.title.y = element_text(size = 12, angle = 90), plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    g4 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y= AssociationListB, label=total, color = AssociationListB)) +
      scale_color_MountSinai('dark', reverse = TRUE)+
      geom_text(size = 5, vjust = "center", hjust = "center", fontface = "bold")+
      geom_hline(yintercept = c(1.5, 2.5), colour='black')+
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
    # data <- arrived.data %>% filter(SITE == "MSW", Appt.MonthYear == "2020-12")
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
      labs(title = paste0(site," ","Treatment Visit Volume Composition\n"), 
           y = "Patient Volume\n", x = NULL, fill = NULL)+
      theme_new_line()+
      theme(axis.title.y = element_text(size = 12, angle = 90), plot.margin=unit(c(1,1,-0.5,1), "cm"))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    g6 <- ggplot(total_visits_break, aes(x=Appt.MonthYear, y= AssociationListT, label=total, color = AssociationListT)) +
      scale_color_MountSinai('dark', reverse = TRUE)+
      geom_text(size = 5, vjust = "center", hjust = "center", fontface = 'bold')+
      geom_hline(yintercept = c(2.5), colour='black')+
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
      labs(title = paste0("Monthly ",visitType, " Volume Breakdown by Site\n"), 
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
      labs(title = paste0("Monthly ",visitType, " Volume Trend by ",input$comp_choices,"\n"), 
           caption = paste0("\nIncludes ",apptType),
           y = NULL, x = NULL, fill = NULL)+
      theme_new_line()+
      theme(axis.text.x = element_text(angle = 40, hjust=1))
    
  }, height = function(x) input$plotHeight)
  
  
  # Population - Zip Code Analysis Tab --------------------------------------------------------------------------------------------
  ## Bubble Map by Zip Code
  output$zipCode_map <- renderLeaflet({
    
    newdata <- dataArrivedPop() %>% group_by(latitude, longitude) %>% dplyr::summarise(total = round(n(),0))
    # newdata <- population.data_filtered %>% group_by(latitude, longitude) %>% dplyr::summarise(total = round(n(),0))
    
    # Create a color palette with handmade bins.
    mybins <- round(seq(min(newdata$total), max(newdata$total), length.out=5),0)
    mypalette <- colorBin(palette=MountSinai_palettes$pinkBlue, domain=quakes$mag, na.color="transparent", bins=mybins)
    
    all <- sum(newdata$total)
    
    # Prepare the text for the tooltip:
    mytext <- paste(
      "Total Visits: ", newdata$total, "<br/>",
      "% of Total: ", paste0(round(newdata$total/all,2)*100, "%")) %>%
      lapply(htmltools::HTML)
    
    # Set icons for each MSHS hospital
    icons <- awesomeIcons(
      icon = 'hospital-o',
      lib = 'fa',
      iconColor = "white",
      markerColor = "lightgray")
    
    # Visit volume map 
    leaflet(newdata) %>% 
      addTiles()  %>% 
      setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(~longitude, ~latitude, 
                       fillColor = ~mypalette(total), fillOpacity = 0.7, color="white", radius= ~total^(1/2), stroke=FALSE,
                       label = mytext,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "18px", direction = "auto")
      ) %>%
      addLegend(pal=mypalette, values=~total, opacity=0.9, title = "Visit Demand", positionyo = "bottomright") %>%
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
                       background = "#d80b8c", color = "white", font_size = 18, align = "center") %>%
      add_indent(c(row_start+1, row_start+2, row_start+3), level_of_indent = 2) %>%
      row_spec(1:nrow(final_tb), background = "	#e6e6e6", color = "black") %>%
      row_spec(c(row_start+1, row_start+2, row_start+3), background = "#f2f2f2") %>%
      row_spec(nrow(final_tb), background = "#fcc9e9", color = "black", bold = T) 
   
  }
  
  
  
  
} # Close Server

shinyApp(ui, server)


n <- 3
hline <- seq(0.5, 0.5*n, by= 0.5)
class(as.vector(hline))


