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
  
  # observeEvent(c(input$selectedCampus,input$selectedSpecialty, input$selectedDepartment),{
  #   updatePickerInput(session,
  #                     inputId = "selectedVisitType",
  #                     choices = NULL
  #   )},
  #   ignoreInit = TRUE)

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
                   input$selectedVisitType, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })

  # Bumped data -----------------------------------------------------------------------------------------------------------------------
  dataBumped<- reactive({
    groupByFilters(bumped.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider, input$selectedrefProvider,
                   input$selectedVisitType, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
# Volume Trend Tab ------------------------------------------------------------------------------------------------------    
  output$trend_totalvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data %>% filter(SITE %in% c("DBC","RTC","MSW"))

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
    
  })
  
  
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
    
  })
  
  
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
    
  })
  
  
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
    
  })
  
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
    
    ggplot(total_visits_break, aes(x=Appt.MonthYear, y=total, group=AssociationListA, fill=AssociationListA))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_MountSinai('dark')+
      scale_y_continuous(limits=c(0,(max(max$max))*1.2))+
      labs(title = paste0(site," ","All Visit Volume Composition\n"), 
           y = "Patient Volume\n", x = NULL, fill = NULL)+
      theme_new_line()+
      theme(axis.title.y = element_text(size = 12, angle = 90))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
  })
  
  
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
    
    ggplot(total_visits_break, aes(x=Appt.MonthYear, y=total, group=AssociationListB, fill=AssociationListB))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_MountSinai('dark', reverse = TRUE)+
      scale_y_continuous(limits=c(0,(max(max$max))*1.2))+
      labs(title = paste0(site," ","Exam Visit Volume Composition\n"), 
           y = "Patient Volume\n", x = NULL, fill = NULL)+
      theme_new_line()+
      theme(axis.title.y = element_text(size = 12, angle = 90))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
  })
  
  
  output$break_treatmentvisitsgraph <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    total_visits_break <- data %>% filter(AssociationListA == "Treatment") %>%
      group_by(Appt.MonthYear, AssociationListT) %>% summarise(total = n())
    
    max <- total_visits_break %>% group_by(Appt.MonthYear) %>% summarise(max = sum(total))
    
    if(length(unique(data$SITE)) == 1){
      site <- unique(data$SITE)
    } else{
      site <- paste(sort(unique(data$SITE)),sep="", collapse=", ")
    }
    
    ggplot(total_visits_break, aes(x=Appt.MonthYear, y=total, group=AssociationListT, fill=AssociationListT))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_MountSinai('dark', reverse = TRUE)+
      scale_y_continuous(limits=c(0,(max(max$max))*1.2))+
      labs(title = paste0(site," ","Treatment Visit Volume Composition\n"), 
           y = "Patient Volume\n", x = NULL, fill = NULL)+
      theme_new_line()+
      theme(axis.title.y = element_text(size = 12, angle = 90))+
      geom_text(aes(label=total), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
  })
  
  # output$break_visitstable <- function(){
  #   
  #   data <- dataArrived()
  #   data <- arrived.data
  #   
  #   visits_tb <- data %>% group_by(Appt.Year, Appt.Month) %>% summarise(total = n()) %>%
  #     spread(Appt.Year, total)
  #   
  #   
  #   months <- append(unique(visits_tb$Appt.Month),"Total")
  #   
  #   total_val <- colSums(visits_tb[,-1])
  #   visits_tb <- rbind(visits_tb, total_val)
  #   
  #   visits_tb$Appt.Month <- months
  #   
  #   visits_tb <- variance 
  #   
  #   visits_tb$variance1 <- visits_tb[,3]-visits_tb[,2]
  #   visits_tb$variance_perc <- visits_tb
  #   
  # }
  

# Volume Comparison Tab ------------------------------------------------------------------------------------------------------       

  
}

shinyApp(ui, server)
