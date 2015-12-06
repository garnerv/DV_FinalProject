# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)

shinyServer(function(input, output) {
  
  dfct <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from COLLEGESTATS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
  
  KPI_Low_Max_value <- reactive({input$KPI1})     
  KPI_Medium_Max_value <- reactive({input$KPI2})
  
  
  output$distPlot1 <- renderPlot(height=600, width=800, {
    
    dfct2 <- dfct %>% mutate(kpi = ifelse(GRADUATIONRATE <= KPI_Low_Max_value(), '03 Low', ifelse(GRADUATIONRATE <= KPI_Medium_Max_value(), '02 Medium', '01 High'))) #%>% View()
    
    crosstab <- eventReactive(c(input$clicks1), 
                {crosstab <- dfct2 %>% group_by(PUBLICPRIVATE, kpi) %>% summarize(avg_tuition = mean(TUITIONFEES1314)) 
                })
    
    output$distPlot1 <- renderPlot(height=600, width=800, {
      plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='College Statitics Graduation Rank Percentile \n By Tuition') +
      labs(x=paste("Public/Private"), y=paste("KPI")) +
      layer(data=crosstab(), 
            mapping=aes(x=PUBLICPRIVATE, y=kpi, label=""), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=crosstab(), 
            mapping=aes(x=PUBLICPRIVATE, y=kpi, label=""), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=crosstab(), 
            mapping=aes(x=PUBLICPRIVATE, y=kpi, label=avg_tuition), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      )
    plot
  }) 
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  # Begin code for Second Tab, Bar Chart:
  
  df <- eventReactive(input$clicks2, { 
    df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from COLLEGESTATS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), )) })
  
  df2 <- df() %>% mutate(avg_pell = mean(AVGAMTFIRSTTIMEUGPELL), avg_state = mean(AVGAMTFIRSTTIMEUGSTATEGRANT), avg_fed = mean(AVGAMTFIRSTTIMEUGOTHFED), avg_ins = mean(AVGAMTFIRSTTIMEUGINSGRANT))
  
  output$distPlot2 <- renderPlot(height=600, width=800, {
    plot1 <- ggplot() + 
      #coord_cartesian() + 
      scale_x_discrete() +
      #scale_x_continuous() +
      scale_y_continuous() +
      facet_wrap(~DRGDEFINITION, ncol=1) +
      labs(title='Public private cost Comparison ') +
      labs(x=paste("I'm not sure"), y=paste("Measure Names")) +
      layer(data=df4(), 
            mapping=aes(x=paste("AVGAMTFIRSTTIMEUGPELL"), y=avg_pell), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="red"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=df4(), 
            mapping=aes(x=paste("AVGAMTFIRSTTIMEUGSTATEGRANT"), y=avg_state), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="blue"), 
            position=position_identity()
      ) +
      layer(data=df4(), 
            mapping=aes(x=paste("AVGAMTFIRSTTIMEUGOTHFED"), y=avg_fed), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="green"), 
            position=position_identity()
      ) +
      layer(data=df4(), 
            mapping=aes(x=paste("AVGAMTFIRSTTIMEUGINSGRANT"), y=avg_ins), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="yellow"), 
            position=position_identity()
      ) +
      layer(data=df4(), 
            mapping=aes(x=paste("AVGAMTFIRSTTIMEUGINSGRANT"), y=avg_ins, label=round(avg_ins)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-0.5), 
            position=position_identity()
      ) 
    plot1
  })
  
  # Begin code for Third Tab, Scatter Plot:
  dfS <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from MEDICALDATA"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
  dfS1 <- dfS %>% mutate(AVG_DIFFERENCE = AVERAGETOTALPAYMENTS - AVERAGEMEDICAREPAYMENTS, AVG_DIFF = cume_dist(AVG_DIFFERENCE))
  
  dfS2 <- eventReactive(input$clicks3, {dfS1})
  
  output$distPlot3 <- renderPlot(height=600, width=800, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='Medical Data \n Percentiles vs Total Discharges') +
      labs(x="Percentile of Average Difference", y=paste("Total Discharges")) +
      layer(data=dfS2(), 
            mapping=aes(x=as.numeric(as.character(AVG_DIFF)), y=as.numeric(as.character(TOTALDISCHARGES))), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      )
    plot3 })
  
}) 
}) 
