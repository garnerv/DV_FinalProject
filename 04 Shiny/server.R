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
  
  dfct <- eventReactive(c(input$clicks1), 
                        {dfct <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from COLLEGESTATS where TUITIONFEES1314 is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
                        })
  
  KPI_Low_Max_value <- reactive({input$KPI1})     
  KPI_Medium_Max_value <- reactive({input$KPI2})
  
  output$distPlot1 <- renderPlot(height=400, width=600, {
    
    crosstab <- dfct() %>% mutate(kpi = ifelse(as.numeric(GRADUATIONRATE) <= KPI_Low_Max_value(), '03 Low', ifelse(as.numeric(GRADUATIONRATE) <= KPI_Medium_Max_value(), '02 Medium', '01 High'))) %>% group_by(PUBLICPRIVATE, kpi) %>% summarize(avg_tuition = mean(as.numeric(TUITIONFEES1314)))
    
    
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='College Statitics Graduation Rank Percentile \n By Average Tuition') +
      labs(x=paste("Public/Private"), y=paste("Graduation Rate Bucket")) +
      layer(data=crosstab, 
            mapping=aes(x=PUBLICPRIVATE, y=kpi, label=""), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=crosstab, 
            mapping=aes(x=PUBLICPRIVATE, y=kpi, label=""), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=crosstab, 
            mapping=aes(x=PUBLICPRIVATE, y=kpi, label=round(avg_tuition, 2)), 
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
  
  
  # Begin code for Third Tab, Scatter Plot:
  dfS <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from COLLEGESTATS where tuitionfees1314 is not null and graduationrate is not null and publicprivate is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
  dfS1 <- dfS %>% mutate(pell_percent = cume_dist(AVGAMTFIRSTTIMEUGPELL))
  levels <- c(0, .25, .5, .75, 1)
  labels <- c("4th Q Highest Pell Grant", "3rd Q Highest Pell Grant", "2nd Q Highest Pell Grant", "1st Q Highest Pell Grant")
  dfS1 <- dfS1 %>% filter(AVGAMTFIRSTTIMEUGPELL != "null") %>% mutate(x = cut(pell_percent, levels, labels = labels))
  dfS1 <- dfS1 %>% group_by(x, PUBLICPRIVATE) %>% summarise(mean_fac = mean(as.numeric(STUDENTFACULTYRATIO)), n=n(), mean_tuition = mean(as.numeric(TUITIONFEES1314)))
  
  dfS2 <- eventReactive(input$clicks3, {dfS1})
  
  output$distPlot3 <- renderPlot(height=600, width=600, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      facet_wrap(~PUBLICPRIVATE) +
      labs(title='Mean Average Faculty\n vs. Mean Tuition and Fees (Year 13-14)\n, Grouped by Quartile Ranking\n of Highest Average Pell Grants Students Receive') +
      labs(x="Average Student:Faculty Ratio", y=paste("Tuition and Fees for 2013-2014 Academic School Year ($)")) +
      layer(data=dfS2(), 
            mapping=aes(x=mean_fac, y=mean_tuition, color=x), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            #position=position_identity()
            position=position_jitter(width=0.3, height=0),
            geom_point(size=5)
      )
    plot3 })
  
  
  
  # Begin code for fourth Tab, Scatter Plot:
  #original DF
  dfJ <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from COLLEGESTATS where tuitionfees1314 is not null and graduationrate is not null and publicprivate is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  #ranking DF
  dfJ2 <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from COLLEGERANKINGS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
  dfJ3 <- dplyr::right_join(dfJ, dfJ2, by="NAME")
  
  dfJ4 <- eventReactive(input$clicks4, {dfJ3})
  
  output$distPlot4 <- renderPlot(height=600, width=600, {
    p1 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      #facet_grid(.~PUBLICPRIVATE, labeller=label_both, drop=TRUE) +
      labs(title='Scatter Plot') +
      labs(x="Average 6 Year Ranking", y=paste("Graduation Rate")) +
      layer(data=dfJ4(), 
            mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(GRADUATIONRATE)), color=PUBLICPRIVATE), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(),
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      ) + geom_smooth(data=dfJ4(), mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(GRADUATIONRATE)), color=PUBLICPRIVATE), method = "lm", se=FALSE)
    
    p2 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      #facet_grid(.~PUBLICPRIVATE, labeller=label_both, drop=TRUE) +
      labs(title='Scatter Plot') +
      labs(x="Average 6 Year Ranking", y=paste("Enrollment Total")) +
      layer(data=dfJ4(), 
            mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(ENROLLMENTTOTAL)), color=PUBLICPRIVATE), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(),
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      ) + geom_smooth(data=dfJ4(), mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(ENROLLMENTTOTAL)), color=PUBLICPRIVATE), method = "lm", se=FALSE)
    
    p3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      #facet_grid(.~PUBLICPRIVATE, labeller=label_both, drop=TRUE) +
      labs(title='Scatter Plot') +
      labs(x="Average 6 Year Ranking", y=paste("Percent of Students Receiving Any Financial Aid")) +
      layer(data=dfJ4(), 
            mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(PFIRSTTIMEUGANYAID)), color=PUBLICPRIVATE), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(),
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      ) + geom_smooth(data=dfJ4(), mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(PFIRSTTIMEUGANYAID)), color=PUBLICPRIVATE), method = "lm", se=FALSE)
    
    p4 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      #facet_grid(.~PUBLICPRIVATE, labeller=label_both, drop=TRUE) +
      labs(title='Scatter Plot') +
      labs(x="Average 6 Year Ranking", y=paste("Tuition and Fees for 2013-2014 School Year")) +
      layer(data=dfJ4(), 
            mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(TUITIONFEES1314)), color=PUBLICPRIVATE), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(),
            position=position_jitter(width=0.3, height=0)
      ) + geom_smooth(data=dfJ4(), mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(TUITIONFEES1314)), color=PUBLICPRIVATE), method = "lm", se=FALSE)
    
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      library(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      # If layout is NULL, then use 'cols' to determine layout
      if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
        print(plots[[1]])
        
      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
      }
    }
    
    multi <- multiplot(p1, p2, p3, p4, cols=2)
    
    multi })
  
})