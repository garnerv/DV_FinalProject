require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)

dfct <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from COLLEGESTATS where TUITIONFEES1314 is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))

KPI_Low_Max_value = 30 #reactive({input$KPI1})     
KPI_Medium_Max_value = 70 #reactive({input$KPI2})


 #output$distPlot1 <- renderPlot(height=600, width=800, {
  
 df <- dfct %>%  mutate(kpi = ifelse(as.numeric(GRADUATIONRATE) <= KPI_Low_Max_value, '03 Low', ifelse(as.numeric(GRADUATIONRATE) <= KPI_Medium_Max_value, '02 Medium', '01 High'))) %>% group_by(PUBLICPRIVATE, kpi) %>% summarize(avg_tuition = mean(as.numeric(TUITIONFEES1314))) #%>% View() 

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  labs(title='College Statitics Graduation Rank Percentile \n By Average Tuition') +
  labs(x=paste("Public/Private"), y=paste("Graduation Rate Bucket")) +
  layer(data=df, 
        mapping=aes(x=PUBLICPRIVATE, y=kpi, label=""), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black"), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=PUBLICPRIVATE, y=kpi, label=""), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black"), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=PUBLICPRIVATE, y=kpi, label=round(avg_tuition)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black"), 
        position=position_identity()
  )