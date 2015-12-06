require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)

# Begin code for Second Tab, Bar Chart:


  df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from COLLEGESTATS where AVGAMTFIRSTTIMEUGINSGRANT is not null and AVGAMTFIRSTTIMEUGPELL is not null and AVGAMTFIRSTTIMEUGOTHFED is not null and AVGAMTFIRSTTIMEUGSTATEGRANT is not null and TUITIONFEES1314 is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))

  # df <- diamonds %>% group_by(color, clarity) %>% summarize(AVG_PRICE = mean(price)) %>% rename(COLOR=color, CLARITY=clarity)
  # df1 <- df %>% ungroup %>% group_by(CLARITY) %>% summarize(WINDOW_AVG_PRICE=mean(AVG_PRICE))
  # df <- inner_join(df, df1, by="CLARITY")
  
  df2 <- df %>% group_by(PUBLICPRIVATE, AVGAMTFIRSTTIMEUGINSGRANT, AVGAMTFIRSTTIMEUGPELL, AVGAMTFIRSTTIMEUGOTHFED, AVGAMTFIRSTTIMEUGSTATEGRANT, TUITIONFEES1314) %>% summarize(avg_pell = mean(AVGAMTFIRSTTIMEUGPELL), avg_state = mean(AVGAMTFIRSTTIMEUGSTATEGRANT), avg_fed = mean(AVGAMTFIRSTTIMEUGOTHFED), avg_ins = mean(AVGAMTFIRSTTIMEUGINSGRANT), avg_tuition = mean(TUITIONFEES1314) )
  
  df3 <- df2 %>% ungroup %>% group_by(PUBLICPRIVATE) %>% summarise(average_pell = mean(avg_pell), average_state = mean(avg_state), average_fed = mean(avg_fed), average_ins = mean(avg_ins), average_tuition = mean(avg_tuition)) 
  
  df4 <- inner_join(df2, df3, by="PUBLICPRIVATE")
  
  #df4 <- df %>% mutate(avg_pell = mean(AVGAMTFIRSTTIMEUGPELL), avg_state = mean(AVGAMTFIRSTTIMEUGSTATEGRANT), avg_fed = mean(AVGAMTFIRSTTIMEUGOTHFED), avg_ins = mean(AVGAMTFIRSTTIMEUGINSGRANT)) %>% View()

ggplot() + 
    #coord_cartesian() + 
    scale_x_discrete() +
    #scale_x_continuous() +
    scale_y_continuous() +
    facet_wrap(~PUBLICPRIVATE, ncol=1) +
    labs(title='Public vs Private \n Cost Comparison ') +
    labs(x=paste("Money given from:"), y=paste("Dollar Value")) +
    layer(data=df4, 
          mapping=aes(x=paste("AVG PELL GRANT"), y=average_pell, label=round(avg_pell)), 
          stat="identity", 
          stat_params=list(), 
          geom="bar",
          geom_params=list(colour="red"), 
          position=position_identity()
    ) + coord_flip() +
    layer(data=df4, 
          mapping=aes(x=paste("AVG STATE GRANT"), y=average_state, label=round(avg_state)), 
          stat="identity", 
          stat_params=list(), 
          geom="bar",
          geom_params=list(colour="blue"), 
          position=position_identity()
    ) +
    layer(data=df4, 
          mapping=aes(x=paste("AVG FEDERAL GRANT"), y=average_fed, label=round(avg_fed)), 
          stat="identity", 
          stat_params=list(), 
          geom="bar",
          geom_params=list(colour="green"), 
          position=position_identity()
    ) +
    layer(data=df4, 
          mapping=aes(x=paste("AVG INSTITUTIONAL GRANT"), y=average_ins, label=round(avg_ins)), 
          stat="identity", 
          stat_params=list(), 
          geom="bar",
          geom_params=list(colour="yellow"), 
          position=position_identity()
    ) +
  layer(data=df4, 
            mapping=aes(yintercept = average_tuition), 
            geom="hline",
            geom_params=list(colour="red")
    )
