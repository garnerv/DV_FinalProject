
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from COLLEGESTATS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
#df

summary(df)
#head(df)

#df2 <- df %>% mutate(AVG_DIFFERENCE = AVERAGETOTALPAYMENTS - AVERAGEMEDICAREPAYMENTS, AVG_DIFF = cume_dist(AVG_DIFFERENCE))

ggplot(df, aes(x=PUBLICPRIVATE, y=TUITIONFEES1314, color=PUBLICPRIVATE)) + geom_boxplot()

ggplot(df, aes(x=TUITIONFEES1314)) +
  geom_histogram(binwidth=1000, alpha=.5, position="identity")

ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_discrete() +
  #facet_grid(.~PUBLICPRIVATE, labeller=label_both, drop=TRUE) +
  labs(title='Box PLot') +
  labs(x="TUITIONFEES1314", y=paste("PUBLICPRIVATE")) +
  layer(data=df, 
        mapping=aes(x=as.numeric(as.character(TUITIONFEES1314)), y=as.character(AVGAMTFIRSTTIMEUGPELL)), 
        stat="identity", 
        stat_params=list(), 
        geom_boxplot(),
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
)
