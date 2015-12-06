
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from COLLEGESTATS where tuitionfees1314 is not null and graduationrate is not null and publicprivate is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
#df

summary(df)
#head(df)

df <- df %>% mutate(pell_percent = cume_dist(AVGAMTFIRSTTIMEUGPELL))
levels <- c(0, .25, .5, .75, 1)
labels <- c("4th Q Highest Pell Grant", "3rd Q Highest Pell Grant", "2nd Q Highest Pell Grant", "1st Q Highest Pell Grant")
df <- df %>% filter(AVGAMTFIRSTTIMEUGPELL != "null") %>% mutate(x = cut(pell_percent, levels, labels = labels))


df %>% group_by(x, PUBLICPRIVATE) %>% summarise(mean_fac = mean(as.numeric(STUDENTFACULTYRATIO)), n=n(), mean_tuition = mean(as.numeric(TUITIONFEES1314))) %>% ggplot(aes(x=mean_fac, y=mean_tuition, color=x)) + geom_point(size=5) + facet_wrap(~PUBLICPRIVATE) + labs(title='Mean Average Faculty\n vs. Mean Tuition and Fees (Year 13-14)\n, Grouped by Quartile Ranking\n of Highest Average Pell Grants Students Receive') + labs(x="Average Student:Faculty Ratio", y=paste("Tuition and Fees for 2013-2014 Academic School Year ($)"))



ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  facet_grid(.~PUBLICPRIVATE, labeller=label_both, drop=TRUE) +
  labs(title='Scatter Plot') +
  labs(x="Graduation rate", y=paste("AVG First Time Pell")) +
  layer(data=df, 
        mapping=aes(x=as.numeric(as.character(GRADUATIONRATE)), y=as.numeric(as.character(AVGAMTFIRSTTIMEUGPELL))), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(),
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
) + geom_smooth(data=df, mapping=aes(x=as.numeric(as.character(GRADUATIONRATE)), y=as.numeric(as.character(AVGAMTFIRSTTIMEUGPELL))), method = "lm", se=FALSE)
