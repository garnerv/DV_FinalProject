#original DF
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from COLLEGESTATS where tuitionfees1314 is not null and graduationrate is not null and publicprivate is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
#ranking DF
df2 <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from COLLEGERANKINGS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

#summary(df)
#head(df)

#right join to retain only the top 109 colleges w/ average rankings
df3 <- dplyr::right_join(df, df2, by="NAME")

p1 <- ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  #facet_grid(.~PUBLICPRIVATE, labeller=label_both, drop=TRUE) +
  labs(title='Scatter Plot') +
  labs(x="Average 6 Year Ranking", y=paste("Graduation Rate")) +
  layer(data=df3, 
        mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(GRADUATIONRATE)), color=PUBLICPRIVATE), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(),
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
) + geom_smooth(data=df3, mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(GRADUATIONRATE)), color=PUBLICPRIVATE), method = "lm", se=FALSE)

p2 <- ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  #facet_grid(.~PUBLICPRIVATE, labeller=label_both, drop=TRUE) +
  labs(title='Scatter Plot') +
  labs(x="Average 6 Year Ranking", y=paste("Enrollment Total")) +
  layer(data=df3, 
        mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(ENROLLMENTTOTAL)), color=PUBLICPRIVATE), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(),
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
  ) + geom_smooth(data=df3, mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(ENROLLMENTTOTAL)), color=PUBLICPRIVATE), method = "lm", se=FALSE)

p3 <- ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  #facet_grid(.~PUBLICPRIVATE, labeller=label_both, drop=TRUE) +
  labs(title='Scatter Plot') +
  labs(x="Average 6 Year Ranking", y=paste("Percent of Students Receiving Any Financial Aid")) +
  layer(data=df3, 
        mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(PFIRSTTIMEUGANYAID)), color=PUBLICPRIVATE), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(),
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
  ) + geom_smooth(data=df3, mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(PFIRSTTIMEUGANYAID)), color=PUBLICPRIVATE), method = "lm", se=FALSE)

p4 <- ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  #facet_grid(.~PUBLICPRIVATE, labeller=label_both, drop=TRUE) +
  labs(title='Scatter Plot') +
  labs(x="Average 6 Year Ranking", y=paste("Tuition and Fees for 2013-2014 School Year")) +
  layer(data=df3, 
        mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(TUITIONFEES1314)), color=PUBLICPRIVATE), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(),
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
  ) + geom_smooth(data=df3, mapping=aes(x=as.numeric(as.character(RANK)), y=as.numeric(as.character(TUITIONFEES1314)), color=PUBLICPRIVATE), method = "lm", se=FALSE)


multiplot(p1, p2, p3, p4, cols=2)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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