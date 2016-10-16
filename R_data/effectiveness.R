library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)

effective.multiplot <- function(dependent, treatments) 
{
  if(!is.vector(treatments)) stop("treatments must be a vector")
  if(length(treatments) != 4) stop("treatments must contain 4 variables")
  print("effective.multiplot: all conditions were met")
}

effective.plot <- function(df, x, y, ZEROS=FALSE) {
  mydf <- data.frame( table(df[,x], df[,y]) )
  if(!ZEROS) { print("ZEROS is false")
    mydf <- mydf[ which(mydf$Freq>0), ] }
  mydf$Radius <- sqrt(mydf$Freq / pi)
  
  myplot <- ggplot(mydf, aes(x=Var1, y=Var2, fill=Freq)) +
            geom_point(aes(size=Radius*6.5), position=position_dodge(width=.02), shape=21, fill="#333333", color="#999999") +
            geom_point(aes(size=Radius*6.3), shape=21) +
            geom_text(aes(label=Freq), size=4) +
            scale_fill_continuous(low="cadetblue1", high="deepskyblue4", guide=FALSE) +
            scale_x_discrete(drop=check_drop(mydf)[1], limits=c("1-5 years", "6-10 years", "11-15 years", "16-20 years", "21-25 years", "26+ years")) +
            scale_y_discrete(drop=check_drop(mydf)[2], limits=c("Not effective at all", "Slightly effective", "Moderately effective", "Very effective", "Extremely Effective")) +
            scale_size_identity() +
            theme(text=element_text(family = "Times New Roman"),
                  plot.title = element_text(face = "bold"),
                  panel.grid.major=element_line(linetype=2, color="black"), 
                  axis.title.y=element_text(face = "bold"),
                  axis.text.x=element_text(angle=90, hjust=1, vjust=0))
  return(myplot)
}

#subset(survey, Q19_1 == "Very effective", select = c(Gender, Q19_1))
#exp, effect, freq


xp_range <- c("1-5 years", "6-10 years", "11-15 years", "16-20 years", "21-25 years", "26+ years")
eff_range <- c("Not effective at all", "Slightly effective", "Moderately effective", "Very effective", "Extremely Effective")

check_drop <- function(df) {
  result <- c(TRUE, TRUE)
  for(xp in xp_range){
    if(!any(df$Var1 == xp)){
      result <- c(FALSE, result[2])
    }
  }
  for(eff in eff_range){
    if(!any(df$Var2 == eff)){
      result <- c(result[1], FALSE)
    }
  }
  return(result)
}

plot_ss  <- effective.plot(survey, "Experience", "Q19_1") + 
  ggtitle("Simple") + 
  labs(y="Small") +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())

plot_sl  <- plot_effective(survey$Q25, survey$Q19_2) + 
  labs(y="Large") +
  theme(axis.title.x=element_blank())

plot_cs <- plot_effective(survey$Q25, survey$Q19_3) + 
  ggtitle("Complex") + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

plot_cl <- plot_effective(survey$Q25, survey$Q19_4) +
  theme(axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank())

multiplot_effective <- grid.arrange(plot_ss, plot_cs, 
                                    plot_sl, plot_cl, 
                                    ncol=2, nrow=2, heights=c(2,2.3), widths=c(2.7,2))
  
# aggregate the frequency of each combination of x and y values into a new data frame
agg_effective <- function(x, y) {
  df <- data.frame(table(x, y))
  names(df) <- c("exp", "effect", "freq")         # relabel the columns
  df <- df[!(is.na(df$exp) | df$exp==""), ]       # remove rows with empty fields
  df <- df[!(is.na(df$effect) | df$effect==""), ] # remove rows with empty fields
  df$radius <- sqrt(df$freq / pi)                 # calculate radius based upon frequency
  return(df)
}

# aggregate and plot combinations as a bubble chart
plot_effective <- function(x, y) {
  df <- agg_effective(x, y)
  plot <- ggplot(df[which(df$freq>0),], aes(x=exp, y=effect, fill=freq)) +
    geom_point(aes(size=radius*7.3), position=position_dodge(width=.02), shape=21,fill="#333333", color="#999999") +
    geom_point(aes(size=radius*7.0), shape=21) +
    geom_text(aes(label=freq), size=4) +
    scale_fill_continuous(low="cadetblue1", high="deepskyblue4", guide=FALSE) +
    scale_x_discrete(drop=check_drop(df)[1], limits=c("1-5 years", "6-10 years", "11-15 years", "16-20 years", "21-25 years", "26+ years")) +
    scale_y_discrete(drop=check_drop(df)[2], limits=c("Not effective at all", "Slightly effective", "Moderately effective", "Very effective", "Extremely Effective")) +
    scale_size_identity() +
    theme(text=element_text(family = "Times New Roman"),
          plot.title = element_text(face = "bold"),
          panel.grid.major=element_line(linetype=2, color="black"), 
          axis.title.y=element_text(face = "bold"),
          axis.text.x=element_text(angle=90, hjust=1, vjust=0))
  return(plot)
}



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