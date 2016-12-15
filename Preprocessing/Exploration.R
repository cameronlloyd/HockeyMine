library("ggplot2")

config <- config::get()

# Import all teams from csv file
importTeams <- function(){
  tryCatch({
    temp = list.files(path="./WebScrape/TeamSchedules/CSV/.",pattern="*.csv")
    for (i in 1:length(temp)) {
      assign(substr(temp[i],1,nchar(temp[i])-4), 
             read.csv(paste("./WebScrape/TeamSchedules/CSV/",temp[i],sep="")),
             envir=.GlobalEnv)
    }
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
  })
}

# Get summary statistics for the given dataframe
getSummaryStats <- function(df){
  # Create summary matrix
  tmp = do.call(data.frame, 
                 list(min = round(apply(df, 2, min),digits=2),
                      firstQ = round(apply(df, 2, quantile,probs=c(0.25),na.rm=T),digits=2),
                      median = round(apply(df, 2, median),digits=2),
                      mean = round(apply(df, 2, mean),digits=2),
                      thirdQ = round(apply(df, 2, quantile,probs=c(0.75),na.rm=T),digits=2),
                      max = round(apply(df, 2, max),digits=2),
                      IQR = round(apply(df, 2, IQR),digits=2)))
  
  # Get transpose of matrix
  #tmp = data.frame(t(tmp))
  
  return(tmp)
}





# Calculate the differences between each feature.
#   Will calculate by: HomeTeamFeature - AwayTeamFeature
calcDiff <- function(df){
  # Column names we are concerned about
  n = nrow(df)
  diffTable = data.frame(matrix(ncol=33,nrow=n))
  cols = c("GP","GF","GA","Wins","Losses","OL","Streak","ROW","L10Wins","L10Losses","L10OL","PPM","PPG","PPO",
                         "SHG","PKM","PKG","PKO","ShotsFor","ShotsAgainst","SV","AvCFClose","AvCF5v5","AvCFEven",
                         "AvGoalieCnt","AvShiftCnt","ATOI","AvAge","PDO","WinP","HomeP","AwayP","Result")
  colnames(diffTable) = cols
  
  # Calculate the difference between each teams attribute
  #   Result will be appended to each row
  for (i in 1:n){
    for (j in 1:(length(cols)-1)){
      # Get column to calculate difference of
      colName = cols[j]
      HomeColName = paste("H",colName,sep=".")
      AwayColName = paste("A",colName,sep=".")
         
      # Calculate difference
      diff = round(df[i,HomeColName] - df[i,AwayColName],digits=2)
      
      # Add difference to table
      diffTable[i,j] = diff
    }
    
    # Add result column back to table
    diffTable[i,'Result'] = df[i,'Result']
  }
  
  return(diffTable)
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


doBoxplots <- function(df){

  p1 <- ggplot(df, aes(x="ROW",y=ROW)) + geom_boxplot(fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())
  
  p2 <- ggplot(df, aes(x="L10Losses",y=L10Losses)) + geom_boxplot(fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())

  p3 <- ggplot(df, aes(x="SHG",y=SHG)) + geom_boxplot(fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())

  p4 <- ggplot(df, aes(x="PKG",y=PKG)) + geom_boxplot(fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())
  
  p5 <- ggplot(df, aes(x="ShotsAgainst",y=ShotsAgainst)) + geom_boxplot(fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())
  
  p6 <- ggplot(df, aes(x="ShotsFor",y=ShotsFor)) + geom_boxplot(fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())

  p7 <- ggplot(df, aes(x="SV",y=SV)) + geom_boxplot(fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())

  p8 <- ggplot(df, aes(x="AvGoalieCnt",y=AvGoalieCnt)) + geom_boxplot(fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())
  
  p9 <- ggplot(df, aes(x="HomeP",y=HomeP)) + geom_boxplot(fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())

  
  multiplot(p1,p2,p3,cols=3)
  multiplot(p4,p5,p6,cols=3)
  multiplot(p7,p8,p9,cols=3)
  
}

doDistributions <- function(df){
  p1 <- ggplot(df, aes(x=Result,y=ROW)) + geom_histogram(fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())
  
  p2 <- ggplot(df, aes(x=L10Losses,y=L10Losses)) + geom_point(shape=1,fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())

  p3 <- ggplot(df, aes(x=SHG,y=SHG)) + geom_point(shape=1,fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())

  p4 <- ggplot(df, aes(x=PKG,y=PKG)) + geom_point(shape=1,fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())
  
  p5 <- ggplot(df, aes(x=ShotsAgainst,y=ShotsAgainst)) + geom_point(shape=1,fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())
  
  p6 <- ggplot(df, aes(x=ShotsFor,y=ShotsFor)) + geom_point(shape=1,fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())

  p7 <- ggplot(df, aes(x=SV,y=SV)) + geom_point(shape=1,fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())

  p8 <- ggplot(df, aes(x=AvGoalieCnt,y=AvGoalieCnt)) + geom_point(shape=1,fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())
  
  p9 <- ggplot(df, aes(x=HomeP,y=HomeP)) + geom_point(shape=1,fill="#FF9999") + 
    theme(axis.title.x=element_blank(), 
          axis.title.y = element_blank())
  
  multiplot(p1,p2,p3,cols=3)
  multiplot(p4,p5,p6,cols=3)
  multiplot(p7,p8,p9,cols=3)
}


# Save dataframe to file
saveDF <- function(df, name){
  # Save matchup table
  saveRDS(df,paste(name," Data.rda",sep=""))
  write.csv(df,paste(name,".csv",sep=""))
}



part1 = FALSE

if(isTRUE(part1)){
  ### Exploration Part 1
  
  # Import matchups dataset
  matchups <<- read.csv("./Data/FilteredMatchups.csv")
  importTeams()
  
  # Calculate differences between each attribute
  #   Save to file
  diffDF = calcDiff(matchups)
  saveDF(diffDF, "./Preprocessing/Data/DifferencesMatchup")
  
  # Retrieve summary stats
  #   Save to file
  diffSummaries = getSummaryStats(diffDF)
  saveDF(diffSummaries,"./Preprocessing/Data/DiffSummaries")
  filtSummaries = getSummaryStats(matchups)
  saveDF(filtSummaries,"./Preprocessing/Data/FiltSummaries")
  
  # Plot boxplots of features
  doBoxplots(diffDF)
  
} else {
  ### Exploration Part 2
  
  # Import second filtered matchup dataframe
  matchups <<- read.csv("./Data/FilteredMatchups2.csv")
  
  # Calculate differences between each attribute
  #   Save to file
  diffDF2 = calcDiff(matchups)
  saveDF(diffDF2, "./Preprocessing/Data/DifferencesMatchup2")
  
  # Plot boxplots of features
  doBoxplots(diffDF2)
}

