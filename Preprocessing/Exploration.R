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

getSummaryStats <- function(df){
  # Create summary matrix
  tmp = do.call(data.frame, 
                 list(min = round(apply(df, 2, min),digits=2),
                      firstQ = round(apply(df, 2, quantile,probs=c(0.25),na.rm=T),digits=2),
                      median = round(apply(df, 2, median),digits=2),
                      mean = round(apply(df, 2, mean),digits=2),
                      thirdQ = round(apply(df, 2, quantile,probs=c(0.75),na.rm=T),digits=2),
                      max = round(apply(df, 2, max),digits=2)))
  
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



saveDF <- function(df, name){
  # Save matchup table
  saveRDS(df,paste(name," Data.rda",sep=""))
  write.csv(df,paste(name,".csv",sep=""))
}


# Import matchups dataset
matchups <<- read.csv("FilteredMatchups.csv")
#importTeams()

# Calculate differences between each attribute
#   Save to file
#diffDF = calcDiff(matchups)
#saveDF(diffDF, "./Preprocessing/Data/DifferencesMatchup")

# Retrieve summary stats of differences
#   Save to file
summaries = getSummaryStats(diffDF)
saveDF(summaries,"./Preprocessing/Data/DiffSummaries")



