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


filterDataset <- function(df){
  
  n = nrow(df)
  
  # Keep track of rows to remove
  remove = c()
  
  # Iterate each row and find rows to remove 
  for (i in (1:n)){
    HomeGP = df[i,'H.GP']
    AwayGP = df[i,'A.GP']

    # Remove any record where either team has played less than 10 games    
    if ((HomeGP < 10) || (AwayGP < 10)){
      remove = c(remove,i)
    }
  }
  if(length(remove) > 0){
    df = df[-remove,]
  }
  
  
  # Remove columns 
  df$X = NULL                 # Old Row ID caused by import
  df$Date = NULL              # Date  
  df$HomeTeam = NULL          # Home Team name
  df$AwayTeam = NULL          # Away Team name
  
  
  # restart row id's from 1 
  row.names(df) <- 1:nrow(df)
  
  return (df)
}


part1 = TRUE

if (isTRUE(part1)){
  ### Preprocessing Part 1
  
  # Import matchups dataset
  matchups <<- read.csv("./Data/Matchups.csv")
  importTeams()
  
  # Pre-Filter dataset
  filteredMatchups <<- filterDataset(matchups)
  saveDF(filteredMatchups, "./Data/FilteredMatchups")
} else {
  ### Preprocessing Part 2
  
  # Import differ and summary 
  filtDF <- read.csv("./Data/FilteredMatchups.csv")
  diffDF <- read.csv("./Preprocessing/Data/DifferencesMatchup.csv")
  filtDF$X = NULL
  diffDF$X = NULL
  diffSummaries <- read.csv("./Preprocessing/Data/DiffSummaries.csv")
  
  # Remove Outliers
  cols = c('ROW','L10Losses','SHG','PKG','ShotsAgainst','ShotsFor','SV',
           'AvGoalieCnt','HomeP')
  for (col in cols){
    row = diffSummaries[diffSummaries$X == col,]
    IQR = as.numeric(row['IQR'])
    Q1 = as.numeric(row['firstQ'])
    Q3 = as.numeric(row['thirdQ'])
    
    # Find high and low ouliers 
    high = Q3 + (IQR*1.5);
    low = Q1 - (IQR*1.5);
    
    # Remove values in column outside range of outliers
    remove = c()
    for (i in 1:nrow(diffDF)){
      if ((diffDF[i,col] > high) || (diffDF[i,col] < low)){
        remove = c(remove, i)
      }
    }
    filtDF = filtDF[-remove,]
  }
  
  # Save filtered matchup dataframe
  saveDF(filtDF, "./Data/FilteredMatchups2")
}

