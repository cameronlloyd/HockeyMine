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





# Import matchups dataset
matchups <<- read.csv("./Data/Matchups.csv")
#importTeams()


# Pre-Filter dataset
filteredMatchups <<- filterDataset(matchups)
saveDF(filteredMatchups, "./Data/FilteredMatchups")


