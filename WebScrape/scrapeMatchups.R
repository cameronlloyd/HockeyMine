library('rvest')
library('XML')

config <- config::get()

# Import all teams from csv file
importTeams <- function(){
  tryCatch({
    temp = list.files(path="./TeamSchedules/CSV/.",pattern="*.csv")
    for (i in 1:length(temp)) {
      assign(substr(temp[i],1,nchar(temp[i])-4), 
             read.csv(paste("./TeamSchedules/CSV/",temp[i],sep="")),
             envir=.GlobalEnv)
    }
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
  })
}


# Import single team from csv file
importTeam <- function(teamAbbr){
  tryCatch({
    assign(teamAbbr,
           read.csv(paste("./TeamSchedules/CSV/",teamAbbr,".csv",sep="")),
           envir=.GlobalEnv)
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
  })  
}


# Update dataframes for each matchup between two teams.  This information will be
#   more descriptive of standard schedule information (Will incl. advanced statistics)
UpdateTeams <- function(){
  
  tryCatch({
    ## For each team, create dataframe of matchup info
    team.Abbrs = config$StandardAbbrs
    for (abbr in names(team.Abbrs)){
      matchups = UpdateAllMatchups(abbr)
    } 
    
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
  })
}

# Update all matchups for current team with additional, more advanced, statistics.
UpdateAllMatchups <- function(teamAbbr){
  df = get(eval(teamAbbr))
  
  # Scrape event for each game at given date against given opponent
  dates = as.Date(df$Date)
  for (i in 1:length(dates)) {
    # Get current event
    event = df[i,]
    
    # Get current day
    splitDate = strsplit(as.character(event$Date),"-")
    year = unlist(splitDate)[1]
    month = unlist(splitDate)[2]
    day = unlist(splitDate)[3]
    
    # Get home team
    homeTeam = teamAbbr
    if (event$Location == "Away"){
      homeTeam = event$Opponent
    }
  }
}


#importTeams()
importTeam("MTL")
UpdateAllMatchups("MTL")
