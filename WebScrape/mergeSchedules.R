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

# Merges each matchup in schedule between two teams from dfs in workspace.  Assuming that it is there
# NOTE: Need to hard code column to delete in function
mergeSchedules <- function(){
  
  # Keep track of game ids already seen
  seen = c()
    
  for (team in names(config$teamAbbrs)){
    df = get(team)
    
    # Merge matchups for each game (not already seen) in df
    for (game in df){
      gid =  df$GID       # Get game ID.  Will be unique
      
      # If matchup hasn't been merged already, merge it
      if (!(gid %in% seen)){
        # Get opponent dataframe
        opp = df$Opponent
        dfOpp = get(opp)
  
        # Merge matchup of the two teams
        matchup = mergeMatchup(df, dfOpp, gid)

        
        
      }
  }
}

# Merge a matchup between two teams on GID
mergeMatchup <- function(df, dfOpp, GID){
  # Where is game played
  loc = df$Location
  
  # Check each record for corresponding GID
  for (g2 in dfOpp){
    if (){
      
    }
  }
  
}






