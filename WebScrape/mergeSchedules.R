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

# Import single team from csv file
importTeam <- function(teamAbbr){
  tryCatch({
    assign(teamAbbr,
           read.csv(paste("./WebScrape/TeamSchedules/CSV/",teamAbbr,".csv",sep="")),
           envir=.GlobalEnv)
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
  })  
}

# Merges each matchup in schedule between two teams from dfs in workspace.  Assuming that it is there
# NOTE: Need to hard code column to delete in function
mergeSchedules <- function(){
  
  tryCatch({ 
    Matchups = data.frame()
    
    # Keep track of game ids already seen
    seen = c()
    for (team in names(config$teamAbbrs)){
      df = get(team)
      games = dim(df)[1]
      
      # Merge matchups for each game (not already seen) in df
      for (i in 1:games){
        gid =  as.character(df[i,'GID'])       # Get game ID.  Will be unique
        
        # If matchup hasn't been merged already, merge it
        if (is.na(gid)) {
          print(team)
          next
        }
        if (!(gid %in% seen)){
          # Get opponent dataframe
          opp = as.character(df[i,'Opponent'])
          dfOpp = get(opp)
    
          # Get row of game id in opponent df.
          #   If not found, throw error
          rowNum = -1
          games2 = dim(dfOpp)[1]
          for (j in 1:games2){
            if (gid == as.character(dfOpp[j,'GID'])){ rowNum = j; break}
          }
          if (rowNum == -1){ 
            warning(paste("Team ",team,"'s GID: ",gid," was not found in team ",
                          opp,"'s schedule",sep=""))
          }
          
          # Decide home and away team. Get rows
          homeTeam = df[i,]
          awayTeam = dfOpp[rowNum,]
          if (as.character(homeTeam$Location) == "Away"){
            homeTeam = awayTeam
            awayTeam = df[i,]
          }
          
          # Merge matchup of the two teams
          matchup = mergeMatchup(homeTeam, awayTeam)
          
          # Add game id to list
          seen = c(seen,gid)
          
          # Add matchup record to dataframe
          Matchups = rbind(Matchups, matchup)
        }
      }
    }
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
  })  
  
  
  return (Matchups)
}

# Merge a matchup between two teams on GID
mergeMatchup <- function(homeTeam, awayTeam){
  
  # Get and rename desired features in desired order
  Date = as.character(homeTeam$Date)
  HomeTeam = as.character(awayTeam$Opponent)
  AwayTeam = as.character(homeTeam$Opponent)

  # Get team statistics
  Home.Stats = getTeamFeatures(homeTeam)
  Away.Stats = getTeamFeatures(awayTeam)
  colnames(Home.Stats) = paste("H",colnames(Home.Stats),sep=".")
  colnames(Away.Stats) = paste("A",colnames(Away.Stats),sep=".")
  
  # Get outcome of game (In respect to home team)
  Result = homeTeam$Outcome               
  if (grepl(Result,"L")){
    Result = 0
  }
  else{
    Result = 1
  }
  
  # Create dataframe
  df = data.frame(Date,HomeTeam,AwayTeam,Home.Stats,Away.Stats,Result)

  return(df)
}


# Get desired features for given team
getTeamFeatures <- function(team){
  features = data.frame(matrix(ncol=33,nrow=0))
  colnames(features) = c("GP","GF","GA","Wins","Losses","OL","Streak","ROW","L10Wins","L10Losses","L10OL","PPM","PPG","PPO",
                         "SHG","PKM","PKG","PKO","ShotsFor","ShotsAgainst","SV","AvCFClose","AvCF5v5","AvCFEven",
                         "AvGoalieCnt","AvShiftCnt","ATOI","AvAge","PDO","FOW","WinP","HomeP","AwayP")
  
  
  GP = team$GamesPlayed               # Games Played
  GF = team$GoalsFor                  # Goals For
  GA = team$GoalsAgainst              # Goals Against
  Wins = team$Wins                    # Wins
  Losses = team$Losses                # Losses
  OL = team$OL                        # Overtime Losses
  
  # Streak (+ if winning, - if losing)
  val = strsplit(as.character(team$Streak)," ")
  Streak = as.numeric(unlist(val)[2])
  if (unlist(val)[1] == "L"){
    Streak = -1 * Streak
  }
  
  ROW = team$ROW                      # Regulation or Overtime Losses
  L10Wins = team$L10Wins              # Last 10 game win total
  L10Losses = team$L10Losses          # Last 10 game loss total
  L10OL = team$L10OL                  # Last 10 game overtime loss total
  
  PPM = team$PKM                      # Power Play Minutes 
  PPG = team$PPG                      # Power Play Goals
  PPO = team$PPO                      # Power Play Opportunities
  SHG = team$SHG                      # Short Handed Goals
  PKM = team$PIM                      # Penalty Kill Minutes
  PKG = team$PKG                      # Penalty Kill Goals
  PKO = team$PKO                      # Penalty Kill Opportunities
  
  ShotsFor = team$ShotsFor            # Shots For
  ShotsAgainst = team$ShotsAgainst    # Shots Against
  SV = team$SVPercentage              # Goalie save percentage
  
  AvCFClose = team$AvgCFPerClose      # Avg Corsi-for % (Close)
  AvCF5v5 = team$AvgCFPer5v5          # Avg Corsi-for % (5v5)
  AvCFEven = team$AvgCFPerEven        # Avg Corsi-for % (Even)
  
  AvGoalieCnt = team$AvgGoalieCount   # Average amount of goalies per game
  AvShiftCnt = team$AvgShiftCount     # Player average amount of shifts per game
  ATOI = team$ATOI                    # Average time on ice for players
  AvAge = team$AvgAge                 # Average age of player
  
  PDO = team$PDO                      # "luck" statistic
  FOW = team$POWPer                   # Faceoff win percentage
  
  WinP = team$WinPer                  # Win percentage
  HomeP = team$HomePer                # Home win percentage
  AwayP = team$AwayPer                # Away win percentage
  
  
  # Create 33 feature record
  features = cbind(GP,GF,GA,Wins,Losses,OL,Streak,ROW,L10Wins,L10Losses,L10OL,PPM,PPG,PPO,
                        SHG,PKM,PKG,PKO,ShotsFor,ShotsAgainst,SV,AvCFClose,AvCF5v5,AvCFEven,
                        AvGoalieCnt,AvShiftCnt,ATOI,AvAge,PDO,FOW,WinP,HomeP,AwayP)
  return (features)
}


# Deletes a column from each team df in workspace.  Assuming that it is there
# NOTE: Need to hard code column to delete in function
DeleteColumn <- function(){
  for (team in names(config$teamAbbrs)){
    df = get(team)
    df$X.1 = NULL
    assign(team, df, envir=.GlobalEnv)
    Write2Files(team, df)
  }
}

# Writes team df to both CSV and RDA files
Write2Files <- function(team, df){
  saveRDS(df,file=paste("./WebScrape/TeamSchedules/RDA/",team," Data.rda",sep=""))
  write.csv(df,file=paste("./WebScrape/TeamSchedules/CSV/",team,".csv",sep=""))
}


# Import teams 
importTeams()
DeleteColumn()

# Create matchup table
matchups = mergeSchedules()
matchups = matchups[order(as.Date(matchups$Date, format="%Y-%m-%d")),]
row.names(matchups) <- 1:nrow(matchups)

# Save matchup table
saveRDS(matchups,"./Data/Matchups Data.rda")
write.csv(matchups,"./Data/Matchups.csv")
