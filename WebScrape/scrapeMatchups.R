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

# Update all matchups for current team with additional statistics.
UpdateAllMatchups <- function(teamAbbr){
  # Add new features to dataframe
  df = get(eval(teamAbbr))
  df$SVPercentage = NA
  df$AvgGoalieCount = NA
  df$AvgShiftCount = NA
  df$ATOI = NA
  
  # Scrape event for each game at given date against given opponent
  tryCatch({
    dates = as.Date(df$Date)
    count = 1
    runningTot = c(0,0,0,0,0,0,0,0,0,0)
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
      awayTeam = as.character(event$Opponent)
      if (event$Location == "Away"){
        homeTeam = awayTeam
        awayTeam = teamAbbr
      }
  
      # Create unique identifier (doubles as link game-reference)
      id = paste(year,month,day,"0",homeTeam,sep="")
      
      ## Update matchup record
      #result = UpdateMatchup(id, homeTeam, awayTeam)
      result = UpdateMatchup(id, teamAbbr, event$Opponent)
      
      ## Increment results
      runningTot[1] = runningTot[1] + as.numeric(result[1])   #HomeAvgShiftCnt
      #runningTot[2] = runningTot[2] + as.numeric(result[3])   #AwayAvgShiftCnt
      runningTot[3] = runningTot[3] + as.numeric(result[5])   #HomeSVPer
      runningTot[4] = runningTot[4] + as.numeric(result[6])   #HomeAvgGoalieCnt
      #runningTot[5] = runningTot[5] + as.numeric(result[7])   #AwaySVPer
      #runningTot[6] = runningTot[6] + as.numeric(result[8])   #AwayAvgGoalieCnt
      HomeAvgShiftCnt = round((runningTot[1]/count), digit=2)
      #AwayAvgShiftCnt = round((runningTot[2]/count), digit=2)
      HomeSvPer = round((runningTot[3]/count), digit=2)
      HomeAvgGoalieCnt = round((runningTot[4]/count), digit=2)
      #AwaySVPer = round((runningTot[5]/count), digit=2)
      #AwayAvgGoalieCnt = round((runningTot[6]/count), digit=2)
      
      # HomeATOI
      atoi = strsplit(as.character(result[2]),"[.]")
      runningTot[7] = runningTot[7] + as.numeric(unlist(atoi)[1])
      runningTot[8] = runningTot[8] + as.numeric(unlist(atoi)[2])
      HomeATOI = convertATOI(runningTot[7], runningTot[8], count)
      
      # AwayATOI
      #atoi = strsplit(as.character(result[2]),"[.]")
      #runningTot[9] = runningTot[9] + as.numeric(unlist(atoi)[1])
      #runningTot[10] = runningTot[10] + as.numeric(unlist(atoi)[2])
      #AwayATOI = convertATOI(runningTot[9], runningTot[10], count)
      
      df[count,'SVPercentage'] = HomeSvPer
      df[count,'AvgGoalieCount'] = HomeAvgGoalieCnt
      df[count,'AvgShiftCount'] = HomeAvgShiftCnt
      df[count,'ATOI'] = HomeATOI
      
      count = count + 1
    }
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
  })
  
  return (df)
}

# Update a single matchup with individual skater stats
UpdateMatchup <- function(id, homeTeam, awayTeam){

  # Get HTML from page
  link = paste("http://www.hockey-reference.com/boxscores/",id,".html",sep="")
  gameInfo = read_html(link)
  
  # Get goalie info
  homeGoalie = getGoalieStats(gameInfo, homeTeam)
  awayGoalie = getGoalieStats(gameInfo, awayTeam)
  
  # Get Skater info
  homeSkater = getSkaterStats(gameInfo, homeTeam)
  awaySkater = getSkaterStats(gameInfo, awayTeam)
  
  # Add stats to data frame
  HomeAvgShiftCnt = homeSkater$avgShift
  HomeATOI = homeSkater$atoi
  AwayAvgShiftCnt = awaySkater$avgShift
  AwayATOI = awaySkater$atoi
  HomeSVPer = homeGoalie$svPer
  HomeGoalieCnt = homeGoalie$count
  AwaySVPer = awayGoalie$svPer
  AwayGoalieCnt = awayGoalie$count
  result = cbind(HomeAvgShiftCnt, HomeATOI, AwayAvgShiftCnt, AwayATOI, 
                 HomeSVPer, HomeGoalieCnt, AwaySVPer, AwayGoalieCnt)
  
  # Close web connection
  closeAllConnections()

  return (result)
}

# Retrieve avg save percentage of goalies and the amount of goalies played using matchup page
getGoalieStats <- function(html, team){
  # Find table
  goalies = html_nodes(html, xpath=paste("//table[@id='",team,"_goalies']/tbody/tr",sep=""))
  
  # Get avg save percentage and count of goalies playing
  svPer = 0
  count = 0
  for (goalie in goalies){
    svPer = svPer + as.numeric(html_nodes(goalie, xpath="./td[6]") %>% html_text())
    count = count + 1
  }
  svPer = round(svPer / count, digits=3)
  
  results = list("svPer"=svPer, "count"=count)
  return (results)
}

# Retrieve avg time on ice, average shift count and count of players for given team
getSkaterStats <- function(html, team){
  # Find table
  skaters = html_nodes(html, xpath=paste("//table[@id='",team,"_skaters']/tbody/tr",sep=""))
  
  # Get ATOI, Avg Shifts and count of skaters
  atoi_m = 0
  atoi_s = 0
  avgShift = 0
  count = 0
  for (skater in skaters){
    avgShift = avgShift + as.numeric(html_nodes(skater, xpath="./td[16]") %>% html_text())
    
    curr_atoi = html_nodes(skater, xpath="./td[17]") %>% html_text()
    curr_atoi = strsplit(as.character(curr_atoi),":")
    atoi_m = atoi_m + as.numeric(unlist(curr_atoi)[1])
    atoi_s = atoi_s + as.numeric(unlist(curr_atoi)[2])
    
    count = count + 1
  }
  avgShift = round(avgShift / count, digits=2)
  
  # Update ATOI
  atoi = convertATOI(atoi_m, atoi_s, count)
  
  results = list("avgShift"=avgShift, "atoi"=atoi, "count"=count)
  return (results)
}

# Converts int totals of minutes and second to a string notation averaged by count
convertATOI <- function(m,s,count){
  m = m / count
  s = s / count
  h = as.integer(m / 60)
  m = as.integer((m %% 60) + as.integer(s / 60))
  s = as.integer(s %% 60)
  atoi = paste(m, s, sep=".")
  
  return (atoi)
}

#importTeams()
importTeam("MTL")
data = UpdateAllMatchups("MTL")
