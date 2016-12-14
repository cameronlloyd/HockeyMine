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
#   more descriptive of standard schedule information
UpdateTeams <- function(){
  
  tryCatch({
    ## For each team, create dataframe of matchup info
    team.Abbrs = config$teamAbbrs
    for (abbr in names(team.Abbrs)){
      print(paste("Updating: ",abbr,sep=""))
      teamInfo = UpdateSchedule(abbr, "2016")
      print("   Saving.")
      saveRDS(teamInfo,file=paste("./TeamSchedules/RDA/",abbr," Data.rda",sep=""))
      write.csv(teamInfo,file=paste("./TeamSchedules/CSV/",abbr,".csv",sep=""))
    } 
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
  })
}

# Update dataframes for a single team
UpdateTeam <- function(abbr){
  
  tryCatch({
    ## Create dataframe of matchup info
    print(paste("Updating: ",abbr,sep=""))
    teamInfo = UpdateSchedule(abbr, "2016")
    print("   Saving.")
    #saveRDS(teamInfo,file=paste("./TeamSchedules/RDA/",abbr," Data.rda",sep=""))
    #write.csv(teamInfo,file=paste("./TeamSchedules/CSV/",abbr,".csv",sep=""))
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
  })
  
  return (teamInfo)
}




# Update schedule for current team with additional statistics.
UpdateSchedule <- function(teamAbbr, year){
  # Add new features to dataframe
  df = get(eval(teamAbbr))
  df$AvgCFPerClose = NA
  df$AvgCFPer5v5 = NA
  df$AvgCFPerEven = NA
  df$SVPercentage = NA
  df$AvgGoalieCount = NA
  df$AvgShiftCount = NA
  df$ATOI = NA
  df$AvgAge = NA
  df$PDO = NA
  df$FOWPer = NA
  
  # Scrape event for each game at given date against given opponent
  tryCatch({
    # These stats will be the same for every record
    fullTeam = AddFullTeamStats(teamAbbr, year)
    
    # Get matchup stats
    dates = as.Date(df$Date)
    count = 1
    runningTot = c(0,0,0,0,0,0,0,0)
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
      result = UpdateMatchup(id, teamAbbr)
      
      ## Increment results
      runningTot[1] = runningTot[1] + as.numeric(result[1])   #AvgCFPerClose
      runningTot[2] = runningTot[2] + as.numeric(result[2])   #AvgCFPer5v5
      runningTot[3] = runningTot[3] + as.numeric(result[3])   #AvgCFPerEven
      runningTot[4] = runningTot[4] + as.numeric(result[4])   #AvgShiftCnt
      runningTot[5] = runningTot[5] + as.numeric(result[6])   #SVPer
      runningTot[6] = runningTot[6] + as.numeric(result[7])   #AvgGoalieCnt
      AvgCFPerClose = round((runningTot[1]/count), digit=2)
      AvgCFPer5v5 = round((runningTot[2]/count), digit=2)
      AvgCFPerEven = round((runningTot[3]/count), digit=2)
      AvgShiftCnt = round((runningTot[4]/count), digit=2)
      SvPer = round((runningTot[5]/count), digit=2)
      AvgGoalieCnt = round((runningTot[6]/count), digit=2)
  
      # ATOI
      atoi = strsplit(as.character(result[5]),"[.]")
      runningTot[7] = runningTot[7] + as.numeric(unlist(atoi)[1])
      runningTot[8] = runningTot[8] + as.numeric(unlist(atoi)[2])
      ATOI = convertATOI(runningTot[7], runningTot[8], count)
      
      # Add values to dataframe
      df[count,'AvgCFPerClose'] = AvgCFPerClose
      df[count,'AvgCFPer5v5'] = AvgCFPer5v5
      df[count,'AvgCFPerEven'] = AvgCFPerEven
      df[count,'SVPercentage'] = SvPer
      df[count,'AvgGoalieCount'] = AvgGoalieCnt
      df[count,'AvgShiftCount'] = AvgShiftCnt
      df[count,'ATOI'] = ATOI
      df[count,'GID'] = id
      df[count,'AvgAge'] = fullTeam$AvAge
      df[count,'PDO'] = fullTeam$PDO
      df[count,'FOWPer'] = fullTeam$FOPer
      
      count = count + 1
    }
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
  })
  
  return (df)
}

# Update each matchup with team stats found from team homepage
AddFullTeamStats <- function(team, year){
  # Get HTML from page
  link = paste("http://www.hockey-reference.com/teams/",team,"/",year,".html",sep="")
  gameInfo = read_html(link)
  
  # Get team stats table
  teamStats = gameInfo %>% html_nodes(xpath="//table[@id='team_stats']/tbody/tr[1]")
  
  # Get AvAge, PDO
  AvAge = teamStats %>% html_nodes(xpath="./td[1]") %>% html_text()
  PDO = teamStats %>% html_nodes(xpath="./td[25]") %>% html_text()
  
  # Get FOW %
  skaters = gameInfo %>% html_nodes(xpath="//table[@id='skaters']/tfoot/tr")
  FOPer = skaters %>% html_nodes(xpath="./td[28]") %>% html_text()
  
  # Close web connection
  closeAllConnections()
  
  results = list("AvAge"=AvAge, "PDO"=PDO, "FOPer"=FOPer)
  return (results)
}

# Update a single matchup with individual skater stats
UpdateMatchup <- function(id, homeTeam, awayTeam){

  # Get HTML from page
  link = paste("http://www.hockey-reference.com/boxscores/",id,".html",sep="")
  gameInfo = read_html(link)
  
  # Get goalie info
  homeGoalie = getGoalieStats(gameInfo, homeTeam)
  
  # Get Skater info
  homeSkater = getSkaterStats(gameInfo, homeTeam)
  
  # Add stats to data frame
  AvgCFPerClose = homeSkater$AvgCFPerClose
  AvgCFPer5v5 = homeSkater$AvgCFPer5v5
  AvgCFPerEven = homeSkater$AvgCFPerEven
  AvgShiftCnt = homeSkater$avgShift
  ATOI = homeSkater$atoi
  SVPer = homeGoalie$svPer
  GoalieCnt = homeGoalie$count
  result = cbind(AvgCFPerClose, AvgCFPer5v5, AvgCFPerEven,
                 AvgShiftCnt, ATOI, SVPer, GoalieCnt)
  
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
  
  # Get Corsi-For (Close), (5v5) and (even)
  skatersAdv = html_nodes(html, xpath=paste("//div[@id='all_",team,"_adv']",sep="")) %>%
    html_nodes(xpath='comment()') %>%
    html_text() %>%
    read_html() %>%
    html_node('table') %>%
    html_node('tfoot')
  AvgCFPerClose = as.numeric(skatersAdv %>% html_nodes(xpath="./tr[@class='CLAll hidden']/td[4]") %>% html_text())
  AvgCFPer5v5 = as.numeric(skatersAdv %>% html_nodes(xpath="./tr[@class='ALL5v5 hidden']/td[4]") %>% html_text())
  AvgCFPerEven = as.numeric(skatersAdv %>% html_nodes(xpath="./tr[@class='ALLEV hidden']/td[4]") %>% html_text())
  
  results = list("avgShift"=avgShift, "atoi"=atoi, "count"=count, "AvgCFPerClose"=AvgCFPerClose, 
                 "AvgCFPer5v5"=AvgCFPer5v5, "AvgCFPerEven"=AvgCFPerEven)
  return (results)
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

# Writes team df to both CSV and RDA files
Write2Files <- function(team, df){
  saveRDS(df,file=paste("./TeamSchedules/RDA/",team," Data.rda",sep=""))
  write.csv(df,file=paste("./TeamSchedules/CSV/",team,".csv",sep=""))
}


#importTeams()
#UpdateTeams()

team = "MTL"
#importTeam(team)
data = UpdateTeam(team)


#DeleteColumn()
