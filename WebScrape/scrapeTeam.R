library('rvest')
library('XML')

config <- config::get()

scrapeTeam <- function(teamAbbr){
    
  tryCatch({
    teamInfo = getTeamInfo(teamAbbr)
    saveRDS(teamInfo,file=paste("./WebScrape/TeamSchedules/RDA/",teamAbbr," Data.rda",sep=""))
    write.csv(teamInfo,file=paste("./WebScrape/TeamSchedules/CSV/",teamAbbr,".csv",sep=""))
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
  })
  
  return (teamInfo)
}

# Scrape all hockey games on a given day
getTeamInfo <- function(team) {
  
  result = data.frame(GP = c(),
                    Date = c(), 
                    Loc = c(),
                    Opp = c(),
                    GF = c(),
                    GA = c(),
                    Outcome = c(),
                    Wins = c(),
                    Losses = c(),
                    ROW = c(),
                    OL = c(),
                    L10Wins = c(),
                    L10Losses = c(),
                    L10OL = c(),
                    Streak = c(),
                    ShotsFor = c(),
                    PIM = c(),
                    PPG = c(),
                    PPO = c(),
                    SHG = c(),
                    ShotsAgainst = c(),
                    PKM = c(),
                    PKG = c(),
                    PKO = c(),
                    TotWinPer = c(),
                    HomeWinPer = c(),
                    AwayWinPer = c())
  
  link = paste("http://www.hockey-reference.com/teams/",team,"/2016_games.html",sep="")
  
  teamSched = read_html(link) #These two lines are not protected
  game.links = html_nodes(teamSched, xpath="//pre/a[starts-with(@href,'/boxes/')]") %>% xml_attr("href")
  
  gameNo <<- 0
  L10 <<- c(0,0,0)
  locPer <<- c(0,0,0,0)
  runningTot <<- c(0,0,0,0,0,0,0,0,0,0,0,0)
  for (link in game.links) {
    gameFrame = scrapeGame(link,as.numeric(month))
    result = rbind(result,gameFrame)

    gameNo <<- gameNo + 1
  }
  
  closeAllConnections()
  return (result)
}

#data = scrapeTeam("MTL")
print(config$teamAbbr)
