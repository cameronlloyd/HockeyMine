library('rvest')
library('XML')

config <- config::get()

scrapeTeam <- function(teamAbbr){
    
  tryCatch({
    teamInfo = getTeamInfo(teamAbbr)
    saveRDS(teamInfo,file=paste("./TeamSchedules/RDA/",teamAbbr," Data.rda",sep=""))
    write.csv(teamInfo,file=paste("./TeamSchedules/CSV/",teamAbbr,".csv",sep=""))
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
  
  
  for (link in game.links) {
    link = paste("http://www.baseball-reference.com",link,sep="")
    print("Scraping a game now")
    print(paste("The link for the game being scraped now is: ",link,sep=""))
    
    
    gameFrame = scrapeGame(link,as.numeric(month))
    result = rbind(result,gameFrame)
    
    print("Finished scraping game")
  }
  
  dateColumn = rep(date,nrow(result))
  result = data.frame(result,dateColumn)
  #names(result) = c("avgHandBatter" ,"avgPlaceBatter","avgHandPitcher","avgPlacePitcher","hit","atBats","sampleHandBatter","samplePlaceBatter","sampleHandPitcher","samplePlacePitcher","Date")
  
  return (result)
  
}

data = scrapeTeam("MTL")
