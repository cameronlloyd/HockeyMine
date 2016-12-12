library('rvest')
library('XML')

config <- config::get()

scrapeTeam <- function(teamAbbr, start, end){
  
  data = data.frame(Date = c(), 
                    Loc = c(),
                    Opp = c(),
                    GF = c(),
                    GA = c(),
                    Outcome = c(),
                    Streak = c(),
                    Wins = c(),
                    Loss = c(),
                    OL = c(),
                    Shots = c(),
                    PIM = c(),
                    PPG = c(),
                    PKSuccess = c(),
                    FOW = c(),
                    Save = c(),
                    EvenGoal4_against=c(),
                    PlaceWinPer = c(),
                    TakeGiveDiff = c())
  
    
  tryCatch({
    teamInfo = getTeamInfo(teamAbbr, start, end)
    data = rbind(data,teamInfo)
    saveRDS(data,file=paste(as.character(dates[i])," Data.rda",sep=""))
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
  })
  
  return (data)
}

# Scrape all hockey games on a given day
getTeamInfo <- function(team, start, end) {
  
  result = data.frame(Date = c(), 
                     StreakL10 =c(),
                     StreakTot = c(),
                     GoalDiff = c(),
                     PPSuccess = c(),
                     PKSuccess = c(),
                     FOW = c(),
                     Save = c(),
                     EvenGoal4_against=c(),
                     PlaceWinPer = c(),
                     TakeGiveDiff = c())
  
  link = paste("http://www.hockey-reference.com/teams/",team,"/2016_games.html",sep="")
  
  dayInBaseball = read_html(link) #These two lines are not protected
  game.links = html_nodes(dayInBaseball, xpath="//pre/a[starts-with(@href,'/boxes/')]") %>% xml_attr("href")
  
  
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

startDate = "12/1"
endDate = "12/1"
data = scrapeTeam("MTL", startDate, endDate)
