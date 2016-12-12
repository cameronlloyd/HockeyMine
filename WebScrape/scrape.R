library('rvest')
library('XML')

config <- config::get()

#Scrape all hockey games between startDate and endDate
#the two arguments have the form "year/month/day" [No zero in front of a number below ten is needed!]
scrapeCalender <- function(startDate,endDate) {
  
  dates = c(seq(as.Date(startDate),as.Date(endDate),"days"))
  
  data = data.frame(avgHandBatter = c(), 
                    avgPlaceBatter =c(),
                    avgHandPitcher = c(),
                    avgPlacePitcher = c(),
                    hit=c(),
                    atBats=c(),
                    sampleHandBatter=c(),
                    samplePlaceBatter=c(),
                    sampleHandPitcher=c(),
                    samplePlacePitcher=c(),
                    Date=c())
  lastYear = 0
  for (i in 1:length(dates)) {
    day = dates[i]
    print(day)
    splitDate = strsplit(as.character(day),"-")
    year = unlist(splitDate)[1]
    month = unlist(splitDate)[2]
    day = unlist(splitDate)[3]
    
    tryCatch({
      dataFromDay = scrapeDay(year,month,day)
      data = rbind(data,dataFromDay)
      saveRDS(data,file=paste(as.character(dates[i])," Data.rda",sep=""))
    }, error = function(e){
      cat("ERROR:",conditionMessage(e),"\n")
    })
  }
  
  data = cleanData(data)
  
  return (data)
}

# Scrape all hockey games on a given day
scrapeDay <- function(year,month,day) {
  
  result = data.frame(avgHandBatterCar = c(), 
                      avgPlaceBatterCar =c(),
                      avgHandPitcherCar = c(),
                      avgPlacePitcherCar = c(),
                      sampleHandBatterCar = c(),
                      samplePlaceBatterCar=c(),
                      sampleHandPitcherCar=c(),
                      samplePlacePitcherCar=c(),
                      avgHandBatter2015 = c(), 
                      avgPlaceBatter2015 =c(),
                      avgHandPitcher2015 = c(),
                      avgPlacePitcher2015 = c(),
                      sampleHandBatter2015 = c(),
                      samplePlaceBatter2015=c(),
                      sampleHandPitcher2015=c(),
                      samplePlacePitcher2015=c(),
                      avgHandBatter2014 = c(), 
                      avgPlaceBatter2014 =c(),
                      avgHandPitcher2014 = c(),
                      avgPlacePitcher2014 = c(),
                      sampleHandBatter2014 = c(),
                      samplePlaceBatter2014=c(),
                      sampleHandPitcher2014=c(),
                      samplePlacePitcher2014=c(),
                      hit=c(),
                      atBats=c(),
                      pitcherInMonth=c(),
                      batterInMonth=c(),
                      parkFactor=c())
  
  date = paste(year,month,day,sep="-")
  link = paste("http://www.baseball-reference.com/games/standings.cgi?date=",date,sep="")
  
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

startDate = "2016/12/1"
endDate = "2016/12/1"