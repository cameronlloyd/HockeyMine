#### nhlscrapr tutorial code for vanHAC 2016
#### by Jack Davis < jackd@sfu.ca , jack.davis.sfu@gmail.com >
#### last updated 2016-04-18 
#### with Kurt Routley < kurtroutley@sfu.ca >
#### original nhlscrapr package by A.C. Thomas < act@acthomas.ca >


######### PART 0: PREAMBLE
### First, place the file 'Patch for nhlscrapr.r' in a directory where you wish you include all the data
setwd("C:\\Users\\Jack\\Desktop\\Projects 2016\\VanHAC")

### Install the package, this only has to be done once
 install.package("nhlscrapr") ## Not run

### Load the package, this must be done every time you open R or Rstudio
library(nhlscrapr)
source("Patch for nhlscrapr.r") ## Fixes a bug in full.game.database() and in player.summary(). Adds aggregate.roster.by.name().


######### PART 1: EXTRACTING DATA FROM NHL.COM


### First define the games you wish to extract
### the extra.seasons parameter in full.game.database determines the number of seasons beyond 2014-15 we wish to extract
fgd = full.game.database(extra.seasons = 1)




############ Option 1: Extract data from only a single season (e.g. 2014-15)
## Get the game-by-game data for that season
thisyear = "20142015"
game_ids = subset(fgd, season == thisyear)

## Download those games, waiting 5 sec between games, unpacking and format, and gather into a single Rdata fill
dummy = download.games(games = game_ids, wait = 5) 
process.games(games=game_ids,override.download=FALSE) 
compile.all.games(output.file="NHL play-by-play 2014-5.RData") 
gc()


############ Option 2: Extract data from every season from 2002-3 to 2014-15
allyears = unique(fgd$season)

for(thisyear in allyears)
{
	## Get the game-by-game data for a season
	game_ids = subset(fgd, season == thisyear)[1:10,]
	print(game_ids)
	## Download those games, waiting 5 sec between games
	dummy = download.games(games = game_ids, wait = 5) 
	
	## Processing, unpacking and formatting
	process.games(games=game_ids,override.download=FALSE) 

	gc() ## Clear up the RAM using (g)arbage (c)ollection.
}

## Put all the processed games into a single file
compile.all.games(output.file="NHL play-by-play all seasons.RData")






############## PART 2: LOADING EXTRACTED DATA

### After extract and compiling, we have two files

### ... the events log
temp = load("source-data\\nhlscrapr-20142015.RData") ## If you used option 1 (1 season)
ev_all = get(temp)

### ... and the player roster
temp = load("source-data\\nhlscrapr-core.RData")
roster = get(temp)




########## PART 3: ANALYSIS OF GOALS INTENSITY (assuming a single season)

### Some key variables to start us off
### seconds: The number of seconds since the beginning of the game of the event. 0-1200 for the first period, ... , 3600.5 and so on is overtime
### gcode: Game code. The number of the game, in chronological order up to the night. 20001-21230 are regulation season, 30001 or more is playoffs.
### ev.team: The team that scored, took the shot, won the faceoff, or  received the penalty
### etype: The general type of event, such as "GOAL", "SHOT", "FAC", "CHANGE", "HIT", "TAKE", "MISS", and "PENL".
### home.score and away.score are the scores immediately before the event

head(ev_all)
table(ev_all$etype)

### One personal interest of mine is how the goal scoring intensity changes over the course of a game.
### To investigate this, it's useful to make a few additional variables


### Grouping seconds together in one minute intervals.
ev_all$minutes = floor((ev_all$seconds - 0.5) / 60)
ev_all$minutes = pmax(ev_all$minutes, 0)

### An indicator function of whether the event pertains to the home team
ev_all$home = 0
ev_all$home[as.character(ev_all$ev.team) == as.character(ev_all$hometeam)] = 1


### Now we can isolate the database to situations that are in regulation time and during the regular season 
ev_reg =  subset(ev_all, period <= 3 & gcode < 30000 & gcode >= 20001)
ev_goals = subset(ev_reg, etype == "GOAL")
ev_shots = subset(ev_reg, etype == "SHOT")

goals_per_hr = as.numeric(table(ev_goals$minutes)) / 1230 * 60
shots_per_hr = as.numeric(table(ev_shots$minutes)) / 1230 * 60
mins = 1:60


### Plot 1. We can get the rate of goal scoring per game
plot(goals_per_hr ~ mins, type='b', xlab="Minutes", ylab="Goals per hour", las=1, ylim=c(0,12), main="Raw goals per hour, both teams")
abline(v=20.5)
abline(v=40.5)


### Plot 2. We can get the rate of shots per game
plot(shots_per_hr ~ mins, type='b', xlab="Minutes", ylab="Shots per hour", las=1, ylim=c(35,65), main="Raw shots per hour, both teams")
abline(v=20.5)
abline(v=40.5)



### Why the jump in rate of goals but not the jump in rate of shots?
### Empty net.
### So how do we isolate the empty net goals? First we need to understand player IDs

##### Player IDs
### Players are identified by an ID that is unique to them across the league. The roster file gives the names and positions for each player
### If there is no relevant player for an event for that variable, number '1' is used instead.
temp = load("source-data\\nhlscrapr-core.RData")
roster = get(temp)
head(roster)


### in the events table, home.G, away.G the ID of the goal during the event
### so any time one or both of these fields is '1', we have an empty net.


### One way is to only count goals where both goalies are in
ev_goals_bothnet = subset(ev_goals, home.G != 1, away.G != 1)

### Another way is to only count goals there were SCORED on a goalies
ev_goals_noten = subset(ev_goals, (home.G != 1 & home == 1) | (away.G != 1 & home == 0))
ev_shots_noten = subset(ev_shots, (home.G != 1 & home == 1) | (away.G != 1 & home == 0))

### In the 2014-5 season,
nrow(ev_goals) ###	of the 6413 non-overtime goals, 
nrow(ev_goals) - nrow(ev_goals_bothnet)  ## 224 happened with an empty net
nrow(ev_goals) - nrow(ev_goals_noten)  ## 148 (66%) happened ON an empty net
  

  
### Plot 3. We can get the rate of goal scoring per game
goals_per_hr = as.numeric(table(ev_goals_bothnet$minutes)) / 1230 * 60
plot(goals_per_hr ~ mins, type='b', xlab="Minutes", ylab="Goals per hour", las=1, ylim=c(0,12), main="Goals per hour, both goalies in, both teams")
abline(v=20.5)
abline(v=40.5)
  
### Plot 4. We can get the rate of goal scoring per game
goals_per_hr = as.numeric(table(ev_goals_noten$minutes)) / 1230 * 60
plot(goals_per_hr ~ mins, type='b', xlab="Minutes", ylab="Goals per hour", las=1, ylim=c(0,12), main="Goals per hour, receiving goalies in, both teams")
abline(v=20.5)
abline(v=40.5)


### Finally, we may want to remove powerplay situations.
### a1, a2, ... , a6, h1, ... , h6 is the ID of the (up to 6) non-goalies on the ice for the away and home team respectively
### home.skaters and away.skaters is the number of players, including the goalie, on the ice at the time of the event

ev_goals_final = subset(ev_goals_noten, home.skaters == away.skaters)
goals_per_hr = as.numeric(table(ev_goals_final$minutes)) / 1230 * 60
plot(goals_per_hr ~ mins, type='b', xlab="Minutes", ylab="Goals per hour", las=1, ylim=c(0,12), main="Goals per hour, even strength and receiving goalie in, both teams")
abline(v=20.5)
abline(v=40.5)


### A comment on goal intensity: We are introducing bias in goals per hour when we remove certain cases beacuse we are not accounting for the reduction in time spent in that situation. 
### In empty net games, this accounts for a large portion of the last 2-3 minutes of one-goal games. Powerplay situations make up about 15% of the 
### time spent in a game. Accounting for this lost time is left as a exercise to the reader.




################### PART 4: PLAYER SUMMARIES

### If we wish to isolate just the Canucks, we can use the event file.
van_goals = subset(ev_goals, ev.team == "VAN")


### If we wish to just the Sedin twins, we can do that with a look up in the roster file.
### Notice that team is not given in the roster, which means player IDs persist through trades.
sedins = subset(roster,last=="SEDIN")$player.id ## 151 and 152 in my dataset
ev_sedin = subset(ev_all, ev.player.1 %in% sedins)

table(ev_sedin$ev.player.1, ev_sedin$etype)

### We can also get player summaries for any collection of events we wish
roster_name = aggregate.roster.by.name(roster)
ps = player.summary(ev_all, roster_name)

### The output is an array of 5 tables
### The first table is the person that did the event (e.g. scored the goal, got the penalty, made the shot, miss, or blocked the shot)
player_summary = as.data.frame(ps[,,1])

### The second table is the second person in the event, if relevant. (i.e. 1st assist, victim in penalty (?)), 2nd block (?))
### The third table is the third person in the event, if relevant. (only 2nd assists)
player_summary$ASSIST = ps[,3,2] + ps[,3,3] # Third column is the GOAL event


### The fourth table is anyone who was on ice when the event happened and it was their team that was ev.team
### The fifth table is anyone who was on ice when the event happened and it was the opposing team that was ev.team
### ev.team: The team that scored, took the shot, won the faceoff, or received the penalty
player_summary$PLUSMINUS = ps[,3,4] - ps[,3,5]
player_summary$PLUSMINUS_SHOTS = ps[,2,4] - ps[,2,5]

head(player_summary)


### Finally, we can use information from the roster to fill in the name information
roster_name = subset(roster_name, firstlast %in% rownames(player_summary))
name_idx = match(row.names(player_summary), roster_name$firstlast)
player_summary$firstlast = roster_name$firstlast[name_idx]
player_summary$first = roster_name$first[name_idx]
player_summary$last = roster_name$last[name_idx]

### And we can look at the sedins again for comparison
subset(player_summary, last == "SEDIN")

