setwd("C:/Users/Ryan/Documents/Kaggle/March Maddness/For 2016-2017/PriorToStart/Kaggle2016Data")
library(data.table)
RS <- fread("Regular Season Basics for each team.csv")
RS$Reg.Season <- 1
TG <- fread("Tournament Basics for each team and game.csv")
TG$Reg.Season <- 0

length(names(RS))
length(names(TG))

RS$G.round <- 0 # G.round indicates which round of the tournament a game is played in.
# therefore, regular season games should have a G.round of 0 

RS <- RS[Season >= 2003] # removing regular season data prior to 2003 because detailed tournament data only goes back to 2003

# Combine RS and TG
Full <- rbind(RS, TG)

Full <- Full[order(Season, team, Daynum)]
Full <- Full[,Game := 1:.N, by = .(Season, team)] # Game will tell us if it's the 1, 2, ... or nth game of the season for a team
max(Full$Game)

Full <- Full[,adjscore := score*100/GameTempo] # Ken Pom stuff
Full <- Full[,adjOscore := Oscore*100/GameTempo] # Ken Pom stuff

# this is used so that KP numbers are updated after every game
Full <- Full[, Cum.adjScore := cumsum(adjscore), by = .(Season, team)] # Ken Pom stuff
Full <- Full[, Cum.adjOScore := cumsum(adjOscore), by = .(Season, team)] # Ken Pom stuff
Full <- Full[, Cum.Tempo := cumsum(GameTempo), by = .(Season, team)] # Ken Pom stuff

Full <- Full[, AdjO := Cum.adjScore/Game] # Ken Pom stuff
Full <- Full[, AdjD := Cum.adjOScore/Game] # Ken Pom stuff
Full <- Full[, Adj.Tempo := Cum.Tempo/Game] # Ken Pom stuff
Full <- Full[, Pyth  := (AdjO^11.5)/(AdjO^11.5+AdjD^11.5)]
Full$Cum.adjScore <- NULL; Full$Cum.adjOScore <- NULL; Full$Cum.Tempo <- NULL

### Merge Season Team Data
Season.Team.Data <- fread("SeasonTeamData.csv")
setkey(Season.Team.Data, ST)

Full$ST <- paste(Full$Season,Full$team ,sep = "_")
setkey(Full, ST)
Full <- merge(Full, Season.Team.Data, all.x = TRUE)
Full$ST <- paste(Full$Season,Full$Oteam ,sep = "_")
setkey(Full, ST)
colnames(Season.Team.Data) <- c("ST", "Oconference", "OD1School", "OSeed", "ORegion", "OPlayers.Lost.Last.Year")
setkey(Season.Team.Data, ST)
Full <- merge(Full, Season.Team.Data, all.x = TRUE)
Full$ST <- NULL

# not needed for conference
# write.csv(Full, "Most Info at this time - Reg Season and Tourn Game Data by team.csv", row.names = F)


#select variable needed for conference strength calcs

New.Conf <- Full[, .(Season, Daynum, team, score, Oteam, Oscore, loc, GameID, win,
                    G.round, Game, conference, D1School, Seed, Region, 
                    Oconference, OD1School,  OSeed, ORegion, Reg.Season)]

New.Conf <- New.Conf[order(Season,conference,Daynum)]
New.Conf <- New.Conf[ , Conf.Game := 1:.N, by = .(Season, conference)]

New.Conf <- New.Conf[ , In.Conf := ifelse(conference == Oconference, 1, 0)]
# remove tournament games?


write.csv(New.Conf,"For Conf strength calc.csv", row.names = F)

####
#    !!!!!!!!!!!!!!
### normalize the numbers before calculating stuff
### norm by season ? norm by team?... not sure






