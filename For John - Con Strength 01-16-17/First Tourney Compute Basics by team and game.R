setwd("C:/Users/Ryan/Documents/Kaggle/March Maddness/For 2016-2017/PriorToStart/Kaggle2016Data")
library(data.table)
RSD <- fread("TourneyDetailedResults2016.csv")
# GameID is needed so I can average Ken Pom stuff by game later
RSD$GameID <- seq(from = 1, to = nrow(RSD), by = 1) 
names(RSD)
grep("^W", names(RSD), value=TRUE) # column names starting with "w"
grep("^L", names(RSD), value=TRUE) # column names starting with "l"
setnames(RSD, old = c("Wloc"),new = c("Tloc"))  # I do this so wloc won't get edited in a few

RSD <- RSD[order(Season,Wteam,Daynum)]
RSD <- RSD[,G.round := 1:.N, by =.(Season, Wteam)]

# wloc this identifies the "location" of the winning team. 
# If the winning team was the home team, this value will be "H"
# If the winning team was the visiting team, this value will be "A". 
# If it was played on a neutral court, then this value will be "N".

#######################################################################
## each row is for 1 game with "w" in front of the winner info and l in front of the loser
## I will make a dataset were each team has one record for each game played 

###################################################################################################
########## make separate datasets for winners and losers  then rename the columns #################

# Winners first
RSDW <- RSD; RSDWnames <- names(RSDW)
RSDWnames <- gsub("^W", "", RSDWnames);RSDWnames <- gsub("^L", "O", RSDWnames)
# "^" means I only want gsub to match if the word starts with "w" or "l"
colnames(RSDW) <- RSDWnames
setnames(RSDW, old = c("Tloc"),new = c("loc"))
RSDW$win <- 1 # add a win/loss indicator

# now losers
RSDL <- RSD; RSDLnames <- names(RSDL)
RSDLnames <- gsub("^L", "", RSDLnames);RSDLnames <- gsub("^W", "O", RSDLnames)
colnames(RSDL) <- RSDLnames
RSDL <- RSDL[Tloc == "A",loc := "H"][Tloc == "H", loc := "A"][Tloc == "N", loc := "N"]
RSDL$Tloc <- NULL
RSDL$win <- 0 # add a win/loss indicator
nrow(RSD)
RSD <- rbind(RSDW,RSDL)
# clean up the trash
RSDL <- NULL; RSDW <- NULL; RSDWnames <- NULL; RSDLnames <- NULL; gc()
# Possessions = FGA-OR+TO+.475*FTA
RSD <- RSD[,Pos := fga - or + to + .475*fta]
RSD <- RSD[,OPos := Ofga - Oor + Oto + .475*Ofta]
RSD <- RSD[,GameTempo := (Pos+OPos)/2]

names(RSD)
##############################################################################################################
## Four Factors
# http://kenpom.com/blog/four-factors/

# Effective field goal percentage => eFGp is like regular field goal percentage except that it gives 50% more credit for made three-pointers.
RSD <- RSD[,eFGp := (fgm + .5*fgm3)/(fga)]
RSD <- RSD[,OeFGp := (Ofgm + .5*Ofgm3)/(Ofga)]
# Turnover percentage => is a pace-independent measure of ball security.
RSD <- RSD[,TOp := to/Pos]
RSD <- RSD[,OTOp := Oto/OPos]
# Offensive rebounding percentage => is a measure of the possible rebounds that are gathered by the offense.
RSD <- RSD[,ORp := or / (or + Odr)]
RSD <- RSD[,OORp := Oor / (Oor + dr)]
# free throw rate => captures a team's ability to get to the free throw line.
RSD <- RSD[,FTRate := fta / fga]
RSD <- RSD[,OFTRate := Ofta / Ofga]

###########################################################################################
# http://kenpom.com/blog/stats-explained/

# There are other team stats that are less important than the Four Factors, with the common approach of converting the standard 
# per-game stats to per-opportunity.

# Assist Rate 
RSD <- RSD[,AssistRate := ast / fgm]
RSD <- RSD[,OAssistRate := Oast / Ofgm]

# Block Rate => Blocked shots / Opp. 2PA
RSD <- RSD[,BlockRate := blk / (Ofga - Ofga3)]
RSD <- RSD[,OBlockRate := Oblk / (fga - fga3)]

# Steal Rate => Steals / Defensive possessions
RSD <- RSD[,StealRate := stl / OPos]
RSD <- RSD[,OStealkRate := Ostl / Pos]

# True shooting percentage (TS%) => Points scored / ( 2* (FGA + 0.475*FTA) )
RSD <- RSD[,TrueShootp := score / (2*(fga + 0.475*fta))]
RSD <- RSD[,OTrueShootp := Oscore / (2*(Ofga + 0.475*Ofta))]



write.csv(RSD,"Tournament Basics for each team and game.csv",row.names = F)


























Tourn <- RSD[,.(rounds.won = sum(win), rounds.played = .N), by = .(Season, team)]
write.csv(Tourn, "Rounds won and played by team and season.csv", row.names = F)
Tourn.days <- RSD[,.(min.day.num = min(Daynum), max.day.num = max(Daynum), games.per.day = ), by = .(Season)]
write.csv(Tourn.days, "first and last tournament daynums by season.csv", row.names = F)
Tourn.teams <- RSD[,.(teams.per.day = .N), by = .(Season, Daynum)]
write.csv(Tourn.teams, "number of teams that play each day.num by season.csv", row.names = F)





























