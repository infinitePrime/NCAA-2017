
setwd("C:/Users/moranry/Desktop")
library(data.table)

RSDW <- fread("RegularSeasonDetailedResults2016Winners.csv")
RSDL <- fread("RegularSeasonDetailedResults2016.csv")
# GameID is needed so I can average Ken Pom stuff by game later
RSDL$GameID <- seq(from = 1, to = nrow(RSD), by = 1)
RSDW$GameID <- seq(from = 1, to = nrow(RSD), by = 1) 
names(RSDL)
grep("^W", names(RSDL), value=TRUE) # column names starting with "w"
grep("^L", names(RSDL), value=TRUE) # column names starting with "l"
setnames(RSDW, old = c("Wloc"),new = c("Tloc"))
setnames(RSDL, old = c("Wloc"),new = c("Tloc"))  # I do this so wloc won't get edited in a few
# wloc this identifies the "location" of the winning team. 
# If the winning team was the home team, this value will be "H"
# If the winning team was the visiting team, this value will be "A". 
# If it was played on a neutral court, then this value will be "N".

#######################################################################
## each row is for 1 game with "w" in front of the winner info and l in front of the loser
## I will make a dataset were each team has one record for each game played 

###################################################################################################
########## make separate datasets for winners and losers  then rename the columns #################


RSDWnames <- names(RSDW);RSDWnames
RSDLnames <- names(RSDL);RSDLnames
# Winners first

RSDWnames <- gsub("^W", "", RSDWnames);
RSDWnames <- gsub("^L", "O", RSDWnames)
# "^" means I only want gsub to match if the word starts with "w" or "l"
colnames(RSDW) <- RSDWnames
setnames(RSDW, old = c("Tloc"),new = c("loc"))
RSDW$win <- 1 # add a win/loss indicator

# now losers
RSDLnames <- gsub("^L", "", RSDLnames);RSDLnames <- gsub("^W", "O", RSDLnames)
colnames(RSDL) <- RSDLnames
RSDL <- RSDL[Tloc == "A",loc := "H"][Tloc == "H", loc := "A"][Tloc == "N", loc := "N"]
RSDL$Tloc <- NULL
RSDL$win <- 0 # add a win/loss indicator
nrow(RSDL)
RSD <- rbind(RSDW,RSDL)
# clean up the trash
RSDL <- NULL; RSDW <- NULL; RSDWnames <- NULL; RSDLnames <- NULL; gc()
# Possessions = FGA-OR+TO+.475*FTA
RSD <- RSD[,tPos := fga - or + to + .475*fta]
RSD <- RSD[,OPos := Ofga - Oor + Oto + .475*Ofta]
RSD <- RSD[,Poss := (tPos+OPos)/2]

names(RSD)
write.csv(RSD,"Regular Season Basics for each team.csv",row.names = F)


Full <- RSD[Season >= 2003] # removing regular season data prior to 2003 because detailed tournament data only goes back to 2003

Full <- Full[order(Season, team, Daynum)]
Full <- Full[,Game := 1:.N, by = .(Season, team)] # Game will tell us if it's the 1, 2, ... or nth game of the season for a team
max(Full$Game)





###
# Used to create a data file that lists each teams ... ny season
# conference
# D1School - just in case
# Seed - will be NA if the team does not make it to the tournament
# Region - will be NA if the team does not make it to the tournament
# Players.Lost.Last.Year - Players lost to NBA draft


Conferences <- fread("Team Conferences and D1 2017.csv")
Seed.Region <- fread("Season.Team.Region.Seed.2016.csv")
#Draft <- fread("C:/Users/Ryan/Documents/Kaggle/March Maddness/Additional Data/College.Players.Lost.to.NBA.Draft.2002.2016.csv")

# Detailed tournament results are not available prior to 2003 so we remove these  
head(Conferences);Conferences <- Conferences[season >= 2003]
head(Seed.Region);Seed.Region <- Seed.Region[Season >= 2003]
#head(Draft); Draft <- Draft[College.TeamID > 0] # if the team ID == 0 then the player was either drafter from highschool or outside the country

# Create a unique variable to merge with 
#Draft$ST <- paste(Draft$Loss.Effect.Season,Draft$College.TeamID, sep = "_")
Conferences$ST <- paste(Conferences$season,Conferences$team_id, sep = "_")
Seed.Region$ST <- paste(Seed.Region$Season,Seed.Region$Team, sep = "_")

# select only the variables we want in the final data set
Conferences <- Conferences[,.(ST, conference, D1School)] 
Seed.Region <- Seed.Region[,.(ST, Seed, Region)]
#Draft <- Draft[, .(ST, Players.Lost)]
#setnames(Draft, old = c("Players.Lost"),new = c("Players.Lost.Last.Year"))

setkey(Conferences,ST)
setkey(Seed.Region,ST)
#setkey(Draft,ST)
nrow(Conferences); nrow(Seed.Region); #nrow(Draft)

Season.Team.Data <- merge(Conferences, Seed.Region, all=TRUE); nrow(Season.Team.Data)
#setkey(Season.Team.Data,ST)
#Season.Team.Data <- merge(Season.Team.Data, Draft, all=TRUE); nrow(Season.Team.Data)

sum(is.na(Season.Team.Data$conference)) # this are likely teams that are no longer D1, or weren't D1 teams during the draft

write.csv(Season.Team.Data,"SeasonTeamData.csv", row.names = F)




















### Merge Season Team Data
Season.Team.Data <- fread("SeasonTeamData.csv")
setkey(Season.Team.Data, ST)

Full$ST <- paste(Full$Season,Full$team ,sep = "_")
setkey(Full, ST)
Full <- merge(Full, Season.Team.Data, all.x = TRUE)
Full$ST <- paste(Full$Season,Full$Oteam ,sep = "_")
setkey(Full, ST)
#colnames(Season.Team.Data) <- c("ST", "Oconference", "OD1School", "OSeed", "ORegion", "OPlayers.Lost.Last.Year")
colnames(Season.Team.Data) <- c("ST", "Oconference", "OD1School", "OSeed", "ORegion")
setkey(Season.Team.Data, ST)
Full <- merge(Full, Season.Team.Data, all.x = TRUE)
sum(is.na(Full))

Full <- Full[ , In.Conf := ifelse(conference == Oconference, 1, 0)]

sum(Full$In.Conf)

write.csv(Full,"Conf_RSD.csv", row.names = F)



Full <- fread("Conf_RSD.csv")

Full <- Full[In.Conf == 1] 


write.csv(Full,"In.Con RSD.csv", row.names = F)

InCon <- fread("In.Con RSD.csv")

InCon <- InCon[,ORteam.game := 100*score/Poss]
InCon <- InCon[,DRteam.game := 100*Oscore/Poss]
head(InCon)
InConTS <- InCon[, .(Con.Games = .N, Cwins = sum(win),AvScore = mean(score), AvOscore = mean(Oscore), 
                     AORteam = mean(ORteam.game), ADRteam = mean(DRteam.game)), by = .(Season,conference, team)]
InConTS <- InConTS[, WinP := Cwins/Con.Games]

InConTS <- InConTS[,Pyth.Score := (AvScore^11.5)/((AvScore^11.5)+(AvOscore^11.5))]

InConTS <- InConTS[,Pyth.ORDR := (AORteam^11.5)/((AORteam^11.5)+(ADRteam^11.5))]
Con.Season <- InConTS[, .(ConAvScore = mean(AvScore), ConSdAvScore = sd(AvScore),
                          ConAvOscore = mean(AvOscore), ConSdAvOscore = sd(AvOscore),
                          ConAORteam = mean(AORteam), ConSdAORteam = sd(AORteam),
                          ConADRteam = mean(ADRteam), ConSdADRteam = sd(ADRteam),
                          
                          ConPythPoints = mean(Pyth.Score), ConSdPythPoints = sd(Pyth.Score),
                          ConPythORDR = mean(Pyth.ORDR), ConSdPythORDR = sd(Pyth.ORDR)
                          ), by = .(Season,conference)]



                           
   
                         
                         


write.csv(Con.Season,"By Conference stuff.csv", row.names = F)
write.csv(InConTS,"In Conference stuff.csv", row.names = F)

InConTS$SC <- paste(InConTS$Season,InConTS$conference ,sep = "_")
Con.Season$SC <- paste(Con.Season$Season,Con.Season$conference ,sep = "_")

setkey(Con.Season,SC)
setkey(InConTS, SC)

Conf <- merge(InConTS, Con.Season, all.x = TRUE)
head(Conf)
Conf <- Conf[,IndScore := ifelse( AvScore <= ConAvScore + 2*ConSdAvScore & AvScore >= ConAvScore - 2*ConSdAvScore , 1, 0)]
Conf <- Conf[,IndOScore := ifelse( AvOscore <= ConAvOscore + 2*ConSdAvOscore & AvOscore >= ConAvOscore - 2*ConSdAvOscore , 1, 0)]
Conf <- Conf[,Ind := IndScore + IndOScore]


Conf <- Conf[,Ind.Pyth.Score := ifelse( Pyth.Score <= ConPythPoints + 2*ConSdPythPoints & Pyth.Score >= ConPythPoints - 2*ConSdPythPoints , 1, 0)]
Conf <- Conf[,Ind.Pyth.ORDR := ifelse( Pyth.ORDR <= ConPythORDR + 2*ConSdPythORDR & Pyth.ORDR >= ConPythORDR - 2*ConSdPythORDR , 1, 0)]
Conf <- Conf[,Ind.P := Ind.Pyth.Score + Ind.Pyth.ORDR]

Conf <- Conf[,Ind.Pyth.Score := ifelse( Pyth.Score <= ConPythPoints + 2*ConSdPythPoints & Pyth.Score >= ConPythPoints - 2*ConSdPythPoints , 1, 0)]
Conf <- Conf[,Ind.Pyth.ORDR := ifelse( Pyth.ORDR <= ConPythORDR + 2*ConSdPythORDR & Pyth.ORDR >= ConPythORDR - 2*ConSdPythORDR , 1, 0)]
Conf <- Conf[,Ind.P := Ind.Pyth.Score + Ind.Pyth.ORDR]

teams <- fread("TeamSpellings.csv")
setkey(teams, team)

setkey(Conf, team)

Conf <- merge(Conf, teams, all.x = TRUE)

write.csv(Conf, "Con all.csv", row.names = F)


library(ggplot2)



OneConf <- Conf[conference.x == "acc"]
OneConf <- OneConf[  Season.x == 2015]
p <- ggplot(OneConf, aes(Pyth.Score,Pyth.ORDR))
p +  geom_point(aes(colour = factor(Ind.P)))

OneYear <- Conf[  Season.x == 2016]
OneYear[AvScore > 85]
OneYear[AvOscore > 85]
plot(OneYear$AvOscore)
plot(OneYear$Pyth.Score)
p <- ggplot(OneYear, aes(AvScore, AvOscore))
p +  geom_point(aes(colour = factor(Ind)))



p <- ggplot(InConTS, aes(AvScore, AvOscore))
p +  geom_point(aes(colour = factor(conference)))+facet_grid(. ~ Season)

unique(InConTS$conference)

p <- ggplot(InConTS, aes(factor(conference), Pyth.ORDR))

p +  geom_point(aes(colour = factor(team)))+facet_grid(. ~ Season)


p <- ggplot(InConTS, aes(AvScore, AvOscore))
p +  geom_point(aes(colour = factor(conference)))+facet_grid(. ~ Season)

p <- ggplot(Con.Season, aes(ConAvScore, ConAvOscore))
p +  geom_point(aes(colour = factor(conference)))+facet_grid(. ~ Season)

OneConf <- InConTS[conference == "acc"]
OneConf <- InConTS[ Season == 2015]
plot(OneConf$Pyth.ORDR)
p <- ggplot(OneConf, aes(AvScore, AvOscore))
p +  geom_point(aes(colour = factor(team)))

p <- ggplot(OneConf, aes(Pyth.ORDR, Pyth.Score))
p +  geom_point(aes(colour = factor(team)))

+facet_grid(. ~ Season)







