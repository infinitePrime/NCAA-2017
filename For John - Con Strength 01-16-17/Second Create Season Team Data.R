setwd("C:/Users/Ryan/Documents/Kaggle/March Maddness/For 2016-2017/PriorToStart/Kaggle2016Data")
library(data.table)

###
# Used to create a data file that lists each teams ... ny season
# conference
# D1School - just in case
# Seed - will be NA if the team does not make it to the tournament
# Region - will be NA if the team does not make it to the tournament
# Players.Lost.Last.Year - Players lost to NBA draft


Conferences <- fread("C:/Users/Ryan/Documents/Kaggle/March Maddness/Team Conferences and D1 2017.csv")
Seed.Region <- fread("C:/Users/Ryan/Documents/Kaggle/March Maddness/For 2016-2017/Season.Team.Region.Seed.2016.csv")
Draft <- fread("C:/Users/Ryan/Documents/Kaggle/March Maddness/Additional Data/College.Players.Lost.to.NBA.Draft.2002.2016.csv")

# Detailed tournament results are not available prior to 2003 so we remove these  
head(Conferences);Conferences <- Conferences[season >= 2003]
head(Seed.Region);Seed.Region <- Seed.Region[Season >= 2003]
head(Draft); Draft <- Draft[College.TeamID > 0] # if the team ID == 0 then the player was either drafter from highschool or outside the country

# Create a unique variable to merge with 
Draft$ST <- paste(Draft$Loss.Effect.Season,Draft$College.TeamID, sep = "_")
Conferences$ST <- paste(Conferences$season,Conferences$team_id, sep = "_")
Seed.Region$ST <- paste(Seed.Region$Season,Seed.Region$Team, sep = "_")

# select only the variables we want in the final data set
Conferences <- Conferences[,.(ST, conference, D1School)] 
Seed.Region <- Seed.Region[,.(ST, Seed, Region)]
Draft <- Draft[, .(ST, Players.Lost)]
setnames(Draft, old = c("Players.Lost"),new = c("Players.Lost.Last.Year"))

setkey(Conferences,ST)
setkey(Seed.Region,ST)
setkey(Draft,ST)
nrow(Conferences); nrow(Seed.Region); nrow(Draft)

Season.Team.Data <- merge(Conferences, Seed.Region, all=TRUE); nrow(Season.Team.Data)
setkey(Season.Team.Data,ST)
Season.Team.Data <- merge(Season.Team.Data, Draft, all=TRUE); nrow(Season.Team.Data)

sum(is.na(Season.Team.Data$conference)) # this are likely teams that are no longer D1, or weren't D1 teams during the draft

write.csv(Season.Team.Data,"SeasonTeamData.csv", row.names = F)