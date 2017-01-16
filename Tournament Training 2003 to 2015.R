setwd("C:/Users/Ryan/Documents/Kaggle/March Maddness/For 2016-2017/PriorToStart/Kaggle2016Data")
library(data.table)
RSD <- fread("TourneyDetailedResults2016.csv")

RSD$GameID <- seq(from = 1, to = nrow(RSD), by = 1) 
# GameID will use this to randomly mix winners as the first team and losers the second team

names(RSD)
grep("^W", names(RSD), value=TRUE) # column names starting with "w"
grep("^L", names(RSD), value=TRUE) # column names starting with "l"

RSD <- RSD[order(Season,Wteam,Daynum)]
RSD <- RSD[,G.round := 1:.N, by =.(Season, Wteam)]
min(RSD$G.round);max(RSD$G.round)

RSD <- RSD[,.(Season, Daynum, Wteam, Wscore, Lteam, Lscore, G.round,GameID)]


#####################################################################################
########  Create a tournament set for binary Win/Lose training and testing
set.seed(1234)
X <- sample(x = RSD$GameID, .5*nrow(RSD), replace = FALSE)

RSDW <- RSD[GameID %in% X]
RSDL <- RSD[!(GameID %in% X)]


# Winners first
RSDWnames <- names(RSDW)
RSDWnames <- gsub("^W", "", RSDWnames);RSDWnames <- gsub("^L", "O", RSDWnames)
# "^" means I only want gsub to match if the word starts with "w" or "l"
colnames(RSDW) <- RSDWnames
RSDW$win <- 1 # add a win/loss indicator

# now losers
RSDLnames <- names(RSDL)
RSDLnames <- gsub("^L", "", RSDLnames);RSDLnames <- gsub("^W", "O", RSDLnames)
colnames(RSDL) <- RSDLnames
RSDL$win <- 0 # add a win/loss indicator

nrow(RSD)
RSD <- rbind(RSDW,RSDL)
# clean up the trash
RSDL <- NULL; RSDW <- NULL; RSDWnames <- NULL; RSDLnames <- NULL; gc()
names(RSD)

write.csv(RSD,"Starter Tournament Training Testing.csv",row.names = F)



























