library(dplyr)
library(tidyr)
library(googlesheets)


sheet_key <- "1BB5kKPN63IO4zfpnIoO4jrRYl0PUC6VPLRqO9IeQww0"
ss <- googlesheets::gs_key(sheet_key)
history <- ss %>% gs_read("History")

getOwnerSeason <- function(history){
    ownerTotals <- group_by(history,Name) %>% summarize(Games = sum(Games) ,Points = sum(Total)) %>% arrange(desc(Points))
    ownerTotals <- mutate(ownerTotals, PtsPerG = Points/Games)
    ownerTotals 
}

getOwnerWeeks <- function(history){
    ownerWeeks <- group_by(history,Week, Name) %>% summarize(Games = sum(Games),Points = sum(Total))
    ownerWeeks <- ungroup(ownerWeeks)
    ownerWeeks <- arrange(ownerWeeks,desc(Points))
    ownerWeeks
}

getPlayerSeason <- function(history){
    playerTotals <- group_by(history, Pick) %>% mutate(TimesPicked = n()) 
    playerTotals <- summarize(playerTotals,TimesPicked = max(TimesPicked),Points = sum(Total), MVPs = sum(MVP,na.rm=T))
    playerTotals <- mutate(playerTotals, PtsPerWeek = Points/TimesPicked)
    playerTotals <- arrange(playerTotals, desc(Points))
    playerTotals
}

getPlayerWeeks <- function(history){
    playerWeeks <- select(history, Pick,Week,Owner = Name,Games,Total,MVP) %>% arrange(desc(Total))
    playerWeeks
}

getPlayerGames <- function(history){
    playerGames <- gather(history, key=Day,value=Points, WED, THU, FRI, SAT, SUN, MON, TUE)
    playerGames <- filter(playerGames,!is.na(Points))
    playerGames <- select(playerGames, Pick,Week,Day,Points)
    playerGames <- arrange(playerGames, desc(Points))
    playerGames
}

getTeamSeason <- function(history){
    teamTotals <- group_by(history, Team) %>% mutate(TimesPicked = n())
    teamTotals <- summarize(teamTotals, TimesPicked = max(TimesPicked), Points = sum(Total))
    teamTotals <- mutate(teamTotals, PtsPerWeek = Points/TimesPicked)
    teamTotals <- arrange(teamTotals, desc(TimesPicked))
    teamTotals
}

getPickStats <- function(history){
    pickStats <- group_by(history,Pick.)
    pickStats <- summarize(pickStats, Round = max(Round), Games = sum(Games), Points = sum(Total))
    pickStats
}
