library(dplyr)
library(tidyr)
library(googlesheets)
library(stringr)


sheet_key <- "1BB5kKPN63IO4zfpnIoO4jrRYl0PUC6VPLRqO9IeQww0"
ss <- googlesheets::gs_key(sheet_key)
history <- ss %>% gs_read("History")

getWeekView <- function(){
    currentWeek <- ss %>% gs_read(gs_ws_ls(ss)[3],
                                  range=cell_limits(c(2,1),c(17,15)))
    currentWeek
}

getWeekSummary <- function(week){
    summary <- gather(week, key=Day,value=Points, WED, THU, FRI, SAT, SUN, MON, TUE) %>%
        group_by(Name) %>%
        summarize(GamesRemaining = sum(str_count(Points,pattern='[A-Z]{3}'),na.rm=T),
                  TotalGames = sum(!is.na(Points)),
                  Zeros =sum(Points==0,na.rm=T),
                  Singles = sum(Points == 1, na.rm=T))
    currentWeekPoints <- group_by(week,Name) %>% summarize(Points = sum(Total))
    summary$Points <- currentWeekPoints$Points
    summary$ProjPts <- summary$GamesRemaining*0.74 + summary$Points
    summary
}

monteCarloSimulationCurrent <- function(numSims=1000){
    summary <- getWeekView()
    summary <- getWeekSummary(summary)
    poisLambda <- 0.78
    simulation <- data.frame(Owner = c("Jumbo","Victor","Will"),
                             wins = c(0,0,0),
                             Zeros = summary$Zeros,
                             Singles = summary$Singles)
    
    completeTies <- 0
    gamesTies <- 0
    zerosTies <- 0
    pointsTies <- 0
    for(j in 1:numSims){
        sim <- summary
        players <- 1:nrow(summary)
        for(player in players){
            pts <- rpois(summary$GamesRemaining[player],poisLambda)
            sim$Points[player] <- sum(pts) + summary$Points[player]
            sim$Zeros[player] <- sim$Zeros[player] + sum(pts==0)
            sim$Singles[player] <- sim$Singles[player] + sum(pts==1)
        }
        winner <- which.max(sim$Points)
        ties <- which(sim$Points==max(sim$Points))
        if(length(ties) > 1){
            pointsTies <- pointsTies + 1
            if(length(unique(sim$TotalGames[ties])) == 1){
                gamesTies <- gamesTies + 1
                if(length(unique(sim$Zeros[ties])) == 1){
                    zerosTies <- zerosTies + 1
                    if(length(unique(sim$Singles[ties])) == 1){
                        completeTies <- completeTies + 1
                        next
                    }
                    else{
                        winner <- ties[which.min(sim$Singles[ties])]
                    }
                }
                else{
                    winner <- ties[which.min(sim$Zeros[ties])]
                }
            }
            else{
                winner <- ties[which.min(sim$TotalGames[ties])]
            }
        }
        simulation$wins[winner] <- simulation$wins[winner] + 1
    }
    summary$ProjWinPct <- simulation$wins*100/numSims
    message(paste("Simulation completed\n",
                  numSims,"runs completed\n",
                  pointsTies,"points Ties\n",
                  gamesTies,"games Ties\n",
                  zerosTies,"zeros Ties\n",
                  completeTies,"complete ties"))
    summary
}

monteCarloTest <- function(numRuns,simSize=1000){
    simulations <- data.frame(Jumbo = numeric(), Victor = numeric(),Will = numeric())
    for(run in 1:numRuns){
        sim <- monteCarloSimulation(summary,simSize)
        simulations <- rbind(simulations,sim$ProjWinPct)
        message(run)
    }
    simulations
}

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
    playerGames <- gather(history, key=Day,value=Points, WED, THU, FRI, SAT, SUN, MON, TUE, WED2)
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
    pickStats$PrevPlace <- c(1,2,3,3,2,1,1,2,3,3,2,1,1,2,3)
    pickStats
}

getPrevFinishStats <- function(){
    prevFinish <- getPickStats(history)
    prevFinish <- prevFinish %>% group_by(PrevPlace) %>% summarize(Games = sum(Games),Points = sum(Points))
    prevFinish <- prevFinish %>% mutate(PPG = Points/Games)
}

