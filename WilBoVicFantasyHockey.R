library(XML)
library(plyr)
library(ggplot2)

# Set working directory and cleanup the environment
setwd("C:/Users/Netsky/Dropbox/Code/Fantasy Hockey")
rm(list = ls())

source("loadStats.R")

# Urls to find player and team stats
statsUrl = "http://sports.yahoo.com/nhl/stats/byposition?pos=C,RW,LW,D&conference=NHL&year=season_2015&qualified=1"
teamStatsURL = "http://www.hockey-reference.com/leagues/NHL_2016.html"
injuryUrl = "http://www.donbest.com/nhl/injuries/"
 
# Shooting percentages by position
fwdPct = 0.1082
defPct = 0.0523
    
players = loadStatsUrl(statsUrl)
players = cleanPlayers(players)
teams = loadStatsUrl(teamStatsURL,"teams")
teams = cleanTeamStats(teams)                       

lgAvg = subset(teams, Team == "League Average")
lgAvg$GAperG = lgAvg$GA/lgAvg$GP
teams = merge(teams,read.csv("teamNames.csv"),by="Team")

#import vukota projections
overall = read.csv("Vukota.csv")
overall = subset(overall[1:8] , Pos != "G")
overall = merge(players,overall,by="Name",all.x=T)

#start analysis
overall$pPtsPerG = overall$pPts/overall$pGP
overall$PtsPerG = overall$Pts/overall$GP
overall$ShootPct = overall$G/overall$SOG

#separate D and F to do separate shooting percentage adjustments
defense = subset(overall, Pos == "D")
forwards = subset(overall, Pos == "F")
missing = subset(overall,is.na(overall$Pos))
defense$regG = defense$SOG * defPct
forwards$regG = forwards$SOG * fwdPct
missing$regG = missing$G
forwards = rbind(forwards,missing)
overall = rbind(forwards,defense)
overall$regPts = overall$regG + overall$A
overall$regPtsPerG = overall$regPts/overall$GP

#team analysis
teams[2:3] = lapply(teams[2:3], as.character)
teams[2:3] = lapply(teams[2:3], as.numeric)
teams$GAperG = teams$GA/teams$GP
teams$GACoeff = teams$GAperG/lgAvg$GAperG

# import schedule
NHLSchedule = read.csv("NHLSchedule.csv")
NHLSchedule$WCTime = NULL

#take input for what week it is
# repeat{
#   weekInput = readline("enter week #: ")
#   if(weekInput > 0 && weekInput < 27){
#     break
#   }
# }
#automated week input
fantasySchedule = read.csv("fantasySchedule.csv")
fantasySchedule$Starts = as.Date(as.character(fantasySchedule$Starts), "%m/%d/%Y")
fantasySchedule$Ends = as.Date(as.character(fantasySchedule$Ends),"%m/%d/%Y")
weekInput = which(fantasySchedule$Ends >= Sys.Date())[1]
weekInput = weekInput+1
NHLSchedule = subset(NHLSchedule, Week==weekInput)

#find opponents
teamNames = as.vector(teams$Team)
teamOpponents = sapply(teamNames, function(x) setdiff(as.vector(t(NHLSchedule[which(NHLSchedule$Away == x | NHLSchedule$Home == x),c("Home", "Away")] )),x))

# calculate number of games in a week
teams$numGames = sapply(teamOpponents, length)

# find mean gaCoeff of opponents
teamOpponents = lapply(teamOpponents, function(x) laply(x,function(x) teams$GACoeff[teams$Team == x]))
teams$oppCoeff = sapply(teamOpponents,mean)

# add team based statistics to skater rows
overall = merge(overall,teams[c("yTeam","numGames","oppCoeff")], by.x="yTeam", by.y = "yTeam", all.x=T)

# remove players that are inactive
inactives = read.csv("inactives.csv")
inactives = subset(inactives, Return > weekInput, select=c(Player))
overall = overall[!overall$Name %in% inactives$Player,]

# remove players that are injured
injuries = loadStatsUrl(injuryUrl,1)
injuries = injuries$V3[injuries$V3 != "Player" & !is.na(injuries$V3)]
overall = overall[!overall$Name %in% injuries,]

# make projections for the week
overall$ActualProjPts = overall$PtsPerG*overall$numGames
overall$RegProjPts = overall$regPtsPerG*overall$numGames*overall$oppCoeff
overall$vukProjPts = overall$pPtsPerG*overall$numGames*overall$oppCoeff
overall$vukProjPts[is.na(overall$vukProjPts)] = 0
overall$pPtsPerG[is.na(overall$pPtsPerG)] = 0
sampleSize = 41
overall$BayesProjPts = ((sampleSize*overall$pPtsPerG + overall$regPts)/(sampleSize+overall$GP))*overall$numGames*overall$oppCoeff

#overall <- overall[order(-overall$BayesProjPts),]
#overall <- 

# kmeans clustering
kmeansFit <-kmeans(overall[c("RegProjPts","vukProjPts")], 50)
overall <- data.frame(overall,as.factor(kmeansFit$cluster))

overall <- overall[order(-overall$BayesProjPts),]
ggplot(overall[1:50,],aes(RegProjPts,vukProjPts))+
  ggtitle(paste("Week ",weekInput," Projections"))+
  geom_point(aes(size=numGames, color=as.factor.kmeansFit.cluster.,shape=Pos)) +
  #coord_cartesian(xlim=c(2,5.75),ylim=c(2,5.75))  +
  geom_text(aes(label=Name,vjust=-1))

