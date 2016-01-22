library(data.table)

# Urls to find player and team stats
statsUrl = "http://sports.yahoo.com/nhl/stats/byposition?pos=C,RW,LW,D&conference=NHL&year=season_2015&qualified=1"
teamStatsURL = "http://www.hockey-reference.com/leagues/NHL_2016.html"
injuryUrl = "http://www.donbest.com/nhl/injuries/"

# loadStatsUrl loads an html table from a specified url into a dataframe
# by default, it will load the biggest table on the url
# by changing the table parameter, you can choose a specific table by name
loadStatsUrl <- function(url,table=""){
    tables = 0
    #while(length(tables) < 2){
        tables <- readHTMLTable(url,stringsAsFactors=F)
    #}  
    n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
    if(table != ""){
        tables[[c(table)]]
    }
    else{
        tables[[which.max(n.rows)]]
    }
}

# cleanPlayers cleans the raw yahoo stats to be usable for analysis
cleanPlayers <- function(players){
    # clean players DF
    names(players)[1] = "Name"
    names(players)[2] = "yTeam"
    players$Name = substring(players$Name,3)
    players = players[c("Name","yTeam","GP","G","A","Pts","SOG")]
    players[3:7] = lapply(players[3:7],as.character)
    players[3:7] = lapply(players[3:7],as.numeric)
    players
}

# cleanTeamStats pulls the relevant team data for opponent analysis
cleanTeamStats <- function(teams){
    #adjust team stats DF
    teams = teams[c("Team","GP","GA")]
    teams[2:3] = lapply(teams[2:3], as.character)
    teams[2:3] = lapply(teams[2:3], as.numeric)
    teams
}

# return the laste completed week of the fantasy season on a date
fantasyWeekFind <- function(date,schedule=fantasySchedule){
    week = which(fantasySchedule$Ends >= as.Date(date))[1]
    week
}

# DEPRECATED: returns a basic hockey-reference ID for the player based on their
# first and last name. replaced by the dataframe generated from loadHRIDs
hockeyRefID <- function(name){
    name = unlist(strsplit(name," "))
    ID = paste(substr(name[2],1,5),substr(name[1],1,2),"01",sep="")
    ID
}

# pulls the gamelog for a playerID from hockey-reference
loadPlayerLog <- function(ID,year="2016"){
    #name = unlist(strsplit(name," "))
    url = paste("http://www.hockey-reference.com/players/",ID[1],"/",ID,"/gamelog/",year,"/",
        sep="")
    playerLog = loadStatsUrl(url)
    playerLog = subset(playerLog, Age != "Age")
    playerLog$Date = as.Date(playerLog$Date)
    playerLog$PTS = as.numeric(as.character(playerLog$PTS))
    playerLog
}


# function to pull all hockey-reference player IDs from the alphabetical lists of all
# skaters. used once and then written to a csv file.
loadHRIDs <- function(){
    Name = ""
    ID = 0
    hrdf <- data.frame(Name,ID)
    for(letter in letters){
        if(letter == 'x'){
            next
        }
        doc = htmlParse(paste("http://www.hockey-reference.com/players/",letter,"/skaters.html",sep=""))
        Name = getNodeSet(doc,"//table[@id='skaters']//td[3][.='2016']
                                  /preceding-sibling::td[2]/strong/a/text()")
        Name = sapply(Name, xmlValue)
        ID = unlist(getNodeSet(doc,"//table[@id='skaters']//td[3][.='2016']
                                  /preceding-sibling::td[2]/strong/a/@href"))
        #Name = as.vector(unlist(Name))
        ID = as.vector(ID)
        temp = data.frame(Name,ID)
        hrdf = rbind(hrdf,temp)
    }
    hrdf$ID = substr(hrdf$ID,12,20)
    hrdf$ID = gsub("[.]","",hrdf$ID)
    hrdf
}

# Returns a vector with a player's name, and each weekly point total
playerWeeklyReport <- function(playerName,playerID,week){
    playerLog = loadPlayerLog(playerID)
    weeks = 1:week
    points = numeric(0)
    for(i in weeks){
        temp = subset(playerLog, Date >= fantasySchedule[i,1] & Date <= fantasySchedule[i,2])
        points = c(points,sum(temp$PTS))  
    }
    report = cbind(playerName,weeks,points)
    data.frame(report)
}

# returns a dataframe with every player and the points theyve gotten in each fantasy week
# SHOULD MAKE THIS DF TIDY, WEEKS ARE VARIABLES
allPlayerWeeklyReport <- function(){
    players <- read.csv("playerIDs.csv",stringsAsFactors = F)
    wk = fantasyWeekFind(Sys.Date())
    for(ID in 1:nrow(players)){
        playerReport = playerWeeklyReport(players$Name[ID],players$ID[ID],wk)
        if(ID == 1){
            report = playerReport
        }
        else{
            report = rbind(report,playerReport)
        }
        print(ID)
    }
    report$weeks = as.numeric(as.character(report$weeks))
    report$points = as.numeric(as.character(report$points))
    names(report) = c("Name","Week","Points")
    report
}
