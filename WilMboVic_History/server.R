library(shiny)

source("History and Records.R")

shinyServer(function(input, output) {
    output$weekView <- DT::renderDataTable(DT::datatable(
        getWeekView(), options = list(searching=F,paging=F)
    ))
    
    output$ownerSeason <- DT::renderDataTable(DT::datatable(
        getOwnerSeason(history), options = list(searching=F, paging=F)
    ))
    
    output$ownerWeeks <- DT::renderDataTable(DT::datatable({
        getOwnerWeeks(history)
    }))
    
    output$playerSeason <- DT::renderDataTable(DT::datatable({
        getPlayerSeason(history)
    }))
    
    output$playerWeeks <- DT::renderDataTable(DT::datatable({
        getPlayerWeeks(history)
    }))
    
    output$playerGames <- DT::renderDataTable(DT::datatable(
        getPlayerGames(history)
    ))
    
    output$teamSeason <- DT::renderDataTable(DT::datatable(
       getTeamSeason(history), options = list(pageLength=30, paging=F) 
    ))
    
    output$pickStats <- DT::renderDataTable(DT::datatable(
        getPickStats(history), options = list(pageLength=15, paging=F, searching = F)
    ))
    output$prevFinishStats <- DT::renderDataTable(DT::datatable(
        getPrevFinishStats(), options = list(paging=F,searching=F)
    ))
})