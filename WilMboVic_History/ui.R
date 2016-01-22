library(shiny)

shinyUI(fluidPage(
    
    titlePanel("WilMboVic Fantasy Hockey History and Records"),
    
    navlistPanel(
        "Owner Stats",
        tabPanel("Season Stats",
                 DT::dataTableOutput("ownerSeason")
        ),
        tabPanel("By Week",
                 DT::dataTableOutput("ownerWeeks")
        ),
        "Player Stats",
        tabPanel("Season Stats",
                 DT::dataTableOutput("playerSeason")
        ),
        tabPanel("By Week",
                 DT::dataTableOutput("playerWeeks")
        ),
        tabPanel("By Game",
                 DT::dataTableOutput("playerGames")
        ),
        "Team Stats",
        tabPanel("Season Stats",
                 DT::dataTableOutput("teamSeason")
        ),
        "Draft Stats",
        tabPanel("Season Stats",
                 DT::dataTableOutput("pickStats"))
    )
))