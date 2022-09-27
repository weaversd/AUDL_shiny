library(shiny)

ui <- navbarPage("AUDL",
                 tabPanel("Historical Data",
                          tableOutput("historical_games"))
)