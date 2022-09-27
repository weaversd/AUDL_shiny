library(shiny)

server <- function(input, output, session) {
  historical_games <- reactive(read.csv("dataFiles/all_games_asof_2022.csv"))
  
  output$historical_games <- renderTable(historical_games())
}
