library(shiny)

source("global.R")

server <- function(input, output, session) {
  historical_games <- reactive(read.csv("dataFiles/all_games_asof_2022.csv"))
  
  display_schedule <- reactive({
    temp <- historical_games()
    if (input$team_pattern != "" && input$season_pattern != "") {
      temp <- temp[(grepl(input$team_pattern, temp$awayTeam, ignore.case = T) | grepl(input$team_pattern, temp$homeTeam, ignore.case = T)) & grepl(input$season_pattern, temp$season) ,]
    } else if (input$team_pattern != "") {
      temp <- temp[(grepl(input$team_pattern, temp$awayTeam, ignore.case = T) | grepl(input$team_pattern, temp$homeTeam, ignore.case = T)),]
    } else if (input$season_pattern != "") {
      temp <- temp[grepl(input$season_pattern, temp$season) ,]
    } else {
      temp
    }
    return(temp)
  })
  
  output$historical_games <- renderTable(display_schedule())
  
  elo_starting_table <- reactive({
    temp <- historical_games()
    temp$previousHomeElo <- input$startingElo
    temp$previousAwayElo <- input$startingElo
    temp$newHomeElo <- NA
    temp$newAwayElo <- NA
    return(temp)
  })
  
  elo_calculated_table <- reactive({
    fill_elo_historical(elo_games = elo_starting_table(), homeAdv = input$homeAdvantage,
                        EloDenom = input$EloDenom, MOVmult = input$MOVmultiplier,
                        championshipK = input$finalK, semiK = input$semiK,
                        playoffK = input$playoffK, regK = input$regularK,
                        startingElo = input$startingElo, regressionMult = input$pctRegression)
  })
  
  Historical_elo_plot_object <- reactive(create_plotable_data(elo_calculated_table()))
  
  Historical_elo_plot_data <- reactive(Historical_elo_plot_object()[[1]])
  Historical_elo_plot_cutoffs <- reactive(Historical_elo_plot_object()[[2]])
  
  Historical_elo_plot <- reactive({
    ggplot(Historical_elo_plot_data(), aes(date = date)) +
      geom_vline(xintercept = Historical_elo_plot_cutoffs()) + 
      geom_line(aes(x = date_numeric, y = elo, color = team)) +
      theme_bw(base_size = 10) +
      theme(panel.grid = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      labs(x = element_blank()) +
      annotate("text", x = mean(c(Historical_elo_plot_cutoffs()[1], Historical_elo_plot_cutoffs()[2])), y = max(Historical_elo_plot_data()$elo) * 1.1, label = "2014", size = 6, color = "grey") +
      annotate("text", x = mean(c(Historical_elo_plot_cutoffs()[2], Historical_elo_plot_cutoffs()[3])), y = max(Historical_elo_plot_data()$elo) * 1.1, label = "2015", size = 6, color = "grey") +
      annotate("text", x = mean(c(Historical_elo_plot_cutoffs()[3], Historical_elo_plot_cutoffs()[4])), y = max(Historical_elo_plot_data()$elo) * 1.1, label = "2016", size = 6, color = "grey") +
      annotate("text", x = mean(c(Historical_elo_plot_cutoffs()[4], Historical_elo_plot_cutoffs()[5])), y = max(Historical_elo_plot_data()$elo) * 1.1, label = "2017", size = 6, color = "grey") +
      annotate("text", x = mean(c(Historical_elo_plot_cutoffs()[5], Historical_elo_plot_cutoffs()[6])), y = max(Historical_elo_plot_data()$elo) * 1.1, label = "2018", size = 6, color = "grey") +
      annotate("text", x = mean(c(Historical_elo_plot_cutoffs()[6], Historical_elo_plot_cutoffs()[7])), y = max(Historical_elo_plot_data()$elo) * 1.1, label = "2019", size = 6, color = "grey") +
      annotate("text", x = mean(c(Historical_elo_plot_cutoffs()[7], Historical_elo_plot_cutoffs()[8])), y = max(Historical_elo_plot_data()$elo) * 1.1, label = "2021", size = 6, color = "grey") +
      annotate("text", x = mean(c(Historical_elo_plot_cutoffs()[8], Historical_elo_plot_cutoffs()[9])), y = max(Historical_elo_plot_data()$elo) * 1.1, label = "2022", size = 6, color = "grey")
  })
  
  output$historicalEloPlot <- renderPlotly(ggplotly(Historical_elo_plot(), tooltip = c("date", "y", "colour")))
  #output$Elo_table <- renderTable(Historical_elo_plot_data())
}
