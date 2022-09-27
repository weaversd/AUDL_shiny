library(shiny)

ui <- navbarPage("AUDL",
                 tabPanel("About",
                          includeMarkdown("Documentation.rmd")),
                 tabPanel("Historical Data",
                          fluidRow(wellPanel(
                            textInput("season_pattern", "Year Search"),
                            textInput("team_pattern", "Team Search")),
                          tableOutput("historical_games"))),
                 tabPanel("Elo Parameters",
                          fluidRow(column(3, wellPanel(h3("Game Importance"),
                                                       numericInput("regularK", "Regular Season Game Value", 30),
                                                       numericInput("playoffK", "Playoff Game Value", 40),
                                                       numericInput("semiK", "Semi Final Game Value", 50),
                                                       numericInput("finalK", "Championship Game Value", 60))),
                                   column(3, wellPanel(h3("Elo Equation Parameters"),
                                                       numericInput("startingElo", "Starting Elo Value", 1400),
                                                       numericInput("EloDenom", "Elo Denominator", 400),
                                                       numericInput("homeAdvantage", "Home Field Advantage", 100),
                                                       numericInput("MOVmultiplier", "Margin of Victory Multiplier", 1))),
                                   column(3, wellPanel(h3("Other Parameters"),
                                                       numericInput("pctRegression", "Regression % to mean after season", 20))))),
                 
                 tabPanel("Historical Elo Plot",
                          withSpinner(plotlyOutput("historicalEloPlot", height = 800))),
                 tabPanel("2023 Season Predictions",
                          "Coming Soon"),
                 tabPanel("Game Predictor",
                          "Coming Soon")
)