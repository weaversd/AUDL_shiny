source("global.R")

url <- "https://www.backend.audlstats.com/web-api/team-game-stats?limit=20&page=1"
data <- fromJSON(url)
games <- data$stats


raw_import <- list()
for (i in 1:128) {
  url <- paste0("https://www.backend.audlstats.com/web-api/team-game-stats?limit=20&page=", i)
  games <- fromJSON(url)[["stats"]]
  raw_import[[i]] <- games
}

games <- bind_rows(raw_import)

games$date <- ""
games$home_code <- NA
games$away_code <- NA
for (i in 1:nrow(games)) {
  split <- str_split(games$gameID[i], "-")[[1]]
  games$away_code[i] <- split[4]
  games$home_code[i] <- split[5]
  games$date[i] <- paste(split[1:3], collapse = "-")
}

games$date <- as.Date(games$date)

for (i in 1:nrow(games)) {
  if (games$home_code[i] == "RAL") {
    games$home_code[i] <- "CAR"
  }
  if (games$away_code[i] == "RAL") {
    games$away_code[i] <- "CAR"
  }
  if (games$home_code[i] == "AUS") {
    games$home_code[i] <- "ATX"
  }
  if (games$away_code[i] == "AUS") {
    games$away_code[i] <- "ATX"
  }
  if (games$home_code[i] == "SJ") {
    games$home_code[i] <- "OAK"
  }
  if (games$away_code[i] == "SJ") {
    games$away_code[i] <- "OAK"
  }
  if (games$home_code[i] == "SLC" && games$date[i] < as.Date("2019-01-01")) {
    games$home_code[i] <- "SAL"
  }
  if (games$away_code[i] == "SLC" && games$date[i] < as.Date("2019-01-01")) {
    games$away_code[i] <- "SAL"
  }
  if (games$home_code[i] == "JAX") {
    games$home_code[i] <- "TB"
  }
  if (games$away_code[i] == "JAX") {
    games$away_code[i] <- "TB"
  }
}

games$type <- "regular"


games$type[games$date == as.Date("2022-08-27")] <- "championship"
games$type[games$date == as.Date("2021-09-11")] <- "championship"
games$type[games$date == as.Date("2019-08-11")] <- "championship"
games$type[games$date == as.Date("2018-08-12")] <- "championship"
games$type[games$date == as.Date("2017-08-27")] <- "championship"
games$type[games$date == as.Date("2016-08-07")] <- "championship"
games$type[games$date == as.Date("2015-08-09")] <- "championship"
games$type[games$date == as.Date("2014-07-27")] <- "championship"

games$type[games$date == as.Date("2022-08-27")-1] <- "semi"
games$type[games$date == as.Date("2021-09-11")-1] <- "semi"
games$type[games$date == as.Date("2019-08-11")-1] <- "semi"
games$type[games$date == as.Date("2018-08-12")-1] <- "semi"
games$type[games$date == as.Date("2017-08-27")-1] <- "semi"
games$type[games$date == as.Date("2016-08-07")-1] <- "semi"
games$type[games$date == as.Date("2015-08-09")-1] <- "semi"
games$type[games$date == as.Date("2014-07-27")-1] <- "semi"

#2022
games$type[games$date >= as.Date("2022-08-13") & games$date < as.Date("2022-08-26")] <- "playoff"
#2021
games$type[games$date >= as.Date("2021-09-03") & games$date < as.Date("2021-09-10")] <- "playoff"
games$type[games$gameID == "2021-08-29-MIN-CHI"] <- "playoff"
games$type[games$gameID == "2021-08-28-DAL-SD"] <- "playoff"
#2019
games$type[games$date >= as.Date("2019-07-20") & games$date < as.Date("2019-08-10")] <- "playoff"
#2018
games$type[games$date >= as.Date("2018-07-21") & games$date < as.Date("2018-08-11")] <- "playoff"
#2017
games$type[games$date >= as.Date("2017-07-29") & games$date < as.Date("2017-08-26")] <- "playoff"
#2016
games$type[games$date >= as.Date("2016-07-16") & games$date < as.Date("2016-08-06")] <- "playoff"
#2015
games$type[games$date >= as.Date("2015-07-24") & games$date < as.Date("2015-08-08")] <- "playoff"
#2014
games$type[games$date >= as.Date("2014-07-18") & games$date < as.Date("2014-07-26")] <- "playoff"

write.csv(games, "../dataFiles/game_stats_asof_2022.csv")

games_list <- list()

for (game in unique(games$gameID)) {
  temp <- games[games$gameID == game,]
  homeCode <- temp$home_code[1]
  awayCode <- temp$away_code[1]
  homeTeam <- team_df$teamName[team_df$teamCode == homeCode]
  awayTeam <- team_df$teamName[team_df$teamCode == awayCode]
  homeScore <- temp$scoreHome[1]
  awayScore <- temp$scoreAway[1]
  date <- temp$date[1]
  gameID <- temp$gameID[1]
  type <- temp$type[1]
  
  if (homeScore > awayScore) {
    winner <- homeTeam
    loser <- awayTeam
  } else if (homeScore < awayScore) {
    winner <- awayTeam
    loser <- homeTeam
  } else {
    winner <- "DRAW"
    loser <- "DRAW"
  }
  
  
  game <- data.frame(homeCode = homeCode,
                     awayCode = awayCode,
                     homeTeam = homeTeam,
                     awayTeam = awayTeam,
                     homeScore = homeScore,
                     awayScore = awayScore,
                     date = date,
                     gameID = gameID,
                     winner = winner,
                     loser = loser,
                     type = type)
  
  games_list[[length(games_list) + 1]] <- game
  
}

all_games <- bind_rows(games_list)

all_games <- all_games[order(all_games$date),]

write.csv(all_games, "../dataFiles/all_games_asof_2022.csv")


write.csv(games, "../dataFiles/raw_games.csv")
