elo_games <- all_games
elo_games$previousHomeElo <- 1400
elo_games$previousAwayElo <- 1400
elo_games$newHomeElo <- NA
elo_games$newAwayElo <- NA


for (i in 1:nrow(elo_games)) {
  Expected_win_and_diff <- calc_expected_home_win_perc(elo_games$previousHomeElo[i], elo_games$previousAwayElo[i])
  
  ExpHome <- Expected_win_and_diff[1]
  ExpAway <- 1 - ExpHome
  
  EloDiff <- Expected_win_and_diff[2]
  
  if (elo_games$type[i] == "championship") {
    importance <- 60
  } else if (elo_games$type[i] == "semi") {
    importance <- 50
  } else if (elo_games$type[i] == "playoff") {
    importance <- 40
  } else {
    importance <- 30
  }
  
  MOV <- abs(elo_games$homeScore[i] - elo_games$awayScore[i])
  
  MOV_multiplier <- (2.2 / ((EloDiff * 0.001) + 2.2)) * log(MOV + 1)
  
  if (elo_games$winner[i] == "DRAW") {
    elo_games$newHomeElo[i] <- elo_games$previousHomeElo[i] + (MOV_multiplier * importance * (0.5 - ExpHome))
    elo_games$newAwayElo[i] <- elo_games$previousAwayElo[i] + (MOV_multiplier * importance * (0.5 - ExpAway))
  } else if (elo_games$homeScore[i] > elo_games$awayScore[i]) {
    elo_games$newHomeElo[i] <- elo_games$previousHomeElo[i] + (MOV_multiplier * importance * (1 - ExpHome))
    elo_games$newAwayElo[i] <- elo_games$previousAwayElo[i] + (MOV_multiplier * importance * (0 - ExpAway))
  } else if (elo_games$homeScore[i] < elo_games$awayScore[i]) {
    elo_games$newHomeElo[i] <- elo_games$previousHomeElo[i] + (MOV_multiplier * importance * (0 - ExpHome))
    elo_games$newAwayElo[i] <- elo_games$previousAwayElo[i] + (MOV_multiplier * importance * (1 - ExpAway))
  }
  
  if (i == nrow(elo_games)) {
    break
  }

  
  for (j in (i+1):nrow(elo_games)) {
    if (elo_games$homeTeam[i] == elo_games$homeTeam[j]) {
      elo_games$previousHomeElo[j] <- elo_games$newHomeElo[i]
      break
    } else if (elo_games$homeTeam[i] == elo_games$awayTeam[j]) {
      elo_games$previousAwayElo[j] <- elo_games$newHomeElo[i]
      break
    }
  }
  
  for (j in (i+1):nrow(elo_games)) {
    if (elo_games$awayTeam[i] == elo_games$homeTeam[j]) {
      elo_games$previousHomeElo[j] <- elo_games$newAwayElo[i]
      break
    } else if (elo_games$awayTeam[i] == elo_games$awayTeam[j]) {
      elo_games$previousAwayElo[j] <- elo_games$newAwayElo[i]
      break
    }
  }
  
}

write.csv(elo_games, "historical_data_with_elos_calculated.csv")

year_vector <- c("2014", "2015", "2016", "2017", "2018", "2019", "2021", "2022")
elo_list_by_team <- list()

year_list <- list()
for (year in year_vector) {
  elo_list_by_team <- list()
  for (i in 1:nrow(team_df)) {
    team <- team_df$teamName[i]
    team_games_df <- elo_games[elo_games$homeTeam == team | elo_games$awayTeam == team,]
    team_games_year <- team_games_df[grepl(year, team_games_df$gameID),]
    elo_list <- list()
    
    if (nrow(team_games_year) != 0) {
      for (j in 1:nrow(team_games_year)) {
        if (team_games_year$homeTeam[j] == team) {
          elo_list[[length(elo_list) + 1]] <- team_games_year$newHomeElo[j]
        } else if (team_games_year$awayTeam[j] == team) {
          elo_list[[length(elo_list) + 1]] <- team_games_year$newAwayElo[j]
        }
      }
      team_elos <- data.frame(team = team,
                              date = team_games_year$date,
                              elo = unlist(elo_list))
      
      elo_list_by_team[[length(elo_list_by_team) + 1]] <- team_elos
    }
  }
  
  year_list[[year]] <- elo_list_by_team
}

cutoffs <- list()
year_list_dfs <- list()
for (i in 1:length(year_list)) {
  print(i)
  year_list_dfs[[i]] <- bind_rows(year_list[[i]])
}

for (i in 1:length(year_list_dfs)) {
  year_list_dfs[[i]]$date_numeric <- as.numeric(year_list_dfs[[i]]$date)
}

for (i in 1:length(year_list_dfs)) {
  year_list_dfs[[i]]$date_numeric <- year_list_dfs[[i]]$date_numeric - min(year_list_dfs[[i]]$date_numeric)
}

for (i in 2:length(year_list_dfs)) {
  year_list_dfs[[i]]$date_numeric <- year_list_dfs[[i]]$date_numeric + max(year_list_dfs[[i-1]]$date_numeric) + 25
  cutoffs[[length(cutoffs) + 1]] <- max(year_list_dfs[[i-1]]$date_numeric)
}

plot_df <- bind_rows(year_list_dfs)

write.csv(plot_df, "data_for_historical_elo_plot_creation.csv")


cutoffplot <- unlist(cutoffs)

plot <- ggplot(plot_df) +
  geom_line(aes(x = date_numeric, y = elo, color = team), size = 0.8) +
  geom_vline(xintercept = cutoffplot) +
  theme_bw(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = element_blank()) +
  annotate("text", x = 36, y = 1950, label = "2014", size = 10, color = "grey") +
  annotate("text", x = mean(c(cutoffplot[1], cutoffplot[2])), y = 1950, label = "2015", size = 10, color = "grey") +
  annotate("text", x = mean(c(cutoffplot[2], cutoffplot[3])), y = 1950, label = "2016", size = 10, color = "grey") +
  annotate("text", x = mean(c(cutoffplot[3], cutoffplot[4])), y = 1950, label = "2017", size = 10, color = "grey") +
  annotate("text", x = mean(c(cutoffplot[4], cutoffplot[5])), y = 1950, label = "2018", size = 10, color = "grey") +
  annotate("text", x = mean(c(cutoffplot[5], cutoffplot[6])), y = 1950, label = "2019", size = 10, color = "grey") +
  annotate("text", x = mean(c(cutoffplot[6], cutoffplot[7])), y = 1950, label = "2021", size = 10, color = "grey") +
  annotate("text", x = 1080, y = 1950, label = "2022", size = 10, color = "grey")
ggplotly(plot)
