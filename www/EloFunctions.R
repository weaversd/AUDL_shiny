calc_expected_home_win_perc <- function(homeElo, awayElo, homeAdv, EloDenom) {
  diff <- homeElo - awayElo + homeAdv
  home_expected <- 1 / ((10^(- diff / EloDenom)) + 1)
  return(c(home_expected, diff))
}

fill_elo_historical <- function(elo_games, homeAdv, EloDenom, MOVmult,
                                championshipK, semiK, playoffK, regK,
                                startingElo, regressionMult) {
  for (i in 1:nrow(elo_games)) {
    Expected_win_and_diff <- calc_expected_home_win_perc(elo_games$previousHomeElo[i],
                                                         elo_games$previousAwayElo[i],
                                                         homeAdv, EloDenom)
    ExpHome <- Expected_win_and_diff[1]
    ExpAway <- 1 - ExpHome
    EloDiff <- Expected_win_and_diff[2]
    
    if (elo_games$type[i] == "championship") {
      importance <- championshipK
    } else if (elo_games$type[i] == "semi") {
      importance <- semiK
    } else if (elo_games$type[i] == "playoff") {
      importance <- playoffK
    } else {
      importance <- regK
    }
    
    MOV <- abs(elo_games$homeScore[i] - elo_games$awayScore[i])
    MOV_factor <- ((2.2 / ((EloDiff * 0.001) + 2.2)) * log(MOV + 1)) * MOVmult
  
    if (elo_games$winner[i] == "DRAW") {
      elo_games$newHomeElo[i] <- elo_games$previousHomeElo[i] + (MOV_factor * importance * (0.5 - ExpHome))
      elo_games$newAwayElo[i] <- elo_games$previousAwayElo[i] + (MOV_factor * importance * (0.5 - ExpAway))
    } else if (elo_games$homeScore[i] > elo_games$awayScore[i]) {
      elo_games$newHomeElo[i] <- elo_games$previousHomeElo[i] + (MOV_factor * importance * (1 - ExpHome))
      elo_games$newAwayElo[i] <- elo_games$previousAwayElo[i] + (MOV_factor * importance * (0 - ExpAway))
    } else if (elo_games$homeScore[i] < elo_games$awayScore[i]) {
      elo_games$newHomeElo[i] <- elo_games$previousHomeElo[i] + (MOV_factor * importance * (0 - ExpHome))
      elo_games$newAwayElo[i] <- elo_games$previousAwayElo[i] + (MOV_factor * importance * (1 - ExpAway))
    }
    if (i == nrow(elo_games)) {
      break
    }
    for (j in (i+1):nrow(elo_games)) {
      if (elo_games$homeTeam[i] == elo_games$homeTeam[j]) {
        if (elo_games$season[i] == elo_games$season[j]) {
          elo_games$previousHomeElo[j] <- elo_games$newHomeElo[i]
        } else {
          elo_games$previousHomeElo[j] <- elo_games$newHomeElo[i] - ((elo_games$newHomeElo[i] - startingElo) * (regressionMult / 100))
        }
        break
      } else if (elo_games$homeTeam[i] == elo_games$awayTeam[j]) {
        if (elo_games$season[i] == elo_games$season[j]) {
          elo_games$previousAwayElo[j] <- elo_games$newHomeElo[i]
        } else {
          elo_games$previousAwayElo[j] <- elo_games$newHomeElo[i] - ((elo_games$newHomeElo[i] - startingElo) * (regressionMult / 100))
        }
        break
      }
    }
    
    for (j in (i+1):nrow(elo_games)) {
      if (elo_games$awayTeam[i] == elo_games$homeTeam[j]) {
        if (elo_games$season[i] == elo_games$season[j]) {
          elo_games$previousHomeElo[j] <- elo_games$newAwayElo[i]
        } else {
          elo_games$previousHomeElo[j] <- elo_games$newAwayElo[i] - ((elo_games$newAwayElo[i] - startingElo) * (regressionMult / 100))
        }
        break
      } else if (elo_games$awayTeam[i] == elo_games$awayTeam[j]) {
        if (elo_games$season[i] == elo_games$season[j]) {
          elo_games$previousAwayElo[j] <- elo_games$newAwayElo[i]
        } else {
          elo_games$previousAwayElo[j] <- elo_games$newAwayElo[i] - ((elo_games$newAwayElo[i] - startingElo) * (regressionMult / 100))
        }
        break
      }
    }
  }
  
  return(elo_games)
}

create_plotable_data <- function(elo_games) {
  year_vector <- unique(elo_games$season)
  
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
    year_list[[length(year_list) + 1]] <- elo_list_by_team
  }
  cutoffs <- list()
  cutoffs[[1]] <- 0
  
  year_list_dfs <- list()
  for (i in 1:length(year_list)) {
    year_list_dfs[[i]] <- bind_rows(year_list[[i]])
    year_list_dfs[[i]]$date <- as.Date(year_list_dfs[[i]]$date)
  }
  
  for (i in 1:length(year_list_dfs)) {
    year_list_dfs[[i]]$date_numeric <- as.numeric(year_list_dfs[[i]]$date)
  }
  
  for (i in 1:length(year_list_dfs)) {
    year_list_dfs[[i]]$date_numeric <- year_list_dfs[[i]]$date_numeric - min(year_list_dfs[[i]]$date_numeric)
  }

  
  for (i in 2:length(year_list_dfs)) {
    year_list_dfs[[i]]$date_numeric <- year_list_dfs[[i]]$date_numeric + max(year_list_dfs[[i-1]]$date_numeric) + 30
    cutoffs[[length(cutoffs) + 1]] <- max(year_list_dfs[[i-1]]$date_numeric) + 15
  }
  
  
  plot_df <- bind_rows(year_list_dfs)
  
  cutoffs[[length(cutoffs) + 1]] <- max(plot_df$date_numeric) + 15
  
  cutoff_vector <- unlist(cutoffs)
  
  return(list(plot_df, cutoff_vector))
  
}


generate_current_team_table <- function(elo_team_list = NULL, date = NULL) {
  
  if (is.null(date)) {
    date <- Sys.Date()
  }
  
  date <- as.Date(date)
  
  current_team_df <- data.frame(teamName = current_teams)
  
  current_team_df$teamCode = ""
  current_team_df$region = ""
  current_team_df$elo = NA
  
  for (i in 1:nrow(current_team_df)) {
    team <- current_team_df$teamName[i]
    current_team_df$teamCode[i] <- team_df$teamCode[team_df$teamName == team]
    current_team_df$region[i] <- team_df$region[team_df$teamName == team]
    
    team_elo_df <- elo_team_list[elo_team_list$team == team & elo_team_list$date <= date,]
    
    if (nrow(team_elo_df) > 0) {
      current_team_df$elo[i] <- team_elo_df$elo[which.min(date - team_elo_df$date)]
    } else {
      current_team_df$elo[i] <- NA
    }
  }
  
  current_team_df <- current_team_df[order(-current_team_df$elo),]
  
  return(current_team_df)
}

get_elo_team_date <- function(elo_team_list, team, date) {
  
  team_elo_df <- elo_team_list[elo_team_list$team == team & elo_team_list$date <= date,]
  
  if (nrow(team_elo_df) > 0) {
    elo_score <- team_elo_df$elo[which.min(date - team_elo_df$date)]
  } else {
    stop("Team did not exist at that time")
  }

  return(elo_score)
}
