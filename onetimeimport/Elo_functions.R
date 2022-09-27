#calculate expected home win perc
calc_expected_home_win_perc <- function(homeElo, awayElo) {
  diff <- homeElo - awayElo + 100 
  home_expected <- 1 / ((10^(- diff / 400)) + 1)
  return(c(home_expected, diff))
}
