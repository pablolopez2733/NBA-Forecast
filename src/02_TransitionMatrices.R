##############################################################################
####### SCRIPT FOR DEFINING THE MATRICES REQUIRED FOR REGRESSION #############
##############################################################################
teams <- sort(unique(grouped_pm$slugTeam)) 


# Function x_matrix for seasons ================================================
# 'Returns the N_i x 82 matrix for the team given as an argument.
x_matrix <- function(team){
  team_series <- lineup_stats %>% 
    filter(slugTeam == team) %>% 
    group_by(idGame,lineup) %>% 
    summarise(
      time_played = sum(totalTime),
      game_pm = sum(netScoreTeam)
      
    ) %>% 
    left_join(grouped_pm, by = c("lineup" = "lineup"))
  # This should output a tibble where we have the lineups used in every game
  # their time, and pm.
  
  
  # Create matrix
  N_i <- nrow(as.data.frame(unique(team_series$l_id)))
  X <- matrix(0L, nrow = 82, ncol = N_i)
  
  #Rename matrix so that columns and rows are lineup_ids:
  colnames(X) <- unique(team_series$l_id) 
  rownames(X) <- unique(team_series$idGame)
  
  
  lineups <- colnames(X)
  games <- rownames(X)
  # Iterate through all the possible lineups and games
  for(l in lineups){
    for (g in games) {
      dato <- team_series[which(team_series$idGame == as.double(g) & team_series$l_id == as.double(l)),]$time_played
      # Because a lineup may not have appeared in a game, we need to account for
      # length zero values
      if(length(dato) == 0)
        X[g,l] <- 0
      else
        X[g,l] <- dato
    }
    
  }
  
  return(X)
  
}

x_matrix_exclution <- function(team, exc)
{
  team_series <- lineup_stats %>% 
    filter(slugTeam == team) %>% 
    filter(!grepl(toString(exc), lineup)) %>% 
    group_by(idGame,lineup) %>% 
    summarise(
      time_played = sum(totalTime),
      game_pm = sum(netScoreTeam)
      
    ) %>% 
    left_join(grouped_pm, by = c("lineup" = "lineup"))
  # This should output a tibble where we have the lineups used in every game
  # their time, and pm.
  
  
  # Create matrix
  N_i <- nrow(as.data.frame(unique(team_series$l_id)))
  X <- matrix(0L, nrow = 82, ncol = N_i)
  
  #Rename matrix so that columns and rows are lineup_ids:
  colnames(X) <- unique(team_series$l_id) 
  rownames(X) <- unique(team_series$idGame)
  
  
  lineups <- colnames(X)
  games <- rownames(X)
  # Iterate through all the possible lineups and games
  for(l in lineups){
    for (g in games) {
      dato <- team_series[which(team_series$idGame == as.double(g) & team_series$l_id == as.double(l)),]$time_played
      # Because a lineup may not have appeared in a game, we need to account for
      # length zero values
      if(length(dato) == 0)
        X[g,l] <- 0
      else
        X[g,l] <- dato
    }
    
  }
  
  return(X)
}

# Function Y_Vector for seasons ================================================
# 'Returns the 82 x 1 vector of point differential for every game
# 'of a given team. 
y_vector <- function(team){
  # game_res has results and scores for every 2014-2015 regular season game
  game_res <- lineup_stats %>% 
    group_by(idGame,slugTeam) %>%
    summarise(
      pt_dif = sum(netScoreTeam),
      pts = last(finalScoreTeam)
    )
  # team_res just has number of points scored in each game by each team
  team_res <- game_res %>% 
    filter(slugTeam == team)
  
  num_vec <- team_res$pt_dif
  
  return(num_vec)
  
}


# Function x_matrix_playoffs for playoffs =====================================
x_matrix_playoffs <- function(team){
  
  #Count games
  counter <- lineup_stats_playoffs %>% 
    filter(slugTeam == team) %>% 
    group_by(idGame)
  n_games <- nrow(as.data.frame(unique(counter$idGame)))
  
  team_series <- lineup_stats %>% 
    filter(slugTeam == team) %>% 
    group_by(idGame,lineup) %>% 
    summarise(
      time_played = sum(totalTime),
      game_pm = sum(netScoreTeam)
      
    ) %>% 
    left_join(grouped_pm, by = c("lineup" = "lineup"))
  # This should output a tibble where we have the lineups used in
  
  team_series_rs <- lineup_stats_playoffs %>% 
    filter(slugTeam == team) %>% 
    group_by(idGame,lineup) %>% 
    summarise(
      time_played = sum(totalTime),
      game_pm = sum(netScoreTeam)
      
    ) %>% 
    left_join(grouped_pm, by = c("lineup" = "lineup"))
  
  
  # Create matrix
  N_i <- nrow(as.data.frame(unique(team_series$l_id)))
  X <- matrix(0L, nrow = n_games, ncol = N_i)
  
  #Rename matrix so that columns and rows are lineup_ids:
  colnames(X) <- unique(team_series$l_id) 
  rownames(X) <- unique(team_series_rs$idGame)
  
  
  lineups <- colnames(X)
  games <- rownames(X)
  # Iterate through all the possible lineups and games
  for(l in lineups){
    for (g in games) {
      dato <- team_series_rs[which(team_series_rs$idGame == as.double(g) & team_series_rs$l_id == as.double(l)),]$time_played
      # Because a lineup may not have appeared in a game, we need to account for
      # length zero values
      if(length(dato) == 0)
        X[g,l] <- 0
      else
        X[g,l] <- dato
    }
    
  }
  
  return(X)
  
}


# Function y_vector for playoffs ===============================================
y_vector_playoffs<- function(team){
  # game_res has results and scores for every 2014-2015 regular season game
  game_res <- lineup_stats_playoffs %>% 
    group_by(idGame,slugTeam) %>%
    summarise(
      pt_dif = sum(netScoreTeam),
      pts = last(finalScoreTeam)
    )
  # team_res just has number of points scored in each game by each team
  team_res <- game_res %>% 
    filter(slugTeam == team)
  
  num_vec <- team_res$pt_dif
  
  return(num_vec)
  
}


# Function for computing R^2 from true and predicted values====================
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}


