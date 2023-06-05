source("Implementation.R")

Simulation_Play_In = function(model, df, R = NULL){
  # A small function to obtain the probabilities for a play-in tournament 
  p <- BT_predict(model, df, R)
  for (i in 1:nrow(p)){
    p$Home_win[i] <- rbinom(1, 1, p$Team_B_win)
  }
  return(p)
}

#Data frame should be the first 4 fixtures of the play-in tournament
#As per data, Team B is always the home team. Hence, we have 4 columns in fixtures: Team A, Team B, Team A Home, Team B Home
PlayInTournament = function(model, df, R = NULL){
  p <- Simulation_Play_In(model, df, R = NULL)
  Seeds <- data.frame(matrix(ncol = 2, nrow = 8))
  colnames(Seeds) <- c("Seeds", "Teams")
  Seeds$Seeds[1] <- "7th Seed - East"
  Seeds$Seeds[2] <- "7th Seed - West"
  Seeds$Seeds[3] <- "8th Seed - East"
  Seeds$Seeds[4] <- "8th Seed - West"
  Seeds$Seeds[5] <- "9th Seed - East"
  Seeds$Seeds[6] <- "9th Seed - West"
  Seeds$Seeds[7] <- "10th Seed - East"
  Seeds$Seeds[8] <- "10th Seed - West"
  p[5,] <- NA
  p[6,] <- NA
  if (p$Home_win[1] == 1){
    Seeds$Teams[1] <- p[1,2]
    p[5,2] <- p[1,1]
  } else {
    Seeds$Teams[1] <- p[1,1]
    p[5,2] <- p[1,2]
  }
  if (p$Home_win[2] == 1){
    Seeds$Teams[2] <- p[2,2]
    p[6,2] <- p[2,1]
  } else {
    Seeds$Teams[2] <- p[2,1]
    p[6,2] <- p[2,2]
  }
  if (p$Home_win[3] == 1){
    p[5,1] <- p[3,2]
    Seeds$Teams[7] <- p[3,1]
  } else {
    p[5,1] <- p[3,1]
    Seeds$Teams[7] <- p[3,2]
  }
  if (p$Home_win[4] == 1){
    p[6,1] <- p[4,2]
    Seeds$Teams[8] <- p[4,1]
  } else {
    p[6,1] <- p[4,1]
    Seeds$Teams[8] <- p[4,2]
  }
  New <- p[5:6,]
  New[,3:4] <- NA
  New[,3] <- 0
  New[,4] <- 0
  Final <- Simulation_Play_In(model,New,R=NULL)
  New <- rbind(p[1:4,],Final)
  if (Final$Home_win[1] == 1){
    Seeds$Teams[3] <- Final[1,2]
    Seeds$Teams[5] <- Final[1,1]
  } else {
    Seeds$Teams[3] <- Final[1,1]
    Seeds$Teams[5] <- Final[1,2]
  }
  if (Final$Home_win[2] == 1){
    Seeds$Teams[4] <- Final[2,2]
    Seeds$Teams[6] <- Final[2,1]
  } else {
    Seeds$Teams[4] <- Final[2,1]
    Seeds$Teams[6] <- Final[2,2]
  }
  Output <- Seeds
  return(Output)
  # Returns the seeds of the teams after the completion of the tournament
}

Probabilities <- BT_predict(Model, Sample, R)
# Sample contains all the fixtures of the regular reason, which BT_predict used to give probabilities of wins
# Model is the model we have used to obtain probabilities
# R is the relationship matrix

Seeds_East = function(Seeds){
# Seeds_East takes the output from PlayInTournament() and gives the appropriate seeding for the 10 teams in the East.
# An example of this season's seeding. 
  Seed_No <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  Team <- c("Milwaukee Bucks", "Boston Celtics", "Philadelphia 76ers", "Cleveland Cavaliers", "New York Knicks", "Brooklyn Nets", Seeds[1,2], Seeds[3,2], Seeds[5,2], Seeds[7,2])
  Seeds_East <- data.frame(Seed_No, Team)
  return(Seeds_East)
}

Simulation_Playoff_East = function(df, SE, Probabilities){
# df is the fixture list of the Eastern Conference playoffs without the play-in teams added with two columns "Team A" and "Team B". Team A has the higher seed in this fixture list.
# Probabilities is the results of BT_predict for the entire season
# SE is the Seeds of the Eastern Conference obtained from Seeds_East()
  p <- df
  p[1,2] <- SE[8,2]
  p[2,2] <- SE[7,2]
  p$Home_Win <- NA
  p$Winner <- NA
  for (i in 1:nrow(p)){
    p$Home_Win[i] <- rbinom(1, 7, c(Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]], 
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]]))
  }
  for (i in 1:nrow(p)){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  p[5:7,] <- NA
  p[5,1] <- p$Winner[1]
  p[5,2] <- p$Winner[4]
  p[6,1] <- p$Winner[2]
  p[6,2] <- p$Winner[3]
  if ((SE$Seed_No[SE$Team == p[5,1]]) > (SE$Seed_No[SE$Team == p[5,2]])){
    p[5,1:2] <- p[5,][c("Team.B", "Team.A")]
  }
  if ((SE$Seed_No[SE$Team == p[6,1]]) > (SE$Seed_No[SE$Team == p[6,2]])){
    p[6,1:2] <- p[6,][c("Team.B", "Team.A")]
  }
  Semis <- p[5:6,]
  for (i in 5:6){
    p$Home_Win[i] <- rbinom(1, 7, c(Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]]))
  }
  for (i in 5:6){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  p[7,1] <- p$Winner[5]
  p[7,2] <- p$Winner[6]
  if ((SE$Seed_No[SE$Team == p[7,1]]) > (SE$Seed_No[SE$Team == p[7,2]])){
    p[7,1:2] <- p[7,][c("Team.B", "Team.A")]
  }
  Final <- p[7,]
  for (i in 7){
    p$Home_Win[i] <- rbinom(1, 7, c(Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]]))
  }
  for (i in 7){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  return(p)
}

Seeds_West = function(Seeds){
  # Seeds_West takes the output from PlayInTournament() and gives the appropraie seeding for the 10 teams in the West. An example of this season's seeding. 
  Seed_No <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  Team <- c("Denver Nuggets", "Memphis Grizzlies", "Sacramento Kings", "Phoenix Suns", "Los Angeles Clippers", "Golden State Warriors", Seeds[2,2], Seeds[4,2], Seeds[6,2], Seeds[8,2])
  Seeds_West <- data.frame(Seed_No, Team)
  return(Seeds_West)
}

Simulation_Playoff_West = function(df, SW, Probabilities){
# df is the fixture list of the Western Conference playoffs without the play-in teams added with two columns "Team A" and "Team B". Team A has the higher seed in this fixture list.
# Probabilities is the results of BT_predict for the entire season
# SW is the Seeds of the Western Conference obtained from Seeds_West
  p <- df
  p[1,2] <- SW[8,2]
  p[2,2] <- SW[7,2]
  p$Home_Win <- NA
  p$Winner <- NA
  for (i in 1:nrow(p)){
    p$Home_Win[i] <- rbinom(1, 7, c(Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]], 
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]]))
  }
  for (i in 1:nrow(p)){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  p[5:7,] <- NA
  p[5,1] <- p$Winner[1]
  p[5,2] <- p$Winner[4]
  p[6,1] <- p$Winner[2]
  p[6,2] <- p$Winner[3]
  if ((SW$Seed_No[SW$Team == p[5,1]]) > (SW$Seed_No[SW$Team == p[5,2]])){
    p[5,1:2] <- p[5,][c("Team.B", "Team.A")]
  }
  if ((SW$Seed_No[SW$Team == p[6,1]]) > (SW$Seed_No[SW$Team == p[6,2]])){
    p[6,1:2] <- p[6,][c("Team.B", "Team.A")]
  }
  Semis <- p[5:6,]
  for (i in 5:6){
    p$Home_Win[i] <- rbinom(1, 7, c(Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]]))
  }
  for (i in 5:6){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  p[7,1] <- p$Winner[5]
  p[7,2] <- p$Winner[6]
  if ((SW$Seed_No[SW$Team == p[7,1]]) > (SW$Seed_No[SW$Team == p[7,2]])){
    p[7,1:2] <- p[7,][c("Team.B", "Team.A")]
  }
  Final <- p[7,]
  for (i in 7){
    p$Home_Win[i] <- rbinom(1, 7, c(Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]]))
  }
  for (i in 7){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  return(p)
}

Seeds_Tournament = function(){
  # Seeds of the entire league with an example of the 2022-23 season
  Seed_No <- c(1:20)
  Team <- c("Milwaukee Bucks", "Boston Celtics", "Philadelphia 76ers", "Denver Nuggets", "Memphis Grizzlies", "Cleveland Cavaliers", "Sacramento Kings", "New York Knicks", "Phoenix Suns", "Brooklyn Nets", "Miami Heat", "Los Angeles Clippers", "Golden State Warriors", "Los Angeles Lakers", "Minnesota Timberwolves", "New Orleans Pelicans", "Atlanta Hawks", "Toronto Raptors", "Chicago Bulls", "Oklahoma City Thunder")
  Seeds_Tournament <- data.frame(Seed_No, Team)
  return(Seeds_Tournament)
}

Final = function(East, West, ST){
# East and West are the 7 games that have been completed in both the playoffs.
# ST is the seeds of the tournament
  Final <- data.frame(matrix(ncol = 2))
  colnames(Final) <- c("Team.A", "Team.B")
  Final[,1] <- East$Winner[7]
  Final[,2] <- West$Winner[7]
  if ((ST$Seed_No[ST$Team == Final[,1]]) > (ST$Seed_No[ST$Team == Final[,2]])){
    Final[,1:2] <- Final[c("Team.B", "Team.A")]
  }
  return(Final)
}

Simulation_Final = function(df, Probabilities){
# df is the output from the function Final(), which adjusts the seeding to obtain the extra home court game
  p <- df
  p$Home_Win <- NA
  p$Winner <- NA
  for (i in 1:nrow(p)){
    p$Home_Win[i] <- rbinom(1, 7, c(Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]],
                                    Probabilities$Team_A_win[Probabilities[,2] == p[i,2] & Probabilities[,1] == p[i,1]],
                                    Probabilities$Team_B_win[Probabilities[,2] == p[i,1] & Probabilities[,1] == p[i,2]]))
  }
  for (i in 1){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  return(p)
}

NBA <- function(){
#Combining all the functions to obtain results of the simulations
  Seeds <- PlayInTournament(Model, fixtures, R = NULL)
  SE = Seeds_East(Seeds)
  East <- Simulation_Playoff_East(Playoffs_East, SE, Probabilities)
  SW = Seeds_West(Seeds)
  West <- Simulation_Playoff_West(Playoffs_West, SW, Probabilities)
  ST = Seeds_Tournament()
  Champions = Final(East, West, ST)
  Championship <- Simulation_Final(Champions, Probabilities)
  Output <- data.frame(East)
  colnames(Output) <- c("Team.A", "Team.B", "Home_Win", "Winner")
  Output[8:14,] <- West
  Output[15,] <- Championship
  return(Output)
}

#Running the simulations 10000 times
Simulations <- data.frame(matrix(ncol = 4, nrow = 0))
Games <- data.frame(matrix(ncol = 4, nrow = 0))
for (i in 1:10000){
  Games <- NBA()
  Simulations <- rbind(Simulations, Games)
}
