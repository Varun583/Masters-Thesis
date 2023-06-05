# Defining a log-likelihood function - Common Hierarchical model (for the simulation)

Log_Like3_1 = function(theta, mat, R){
  # mat contains mat$h, mat$w, mat$l, mat$nw, mat$nl
  # theta is the vector of thetas (skill levels)
  Teams = colnames(mat$h)
  number = length(Teams)
  
  log_like3_1 = 0 
  for(i in 1:number){
    for(j in (1:number)[-i]){
      if (R[i,j] == 1){
        alpha <- 0.20729403
      } else if (R[i,j] == 2){
        alpha <- 0.33840892
      } else {
        alpha <- 0.21253892
      }
      log_like3_1 = log_like3_1 + 
        mat$w[i,j]*(theta[i] + alpha - log(exp(theta[i] + alpha) + exp(theta[j]))) +
        mat$l[j, i]*(theta[i] - log(exp(theta[i]) + exp(alpha + theta[j]))) +
        mat$nw[i,j]*(theta[i] - log(exp(theta[i]) + exp(theta[j])))
    }
  }
  return(-1*log_like3_1)
}

# Defining a gradient for the likelihood function - Common Hierarchical (for the simulation)

Log_Like_deriv3_1 = function(theta, mat, R){
  Teams = colnames(mat$h)
  number = length(Teams)
  output = rep(0, ntheta(mat,3,R))
  
  # dL/dtheta
  for(i in 1:number){
    log_like_deriv3 = 0
    for(j in (1:number)[-i]){
      if (R[i,j] == 1){
        alpha <- 0.20729403
      } else if (R[i,j] == 2){
        alpha <- 0.33840892
      } else {
        alpha <- 0.21253892
      }
      log_like_deriv3 = log_like_deriv3 - 
        (mat$h[i,j])*((exp(theta[i] + alpha))/(exp(theta[i] + alpha) + exp(theta[j]))) -
        (mat$h[j,i])*((exp(theta[i]))/(exp(theta[j] + alpha) + exp(theta[i]))) -
        (mat$nw[i,j])*(exp(theta[i]))/(exp(theta[i]) + exp(theta[j])) - 
        (mat$nw[j,i])*(exp(theta[i]))/(exp(theta[i]) + exp(theta[j]))
      output[i] = sum(mat$w[i,]) + sum(mat$l[,i]) + sum(mat$nw[i,]) + log_like_deriv3
    }
  }
  return(-1*output)
}

#Here, mat is the win-loss matrices for the 2022-23 season. After obtaining the optimal values for theta (alpha is decided 
# from previous years' data, as seen in the function), we format them into a table

#Simulation Ready Data for 2022 season - Obtaining Theta Values
Model = optim(par = theta3, fn = Log_Like3_1, gr = Log_Like_deriv3_1, method = "BFGS", mat = mat2, R = R)
#This optim function churns out alpha parameters equal to 0

Table = function(Model, Teams){
#This function gives out a table for the Model built for simulation only. This is a seperate function built only for the current simulation.
#Teams is a list of all the teams in the NBA  
  theta_optim <- Model$par
  Model_table <- data.frame(Teams)
  Model_table[31,] <- "Alpha - Inter - conference"
  Model_table[32,] <- "Alpha - Intra - conference"
  Model_table[33,] <- "Alpha - Intra - division"
  Model_table$theta_values <- theta_optim
  least <- min(Model_table$theta_values[1:30])
  Model_table$theta_values[1:30] <- Model_table$theta_values[1:30] - least
  Model_table <- Model_table[1:30,]
  Model_table$theta_values_1 <- NA
  Model_table$theta_values_2 <- NA
  Model_table$theta_values_3 <- NA
  Model_table$theta_values_1[1:30] <- Model_table$theta_values[1:30] + 0.20729403
  Model_table$theta_values_2[1:30] <- Model_table$theta_values[1:30] + 0.33840892
  Model_table$theta_values_3[1:30] <- Model_table$theta_values[1:30] + 0.21253892
  return(Model_table)
}

Model_table <- Table(Model, Teams)

BT_probabilities = function(df, R){
  #Provide the data frame with the fixtures where Team A is away and Team B is home in case of the approved model
  #The model returns the same data frame along with the probabilities. 
  #This is a slightly specific version of BT_predict() function
  df$Hierarchy <- NA
  for(i in 1:nrow(df)){
    df$Hierarchy[i] <- R[df[i,1],df[i,2]]
  }
  df$A_prob <- NA
  df$B_prob <- NA
  for(i in 1:nrow(df)){
    if (df$Hierarchy[i] == 3){
      df$A_prob[i] <- exp(Model_table$theta_values[Model_table$Teams == df[i,1]])/(exp(Model_table$theta_values[Model_table$Teams == df[i,1]]) + exp(Model_table$theta_values_3[Model_table$Teams == df[i,2]]))
      df$B_prob[i] <- exp(Model_table$theta_values_3[Model_table$Teams == df[i,2]])/(exp(Model_table$theta_values[Model_table$Teams == df[i,1]]) + exp(Model_table$theta_values_3[Model_table$Teams == df[i,2]]))
    }
    if (df$Hierarchy[i] == 2){
      df$A_prob[i] <- exp(Model_table$theta_values[Model_table$Teams == df[i,1]])/(exp(Model_table$theta_values[Model_table$Teams == df[i,1]]) + exp(Model_table$theta_values_2[Model_table$Teams == df[i,2]]))
      df$B_prob[i] <- exp(Model_table$theta_values_2[Model_table$Teams == df[i,2]])/(exp(Model_table$theta_values[Model_table$Teams == df[i,1]]) + exp(Model_table$theta_values_2[Model_table$Teams == df[i,2]]))
    }
    if (df$Hierarchy[i] == 1){
      df$A_prob[i] <- exp(Model_table$theta_values[Model_table$Teams == df[i,1]])/(exp(Model_table$theta_values[Model_table$Teams == df[i,1]]) + exp(Model_table$theta_values_1[Model_table$Teams == df[i,2]]))
      df$B_prob[i] <- exp(Model_table$theta_values_1[Model_table$Teams == df[i,2]])/(exp(Model_table$theta_values[Model_table$Teams == df[i,1]]) + exp(Model_table$theta_values_1[Model_table$Teams == df[i,2]]))
    }
  }
  return(df)
}

Simulation_Play_In_2 = function(df, R = NULL){
  p <- BT_probabilities(df, R)
  for (i in 1:nrow(p)){
    p$Home_win[i] <- rbinom(1, 1, p$B_prob)
  }
  return(p)
}

#Data frame should be the first 4 fixtures of the play-in tournament
PlayInTournament_2 = function(df, R){
  p <- Simulation_Play_In_2(df, R)
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
  Final <- Simulation_Play_In_2(New,R)
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
}


#Obtain the probabilities for the regular season's fixtures using BT_probabilities(). After obtaining the probabilities, the same process as Simulation.R continues. 