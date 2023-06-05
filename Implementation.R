source("Functions.R")

BT_Model = function(matrix, Model_type, R = NULL){
  # Returns the 'thetas' with the least parameter set to 0 for the specified Bradley-Terry model
  # The model type has to be specified in this manner
  # VAN - Vanilla, CHA - Common Homeground advantage, CHI - Common Hierarchical Model, TSH - Team Specific Homeground,
  # HIE - Hierarchical, PAI - Pairwise Homeground 
  
  teams = matrix$Teams
  
  if(Model_type == 'VAN'){
    theta = rep(0, ntheta(matrix, 1)) # Optimization starting from 0
    Model = optim(par = theta, fn = Log_Like1, gr = Log_Like_deriv1, mat = matrix, method = 'BFGS') # Fitting Model
    least = teams[which.min(Model$par)]
    best = teams[which.max(Model$par)]
    
    Theta = Model$par - Model$par[which(teams == least)] # Re-basing so that the least ranked team has theta = 0
    Table = data.frame(teams, Theta)
    return(list(Table = Table, type = Model_type, Worst = least, Teams = teams))
  }
  
  if(Model_type == 'CHA'){
    theta = rep(0,ntheta(matrix, 2))
    Model = optim(par = theta, fn = Log_Like2, gr = Log_Like_deriv2, mat = matrix, method = 'BFGS')
    
    least = teams[which.min(Model$par[1:length(teams)])]
    best = teams[which.max(Model$par[1:length(teams)])]
    
    theta = Model$par[1:length(teams)] - Model$par[which(teams == least)]
    alpha = Model$par[(1+length(teams))]
    
    Table = data.frame(teams, Theta = theta)
    return(list(Table = Table, Alpha = alpha, type = Model_type, Worst = least, Best = best, Teams = teams))
  }
  
  if(Model_type == 'CHI'){
    advantages = c("Level 1", "Level 2", "Level 3")
    theta = rep(0, ntheta(matrix, 3, R))
    Model = optim(par = theta, fn = Log_Like3, gr = Log_Like_deriv3, mat = matrix, R = R, method = 'BFGS')
    
    least = teams[which.min(Model$par[1:length(teams)])]
    bigger_advantage = advantages[which.max(Model$par[(length(teams) + 1):(length(teams) + max(R))])]
    
    theta = Model$par[1:length(teams)] - Model$par[which(teams == least)]
    alpha = Model$par[(length(teams) + 1):(length(teams) + max(R))]
    
    Table = (data.frame(Theta = theta, row.names = teams))
    Alpha = data.frame(Alpha = alpha, row.names = 1:max(R))
    
    return(list(Table = Table, Alpha = Alpha, type = Model_type, Advantage = bigger_advantage, Teams = teams))
  }
  
  if(Model_type == 'TSH'){
    theta = rep(0,ntheta(matrix, 4, R))
    Model = optim(par = theta, fn = Log_Like4, gr = Log_Like_deriv4, mat = matrix, method = 'BFGS')
    
    least = teams[which.min(Model$par[1:length(teams)])]
    bigger_advantage = teams[which.max(Model$par[(length(teams) + 1):(2*length(teams))])]
    
    theta = Model$par[1:length(teams)] - Model$par[which(teams == least)]
    alpha = Model$par[(length(teams) + 1):(2*length(teams))]
    
    Table = data.frame(Theta = theta, row.names = teams)
    Alpha = data.frame(Alpha = alpha, row.names = teams)
    
    return(list(Table = Table, Alpha = Alpha, type = Model_type, Highest_Home_Advantage = bigger_advantage, Teams = teams))
  }
  
  if(Model_type == 'HIE'){
    theta = rep(0,ntheta(matrix, 5, R))
    Model = optim(par = theta, fn = Log_Like5, gr = Log_Like_deriv5, mat = matrix, R = R, method = 'BFGS')
    
    least = teams[which.min(Model$par[1:length(teams)])]
    
    theta = Model$par[1:length(teams)] - Model$par[which(teams == least)]
    Table = data.frame(theta, row.names = teams)
    Alpha = NA
    level = 1
    while(level <= max(R)){
      Level = Model$par[(level*length(teams) + 1) : ((level+1)*length(teams))]
      Alpha = data.frame(Alpha, Level, row.names = teams)
      level = level + 1
    }
    return(list(Table = Table, 
                Alpha = Alpha[,-1], type = Model_type, Teams = teams))
  }
  
  if(Model_type == 'PAI'){
    theta = rep(0,ntheta(matrix, 6, R))
    Model = optim(par = theta, fn = Log_Like6, gr = Log_Like_deriv6, mat = matrix, method = 'BFGS')
    
    least = teams[which.min(Model$par[1:length(teams)])]
    
    Model$par[1:length(teams)] = Model$par[1:length(teams)] - Model$par[which(teams == least)]
    return(list(par = Model$par, teams = teams, type = Model_type))
  }
  
}

# Obtains the model number based on the model specification for the BT_test() function
ModelNumber = function(model){
  if (model == 'VAN'){
    return(1)
  } else if (model == 'CHA'){
    return(2)
  } else if (model == 'CHI'){
    return(3)
  } else if (model == 'TSH'){
    return(4)
  } else if (model == 'HIE'){
    return(5)
  } else if (model == 'PAI'){
    return(6)
  } else {
      return("Check the specification")
    }
}

BT_test = function(model1, model2, matrix, R = NULL){
  # Function for performing the hypothesis tests outlined in the thesis
  # Function fits the thetas while performing the hypothesis tests. Hence, this could also be used a precursor before obtaining theta values
  
  m1 = model1; m2 = model2
  if(ModelNumber(model1) > ModelNumber(model2)){
    m1 = model2; m2 = model1
  }
  if(ModelNumber(model1) == ModelNumber(model2)){
    return("Same order")
  }
  if(ModelNumber(m1) == 1 & ModelNumber(m2) == 2){
    stat = -2*(optim(par = rep(0,ntheta(matrix,2)), fn = Log_Like2, gr = Log_Like_deriv2, method = "BFGS", mat = matrix)$value - 
                 optim(par = rep(0,ntheta(matrix,1)), fn = Log_Like1, gr = Log_Like_deriv1, method = "BFGS", mat = matrix)$value)
    cat('P-Value is ', 1 - pchisq(stat, 1), '\n')
    cat('with a statistic of ', stat, 'and 1 degree of freedom')
  }
  if(ModelNumber(m1) == 1 & ModelNumber(m2) == 3){
    stat = -2*(optim(par = rep(0,ntheta(matrix,3,R)), fn = Log_Like3, gr = Log_Like_deriv3, method = "BFGS", mat = matrix, R = R)$value - 
                 optim(par = rep(0,ntheta(matrix,1)), fn = Log_Like1, gr = Log_Like_deriv1, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,3,R) - ntheta(matrix,1)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 1 & ModelNumber(m2) == 4){
    stat = -2*(optim(par = rep(0,ntheta(matrix,4)), fn = Log_Like4, gr = Log_Like_deriv4, method = "BFGS", mat = matrix)$value - 
                 optim(par = rep(0,ntheta(matrix,1)), fn = Log_Like1, gr = Log_Like_deriv1, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix, 4) - ntheta(matrix,1)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 1 & ModelNumber(m2) == 5){
    stat = -2*(optim(par = rep(0,ntheta(matrix,5,R)), fn = Log_Like5, gr = Log_Like_deriv5, method = "BFGS", mat = matrix, R = R)$value - 
                 optim(par = rep(0,ntheta(matrix,1)), fn = Log_Like1, gr = Log_Like_deriv1, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,5,R) - ntheta(matrix,1)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 1 & ModelNumber(m2) == 6){
    stat = -2*(optim(par = rep(0,ntheta(matrix,6)), fn = Log_Like6, gr = Log_Like_deriv6, method = "BFGS", mat = matrix)$value - 
                 optim(par = rep(0,ntheta(matrix,1)), fn = Log_Like1, gr = Log_Like_deriv1, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,6) - ntheta(matrix,1)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 2 & ModelNumber(m2) == 3){
    stat = -2*(optim(par = rep(0,ntheta(matrix,3,R)), fn = Log_Like3, gr = Log_Like_deriv3, method = "BFGS", mat = matrix, R = R)$value - 
                 optim(par = rep(0,ntheta(matrix,2)), fn = Log_Like2, gr = Log_Like_deriv2, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,3,R) - ntheta(matrix,2)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 2 & ModelNumber(m2) == 4){
    stat = -2*(optim(par = rep(0,ntheta(matrix,4)), fn = Log_Like4, gr = Log_Like_deriv4, method = "BFGS", mat = matrix)$value - 
                 optim(par = rep(0,ntheta(matrix,2)), fn = Log_Like2, gr = Log_Like_deriv2, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,4) - ntheta(matrix,2)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 2 & ModelNumber(m2) == 5){
    stat = -2*(optim(par = rep(0,ntheta(matrix,5,R)), fn = Log_Like5, gr = Log_Like_deriv5, method = "BFGS", mat = matrix, R = R)$value - 
                 optim(par = rep(0,ntheta(matrix,2)), fn = Log_Like2, gr = Log_Like_deriv2, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,5,R) - ntheta(matrix,2)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 2 & ModelNumber(m2) == 6){
    stat = -2*(optim(par = rep(0,ntheta(matrix,6)), fn = Log_Like6, gr = Log_Like_deriv6, method = "BFGS", mat = matrix)$value - 
                 optim(par = rep(0,ntheta(matrix,2)), fn = Log_Like2, gr = Log_Like_deriv2, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,6) - ntheta(matrix,2)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 3 & ModelNumber(m2) == 4){
    stat = -2*(optim(par = rep(0,ntheta(matrix,4)), fn = Log_Like4, gr = Log_Like_deriv4, method = "BFGS", mat = matrix)$value - 
                 optim(par = rep(0,ntheta(matrix,3,R)), fn = Log_Like3, gr = Log_Like_deriv3, method = "BFGS", mat = matrix, R = R)$value)
    df = ntheta(matrix,4) - ntheta(matrix,3,R)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 3 & ModelNumber(m2) == 5){
    stat = -2*(optim(par = rep(0,ntheta(matrix,5,R)), fn = Log_Like5, gr = Log_Like_deriv5, method = "BFGS", mat = matrix, R = R)$value - 
                 optim(par = rep(0,ntheta(matrix,3,R)), fn = Log_Like3, gr = Log_Like_deriv3, method = "BFGS", mat = matrix, R = R)$value)
    df = ntheta(matrix,5,R) - ntheta(matrix,3,R)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 3 & ModelNumber(m2) == 6){
    stat = -2*(optim(par = rep(0,ntheta(matrix,6)), fn = Log_Like6, gr = Log_Like_deriv6, method = "BFGS", mat = matrix)$value - 
                 optim(par = rep(0,ntheta(matrix,3,R)), fn = Log_Like3, gr = Log_Like_deriv3, method = "BFGS", mat = matrix, R = R)$value)
    df = ntheta(matrix,6) - ntheta(matrix,3,R)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 4 & ModelNumber(m2) == 5){
    stat = -2*(optim(par = rep(0,ntheta(matrix,5,R)), fn = Log_Like5, gr = Log_Like_deriv5, method = "BFGS", mat = matrix, R = R)$value - 
                 optim(par = rep(0,ntheta(matrix,4)), fn = Log_Like4, gr = Log_Like_deriv4, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,5,R) - ntheta(matrix,4)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 4 & ModelNumber(m2) == 6){
    stat = -2*(optim(par = rep(0,ntheta(matrix,6)), fn = Log_Like6, gr = Log_Like_deriv6, method = "BFGS", mat = matrix)$value - 
                 optim(par = rep(0,ntheta(matrix,4)), fn = Log_Like4, gr = Log_Like_deriv4, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,6) - ntheta(matrix,4)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 5 & ModelNumber(m2) == 6){
    stat = -2*(optim(par = rep(0,ntheta(matrix,6)), fn = Log_Like6, gr = Log_Like_deriv6, method = "BFGS", mat = matrix)$value - 
                 optim(par = rep(0,ntheta(matrix,5,R)), fn = Log_Like5, gr = Log_Like_deriv5, method = "BFGS", mat = matrix, R = R)$value)
    df = ntheta(matrix,6) - ntheta(matrix,5,R)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
}
  

BT_plots = function(model){
  # Takes a fitted Bradley-Terry model from the BT_Model() function and produces a plot of the theta values by team.
  # For hierarchical models (CHI and HIE), it prints each plot every ten seconds, starting with R_1, up to R_n
  
  if(model$type == 'VAN'){
    # Vanilla Model
    Teams = model$Teams
    table = model$Table
    Theta = table[order(-table$Theta),]
    PLOT <- Theta %>%
      arrange(Theta) %>%
      mutate(Teams = factor(teams, unique(teams))) %>%
      ggplot() + aes(x=Teams, y=Theta) +
      geom_segment( aes(x=Teams, xend=Teams, y=0, yend=Theta), color="black") +
      geom_point(color=rgb(0.2,0.7,0.1,0.5), size=3, alpha=0.6) +
      ylab("Theta") + xlab("Teams") + 
      theme_light() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      )
    return(PLOT)
  }
  if(model$type == 'CHA'){
    # Common Homeground Advantage Model
    Teams = model$Teams
    table = model$Table
    Theta = table[order(-table$Theta),]
    Theta$home <- Theta$Theta + model$Alpha
    PLOT <- Theta %>%
      arrange(Theta) %>%
      mutate(Teams = factor(teams, unique(teams))) %>%
      ggplot() +
      geom_segment(aes(x=Teams, xend=Teams, y=Theta, yend=home), color="black") +
      geom_point(aes(x=Teams, y=Theta), color=rgb(0.2,0.7,0.1,0.5), size=3, alpha = 0.6 ) +
      geom_point(aes(x=Teams, y=home), color=rgb(0.7,0.2,0.1,0.5), size=3, alpha = 0.6 ) +
      coord_flip()+
      theme_light()+
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())+
      xlab("Teams") +
      ylab("Theta")
    return(PLOT)
  }
  if(model$type == 'CHI'){
    # Common Hierarchical Home-ground Advantage Model
    for(i in model$Alpha[,]){
      table = data.frame(teams = model$Teams, Theta = model$Theta[,], home = model$Theta[,] + i)
      Teams = model$Teams
      Theta = table[order(-table$Theta),]
      print(Theta %>%
        arrange(Theta) %>%
        mutate(Teams = factor(teams, unique(teams))) %>%
        ggplot() +
        geom_segment(aes(x=Teams, xend=Teams, y=Theta, yend=home), color="black") +
        geom_point(aes(x=Teams, y=Theta), color=rgb(0.2,0.7,0.1,0.5), size=3, alpha = 0.6 ) +
        geom_point(aes(x=Teams, y=home), color=rgb(0.7,0.2,0.1,0.5), size=3, alpha = 0.6 ) +
        coord_flip()+
        theme_light()+
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank())+
        xlab("Teams") + ylab("Theta"))
    Sys.sleep(10)
    }
  }
  
  if(model$type == 'TSH'){
    # Team-specific Home-ground Advantage Model
    table = data.frame(teams = model$Teams, model$Theta, home = NA)
    for (i in 1:nrow(model$Theta)){
      table$home[i] <- model$Theta[i,] + model$Alpha[i,]
    }
    Teams = model$Teams
    Theta = table[order(-table$Theta),]
    PLOT <- Theta %>%
      arrange(Theta) %>%
      mutate(Teams = factor(teams, unique(teams))) %>%
      ggplot() +
      geom_segment(aes(x=Teams, xend=Teams, y=Theta, yend=home), color="black") +
      geom_point(aes(x=Teams, y=Theta), color=rgb(0.2,0.7,0.1,0.5), size=3, alpha = 0.6 ) +
      geom_point(aes(x=Teams, y=home), color=rgb(0.7,0.2,0.1,0.5), size=3, alpha = 0.6 ) +
      coord_flip()+
      theme_light()+
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())+
      xlab("Teams") +
      ylab("Theta")
    return(PLOT)
  }
  if(model$type == "HIE"){
    # Hierarchical Home-ground Advantage Model
    for(i in 1:ncol(model$Alpha)){
      table = data.frame(teams = model$Teams, Theta = model$Theta[,], home = model$Theta[,] + model$Alpha[,i])
      Teams = model$Teams
      Theta = table[order(-table$Theta),]
      print(Theta %>%
              arrange(Theta) %>%
              mutate(Teams = factor(teams, unique(teams))) %>%
              ggplot() +
              geom_segment(aes(x=Teams, xend=Teams, y=Theta, yend=home), color="black") +
              geom_point(aes(x=Teams, y=Theta), color=rgb(0.2,0.7,0.1,0.5), size=3, alpha = 0.6 ) +
              geom_point(aes(x=Teams, y=home), color=rgb(0.7,0.2,0.1,0.5), size=3, alpha = 0.6 ) +
              coord_flip()+
              theme_light()+
              theme(
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                axis.ticks.y = element_blank())+
              xlab("Teams") + ylab("Theta"))
      Sys.sleep(10)
    }
  }
}

BT_predict = function(model, df, R = NULL){
  # Predicts the probability of either side winning a match based off the fitted BT Model
  # Data frame consists of Team A, Team B, Team A Home, Team B Home
  Table <- model$Table
  for (i in 1:nrow(df)){
    if(df[i,1] %in% model$Teams & df[i,2] %in% model$Teams){
      if(df[i,3] != df[i,4]){
        # Essentially saying Team A Home = 0 and Team B Home = 1
        if(model$type == 'VAN'){
          df$Team_B_win[i] = exp(Table$Theta[Table$teams == df[i,2]])/(exp(Table$Theta[Table$teams == df[i,2]]) + exp(Table$Theta[Table$teams == df[i,1]]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'CHA'){
          df$Team_B_win[i] = exp(Table$Theta[Table$teams == df[i,2]] + model$Alpha)/(exp(Table$Theta[Table$teams == df[i,2]] + model$Alpha) + exp(Table$Theta[Table$teams == df[i,1]]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'CHI'){
          df$Team_B_win[i] = exp(Table[df[i,2],] + model$Alpha[R[df[i,2],df[i,1]],])/(exp(Table[df[i,2],] + model$Alpha[R[df[i,2],df[i,1]],]) + exp(Table[df[i,1],]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'TSH'){
          Alpha <- model$Alpha
          df$Team_B_win[i] = exp(Table[df[i,2],] + Alpha[df[i,2],])/(exp(Table[df[i,2],] + Alpha[df[i,2],]) + exp(Table[df[i,1],]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'HIE'){
          Alpha <- model$Alpha
          df$Team_B_win[i] = exp(Table[df[i,2],] + model$Alpha[df[i,2], R[df[i,2],df[i,1]]])/(exp(Table[df[i,2],] + model$Alpha[df[i,2], R[df[i,2],df[i,1]]]) + exp(Table[df[i,1],]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
      } 
      if(df[i,3] == df[i,4]){
        # Essentially saying Team A Home = 0 and Team B Home = 0
        if(model$type == 'VAN'){
          df$Team_B_win[i] = exp(Table$Theta[Table$teams == df[i,2]])/(exp(Table$Theta[Table$teams == df[i,2]]) + exp(Table$Theta[Table$teams == df[i,1]]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'CHA'){
          df$Team_B_win[i] = exp(Table$Theta[Table$teams == df[i,2]])/(exp(Table$Theta[Table$teams == df[i,2]]) + exp(Table$Theta[Table$teams == df[i,1]]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'CHI'){
          df$Team_B_win[i] = exp(Table[df[i,2],])/(exp(Table[df[i,2],]) + exp(Table[df[i,1],]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'TSH'){
          Alpha <- model$Alpha
          df$Team_B_win[i] = exp(Table[df[i,2],])/(exp(Table[df[i,2],]) + exp(Table[df[i,1],]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'HIE'){
          Alpha <- model$Alpha
          df$TeamB_win[i] = exp(Table[df[i,2],])/(exp(Table[df[i,2],]) + exp(Table[df[i,1],]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
      }
    }
  }
  return(df)
}
