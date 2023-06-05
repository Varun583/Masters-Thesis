DesiredFormatNBA = function(df){
  # Convert the NBA Data as shown in image to n x 6 matrix where the columns are mentioned like below.
  BT <- data.frame(matrix(ncol=6,nrow=nrow(df), dimnames=list(NULL, c("Team A", "Team B", "Team A win", "Team B win", "Team A home", "Team B home"))))
  my_range <- 1:nrow(df)
  for(i in my_range){  
    BT[i,1] <- df[i,3]
    BT[i,2] <- df[i,5]
  }
  
  for (i in my_range){
    BT[i,3] <- ifelse(df[i,4] > df[i,6], 1, 0)
    BT[i,4] <- ifelse(df[i,6] > df[i,4], 1, 0)
    
    BT[i,5] <- ifelse(BT[i,1] == df[i,5], 1, 0)
    BT[i,6] <- ifelse(BT[i,2] == df[i,5], 1, 0)
  }
  return(BT)
}
# Since neutral venues are special cases, we should make manual entries of them in the data. 
# We choose the appropriate games and change the 6th column value to 0 after the function has been executed.

DesiredFormatBlast = function(df){
  # Convert the T20 Blast Data as shown in image to n x 6 matrix where the columns are mentioned like below.
  Blast <- data.frame(matrix(ncol=6, nrow = nrow(df), dimnames=list(NULL, c("Team A", "Team B", "Team A win", "Team B win", "Team A home", "Team B home"))))
  my_range <- 1:nrow(df)
  for(i in my_range){  
    Blast[i,1] <- df[i,2]
    Blast[i,2] <- df[i,1]
    Blast[i,5] <- 0
    Blast[i,6] <- 1
  }
  for(i in my_range){
    if(df[i,3] == Blast[i,2]){
      Blast[i,4] <- 1
      Blast[i,3] <- 0
    } else {
      Blast[i,3] <- 1
      Blast[i,4] <- 0
    }
  }
  for(i in my_range){
    if (df[i,3] == "tied"){
      Blast[i,3] <- 0.5
      Blast[i,4] <- 0.5
    }
    if (df[i,3] == "no result"){  
      Blast[i,3] <- 0
      Blast[i,4] <- 0
    }
  }
  Blast[Blast == "Derbyshire"] <- "Derbyshire Falcons"
  Blast[Blast == "Essex"] <- "Essex Eagles"
  Blast[Blast == "Gloucs"] <- "Gloucestershire"
  Blast[Blast == "Hampshire"] <- "Hampshire Hawks"
  Blast[Blast == "Kent"] <- "Kent Spitfires"
  Blast[Blast == "Lancashire"] <- "Lanchashire Lightning"
  Blast[Blast == "Leics"] <- "Leicestershire Foxes"
  Blast[Blast == "Northants"] <- "Northamptonshire Steelbacks"
  Blast[Blast == "Notts"] <- "Notts Outlaws"
  Blast[Blast == "Sussex"] <- "Sussex Sharks"
  Blast[Blast == "WORCS"] <- "Worcestershire Rapids"
  Blast[Blast == "Yorkshire"] <- "Yorkshire Vikings"
  Blast[Blast == "Bears"] <- "Warwickshire Bears"
  return(Blast)
}

# Since neutral venues are special cases, we should make manual entries of them in the data. 
# We choose the appropriate games and change the 6th column value to 0 after the function has been executed.
# Some additional formatting has been done to make the data more formal

Data2Mat = function(df){
  # Data frame should have the columns 'Team.A', 'Team.B', 'Team.A.win', 'Team.B.win', 'Team.A.Home', 'Team.B.Home', as the desired format
  # This function then returns the h, w, nw, l and nl matrices required for our BT package.
  # In the NBA data, Team B was always the home team. Hence, essentially, 'Team.A.Home' is a column of 0s.
  
  Teams = unique(df$Team.B)
  
  df[,3] = as.numeric(df[,3])
  df[,4] = as.numeric(df[,4])
  df[,6] = as.numeric(df[,6])
  
  w = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  l = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  
  nw = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  nl = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  
  for(i in 1:nrow(df)){
    if (df[i,6] == 1){
      row = df[i,]
      w[row$Team.B, row$Team.A] = w[row$Team.B, row$Team.A] + (row$Team.B.win == 1) 
      l[row$Team.B, row$Team.A] = l[row$Team.B, row$Team.A] + (row$Team.A.win == 1) 
    }
  }  
  for(i in 1:nrow(df)){
    if (df[i,6] == 0){
      row = df[i,]
      nw[row$Team.B, row$Team.A]  = nw[row$Team.B, row$Team.A] + (row$Team.B.win == 1)
      nw[row$Team.A, row$Team.B]  = nw[row$Team.A, row$Team.B] + (row$Team.A.win == 1)
      nl[row$Team.A, row$Team.B]  = nl[row$Team.A, row$Team.B] + (row$Team.B.win == 1)
      nl[row$Team.B, row$Team.A]  = nl[row$Team.B, row$Team.A] + (row$Team.A.win == 1)
    }
  }  
  
  h = w + l
  return(list(h=h, w=w, l=l, nw = nw, nl = nl, Teams = Teams))
}

# This function returns the number of parameters for a specific type of model
ntheta = function(mat, model_number, R = NULL){
  if(model_number == 1){return(length(mat$Teams))}
  if(model_number == 2){return(length(mat$Teams) + 1)}
  if(model_number == 3){return(length(mat$Teams) + max(R))}
  if(model_number == 4){return(2*length(mat$Teams))}
  if(model_number == 5){return((1 + max(R))*length(mat$Teams))}
  if(model_number == 6){return(length(mat$Teams)*(length(mat$Teams) + 1))}
}

# Defining a log-likelihood function - Vanilla model
Log_Like1 = function(theta, mat){
  # mat contains mat$h, mat$w, mat$l, mat$nw, mat$nl
  # theta is the vector of thetas (skill levels)
  Teams = colnames(mat$h)
  
  log_like1 = 0 
  
  for(i in 1:length(Teams)){
    for(j in 1:length(Teams)){
      log_like1 = log_like1 + (mat$w[i, j] + mat$l[j, i] + mat$nw[i, j])*(theta[i] - log(exp(theta[i]) + exp(theta[j])))
    }
  }
  return(-log_like1)
}

# Defining a gradient for the likelihood function 
Log_Like_deriv1 = function(theta, mat){
  Teams = colnames(mat$h)
  output = rep(0,ntheta(mat, 1, R))
  for(i in 1:length(Teams)){
    log_like_deriv1 = 0
    for(j in 1:length(Teams)){
      log_like_deriv1 = log_like_deriv1 - (mat$h[i,j] + mat$h[j,i] + mat$nw[i,j] + mat$nw[j,i])*((exp(theta[i]))/(exp(theta[i]) + exp(theta[j]))) 
    }
    output[i] = sum(mat$w[i,]) + sum(mat$l[,i]) + sum(mat$nw[i,]) + log_like_deriv1
  }
  return(-1*output)
}

# Defining a log-likelihood function - Common Homeground model
Log_Like2 = function(theta, mat){
  # mat contains mat$h, mat$w, mat$l, mat$nw, mat$nl
  # theta is the vector of thetas (skill levels)
  Teams = colnames(mat$h)
  number = length(Teams)
  
  log_like2 = 0 
  
  for(i in 1:number){
    for(j in 1:number){
      log_like2 = log_like2 + 
        mat$w[i,j]*(theta[i] + theta[1+number] - log(exp(theta[i] + theta[1+number]) + exp(theta[j]))) +
        mat$l[j,i]*(theta[i] - log(exp(theta[i]) + exp(theta[1+number] + theta[j]))) +
        mat$nw[i,j]*(theta[i] - log(exp(theta[i])+exp(theta[j])))
    }
  }
  return(-1*log_like2)
}

# Defining a gradient for the likelihood function - Common Homeground
Log_Like_deriv2 = function(theta, mat){
  Teams = colnames(mat$h)
  number = length(Teams)
  output = rep(0,ntheta(mat, 2, R))
  
  # dL/dtheta
  for(i in 1:number){
    log_like_deriv2 = 0
    for(j in 1:number){
      log_like_deriv2 = log_like_deriv2 - 
        (mat$h[i,j])*((exp(theta[i] + theta[1+number]))/(exp(theta[i] + theta[1+number]) + exp(theta[j]))) -
        (mat$h[j,i])*((exp(theta[i]))/(exp(theta[j] + theta[1+number]) + exp(theta[i]))) -
        (mat$nw[i,j])*(exp(theta[i]))/(exp(theta[i]) + exp(theta[j])) - 
        (mat$nw[j,i])*(exp(theta[i]))/(exp(theta[i]) + exp(theta[j]))
    }
    output[i] = sum(mat$w[i,]) + sum(mat$l[,i]) + sum(mat$nw[i,]) + log_like_deriv2
  }
  
  # dL/dAlpha
  output[number + 1] = 0
  for(i in 1:number){
    alpha = 0
    for(j in 1:number){
      alpha = alpha - ((mat$h[i,j])*(exp(theta[i] + theta[1+number]))/(exp(theta[i] + theta[1+number]) + exp(theta[j])))  
    }
    output[number + 1] = output[number + 1] + sum(mat$w[i,]) + alpha
  }
  return(-1*output)
}

# Relationship Matrix for the NBA when we input the formatted fixtures 
NBA_R = function(fixtures){
  data = fixtures
  Teams = unique(fixtures$Team.B)
  
  Atlantic = c('Boston Celtics', 'Brooklyn Nets', 'New York Knicks', 'Philadelphia 76ers', 'Toronto Raptors')
  Central = c('Chicago Bulls', 'Cleveland Cavaliers', 'Detroit Pistons', 'Indiana Pacers', 'Milwaukee Bucks')
  Southeast = c('Atlanta Hawks', 'Charlotte Hornets', 'Miami Heat', 'Orlando Magic', 'Washington Wizards')
  Northwest = c('Denver Nuggets', 'Minnesota Timberwolves', 'Oklahoma City Thunder', 'Portland Trail Blazers', 'Utah Jazz')
  Pacific = c('Golden State Warriors', 'Los Angeles Clippers', 'Los Angeles Lakers', 'Phoenix Suns','Sacramento Kings')
  Southwest = c('Dallas Mavericks', 'Houston Rockets','Memphis Grizzlies','New Orleans Pelicans','San Antonio Spurs')
  
  D = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  C = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  L = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  R = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  
  East = data.frame(Atlantic, Southeast, Central)
  West = data.frame(Southwest, Pacific, Northwest)
  NBA = data.frame(East, West)
  
  # Basic designation of relationship
  for(i in Teams){
    for(j in Teams){
      if(i != j){
        L[i, j] = 1
      }
    }
  }
  
  # Designating Conferences
  for(con1 in 1:ncol(East)){
    for(con2 in 1:ncol(East)){
      for(i in 1:nrow(East[con1])){
        for(j in 1:nrow(East[con2])){
          if(East[i, con1] != East[j, con2]){
            C[East[i, con1], East[j, con2]] = 1
          }
        }
      }
    }
  }
  
  for(con1 in 1:ncol(West)){
    for(con2 in 1:ncol(West)){
      for(i in 1:nrow(West[con1])){
        for(j in 1:nrow(West[con2])){
          if(West[i, con1] != West[j, con2]){
            C[West[i, con1], West[j, con2]] = 1
          }  
        }
      }
    }
  }
  
  # Designating Divisions
  for(div in 1:ncol(NBA)){
    for(i in 1:nrow(NBA[div])){
      for(j in 1:nrow(NBA[div])){
        if(NBA[i, div] != NBA[j, div])
          D[NBA[i, div], NBA[j, div]] = 1
      }
    }
  }
  
  R = C + D + L
  return(R)
}

R <- NBA_R(fixtures)

# Relationship Matrix for the T20 Blast when we input the formatted fixtures 
Blast_R = function(fixtures){
  data = fixtures
  Teams = unique(fixtures$Team.B)

  G = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  L_Blast = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  R = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))

  North = c("Derbyshire Falcons", "Durham", "Lancashire Lightning", "Leicestershire Foxes", "Northamptonshire Steelbacks", "Nottinghamshire Outlaws", "Warwickshire Bears", "Worcestershire Rapids", "Yorkshire Vikings")
  South = c("Essex Eagles", "Glamorgan", "Gloucestershire", "Hampshire Hawks", "Kent Spitfires", "Middlesex", "Somerset", "Surrey", "Sussex Sharks")
  League = data.frame(North, South)
  
  # Basic designation of relationship
  for(i in Teams){
    for(j in Teams){
      if(i != j){
        L_Blast[i, j] = 1
      }
    }
  }
  
  # Designating groups
  for(gro in 1:ncol(League)){
    for(i in 1:nrow(League[gro])){
      for(j in 1:nrow(League[gro])){
        if(League[i, gro] != League[j, gro])
          G[League[i, gro], League[j, gro]] = 1
      }
    }
  }


  R_Blast = G + L_Blast
  return(R_Blast)
}

R_Blast = Blast_R(fixtures)

# Defining a log-likelihood function - Common Hierarchical model 
Log_Like3 = function(theta, mat, R){
  # mat contains mat$h, mat$w, mat$l, mat$nw, mat$nl
  # theta is the vector of thetas (skill levels)
  Teams = colnames(mat$h)
  number = length(Teams)
  
  log_like3 = 0 
  
  for(i in 1:number){
    for(j in (1:number)[-i]){
      log_like3 = log_like3 + 
        mat$w[i,j]*(theta[i] + theta[R[i,j] + number] - log(exp(theta[i] + theta[R[i,j] + number]) + exp(theta[j]))) +
        mat$l[j,i]*(theta[i] - log(exp(theta[i]) + exp(theta[R[i,j] + number] + theta[j]))) +
        mat$nw[i,j]*(theta[i] - log(exp(theta[i])+exp(theta[j])))
    }
  }
  return(-1*log_like3)
}

# Defining a gradient for the likelihood function - Common Hierarchical
Log_Like_deriv3 = function(theta, mat, R){
  Teams = colnames(mat$h)
  number = length(Teams)
  output = rep(0, ntheta(mat, 3, R))
  
  # dL/dtheta
  for(i in 1:number){
    log_like_deriv3 = 0
    for(j in (1:number)[-i]){
      log_like_deriv3 = log_like_deriv3 - 
        (mat$h[i,j])*((exp(theta[i] + theta[R[i,j]+number]))/(exp(theta[i] + theta[R[i,j]+number]) + exp(theta[j]))) -
        (mat$h[j,i])*((exp(theta[i]))/(exp(theta[j] + theta[R[i,j]+number]) + exp(theta[i]))) -
        (mat$nw[i,j])*(exp(theta[i]))/(exp(theta[i]) + exp(theta[j])) - 
        (mat$nw[j,i])*(exp(theta[i]))/(exp(theta[i]) + exp(theta[j]))
    }
    output[i] = sum(mat$w[i,]) + sum(mat$l[,i]) + sum(mat$nw[i,]) + log_like_deriv3
  }
  
  # dL/dAlpha
  for (i in 1:number){ 
    for(j in 1:number){
      output[number + R[i,j]] = output[number + R[i,j]] + mat$w[i,j] - ((mat$h[i,j])*(exp(theta[i] + theta[R[i,j]+number]))/(exp(theta[i] + theta[R[i,j]+number]) + exp(theta[j])))  
    }
  }
  return(-1*output)
}

# Defining a log-likelihood function - Team-Specific Homeground model
Log_Like4 = function(theta, mat){
  # mat contains mat$h, mat$w, mat$l, mat$nw, mat$nl
  # theta is the vector of thetas (skill levels)
  Teams = colnames(mat$h)
  number = length(Teams)
  
  log_like4 = 0 
  
  for(i in 1:number){
    for(j in 1:number){
      log_like4 = log_like4 + 
        mat$w[i,j]*(theta[i] + theta[i+number] - log(exp(theta[i] + theta[i+number]) + exp(theta[j]))) +
        mat$l[j, i]*(theta[i] - log(exp(theta[i]) + exp(theta[j+number] + theta[j]))) +
        mat$nw[i,j]*(theta[i] - log(exp(theta[i])+exp(theta[j])))
    }
  }
  return(-1*log_like4)
}

# Defining a gradient for the likelihood function - Team-Specific Homeground
Log_Like_deriv4 = function(theta, mat){
  Teams = colnames(mat$h)
  number = length(Teams)
  output = rep(0,ntheta(mat, 4, R))
  
  # dL/dtheta
  for(i in 1:number){
    log_like_deriv4 = 0
    for(j in 1:number){
      log_like_deriv4 = log_like_deriv4 - 
        (mat$h[i,j])*((exp(theta[i] + theta[i+number]))/(exp(theta[i] + theta[i+number]) + exp(theta[j]))) -
        (mat$h[j,i])*((exp(theta[i]))/(exp(theta[j] + theta[j+number]) + exp(theta[i]))) -
        (mat$nw[i,j])*(exp(theta[i]))/(exp(theta[i]) + exp(theta[j])) - 
        (mat$nw[j,i])*(exp(theta[i]))/(exp(theta[i]) + exp(theta[j]))
    }
    output[i] = sum(mat$w[i,]) + sum(mat$l[,i]) + sum(mat$nw[i,]) + log_like_deriv4
  }
  
  # dL/dAlpha
  for(i in 1:number){
    alpha = 0
    for(j in 1:number){
      alpha = alpha - ((mat$h[i,j])*(exp(theta[i] + theta[i+number]))/(exp(theta[i] + theta[i+number]) + exp(theta[j])))  
    }
    output[number + i] = sum(mat$w[i,]) + alpha
  }
  return(-1*output)
}

# Defining a log-likelihood function - Hierarchical model 
Log_Like5 = function(theta, mat, R){
  # mat contains mat$h, mat$w, mat$l, mat$nw, mat$nl
  # theta is the vector of thetas (skill levels)
  Teams = colnames(mat$h)
  number = length(Teams)
  
  log_like5 = 0 
  
  for(i in (1:number)){
    for(j in (1:number)[-i]){
      log_like5 = log_like5 + 
        mat$w[i,j]*(theta[i] + theta[i + number * R[i,j]] - log(exp(theta[i] + theta[i + number * R[i,j]]) + exp(theta[j]))) + 
        mat$l[j,i]*(theta[i] - log(exp(theta[i]) + exp(theta[j + number * R[i,j]] + theta[j]))) +
        mat$nw[i,j]*(theta[i] - log(exp(theta[i]) + exp(theta[j])))
    }
  }
  return(-1*log_like5)
}

# Defining a gradient for the likelihood function - Hierarchical model
Log_Like_deriv5 = function(theta, mat, R){
  Teams = colnames(mat$h)
  number = length(Teams)
  output = rep(0,ntheta(mat, 5, R))
  
  # dL/dtheta
  for(i in 1:number){
    log_like_deriv5 = 0
    for(j in (1:number)[-i]){
      log_like_deriv5 = log_like_deriv5  + mat$w[i,j] + mat$l[j,i] + mat$nw[i,j] - 
        (mat$h[i,j] * (exp(theta[i] + theta[i + number * R[i,j]])/(exp(theta[i] + theta[i + number * R[i,j]]) + exp(theta[j])))) -
        (mat$h[j,i] * (exp(theta[i])/(exp(theta[j] + theta[j + number * R[j,i]]) + exp(theta[i])))) -
        (mat$nw[i,j] * (exp(theta[i])/(exp(theta[i]) + exp(theta[j])))) - 
        (mat$nw[j,i] * (exp(theta[i])/(exp(theta[i]) + exp(theta[j]))))
    }
    output[i] = log_like_deriv5
  }
  
  # dL/dAlpha
  for (i in 1:number){ 
    for (j in (1:number)){
      output[i + number * R[i,j]] = output[i + number * R[i,j]] + mat$w[i,j] - 
        ((mat$h[i,j])*(exp(theta[i] + theta[i + number * R[i,j]]))/(exp(theta[i] + theta[i + number * R[i,j]]) + exp(theta[j])))   
    }
  }
  return(-1*output)
}  

# Defining a log-likelihood function - Pairwise Homeground model 
Log_Like6 = function(theta, mat){
  # mat contains mat$h, mat$w, mat$l, mat$nw, mat$nl
  # theta is the vector of thetas (skill levels)
  Teams = colnames(mat$h)
  number = length(Teams)
  
  log_like6 = 0 
  
  for(i in (1:number)){
    for(j in (1:number)[-i]){
      log_like6 = log_like6 + 
        mat$w[i,j]*((theta[i] + theta[i*number + j]) - log(exp(theta[i] + theta[i*number + j]) + exp(theta[j]))) + 
        mat$l[j,i]*(theta[i] - log(exp(theta[j] + theta[j*number + i]) + exp(theta[i]))) +
        mat$nw[i,j]*(theta[i] - log(exp(theta[i]) + exp(theta[j])))
    }
  }
  return(-1*log_like6)
}

# Defining a gradient for the likelihood function - Pairwise Homeground
Log_Like_deriv6 = function(theta, mat){
  Teams = colnames(mat$h)
  number = length(Teams)
  output = rep(0,ntheta(mat, 6, R))
  
  # dL/dtheta
  for(i in 1:number){
    log_like_deriv6 = 0
    for(j in (1:number)){
      log_like_deriv6 = log_like_deriv6 - 
        (mat$h[i,j] * exp(theta[i] + theta[i*number + j])/(exp(theta[i] + theta[i*number + j]) + exp(theta[j]))) -
        (mat$h[j,i] * exp(theta[i])/(exp(theta[j] + theta[j*number + i]) + exp(theta[i]))) -
        (mat$nw[i,j] * exp(theta[i])/(exp(theta[i]) + exp(theta[j]))) - 
        (mat$nw[j,i] * exp(theta[i])/(exp(theta[i]) + exp(theta[j])))
    }
    output[i] = sum(mat$w[i,]) + sum(mat$l[,i]) + sum(mat$nw[i,]) + log_like_deriv6
  }
  
  # dL/dAlpha
  for (i in 1:number){ 
    for(j in 1:number){
      output[i*number + j] = output[i*number + j] + mat$w[i,j] - (mat$h[i,j])*(exp(theta[i] + theta[i*number + j])/(exp(theta[i] + theta[i*number + j]) + exp(theta[j])))   
    }
  }
  return(-1*output)
}

PCT_Table <- function(matrix){
  #Returns the wins and losses of the teams, along with their winning percentage
  Wins <- NA
  Losses <- NA
  Teams <- matrix$Teams
  WL <- data.frame(Teams, Wins, Losses)
  for (i in 1:nrow(WL)){
    WL[i,2] <- sum(matrix$w[i,] + matrix$l[,i] + matrix$nw[i,])
    WL[i,3] <- sum(matrix$w[,i] + matrix$l[i,] + matrix$nl[i,])
  }
  WL$PCT <- WL$Wins/(WL$Losses + WL$Wins)
  WL <- WL[order(-WL$PCT),]
  return(WL)
}

Home_Table <- function(matrix){
  #Returns the home wins and losses of the teams, along with their home winning percentage
  Home_Wins <- NA
  Home_Losses <- NA
  Teams <- matrix$Teams
  WL <- data.frame(Teams, Home_Wins, Home_Losses)
  for (i in 1:nrow(WL)){
    WL[i,2] <- sum(matrix$w[i,])
    WL[i,3] <- sum(matrix$l[i,])
  }
  WL$PCT <- WL$Home_Wins/(WL$Home_Losses + WL$Home_Wins)
  WL <- WL[order(-WL$PCT),]
  return(WL)
}

