rm(list=ls())

# global variables -------------------------
# away teams = winners, put them in the correct order (group A, group B, group C, ...)
# home teams = runners up, in correct order
awayteams <- c("atletico", "real",  "monaco",    "dortmund","munchen","barcelona", "chelsea", "porto")  
hometeams <- c("juventus", "basel", "leverkusen","arsenal", "city",   "psg",       "schalke", "donetsk")

nrgames <- length(awayteams) # how many games will be played in the next round?

# create a matrix for games that are not allowed
# constraints1 = teams from same group
constraints1 <- matrix(data=NA,nrow=nrgames,ncol=2) 
constraints1[,1] <- hometeams
constraints1[,2] <- awayteams

# constraints2 = teams from same country
constraints2 <- matrix(data=NA,nrow=7,ncol=2) 
constraints2[1,] <- c("leverkusen","dortmund")
constraints2[2,] <- c("leverkusen","munchen")
constraints2[3,] <- c("arsenal","chelsea")
constraints2[4,] <- c("city","chelsea")
constraints2[5,] <- c("psg","monaco")
constraints2[6,] <- c("schalke","dortmund")
constraints2[7,] <- c("schalke","munchen")

constraints <- rbind(constraints1,constraints2)

amountofconstraints <- nrow(constraints) # create a variable with the amount of constraints

# constraints are set, begin the simulation -------------------------------
k = 1
totalamountofsimulations <- 1 # Keep track of how many simulations you need to draw to get a certain amount of "good" ones. By good I mean a simulation that does not violate any constraint.
simulations <- 10000          # Set the amount of desired "good" simulations.

draw <- matrix (data=NA,nrow=simulations,ncol=nrgames*2+amountofconstraints+nrgames) 
# Create a dataset. Each row = one "good" simulation. 
# In each row, column 1 = home team for game 1, column 2 = home team for game 2, ...
# column 9 = away team for game 1, column 10 = away team for game 2, ...
# column 17 checks whether constraint 1 is violated in any of the games, column 18 checks ... constraint 2 ..., ...
# the last 8 columns give the opponent for hometeam1 = juventus, opponent for hometeam2 = basel, ...

firstconstraintcolumn <- nrgames*2+1
lastconstraintcolumn  <- nrgames*2 + amountofconstraints


# dataset for draw is created, begin the draw -----------------------------
while (k < simulations+1) { # start a loop
  # One simulation = One unconstrained draw (i.e., could include games that are disallowed)
  # Draw 8 home teams, draw 8 away teams, and then put in zero's that will be replaced soon
  draw[k,] <- c(sample(hometeams,nrgames,replace=F),sample(awayteams,nrgames,replace=F),numeric(amountofconstraints+nrgames))
  violation_already <- 0 # Later we will go through each of the constraints. As soon as one is violated, the draw should be dismissed to reduce computation time.

  for (j in 1:amountofconstraints) {  # go through each constraint
    if (violation_already == 0) {     # as long as there is not already a violation 
      for (i in 1:nrgames){     # go through each game in the draw
        if (draw[k,i] == constraints[j,1] & draw[k,i+nrgames] == constraints[j,2]) # check whether a game is disallowed
          {
          draw[k,nrgames*2+j] <- 1 # if constraint j is violated
          violation_already <- 1
          } 
        }
      }
    }

for (l in 1:nrgames)  {                               # go through each home team and check who is the opponent
 opponent <- which(draw[k,]==hometeams[l]) + nrgames  # who is home team l's opponent?
 correctcolumn <- nrgames*2+amountofconstraints+l     # what is home team l's column?
 draw[k,correctcolumn] <- draw[k,opponent]                  # keep track of home team l's opponent in correct column
}

violations <- sum(as.numeric(draw[k,firstconstraintcolumn:lastconstraintcolumn])) # any violations?
if(violations==0){k <- k+1} # only if no violations, go on with next simulation
totalamountofsimulations <- totalamountofsimulations + 1 # keep track of how many good+bad simulations are necessary to get the desired amount of good simulations
}


# simulation done. get probabilities now ----------------------------------
probabilities <- matrix(data=NA,nrow=nrgames,ncol=nrgames)

for (i in 1:nrgames)   # for each home team
{
  for (k in 1:nrgames) # for each away team
  {
probabilities[k,i] <- length(which(draw[,nrgames*2+amountofconstraints+i]==awayteams[k]))/simulations*100
  }
}

rownames(probabilities) <- awayteams
colnames(probabilities) <- hometeams
probabilities
