# check out the infographic that Arsenal F.C. made for this: 
# https://twitter.com/Arsenal/status/544437197714501632

# The CL draw works as follows:
# The runner-up from each group will be drawn against a winner from another group.
# There are two constraints, however:
# 1. teams from the same group   cannot be drawn against eachother.
# 2. teams from the same country cannot be drawn against eachother.
#
# I feel it's much easier to first perform draws as if there are no constraints 
# and afterwards eliminate the draws that violate constraints.
# This will give a desired number of draws that do not violate constraints.
# We can then see how many times each team is drawn against each opponent.


# teams that made it through ----------------------------------------------
# add the teams in the right order (group A winner, group A runner up, group B winner, group B runner up, ...)
# add the teams' countries as well with a country code of your choice

A1 <- c("united","e")
A2 <- c("basel","sw")
B1 <- c("psg","f")
B2 <- c("bayern","d")
C1 <- c("chelsea","e")
C2 <- c("roma","i")
D1 <- c("barcelona","s")
D2 <- c("juventus","i")
E1 <- c("liverpool","e")
E2 <- c("sevilla","s")
F1 <- c("city","e")
F2 <- c("donetsk","u")
G1 <- c("besiktas","t")
G2 <- c("porto","p")
H1 <- c("tottenham","e")
H2 <- c("madrid","s")

teams <- as.data.frame(cbind(rbind(A1,B1,C1,D1,E1,F1,G1,H1),rbind(A2,B2,C2,D2,E2,F2,G2,H2)))
names(teams) <- c("away","away.country","home","home.country") # winners will be away team in next game
nrgames <- nrow(teams) # how many games will be played in the next round?


# constraints (which teams cannot be drawn against each other) ------------
# constraints1 = teams from same group
constraints1 <- matrix(data=NA,nrow=nrgames,ncol=2) 
constraints1[,1] <- levels(teams$home)[teams$home]
constraints1[,2] <- levels(teams$away)[teams$away]

# constraints2 = teams from same country
constraints2 <- matrix(data=NA,nrow=0,ncol=2)
countries <- unique(c(levels(teams$away.country),levels(teams$home.country)))
for (i in 1:length(countries)){
  dummy <- expand.grid(teams$home[teams$home.country == countries[i]], teams$away[teams$away.country == countries[i]])
  constraints2 <- rbind(constraints2,dummy)
}

# put the constraints together in 1 dataframe
constraints <- rbind(constraints1,as.matrix(constraints2)) 

# create matrix for draws -------------------------------
k = 1
totalamountofsimulations <- 1 # Keep track of how many simulations you need to draw to get a certain amount of "good" ones. By good I mean a simulation that does not violate any constraint.
simulations <- 10000 # Set the amount of desired "good" simulations.
amountofconstraints <- nrow(constraints) # create a variable with the amount of constraints

draw <- matrix (data=NA,nrow=simulations,ncol=nrgames*2+amountofconstraints+nrgames) 
# Create a dataset. Each row = one "good" simulation. 
# In each row, column 1 = home team for game 1, column 2 = home team for game 2, ...
# column 9 = away team for game 1, column 10 = away team for game 2, ...
# column 17 checks whether constraint 1 is violated in any of the games, column 18 checks ... constraint 2 ..., ...
# the last 8 columns give the opponent for hometeam1 = juventus, opponent for hometeam2 = basel, ...
colnames(draw) <- c( paste( rep("game",nrgames*2), c(seq(1:nrgames),seq(1:nrgames)), c(rep("_home_team",nrgames),rep("_away_team",nrgames) ),sep="") , paste("constraint",seq(1:amountofconstraints),sep="") , paste(teams$home,"_opponent",sep=""))

firstconstraintcolumn <- nrgames*2+1                        # in case of CL: 8*2+1  = 17
lastconstraintcolumn  <- nrgames*2 + amountofconstraints    # in case of CL: 8*2+15 = 31

# begin the draw -----------------------------
while (k < simulations+1) { # start simulation
  # One simulation = One unconstrained draw (i.e., could include games that are disallowed)
  # Draw 8 home teams, draw 8 away teams, and then put in zero's where the constraints are (these zeroes will be replaced soon)
  draw[k,] <- c(sample(levels(teams$home),nrgames,replace=F),sample(levels(teams$away),nrgames,replace=F),numeric(amountofconstraints+nrgames))
  violation_already <- 0 # Later we will go through each of the constraints. As soon as one is violated, there is no need to check the other constraints.
  
  for (j in 1:amountofconstraints) 
  {  # go through each constraint
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
  
  violations <- sum(as.numeric(draw[k,firstconstraintcolumn:lastconstraintcolumn])) # any violations?
  if(violations==0){k <- k+1} # only if no violations, go on with next simulation ; if violation, redo k
  totalamountofsimulations <- totalamountofsimulations + 1 # keep track of how many good+bad simulations are necessary to get the desired amount of good simulations
}


# Draw is done. Who plays who? ------------------------------------------------------------
# By now we have a dataset with 8 games in it that do not violate any constraints.
# Now we concentrate on the last 8 columns: For each draw, who is the opponent of hometeam1, who is the opponent of hometeam2, ...

for (k in 1:simulations) {
  for (l in 1:nrgames)  {                                # go through each home team and check who is the opponent
    opponent <- which(draw[k,]==teams$home[l]) + nrgames # in which column can I find hometeam l's opponent
    correctcolumn <- nrgames*2+amountofconstraints+l     # in which column should I put hometeam l's opponent
    draw[k,correctcolumn] <- draw[k,opponent]            # put hometeam l's opponent in the correct column
  }
}

# Get probabilities now: For each hometeam, how many times was opponent A drawn, how many times was opponent B drawn, ... 
probabilities <- matrix(data=NA,nrow=nrgames,ncol=nrgames)

for (i in 1:nrgames)   # for each home team
{
  for (k in 1:nrgames) # for each away team
  {
    probabilities[k,i] <- length(which(draw[,nrgames*2+amountofconstraints+i]==teams$away[k]))/simulations*100
  }
}

# probabilities are calculated --------------------------------------------
rownames(probabilities) <- teams$away
colnames(probabilities) <- teams$home
probabilities
# write.csv(probabilities,"prob.csv")
