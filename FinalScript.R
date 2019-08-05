####Final R Script for Dissertation
#############################################################
#############################################################
#############        SOHAIB ANWER ###########################
#############################################################

## Date created: 20/3/'18
## Last modified: 3/8/'18

library(rjags)
library(runjags)
library(coda)
library(ggplot2)
library(mda)
library(xtable)
library(hBayesDM)

##Read the dataset
j1718 = read.csv("http://www.football-data.co.uk/mmz4281/1718/E0.csv",stringsAsFactors = FALSE)

#Create a vector with the list of team names

teams = unique(c(j1718$HomeTeam, j1718$AwayTeam))
teams = sort(teams)

#Create new variable assigning a number to each team for JAGS

j1718$HTnum = as.integer(as.factor(j1718$HomeTeam))
j1718$ATnum = as.integer(as.factor(j1718$AwayTeam))

############################################################
### Define JAGS model

model_string <- "model{
  #Likelihood

for (i in 1:n_games){

#goals scored by each team

  HG[i] ~ dpois(lambda[i])
  AG[i] ~ dpois(mu[i])


#avg intensities
  log(lambda[i]) = home + attack[HT[i]] + def[AT[i]]
  log(mu[i])     = attack[AT[i]] + def[HT[i]]

}



#Enforce contstraint sum to zero

for (j in 1:n_teams){
  attack.t[j] ~ dnorm(0,phi.att)
  def.t[j]    ~ dnorm(0,phi.def)

  attack[j] = attack.t[j] - mean(attack.t[])
  def[j]    = def.t[j]    - mean(def.t[])

}


#Priors

home ~ dnorm(0,0.0001)

phi.att ~ dgamma(1,1)

phi.def ~ dgamma(1,1)


}"

## Create new variables. PHG = Predicted home goals
##                       PAG = Predicted away goals
##                       PRes = Predicted Results
##                       PDiff = Predicted difference in goals between home and away side  

j1718$PHG = 0
j1718$PAG = 0
j1718$PRes = 0
j1718$PDiff = 0 



for (i in 8:37){
  newi = i*10
  newdf = j1718[1:newi,] #indexing data only up to that week
  
  #specifying variables for the JAGS model
  newdf_list = list(HG = newdf$FTHG, AG = newdf$FTAG, AT = newdf$ATnum, HT = newdf$HTnum,n_teams = 20,
                    n_games = nrow(newdf))
  
  ##Running the JAGS model
  
  mod = jags.model(textConnection(model_stringcc), data=newdf_list, n.chains = 2, n.adapt = 5000)
  
  ## Burn in iterations
  
  update(mod, 5000)
  
  ## Calculating posterior estimates for attack, def, and home adv parameter
  
  s1 <- coda.samples(mod, variable.names = c("home","attack","def"), n.iter = 10000, thin = 2)
  
  ## Predicting results of the next week
  
  j = newi+1
  k = j + 9
  c_1718 = combine.MCMC(s1)
  a_1718 = c_1718[,1:20]
  d_1718 = c_1718[,21:40]
  h_1718 = c_1718[,41]
  for (g in j:k){
    HTeam = j1718$HomeTeam[g]
    ATeam = j1718$AwayTeam[g]
    HTeamIndex = which(ht2 == HTeam)
    ATeamIndex = which(ht2 == ATeam)
    j1718$PHG[g] = median(rpois(50000, exp(as.numeric(a_1718half[,HTeamIndex]) + as.numeric(d_1718half[,ATeamIndex]) + as.numeric(h_1718))))
    j1718$PAG[g] = median(rpois(50000, exp(as.numeric(a_1718half[,ATeamIndex]) + as.numeric(d_1718half[,HTeamIndex]))))
    
  }
  
}

## Calculating difference between home goals and away goals in a match
j1718$PDiff = j1718$PHG - j1718$PAG

## Outputting predicted result based on PDiff
for (i in 1:380){
  if (k1718$PDiff[i] > 0)
    k1718$PRes[i] = "H"
  else if (k1718$PDiff[i] < 0)
    k1718$PRes[i] = 'A'
  else
    k1718$PRes[i] = 'D'
}


## Output confusion matrix of results

confusion(j1718$FTR,j1718$PRes)

## All plots code is in FinalPlots.R
