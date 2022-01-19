#Simulation Study

#load required packages
require(poLCA)

#load simulation/evaluation functions
source("Functions/Simulate.R")
source("Functions/NAME.R") #Barnard and Rubin (1999) df rules used

#simulation parameters
nboot = 5
populationsize = 1000
nsim = 10000

#execute respective scripts
source("Simulation conditions/Simulation_multivariate 10.R")
source("Simulation conditions/Simulation_multivariate 20.R")


rm(SIM)

save.image("Workspaces/Simulation_Multivariate.RData")