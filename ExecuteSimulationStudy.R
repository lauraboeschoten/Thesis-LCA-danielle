#Simulation Study

#load required packages
library(poLCA) #for data simulation
library(confreq) #used for making bootstrap datasets 

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


#R file with all steps for MILC simulation study (data simulation, bootstraps,LC model, imputations, and results)



save.image("Workspaces/Simulation_Multivariate.RData")