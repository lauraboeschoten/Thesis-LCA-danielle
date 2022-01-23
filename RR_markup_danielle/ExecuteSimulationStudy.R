#Simulation Study
setwd("C:/Users/danie/OneDrive/Statistiek master/Thesis/Thesis-LCA-danielle/Thesis-LCA-danielle/RR_markup_danielle")
getwd()
#load required packages
library(poLCA) #for data simulation
library(dplyr) #for data manipulation
library(confreq) #used for making bootstrap datasets 


#simulation parameters
nsim = 10 #increase to higher number of simulations when you have the time
nboot = 5 #FIXED, nboot determines the number of imputations per simulation iteration. 5 is sufficient.
populationsize = 5000 #for the data simulation

#execute respective scripts
source("1_SimulateData.R")    #Simulate the data
source("2_BootstrapData.R")   #Bootstrap the data
source("3_LCmodel.R")         #apply LC model to the data
source("4_Imputations.R")     #create imputations by sampling from the posterior probabilities
source("5_Results.R")         #Calculate bias of the overall group sizes (proportions of the classes)
Average #the pooled average class sizes
trueclass #original simulated class sizes
bias = Average-trueclass
save.image("Simulation_Multivariate.RData")