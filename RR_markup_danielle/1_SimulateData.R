#-------------------------------1. DATA SIMULATION-----------#
#necessary packages
library(poLCA) #for data simulation
library(dplyr) #for data manipulation

#Variant A (5% selection error and 5% measurement error)
options(scipen = 999)
set.seed(123)
nsim=10
populationsize = 5000

SimData = list(NA) #empty list to store simulated data in
for (sim in 1:nsim) { #iteration over number of simulations
  
#5% selection error
select_5 <-  list(matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                  matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                  matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                  matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T))

#5% measurement error
meas_5 <- list(matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
               matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
               matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
               matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE)) 

# poLCA simdata object selection part
mod1 <- poLCA.simdata(N       = populationsize,
                      nclass  = 2,
                      probs   = select_5, 
                      P       = c(0.15, 0.85), 
                      missval = F)

# simulated dataset selection part
df1 <- cbind(mod1$dat,
             trueclass=mod1$trueclass)

# poLCA simdata object measurement part
mod2 <- poLCA.simdata(N       = populationsize, 
                      nclass  = 3,
                      probs   = meas_5,
                      P       = c(0.4,0.35,0.25),
                      missval = F)

# simulated dataset measurement part
df2 <- cbind(mod2$dat[,1:4]+1,
             sectors = mod2$trueclass+1) 


# combine selection error and measurement error in one set
for(i in 1:ncol(df1)){
  to_replace = which(df1[,i] == 2)
  df1[,i][to_replace] <- df2[,i][to_replace]
}

#store each simulation of the dataframe in one list
SimData[[sim]] <- df1

}

save.image("simulated_dataset_RR.RData")
