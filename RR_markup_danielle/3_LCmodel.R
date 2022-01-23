#-------------------------------3. LCA STEP + LABEL check and correction -----------#
load("bootstraps_RR.RData")
library(confreq) #used for making bootstrap datasets
SimData = SimData #keep the dataset in the .RData file
dfboot = dfboot #keep the bootstrapped datasets .RData file
LCASIM = list(NA) #empty list to store simulation results in
for (sim in 1:nsim) { #iteration over number of simulations
#store results
bootdata <- list(NA)
LCAS <- list(NA)
LCAS2 <- list(NA)

#to test if labels are correct
LCAS_probs <- list(NA)
LCAS2_probs <- list(NA)
set.seed(123)
for (i in 1:5) {
  #create dataset per bootstrap sample with the following code:
  bootdata[[i]] <- as.data.frame(confreq::fre2dat(dfboot[[sim]][,c(1:4, (i+5))])) #converge frequency table to dataframe
  #run LC model on each bootstrap sample
  LCAS[[i]] = poLCA(formula = cbind(Y1, Y2, Y3, Y4) ~ 1,  
                    bootdata[[i]],       nclass = 4,      nrep = 10)
  LCAS_probs[[i]] <-   LCAS[[i]]$P #display proportions per class, for each bootstrap sample (conclusion: we have a label switching problem)
  #column maxima switched label detection algorithm (for each column/response we want to find which class has the highest likelihood)
  order = c(which.max(LCAS[[i]]$probs$Y1[,1]), #class for which column 1 has the highest probability
            which.max(LCAS[[i]]$probs$Y1[,2]), # "    column 2  "
            which.max(LCAS[[i]]$probs$Y1[,3]),
            which.max(LCAS[[i]]$probs$Y1[,4]))
  
  LCAS2[[i]] <- LCAS[[i]] #assign values to new object. Next, change the order of the classes
  LCAS2[[i]]$probs$Y1 = LCAS[[i]]$probs$Y1[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y2 = LCAS[[i]]$probs$Y2[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y3 = LCAS[[i]]$probs$Y3[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y4 = LCAS[[i]]$probs$Y4[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$P        = LCAS[[i]]$P[c(as.numeric(paste(order)))]
  # WARNING: other elements in the LC output are not switched!! 
  LCAS2_probs[[i]] <-   LCAS2[[i]]$P #results, to check if label detection algorithm worked
}
LCASIM[[sim]] <- LCAS2
}#end of LC model script
save.image("poLCA_and_Posteriors_RR.RData")

