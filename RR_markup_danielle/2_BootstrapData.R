#-------------------------------2. BOOTSTRAP DATA-----------#
load("simulated_dataset_RR.RData")

#create nboot bootstrap samples
nboot = 5
# for each profile, sample nboot times, 
# from the total length of the dataset,
# with a probability equal to the observed frequency of that profile
boots = rmultinom(nboot, 
                  sum(dffreq$n), 
                  dffreq$n/sum(dffreq$n))
dfboot= cbind(dffreq, boots)

save.image("bootstraps_RR.RData")