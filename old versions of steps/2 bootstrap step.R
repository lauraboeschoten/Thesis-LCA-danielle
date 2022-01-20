#2. Bootstrap step 

library(tidyverse)
library(confreq)
dat <- load("simulated_dataset_RR.RData")
summary(dat)
set.seed(123)

nboot=5
# aggregate data
dffreq <- df1[,1:4] %>% 
  count(Y1, Y2, Y3, Y4)

dffreq <- Dat_var_D[,1:4] %>% 
  count(Y1, Y2, Y3, Y4)

#create nboot bootstrap samples
nboot = 5
# for each profile, sample nboot times, 
# from the total length of the dataset,
# with a probability equal to the observed frequency of that profile
boots = rmultinom(nboot, 
                  sum(dffreq$n), 
                  dffreq$n/sum(dffreq$n))
dfboot= cbind(dffreq, boots)
dfboot
data1 <- list(NA)
for (i in 5:10) { #for the original freq pattern and the five bootstrapped patterns
  data1[[(i-4)]] <- as.data.frame(confreq::fre2dat(dfboot[,c(1:4, i)])) #converge frequency table to dataframe
  assign(paste0("BootstrappedData",(i-4)), data1, envir = .GlobalEnv) #store the datasets to global environment
}
data1



