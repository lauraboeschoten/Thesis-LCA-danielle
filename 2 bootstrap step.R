#2. Bootstrap step 
cbind(head(Dat_var_A), head(Dat_var_B), head(Dat_var_C), head(Dat_var_D))
#take m bootstrap samples from the original dataset. We use m=5 (this is sufficient, see Boeschoten (2017))
#sample from  posterior membership probabilities

library(tidyverse)
library(confreq)
dat <- load("simulated_dataset_RR.RData")
summary(dat)
set.seed(123)
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
4^4 #256 different possible score patterns
256*4 #four classes so 1024 different posterior membership probabilities 
for (i in 5:10) { #for the original freq pattern and the five bootstrapped patterns
  data1 <- as.data.frame(confreq::fre2dat(dfboot[,c(1:4, i)])) #converge frequency table to dataframe
  assign(paste0("BootstrappedData",(i-4)), data1, envir = .GlobalEnv) #store the datasets to global environment
}
varD_boots <- list(BootstrappedData1, BootstrappedData2, BootstrappedData3, BootstrappedData4, BootstrappedData5, BootstrappedData6)
BootstrappedData6
save.image("bootstrap_dataset_RR.RData")
