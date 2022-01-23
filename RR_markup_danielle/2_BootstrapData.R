#-------------------------------2. BOOTSTRAP DATA-----------#
load("simulated_dataset_RR.RData") #we now have SimData
dfboot = list(NA) #empty list to store bootstrapped datasets in

for (sim in 1:nsim) { #iteration over number of simulations
  
  df1 <- SimData[[sim]] #assign dataframe of one iteration to "df1"
  # aggregate data and store each dataset in the list "dffreq"
  dffreq <- df1[,1:4] %>%  #select columns with variables
    count(Y1, Y2, Y3, Y4) 

#create nboot bootstrap samples
nboot = 5
# for each profile, sample nboot times, 
# from the total length of the dataset,
# with a probability equal to the observed frequency of that profile
boots = rmultinom(nboot, 
                  sum(dffreq$n), 
                  dffreq$n/sum(dffreq$n))
dfboot[[sim]]= cbind(dffreq, boots)
} #end bootstrap script
save.image("bootstraps_RR.RData")
