#-------------------------------5. Results-----------#
load("imputations_RR.RData")
df1 = list(NA)
implist = list(NA)
prop.classes = list(NA)
bias = list(NA)
SimProp.classes = list(NA)
SimBias = list(NA)

for (sim in 1:nsim) { #iteration over number of simulations
  df1 <- SimData[[sim]] #assign dataframe of 1 simulation to "df1"
  implist <- ImpSim[[sim]] 
#Overall group sizes
trueclass <- prop.table(table(df1$trueclass)) #proportions original data

#calculate bias between group sizes of original data and the imputations of the bootstrap data
for(m in 1:5){ #bias 
  prop.classes[[m]] <- prop.table(table(implist[[m]]$imp))
  bias[[m]] <- trueclass-prop.classes[[m]]
  #First pool within simulation the 5 imputations
  Pooled.prop.classes <- rowMeans(sapply(prop.classes, unlist))
}

SimProp.classes[[sim]] <- Pooled.prop.classes #pooled mean group sizes per simulation iteration
Average <- rowMeans(sapply(SimProp.classes, unlist))

return(Average)
} #end loop over simulations
save.image("Results_RR.RData")