#-------------------------------5. Results-----------#
load("imputations_RR.RData")
library(reshape2)
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
}
Pooled.prop.classes <- 1
rowMeans(sapply(prop.classes, unlist))

sapply(prop.classes, FUN=mean, n=5)
SimProp.classes[[sim]] = prop.classes #list of nsim lists, with each nboot (=5) lists with the imputed class proportions
SimBias[[sim]] = bias #list of nsim lists, with each nboot (=5) lists with the bias of the imputations of each bootstrap sample

#pool bias

#First pool within simulation the 5 imputations


Pooled.prop.classes <- 1
sapply(X= testset, FUN=mean, n=5)

} #end loop over simulations
save.image("Results_RR.RData")