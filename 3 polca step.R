#3. poLCA model (LC)
#a latent class model is estimated for each bootstrapped sample (imputed datasets)

##NOTES ON POLCA##
#1. if nrep > 1, then any user-specified probs.start values are only used in the first of the nrep attempts
#2. "Because the latent classes are unordered categories, the numerical order of the estimated latent classes in the model output is arbitrary, and is determined solely by the start values of the EM algorithm. (Linzer and Lewis, 2011, poLCA, 4.6) -> this is important when dealing with the label problem


props_A <- prop.table(summary(as.factor(Dat_var_A$trueclass))) #proportions of each class
check2 <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~cbind(COV1,COV2)), data = Dat_var_A, nclass=2) 
varA_boots <- list(BootstrappedData5, BootstrappedData6, BootstrappedData7, BootstrappedData8, BootstrappedData9, BootstrappedData10)
predictedP <- list() #create empty list to store proportions
for (i in 1:length(varA_boots)) {
  obj <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1), data = varA_boots[[i]], nclass=2)
  PredP <- obj$P
  predictedP[[i]] <- PredP 
}
#rename , 2 klassen -> LC model op 
  predictedP #show the proportions of each LC (with nclass=2)
  
obj <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1), data = varA_boots[[1]], nclass=2)
obj$P
#5 bootstrapped datasets, 5 LC estimates, calculate posteriors and estimates, then impute and pool (and go to the next/tree step)
    #Tree-step
    df_tree2 <- cbind(Dat_var_A,check2$predclass) #dataset with 
    selected2 <-  df_tree2[which(df_tree2[,6]==2),] #make sure while selecting the cases that the right ones are selcted, check the class proportions of the results, polca does not care which one is 1 or 2
    df_tree2[which(df_tree2$`check2$predclass`==2),]
    check2_3 <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1),data = selected2, nclass=3)
check4 <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1),data = Dat_var_A, nclass=4)
check4$P
props_A #compare, equal except for label problem!

