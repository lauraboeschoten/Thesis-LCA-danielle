#3. poLCA model (LC)

lca_for <- cbind(Y1,Y2,Y3,Y4)~cbind(X1,X2) #cbind(predictors)~cbind(covariates)
lca <- poLCA(formula = lca_for,simdata,nclass=4,nrep=10) #repeat 10 times


summary((as.factor(df1[,5]))) #how many per class  
check2 <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1), nrep = 10,data = df1, nclass=2) 
    #Tree-step
    df_tree2 <- cbind(df1,check2$predclass) #dataset with 
    selected2 <-  df_tree2[which(df_tree2[,6]==2),] #make sure while selecting the cases that the right ones are selcted, check the class proportions of the results, polca does not care which one is 1 or 2
    df_tree2[which(df_tree2$`check2$predclass`==2),]
    check2_3 <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1),data = selected2, nclass=3)
check4 <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1),data = df1, nclass=4)