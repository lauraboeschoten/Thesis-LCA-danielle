library(poLCA)


#Explore options of simulation with poLCA 
set.seed(123)
sim <- poLCA.simdata(5000,nclass=3,ndv=4, niv=1, missval = TRUE, pctmiss = .15) 
#ndv is nr of questions , niv=covariate
prob.X <- sim$P #the probability of being in each class, accessible via []
sim$nresp #4 questions with 3,5,3 and 5 possible responses
simdata <- sim$dat #response pattern for each individual (rows) on each question (columns)
sim$probs #the class-conditional outcome probabilities for the manifest variables
sim$probs[[1]] #how to access each question
sim$probs[[4]] [2,4] #prob of answering 4 if in 2d class 
#how to access those more easily? and store results for each score pattern in matrix?

poLCA.posterior(lca,simdata)

##perform LCA
lca_for <- cbind(Y1,Y2,Y3,Y4)~X1
lca <- poLCA(formula = lca_for,simdata,nclass=3,na.rm = F,nrep=10)
#maximum likelihood not found, is this problematic?

poLCA.predcell(lca,y=c(1,1,1,1))
#make a for loop with all cell probabilities?
responses <- lca$predcell[,1:4]
check <- unlist(lca$predcell[1,1:4])
check
predicted <- c()
for(i in 1:length(responses)){
responses[i,5] <- poLCA.predcell(lca,y=c(unlist(responses[i,1:4])))
}
responses[1:5,]
lca$predcell
ab <- c(unlist(responses[1,]))
lca$posterior #prob of being in a class (column) for each individual (row)
