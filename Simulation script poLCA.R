library(poLCA)


#Explore options of simulation with poLCA 
set.seed(123)
simdata <- poLCA.simdata(5000,nclass=3,ndv=4, niv=1, missval = TRUE, pctmiss = .15) 
#ndv is nr of questions , niv=covariate
simdata$P 
simdata$dat #response pattern for each individual (rows) on each question (columns)
simdata$probs #the class-conditional outcome probabilities for the manifest variables
simdata$probs[[1]] #how to access each question
simdata$probs[[4]] [2,4] #prob of answering 4 if in 2d class 
