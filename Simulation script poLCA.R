library(poLCA)
options(scipen = 999) # remove scientific notation


#Explore options of simulation with poLCA 
set.seed(123)
sim <- poLCA.simdata(5000,nclass=3,ndv=4, niv=1, nresp = c(3,5,3,5) , missval = TRUE, pctmiss = .15) 
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

##POSTERIOR MEMBERSHIP PROBABILITIES WITH POLCA FUNCTION
poLCA.predcell(lca,y=c(1,1,1,1)) #predicted cell count with this response pattern 
responses <- lca$predcell[,1:4] #all possible responses patterns  
#goal: calculate posterior membership probabilities for each response pattern 
poLCA.posterior(lca, y=(responses[1,])) #posterior probability of one score pattern
posterior_probs <- array(NA,dim=c(219,3))  #create empty matrix as storeage 
for(i in 1:219){ #Posterior prob for each class (columns) for each response pattern(rows)
posterior_probs[i,] <- poLCA.posterior(lca, y=responses[i,])
}
posterior_probs[1:5,]

dataset = cbind(simdata, lca$posterior) #data of all individuals and their posteriors 
dataset[1:5,]

##POSTERIOR MEMBERSHIP PROBABILITIES BY HAND
#Posterior prob for each class (columns) for each response pattern(rows)
nrow(responses)
prob.x <- lca$P
Npatterns <- nrow(responses)
Nclasses <- ncol(responses)
prob.y <- c()
prob.y.given.x <- array(NA,dim=c(Npatterns,Nclasses))
posterior_probs_hand2 <- array(NA,dim=c(Npatterns,Nclasses))  #create empty matrix as storage 

for(i in 1:219){ #nr of score patterns
  for(j in 1:3){ #nr of classes
    prob.y.given.x[i,j] <- lca$probs$Y1[j,(responses[i,1])]*lca$probs$Y2[j,(responses[i,2])]*lca$probs$Y3[j,(responses[i,3])]*lca$probs$Y4[j,(responses[i,4])]
    }}
for(i in 1:219){ #nr of score patterns
      for(j in 1:3){ #nr of classes
        prob.y[i] <- prob.x[1]*prob.y.given.x[i,1]+prob.x[2]*prob.y.given.x[i,2]+prob.x[3]*prob.y.given.x[i,3]
       }}
for(i in 1:219){ #nr of score patterns
          for(j in 1:3){ #nr of classes
          posterior_probs_hand2[i,j] <-  (prob.x[j]*prob.y.given.x[i,j])/prob.y[i]  
  }#end loop classes
}#end loop score patterns
posterior_probs_hand[1:5,]
posterior_probs[1:5,]

#Correct! (matches with polca.posterior function) However, covariate is not taken into account yet for both

