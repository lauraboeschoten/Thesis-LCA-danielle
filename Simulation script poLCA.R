library(poLCA)
options(scipen = 999) # remove scientific notation
set.seed(123) #set.seed for replicability

#probabilities for two classes 
probs2 <- list(matrix(c(0.9, 0.1,
                       0.1, 0.9), ncol=2, byrow=T),
              matrix(c(0.9, 0.1,
                       0.1, 0.9), ncol=2, byrow=T),
              matrix(c(0.9, 0.1,
                       0.1, 0.9), ncol=2, byrow=T),
              matrix(c(0.1, 0.9,
                       0.9, 0.1), ncol=2, byrow=T))
dat1 <- poLCA.simdata(5000,nclass=2,probs= probs2, P= c(0.6,0.4), missval = F) #gesimuleerde dataset voor woningen en bedrijven
dat1$b
dat1$P # P specificieren heeft geen zin als niv>0 (als er covarianten zijn, dan hangt P daarvan af).
dataset <- cbind(dat1$dat,dat1$trueclass)
head(dataset)
#elke Y-waarde die 2 is (=bedrijf) vervangen door een nieuwe waarde (die de klasse/sector) aangeeft

#probabilities for three classes 
probs3 <- list(matrix(c(0.9,0.05,0.05,
                        0.05,0.9,0.05,
                        0.05,0.05,0.9 ), ncol=3,   byrow=TRUE), # Y1
               matrix(c(0.9,0.05,0.05,
                        0.05,0.9,0.05,
                        0.05,0.05,0.9 ), ncol=3, byrow=TRUE), # Y2
               matrix(c(0.90,0.05,0.05,
                        0.05,0.9,0.05,
                        0.05,0.05,0.9 ), ncol=3, byrow=TRUE), #Y3
               matrix(c(0.05,0.05,0.90,
                        0.05,0.9,0.05,
                        0.90,0.05,0.05 ), ncol=3, byrow=TRUE))#Y4
set.seed(125)
dat_sectoren <- poLCA.simdata(N=5000, probs=probs3, niv=2) #gesimuleerde dataset voor bedrijven (interesse in de SBI, hier 3 sectoren))
#nog toeveoegen, b= 
dat_sectoren$P
newdat <- cbind(dat_sectoren$dat[,1:4]+1,dat_sectoren$dat[,5:6],sectors=dat_sectoren$trueclass+1) #+1 zodat '1' hier niet meer voorkomt, dit is de klas van de woningen
head(newdat)
df2 <- newdat

##replace with or without covariants? probeer allebei een keer (covariants in de 1e simuleren of juist in de 2e)
#voor elke '2' (de bedrijven) die voorkomt in dataset1, deze waarde van Y vervangen door een waarde van dat_sectoren
df1 <- dataset
head(df1)
head(df2)

for(i in 1:ncol(df1)){
  
  to_replace = which(df1[,i] == 2)
  
  df1[,i][to_replace] <- df2[,i][to_replace]
}

df1
summary(as.factor(df1[,7])) #how many per class  
check2 <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~X1*X2),data = df1, nclass=2)
check3 <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~X1*X2),data = df1, nclass=3)
check4 <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~X1*X2),data = df1, nclass=4)
check2$coeff #deze is hetzelfde (lijkt woning vs bedrijf goed te pakken)
dat1$b

check3$coeff
check4$coeff
dat_sectoren$b

# tweede probleem is dat dmv sampling (we selecteren alleen degenen waar in dezelfde
# rij in de andere dataset = bedrijf) misschien de relaties niet zo nauwkeurig worden 
# behouden als we hadden gesimuleerd

#testen via vastgestelde P

# derde probleem is dat de relatie met de covariaat alleen geldt voor de dataset
# waarbij je die hebt gesimuleerd, dus of alleen voor woning/bedrijf of alleen
# voor de sectoren. 
# oplossing kan zijn om nu pas, dus achteraf, een covariaat toe te voegen, maar 
# dat kan dan niet met de poLCA sim functie, dus dat kun je proberen los te 
# maken door een relatie te specificeren tov 'trueclass' hier. 

#2 mogelijkheden proberen
#correlatie met trueclass 

# 
rnorm(10,0,2)
head(df1)
summary(df1$X1)

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


##POSTERIOR MEMBERSHIP PROBABILITIES BY HAND per score pattern
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

#Correct! (matches with polca.posterior function) However, covariate is not taken into account yet for both(?)



##POSTERIOR MEMBERSHIP PROBABILITIES BY HAND per individual
#Posterior prob for each class (columns) for each person (rows)
nrow(responses)
prob.x <- lca$P
Nclasses <- ncol(responses)
prob.y <- c()
prob.y.given.x <- array(NA,dim=c(Npatterns,Nclasses))
posterior_probs_hand2 <- array(NA,dim=c(Npatterns,Nclasses))  #create empty matrix as storage 

for(i in 1:length()){ 
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



