#4. calculate Posteriors
#posterior = P(Class|Y)= P(Class=x)P(Y=y|Class=x) / P(Y=y)
#The posterior membership probabilities represent the probability of being in a LC given a specific combination of scores on the indicators and covariates.
#P(scorepattern|Class=1)=P(Q1=A1|X=1) * P(Q2=A2|X=1)* P(Q3=A3|X=1) 
      #necessary to calculate the conditional probabilities P(Yn|Class)
#P(scorepattern)=P(Class=1)P(Y|Class)+ ... +P(Class=C)P(Y|Class=C).
  

#datasets: 
cbind(head(Dat_var_A), head(Dat_var_B), head(Dat_var_C), head(Dat_var_D))
props_A <- prop.table(summary(as.factor(Dat_var_A$trueclass))) #proportions of each class
props_B <- prop.table(summary(as.factor(Dat_var_B$trueclass))) #proportions of each class
props_C <- prop.table(summary(as.factor(Dat_var_C$trueclass))) #proportions of each class
props_D <- prop.table(summary(as.factor(Dat_var_D$trueclass))) #proportions of each class
props_A

poLCA.posterior(lca,simdata) #function to calculate posterior
#we need to calculate the posteriors by hand, because the function does not work anymore on the imputed datasets
#PROBLEM: we manipulated the datasets (by combining and imputing) so we are not able to access $probs of an polca object anymore.
#uitwerken hoe dit zelf kan zonder $probs -> we zien meteen hoe goed de structuur behouden blijft

props_A[1] #proportion being in class 1
dat_sectoren$probs
dat1$probs #voorbeeld, van dataset met 20% selection error en twee klassen 
dat_sectoren$probs #voorbeeld, van dataset met 20% selection error en drie klassen 
#doel: nu een probs tabel met vier klassen maken... op basis van geobserveerde score patterns en true class
#$probs are the class-conditional outcome probabilities for the indicator variables

#een 4 bij 4 matrix vullen met de volgende 16 proportions (en dat dan 4 keer)
    #afgaan per source (dat zijn er vier), en dan dus vier matrices met op de rijen de 4 klassen en op de kolommen de categorieën/opties
var_A_class1 <- Dat_var_A[which(Dat_var_A$trueclass==1),] #class 1 (houses)
var_A_class2 <- Dat_var_A[which(Dat_var_A$trueclass==2),] #class 2
var_A_class3 <- Dat_var_A[which(Dat_var_A$trueclass==3),] #class 3
var_A_class4 <- Dat_var_A[which(Dat_var_A$trueclass==4),] #class 4
list_classes <- list(var_A_class1, var_A_class2, var_A_class3, var_A_class4)


#alles voor variabele Y1 in een matrix. met op elke row de trueclass, en elke kolom een mogelijk antwoord.(answer i|class =c) 
Conditionals_Y1 <- matrix(data=NA,nrow=4, ncol=4)
Conditionals_Y2 <- matrix(data=NA,nrow=4, ncol=4)
Conditionals_Y3 <- matrix(data=NA,nrow=4, ncol=4)
Conditionals_Y4 <- matrix(data=NA,nrow=4, ncol=4)

for (c in 1:4){ #rows, iterate over classes 
  for (i in 1:4){ #columns, iterate over answers
  Conditionals_Y1[c,i] <- sum(list_classes[[c]]$Y1[which(list_classes[[c]]$Y1==i)])/sum(list_classes[[c]]$Y1)
  Conditionals_Y2[c,i] <- sum(list_classes[[c]]$Y2[which(list_classes[[c]]$Y2==i)])/sum(list_classes[[c]]$Y2)
  Conditionals_Y3[c,i] <- sum(list_classes[[c]]$Y3[which(list_classes[[c]]$Y3==i)])/sum(list_classes[[c]]$Y3)
  Conditionals_Y4[c,i] <- sum(list_classes[[c]]$Y4[which(list_classes[[c]]$Y4==i)])/sum(list_classes[[c]]$Y4)
    } }
conditionals <- list(Conditionals_Y1, Conditionals_Y2,Conditionals_Y3 ,Conditionals_Y4 )
conditionals    #resultaten zijn opgeslagen in een list van matrices


        

#-------------------------------function to calculate the posterior probabilities-----------#
Pclasses <- prop.table(summary(as.factor(dataset$trueclass))) #proportions of each class
 ssize <- 5000
    #create storage 
    prob.y.given.x <- array(NA,dim=c(ssize,4))
    prob.y <- c()
    posterior_probs <- array(NA,dim=c(ssize,4))
    #function to calculate posterior probabilities, P(class|scores)
    posterior_function <- function(dataset,  ssize=5000, conditionals){
      
      Pclasses <- prop.table(summary(as.factor(dataset$trueclass))) #proportions of each class
            for(i in 1:ssize){ #nr of observations in dataset
      for(c in 1:4){ #nr of classes
        prob.y.given.x[i,c] <- conditionals[[1]][c,(dataset[i,1])]*conditionals[[2]][c,(dataset[i,2])]*conditionals[[3]][c,(dataset[i,3])]*conditionals[[4]][c,(dataset[i,4])]
      }}
    for(i in 1:ssize){ 
        prob.y[i] <- Pclasses[1]*prob.y.given.x[i,1]+Pclasses[2]*prob.y.given.x[i,2]+Pclasses[3]*prob.y.given.x[i,3]+Pclasses[4]*prob.y.given.x[i,4]
      }
    for(i in 1:ssize){ 
      for(c in 1:4){ #nr of classes
        posterior_probs[i,c] <-  (Pclasses[c]*prob.y.given.x[i,c])/prob.y[i]  
      }}
      posterior_probs<<-posterior_probs #assign result to the environment (outside the function)
      }#end function
    posterior_function(dataset = Dat_var_A, conditionals = conditionals)
   #show first five results 
    posterior_probs[1:5,]
 #conclusion: most cases in dataset A can be quite confidently assigned to one class via modal assignment        

    #what would happen with larger errors?