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

#lca$probs$Y1[j,(responses[i,1])]*lca$probs$Y2[j,(responses[i,2])]*lca$probs$Y3[j,(responses[i,3])]*lca$probs$Y4[j,(responses[i,4])]
#een 4 bij 4 matrix vullen met de volgende 16 proportions (en dat dan 4 keer)
    #afgaan per source (dat zijn er vier), en dan dus vier matrices met op de rijen de 4 klassen en op de kolommen de categorieën/opties
var_A_class1 <- Dat_var_A[which(Dat_var_A$trueclass==1),] #class 1 (houses)
var_A_class2 <- Dat_var_A[which(Dat_var_A$trueclass==2),] #class 2
var_A_class3 <- Dat_var_A[which(Dat_var_A$trueclass==3),] #class 3
var_A_class4 <- Dat_var_A[which(Dat_var_A$trueclass==4),] #class 4

#de code kan vast korten door er een functie van te maken en ook over de rijen/klassen te itereren

#alles voor variabele Y1 in een matrix. met op elke row trueclass=C, en elke kolom een mogelijk antwoord.
Conditionals_Y1 <- matrix(data=NA,nrow=4, ncol=4)
#for j in (1:4){ #rows (i|Class =j)
  for (i in 1:4){ #columns (answer i|class =j)     row 1 = class 1, iterate over answers to Y1
  Conditionals_Y1[1,i] <- sum(var_A_class1$Y1[which(var_A_class1$Y1==i)])/sum(var_A_class1$Y1)
  Conditionals_Y1[2,i] <- sum(var_A_class2$Y1[which(var_A_class2$Y1==i)])/sum(var_A_class2$Y1)
  Conditionals_Y1[3,i] <- sum(var_A_class3$Y1[which(var_A_class3$Y1==i)])/sum(var_A_class3$Y1)
  Conditionals_Y1[4,i] <- sum(var_A_class4$Y1[which(var_A_class4$Y1==i)])/sum(var_A_class4$Y1)
  }
Conditionals_Y1 
Conditionals_Y2 <- matrix(data=NA,nrow=4, ncol=4)
#for j in (1:4){ #rows (i|Class =j)
for (i in 1:4){ #columns (answer i|class =j)     row 1 = class 1, iterate over answers to Y1
  Conditionals_Y2[1,i] <- sum(var_A_class1$Y2[which(var_A_class1$Y2==i)])/sum(var_A_class1$Y2)
  Conditionals_Y2[2,i] <- sum(var_A_class2$Y2[which(var_A_class2$Y2==i)])/sum(var_A_class2$Y2)
  Conditionals_Y2[3,i] <- sum(var_A_class3$Y2[which(var_A_class3$Y2==i)])/sum(var_A_class3$Y2)
  Conditionals_Y2[4,i] <- sum(var_A_class4$Y2[which(var_A_class4$Y2==i)])/sum(var_A_class4$Y2)
}
Conditionals_Y2

Conditionals_Y3 <- matrix(data=NA,nrow=4, ncol=4)
#for j in (1:4){ #rows (i|Class =j)
for (i in 1:4){ #columns (answer i|class =j)     row 1 = class 1, iterate over answers to Y1
  Conditionals_Y3[1,i] <- sum(var_A_class1$Y3[which(var_A_class1$Y3==i)])/sum(var_A_class1$Y3)
  Conditionals_Y3[2,i] <- sum(var_A_class2$Y3[which(var_A_class2$Y3==i)])/sum(var_A_class2$Y3)
  Conditionals_Y3[3,i] <- sum(var_A_class3$Y3[which(var_A_class3$Y3==i)])/sum(var_A_class3$Y3)
  Conditionals_Y3[4,i] <- sum(var_A_class4$Y3[which(var_A_class4$Y3==i)])/sum(var_A_class4$Y3)
}
Conditionals_Y3
Conditionals_Y4 <- matrix(data=NA,nrow=4, ncol=4)
#for j in (1:4){ #rows (i|Class =j)
for (i in 1:4){ #columns (answer i|class =j)     row 1 = class 1, iterate over answers to Y1
  Conditionals_Y4[1,i] <- sum(var_A_class1$Y4[which(var_A_class1$Y4==i)])/sum(var_A_class1$Y4)
  Conditionals_Y4[2,i] <- sum(var_A_class2$Y4[which(var_A_class2$Y4==i)])/sum(var_A_class2$Y4)
  Conditionals_Y4[3,i] <- sum(var_A_class3$Y4[which(var_A_class3$Y4==i)])/sum(var_A_class3$Y4)
  Conditionals_Y4[4,i] <- sum(var_A_class4$Y4[which(var_A_class4$Y4==i)])/sum(var_A_class4$Y4)
}
Conditionals_Y4

        
conditionals <- list(Conditionals_Y1, Conditionals_Y2,Conditionals_Y3 ,Conditionals_Y4 )
conditionals    #resultaten zijn opgeslagen in een list van matrices

#-------------------------------function to calculate the posterior probabilities-----------#
    Pclasses <- props_A 
    prob.y <- c()
    prob.y.given.x <- array(NA,dim=c(Npatterns,Nclasses))
    
    for(i in 1:length(Dat_var_A)){ #nr of observations in dataset
      for(j in 1:4){ #nr of classes
        prob.y.given.x[i,j] <- conditionals[j,(responses[i,1])]*lca$probs$Y2[j,(responses[i,2])]*lca$probs$Y3[j,(responses[i,3])]*lca$probs$Y4[j,(responses[i,4])]
      }}
    for(i in 1:length(Dat_var_A)){ 
      for(j in 1:4){ #nr of classes
        prob.y[i] <- prob.x[1]*prob.y.given.x[i,1]+prob.x[2]*prob.y.given.x[i,2]+prob.x[3]*prob.y.given.x[i,3]+prob.x[4]*prob.y.given.x[i,4]
      }}
    for(i in 1:length(Dat_var_A)){ 
      for(j in 1:4){ #nr of classes
        posterior_probs[i,j] <-  (prob.x[j]*prob.y.given.x[i,j])/prob.y[i]  
      }#end loop classes
    }#end loop score patterns
    posterior_probs[1:5,]    #show first five results 
        
    
#notes: 
    #deze 'functie' werkte voor een dataset met alle response patterns en de "probs" uit een polca object
    #ik zal er nog een daadwerkelijke functie (ipv reeks aan for loops) van maken
    
    #posterior_function <- function(dataset, Pclasses){}
    #maak functie waarbij het mogelijk is dataset in te vullen (en daarmee voor elke dataset en variant de posteriors te berekenen)
    
