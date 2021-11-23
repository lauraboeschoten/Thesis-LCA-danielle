#4. calculate Posteriors
#datasets: 
cbind(head(Dat_var_A), head(Dat_var_B), head(Dat_var_C), head(Dat_var_D))
props_A <- prop.table(summary(as.factor(Dat_var_A$trueclass))) #proportions of each class
props_B <- prop.table(summary(as.factor(Dat_var_B$trueclass))) #proportions of each class
props_C <- prop.table(summary(as.factor(Dat_var_C$trueclass))) #proportions of each class
props_D <- prop.table(summary(as.factor(Dat_var_D$trueclass))) #proportions of each class


poLCA.posterior(lca,simdata) #function to calculate posterior
#we need to calculate the posteriors by hand, because the function does not work anymore on the imputed datasets
#PROBLEM: we manipulated the datasets (by combining and imputing) so we are not able to access $probs of an polca object anymore.
#uitwerken hoe dit zelf kan zonder $probs -> we zien meteen hoe goed de structuur behouden blijft

props_A[1] #proportion being in class 1

dat1$probs #voorbeeld, van dataset met 20% selection error en twee klassen 
dat_sectoren$probs #voorbeeld, van dataset met 20% selection error en drie klassen 
#doel: nu een probs tabel met vier klassen maken... op basis van geobserveerde score patterns en true class
#$probs are the class-conditional outcome probabilities for the indicator variables

#lca$probs$Y1[j,(responses[i,1])]*lca$probs$Y2[j,(responses[i,2])]*lca$probs$Y3[j,(responses[i,3])]*lca$probs$Y4[j,(responses[i,4])]
#een 4 bij 4 matrix vullen met de volgende 16 proportions
var_A_class1 <- Dat_var_A[which(Dat_var_A$trueclass==1),] #class 1 (houses)
var_A_class2 <- Dat_var_A[which(Dat_var_A$trueclass==2),] #class 2
var_A_class3 <- Dat_var_A[which(Dat_var_A$trueclass==3),] #class 3
var_A_class4 <- Dat_var_A[which(Dat_var_A$trueclass==4),] #class 4
    var_A_class4_Y1 <-  sum(var_A_class4$Y1[which(var_A_class4$Y1==1)])/sum(var_A_class4$Y1)
    var_A_class4_Y2 <-  sum(var_A_class4$Y2[which(var_A_class4$Y2==1)])/sum(var_A_class4$Y2)
    var_A_class4_Y3 <-  sum(var_A_class4$Y3[which(var_A_class4$Y3==1)])/sum(var_A_class4$Y3)
    var_A_class4_Y4 <-  sum(var_A_class4$Y4[which(var_A_class4$Y4==1)])/sum(var_A_class4$Y4)
    cbind(var_A_class4_Y1,var_A_class4_Y2,var_A_class4_Y3,var_A_class4_Y4) #gemiddeld 0.06 errors 
    
    
    ##: Door dit te doen voor alle, Y's, en alle klassen (&later ook nog voor alle varianten) kan ik zelf de probs matrices berekenen.
    #dit is straks sneller met for loops. wel nog even een handig manier uitzoeken om de resultaten op te slaan in een matrix, en goede namen om naar te verwijzen in de berekening van de posteriors

    
#-------------------------------function to calculate the posterior probabilities-----------#
    Pclasses <- props_A 
    prob.y <- c()
    prob.y.given.x <- array(NA,dim=c(Npatterns,Nclasses))
    posterior_probs <- array(NA,dim=c(Npatterns,Nclasses))  #create empty matrix as storage 
    
    for(i in 1:length(Dat_var_A)){ #nr of observations in dataset
      for(j in 1:4){ #nr of classes
        prob.y.given.x[i,j] <- lca$probs$Y1[j,(responses[i,1])]*lca$probs$Y2[j,(responses[i,2])]*lca$probs$Y3[j,(responses[i,3])]*lca$probs$Y4[j,(responses[i,4])]
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
    
