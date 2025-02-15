#1. Data simuleren
library(poLCA)
options(scipen = 999) # remove scientific notation
set.seed(123) #set.seed for replicability

#5% selection error
select_5 <-           list(matrix(c(0.95, 0.05,     0.05, 0.95), ncol=2, byrow=T),
                           matrix(c(0.95, 0.05,     0.05, 0.95), ncol=2, byrow=T),
                           matrix(c(0.95, 0.05,     0.05, 0.95), ncol=2, byrow=T),
                           matrix(c(0.95, 0.05,     0.05, 0.95), ncol=2, byrow=T))

#20% selection error
select_20 <- list(matrix(c(0.8, 0.2,     0.2, 0.8), ncol=2, byrow=T),
               matrix(c(0.8, 0.2,     0.2, 0.8), ncol=2, byrow=T),
               matrix(c(0.8, 0.2,     0.2, 0.8), ncol=2, byrow=T),
               matrix(c(0.8, 0.2,     0.2, 0.8), ncol=2, byrow=T))


#5% classification error
 
meas_5 <- list(matrix(c(0.95,0.025,0.025,    0.025,0.95,0.025,    0.025,0.025,0.95 ), ncol=3, byrow=TRUE), # Y1
               matrix(c(0.95,0.025,0.025,    0.025,0.95,0.025,    0.025,0.025,0.95 ), ncol=3, byrow=TRUE), # Y2
               matrix(c(0.95,0.025,0.025,    0.025,0.95,0.025,    0.025,0.025,0.95 ), ncol=3, byrow=TRUE), #Y3
               matrix(c(0.95,0.025,0.025,    0.025,0.95,0.025,    0.025,0.025,0.95 ), ncol=3, byrow=TRUE)) #Y4

#20% classification error

meas_20 <- list(matrix(c(0.8,0.1,0.1,    0.1,0.8,0.1,     0.1,0.1,0.8 ), ncol=3,   byrow=TRUE), # Y1
               matrix(c(0.8,0.1,0.1,    0.1,0.8,0.1,     0.1,0.1,0.8 ), ncol=3, byrow=TRUE), # Y2
               matrix(c(0.80,0.1,0.1,   0.1,0.8,0.1,     0.1,0.1,0.8 ), ncol=3, byrow=TRUE), #Y3
               matrix(c(0.80,0.1,0.1,   0.1,0.8,0.1,     0.1,0.1,0.8  ), ncol=3, byrow=TRUE)) #Y4

select_errors <- list(select_5, select_20)
meas_errors <- list(meas_5, meas_20)

#--------------------------FUNCTION----------------------------------#

mod1 <- poLCA.simdata(N=5000,nclass=2,probs= select_5, P= c(0.2,0.8), missval = F) #poLCA simdata selection error part, 2 classes
df1 <- cbind(mod1$dat,trueclass=mod1$trueclass) #simulated dataset selection 
mod2 <- poLCA.simdata(N=5000, P=(c(0.4,0.35,0.25)), probs=meas_5) 
df2 <- cbind(mod2$dat[,1:4]+1, sectors=mod2$trueclass+1) #+1 to avoid overlap classes 



#--------------------------VARIANTS----------------------------------#

### variant A 
#5% selection error, 5% classification error 
set.seed(123)
mod1 <- poLCA.simdata(5000,nclass=2,probs= select_5, P= c(0.2,0.8), missval = F) 
df1 <- cbind(mod1$dat,trueclass=mod1$trueclass)
set.seed(125)
mod2 <- poLCA.simdata(N=5000, P=(c(0.4,0.35,0.25)), probs=meas_5) 
df2 <- cbind(mod2$dat[,1:4]+1, sectors=mod2$trueclass+1) #+1 zodat '1' hier niet meer voorkomt, dit is de klas van de woningen
#elke Y-waarde die 2 is (=bedrijf) vervangen door een nieuwe waarde (die de klasse/sector) aangeeft
for(i in 1:ncol(df1)){
  to_replace = which(df1[,i] == 2)
  df1[,i][to_replace] <- df2[,i][to_replace]
}
Dat_var_A <- df1
head(Dat_var_A)
summary((as.factor(Dat_var_A[,5]))) #how many per class 



#VARIANT A WITH COVARIATES
#in probs2 0.05 (5% selection error)
#in probs3 twee keer 0.025 (5% classification error )

set.seed(123)
#probabilities for two classes for four indicator variables
probs2 <- list(matrix(c(0.95, 0.05,     0.05, 0.95), ncol=2, byrow=T),
               matrix(c(0.95, 0.05,     0.05, 0.95), ncol=2, byrow=T),
               matrix(c(0.95, 0.05,     0.05, 0.95), ncol=2, byrow=T),
               matrix(c(0.05, 0.95,     0.95, 0.05), ncol=2, byrow=T),
               matrix(c(0.5, 0.5,        0.5, 0.5), ncol=2, byrow=T)) #COVARIATE 
 
mod1 <- poLCA.simdata(5000,nclass=2,probs= probs2, P= c(0.2,0.8), missval = F) 
df1 <- cbind(mod1$dat[,1:4], COV1=mod1$dat[,5]*10, trueclass=mod1$trueclass)
head(df1)
#probabilities with 5% classification error for three classes 
probs3 <- list(matrix(c(0.95,0.025,0.025,    0.025,0.95,0.025,    0.025,0.025,0.95 ), ncol=3, byrow=TRUE), # Y1
               matrix(c(0.95,0.025,0.025,    0.025,0.95,0.025,    0.025,0.025,0.95 ), ncol=3, byrow=TRUE), # Y2
               matrix(c(0.95,0.025,0.025,    0.025,0.95,0.025,    0.025,0.025,0.95 ), ncol=3, byrow=TRUE), #Y3
               matrix(c(0.025,0.025,0.95,    0.025,0.95,0.025,    0.95,0.025,0.025 ), ncol=3, byrow=TRUE), #Y4
               matrix(c((1/3), (1/3),(1/3)), ncol=1, byrow=T)) #COVARIATE with equal relations

set.seed(125)
mod2 <- poLCA.simdata(N=5000, P=(c(0.4,0.35,0.25)), probs=probs3) 
df2 <- cbind(mod2$dat[,1:4]+1, COV2=mod2$dat[,5]*10, sectors=mod2$trueclass+1) #+1 zodat '1' hier niet meer voorkomt, dit is de klas van de woningen
head(df1)
head(df2)
#elke Y-waarde die 2 is (=bedrijf) vervangen door een nieuwe waarde (die de klasse/sector) aangeeft
for(i in c(1:4, 6)){ #nu wordt de covariaat niet meegenomen. Let op: op de 5e plek moet nu dus in beide datasets een covariaat staan
  to_replace = which(df1[,i] == 2)
  df1[,i][to_replace] <- df2[,i][to_replace]
}
Dat_var_A <- cbind(df1[,1:5], COV2=df2$COV2, trueclass=df1[,6])
head(Dat_var_A)
summary((as.factor(Dat_var_A$trueclass))) #how many per class #check to see that it check the column with the trueclass
#constateer: zelfde resultaten als zonder covariates (logisch want covariates zonder relatie met klassen)
cbind(COV1=summary((as.factor(Dat_var_A$COV1))), COV2=summary((as.factor(Dat_var_A$COV2))))
#vreemd: er zijn meer cov2? kijk naar Nrow en Ncol in de covariaten. dit klopt nog niet.

#PROBLEM: covariates -> met aandacht naar kijken of het klopt. 
          #ik heb nu de covariates *10 gedaan zodat ze a) niet vervangen worden en b) om ze beter te onderscheiden van de Y indicator variabelen. 
          # maar "for(i in c(1:4, 6)" werkt waarschijnlijk ook voor a)

###Variant A _ met sterke covariaat (voor selectiefout)
set.seed(123)
#probabilities for two classes for four indicator variables
probs2 <- list(matrix(c(0.95, 0.05,     0.05, 0.95), ncol=2, byrow=T),
               matrix(c(0.95, 0.05,     0.05, 0.95), ncol=2, byrow=T),
               matrix(c(0.95, 0.05,     0.05, 0.95), ncol=2, byrow=T),
               matrix(c(0.05, 0.95,     0.95, 0.05), ncol=2, byrow=T),
               matrix(c(0.3, 0.7,        0.3, 0.7), ncol=2, byrow=T)) #COVARIATE 

mod1 <- poLCA.simdata(5000,nclass=2,probs= probs2, P= c(0.2,0.8), missval = F) 
df1 <- cbind(mod1$dat[,1:4], COV1=mod1$dat[,5]*10, trueclass=mod1$trueclass)
head(df1)
#probabilities with 5% classification error for three classes 
probs3 <- list(matrix(c(0.95,0.025,0.025,    0.025,0.95,0.025,    0.025,0.025,0.95 ), ncol=3, byrow=TRUE), # Y1
               matrix(c(0.95,0.025,0.025,    0.025,0.95,0.025,    0.025,0.025,0.95 ), ncol=3, byrow=TRUE), # Y2
               matrix(c(0.95,0.025,0.025,    0.025,0.95,0.025,    0.025,0.025,0.95 ), ncol=3, byrow=TRUE), #Y3
               matrix(c(0.025,0.025,0.95,    0.025,0.95,0.025,    0.95,0.025,0.025 ), ncol=3, byrow=TRUE), #Y4
               matrix(c((1/3), (1/3), (1/3),            (1/3), (1/3), (1/3)), ncol=2, byrow=T)) #COVARIATE with equal relations

set.seed(125)
mod2 <- poLCA.simdata(N=5000, P=(c(0.4,0.35,0.25)), probs=probs3) 
df2 <- cbind(mod2$dat[,1:4]+1, COV2=mod2$dat[,5]*10, sectors=mod2$trueclass+1) #+1 zodat '1' hier niet meer voorkomt, dit is de klas van de woningen
head(df1)
head(df2)
#elke Y-waarde die 2 is (=bedrijf) vervangen door een nieuwe waarde (die de klasse/sector) aangeeft
for(i in c(1:4, 6)){ #nu wordt de covariaat niet meegenomen. Let op: op de 5e plek moet nu dus in beide datasets een covariaat staan
  to_replace = which(df1[,i] == 2)
  df1[,i][to_replace] <- df2[,i][to_replace]
}
Dat_var_A <- cbind(df1[,1:5], COV2=df2$COV2, trueclass=df1[,6])
head(Dat_var_A)
summary((as.factor(Dat_var_A$trueclass))) #how many per class #check to see that it check the column with the trueclass
cbind(COV1=summary((as.factor(Dat_var_A$COV1))), COV2=summary((as.factor(Dat_var_A$COV2))))








### variant B
#5% selection error, 20% classification error 

set.seed(123)
#probabilities for two classes for four indicator variables
mod1 <- poLCA.simdata(5000,nclass=2,probs= select_5, P= c(0.2,0.8), missval = F) 
df1 <- cbind(mod1$dat,trueclass=mod1$trueclass)
head(df1)
set.seed(125)
mod2 <- poLCA.simdata(N=5000, P=(c(0.4,0.35,0.25)), probs=meas_20) 
df2 <- cbind(mod2$dat[,1:4]+1,sectors=mod2$trueclass+1) #+1 zodat '1' hier niet meer voorkomt, dit is de klas van de woningen


#elke Y-waarde die 2 is (=bedrijf) vervangen door een nieuwe waarde (die de klasse/sector) aangeeft
for(i in 1:ncol(df1)){
  to_replace = which(df1[,i] == 2)
  df1[,i][to_replace] <- df2[,i][to_replace]
}

Dat_var_B <- df1
head(Dat_var_B)
summary((as.factor(Dat_var_B[,5]))) #how many per class 



### variant C
#20% selection error, 5% classification error 
set.seed(123)
#probabilities for two classes for four indicator variables
mod1 <- poLCA.simdata(5000,nclass=2,probs= select_20, P= c(0.2,0.8), missval = F) 
df1 <- cbind(mod1$dat,trueclass=mod1$trueclass)
head(df1)
set.seed(125)
mod2 <- poLCA.simdata(N=5000, P=(c(0.4,0.35,0.25)), probs=meas_5) 
df2 <- cbind(mod2$dat[,1:4]+1,sectors=mod2$trueclass+1) #+1 zodat '1' hier niet meer voorkomt, dit is de klas van de woningen
#elke Y-waarde die 2 is (=bedrijf) vervangen door een nieuwe waarde (die de klasse/sector) aangeeft
for(i in 1:ncol(df1)){
  to_replace = which(df1[,i] == 2)
  df1[,i][to_replace] <- df2[,i][to_replace]
}

Dat_var_C <- df1
head(Dat_var_C)
summary((as.factor(Dat_var_C[,5]))) #how many per class


### variant D
#20% selection error, 20% classification error 
set.seed(123)
mod1 <- poLCA.simdata(5000,nclass=2,probs= select_20, P= c(0.2,0.8), missval = F) 
df1 <- cbind(mod1$dat,trueclass=mod1$trueclass)
mod1$P
head(df1)
set.seed(125)
mod2 <- poLCA.simdata(N=5000, P=(c(0.4,0.35,0.25)), probs=meas_20) 
df2 <- cbind(mod2$dat[,1:4]+1,sectors=mod2$trueclass+1) #+1 zodat '1' hier niet meer voorkomt, dit is de klas van de woningen
head(df2)

#elke Y-waarde die 2 is (=bedrijf) vervangen door een nieuwe waarde (die de klasse/sector) aangeeft
for(i in 1:ncol(df1)){
  to_replace = which(df1[,i] == 2)
  df1[,i][to_replace] <- df2[,i][to_replace]
}
Dat_var_D <- df1
head(Dat_var_D)
summary((as.factor(Dat_var_D[,5]))) #how many per class

#----------------------------Data patroon-------------------------------#
library(dplyr)
dffreq <- df1[,1:4] %>% 
  count(Y1, Y2, Y3, Y4)
dffreq_A <- Dat_var_A[,1:4] %>% 
  count(Y1, Y2, Y3, Y4)
dffreq_B <- Dat_var_B[,1:4] %>% 
  count(Y1, Y2, Y3, Y4)

#-----------------------------------NOTES------------------------------------#
#due to the replacement of 80% of the first dataset, the relations might not remain as accurate as we simulated.
#-> we can see this in the calculation of the posteriors since we have to calculate the $probs of the combined dataset


