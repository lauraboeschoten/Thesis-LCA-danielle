library(poLCA)
data("carcinoma")
data("cheating")
data("election")
data("gss82") #2dichotmous 2trichotomous
data("values") #dichotomous

##NOTES ON POLCA##
#1. #Note that if nrep > 1, then any user-specified probs.start values are only used in the first of the nrep attempts
#2. "Because the latent classes are unordered categories, the numerical order of the estimated latent classes in the model output is arbitrary, and is determined solely by the start values of the EM algorithm. (Linzer and Lewis, 2011, poLCA, 4.6)

g <- cbind(LIEEXAM,LIEPAPER,FRAUD,COPYEXAM)~1
ch2 <- poLCA(g,cheating,nclass=2, graphs = T)
poLCA.table(formula=FRAUD~1, condition= list(),lc=ch2) #to inspect cell counts
poLCA.table(formula=FRAUD~COPYEXAM, condition= list(LIEEXAM=2, LIEPAPER=1),lc=ch2) 
poLCA.predcell(lc=ch2,y=c(1,1,1,1))#predicted cell percentages


#tekst new 
ch2$probs #for the four variables matrices with probability for each answer (in columns, 1= no, 2=yes) given the class (in rows)
ch2$probs$LIEEXAM
ch2$probs[[1]]
ch2$probs[[1]][1,1] 
ch2$predclass
ch2$y #y scores
ch2$predcell #predicted for each score pattern 
Prob.X <- ch2$P
Prob.X[1] #probability of being in group 1 
ncol(ch2$y) #4 questions with 2 
summary(ch2$y)
class(ch2$y)

prob.YgivenX <- 2 #product of each probabilities for each question given X

#opslaan in matrix o.i.d. met row =, column = 
#CALCULATION OF POSTERIOR FOR SCORE PATTERN 1,1,1,1
prob.Y1givenX1 <- ch2$probs[[1]][1,1] * ch2$probs[[2]][1,1] *ch2$probs[[3]][1,1] *ch2$probs[[4]][1,1]
prob.Y1givenX1
prob.Y1givenX2 <- ch2$probs[[1]][2,1] * ch2$probs[[2]][2,1] *ch2$probs[[3]][2,1] *ch2$probs[[4]][2,1]
prob.Y1givenX2
prob.Y1 <- Prob.X[1]*prob.Y1givenX1+Prob.X[2]*prob.Y1givenX2
prob.X1givenY1 <- (Prob.X[1]*prob.Y1givenX1)/prob.Y1 #0.9788363
ch2$posterior[1] #one of the subjects with score pattern 1,1,1,1 has as posterior probability 0.9788363

#CALCULATION OF POSTERIOR FOR SCORE PATTERN 1,2,1,2
prob.Y1212givenX1 <- ch2$probs[[1]][1,1] * ch2$probs[[2]][1,2] *ch2$probs[[3]][1,1] *ch2$probs[[4]][1,2]
prob.Y1212givenX1
prob.Y1212givenX2 <- ch2$probs[[1]][2,1] * ch2$probs[[2]][2,2] *ch2$probs[[3]][2,1] *ch2$probs[[4]][2,2]
prob.Y1212givenX2
prob.Y1212 <- Prob.X[1]*prob.Y1212givenX1+Prob.X[2]*prob.Y1212givenX2
prob.X1givenY1212 <- (Prob.X[1]*prob.Y1212givenX1)/prob.Y1212 #0.7363866 
prob.X2givenY1212 <- (Prob.X[2]*prob.Y1212givenX2)/prob.Y1212 #0.2636134
list(P.class1=prob.X1givenY1212, P.class2=prob.X2givenY1212)
ch2$y[250:319,]
ch2$posterior[282,] #0.263613   #0.736387


Prob.X2givenY <- (Prob.X[2]*Prob.YgivenX)/Prob.Y


#this is from an example in a poLCA file
gpa <-  cbind(LIEEXAM,LIEPAPER,FRAUD,COPYEXAM)~GPA
ch2c$coeff
ch2c <- poLCA(gpa, cheating,nclass=2)
GPAmat <- cbind(1,c(1:5))
exb <- exp(GPAmat%*% ch2c$coeff)
matplot(c(1:5),cbind(1/(1+exb),exb/(1+exb)),type="l",lwd=2,main="GPA as a predictor of persistent cheating",
        xlab="GPA category, low to high",
        ylab="Probability of latent class membership")
text(1.7,0.3,"Cheaters")
text(1.7,0.7,"Non-cheaters")

cheatcl <- which.min(ch2$P)
predcc <- sapply(c(1:5),function(v) mean(ch2$posterior[cheating$GPA==v,cheatcl],na.rm=TRUE))
## Having run Ex.2, add to plot:
matplot(c(1:5),cbind(1-predcc,predcc),type="l",lwd=2,add=TRUE)
text(4,0.14,"Cheaters\n (non-simul. estimate)")
text(4,0.87,"Non-cheaters\n (non-simul. estimate)")


#-----------------------------------------------------------------------------------------------------------------------------------#
##LCA with multiple classes based on gss82 data (attitude towards surveys)
summary(gss82)
colnames(gss82)
gss <- cbind(PURPOSE,ACCURACY,UNDERSTA,COOPERAT)~1
ch3 <- poLCA(gss,gss82,nclass=3,nrep=10, graphs = T)

#multiple classes, car data 
data("cars")
data("Cars93")
summary(Cars93)
carsvars <- cbind( Type, AirBags, DriveTrain, Cylinders,Origin)~1
cars2 <- poLCA(carsvars, Cars93,nclass=2)
cars3 <- poLCA(carsvars, Cars93,nclass=3)
cars4 <- poLCA(carsvars, Cars93,nclass=4)
cars5 <- poLCA(carsvars, Cars93,nclass=5)



