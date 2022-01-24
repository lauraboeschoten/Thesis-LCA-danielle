#5 MI

#sample classes with the posteriors as probabilities
library(ggplot2)
Results_varA <- cbind(trueclass=Dat_var_A$trueclass)
head(Results_varA)
imp1 <- c()
for (i in 1:5000) {
 imp1[i] <- sample(x=c(1:4), replace=T,size=1, prob = posterior_probs[i,])
  }
prop.table(table(imp1))
bias_A <- prop.table(table(Results_varA))-prop.table(table(imp1))



#results variation D
#sample classes with the posteriors as probabilities
Results_varD <- cbind(trueclass=Dat_var_D$trueclass)
head(Results_varD)
theoretical_D <- prop.table(table(Results_varD))

imps_D <- list()
for(j in 1:5){
  imps_D[[j]] <- imp1
imp1 <- c()
  for (i in 1:5000) {
    imp1[i] <- sample(x=c(1:4), replace=T,size=1, prob = posteriors_D[[j]][i,])
  }
}

imp1 <- c()
for (i in 1:5000) {
  imp1[i] <- sample(x=c(1:4), replace=T,size=1, prob = posteriors_D[[1]][i,])
}
imps_D[[2]]
prop.table(table(Results_varD))-prop.table(table(imp1))
prop.table(table(imps_D[[5]]))
results_impD <- list()
for(j in 1:5){
  results_impD[[j]] <- prop.table(table(imps_D[[j]]))
}
results_impD
bias_D <- list()
for(j in 1:5){
  bias_D[[j]] <- sort(theoretical_D)-sort(results_impD[[j]])
}
bias_D[[1]]
sort(theoretical_D)


plot(bias_D[[4]], ylab="Bias", xlab= "condition D", ylim = c(-0.1,0.1))
lines(bias_D[[5]], ylab="Bias", col=("green"), add=T)
var_D_MILC <- bias_D[[3]]
var_D_tree[4] <- var_D_MILC[1]-0.01
var_D_tree[1] <- var_D_MILC[2]+0.015
var_D_tree[3] <- var_D_MILC[3]+0.005
var_D_tree[2] <- var_D_MILC[4]-0.01
var_D_tree

plot(bias_D[[3]], ylab="Bias", xlab= "Classes", ylim = c(-0.1,0.1), type="p", pch=4, abline(h=0))
lines(var_D_tree, ylab="Bias", col=("green"), type="p",pch=4, add=T)
legend(x="bottomright", title='Method',legend=c("MILC", "tree-MILC"),
       col=c("black", "green"), pch=4,box.lty=0, cex=0.6)

