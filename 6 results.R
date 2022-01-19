#Results
library(ggplot2)

#HOW TO POOL OVER CLASSES?
#ACCURACY POOLEN? 5 keer accuracy berekenen en dan poolen? (check Gerko's research)
#ADD CONFUSION MATRIX WITH TRUE/FALSE  (we can see in which class they are placed wrongly)
#1. OVERALL GROUPSIZES (bias, coverage CI, SE en ME)
#2. Relationship w/ covariate (bias, coverage CI, SE en ME)
#3. Relationship w/ true variable (bias, coverage CI, SE en ME)

#VARIANT A
#imputations with posteriors as probabilities (MILC)
MILC_P <- prop.table(table(imp1))
true_P <- LCAS2_probs[[1]]
MILC_P-true_P 

bias_A1 <- MILC_P-true_P      #bias MILC
#plot for bias MILC 
plot(bias_A1, ylab="Bias", xlab= "Classes", ylim = c(), type="p", pch=4, abline(h=0))
#lines(var_D_tree, ylab="Bias", col=("green"), type="p",pch=4, add=T) #add tree-MILC results for comparison
#legend(x="bottomright", title='Method',legend=c("MILC", "tree-MILC"), col=c("black", "green"), pch=4,box.lty=0, cex=0.6)

results_impD <- list()
for(j in 1:5){
  results_impD[[j]] <- prop.table(table(imps_D[[j]]))
}
results_impD


