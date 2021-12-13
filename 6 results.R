#Results


#VARIANT A
#original simulated data - the reference point 
true_P <- prop.table(table((as.factor(Dat_var_A[,5])))) #proportions per class 

#modal assignment via polca object
predictedP #sort classes by size to be able to compare

#imputations with posteriors as probabilities (MILC)
MILC_P <- prop.table(table(imp1))

#imputations (tree-MILC)
                      #yet to be done

#make bias plots   #(make sure the classes are called the same!)
MILC_P-true_P      #bias MILC
          #bias tree-MILC


plot()


