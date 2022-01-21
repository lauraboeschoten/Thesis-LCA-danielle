#-------------------------------5. Results-----------#
load("imputations_RR.RData")

imp = list(NA)
imp2 = list(NA)
bias = list(NA)
method = list(NA)

#Overall group sizes
trueclass <- prop.table(table(df1$trueclass)) #proportions original data

##bias
for(m in 1:5){ #bias 
  imp[[m]] <- prop.table(table(implist[[m]]$imp))
  bias[[m]] <- trueclass-imp[[m]]
}
bias #bias between group sizes of original data and the imputations of the bootstrap data
#pool bias
par(mfrow=c(2,3))
for(m in 1:5){plot(bias[[m]],main = paste("Bias of bootstrap", m), ylab="Bias", xlab= "Classes", ylim = c(-.15, 0.15), type="p", pch=4, abline(h=0), add=T)}

save.image("Results_RR.RData")