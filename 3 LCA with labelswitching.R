#understand code 3, LCA
boots_5

LCAS    = list(NA)
LCAS2   = list(NA)
LCA3    = list(NA)





for (i in 1:length(boots_5)) {
  LCAS[[i]] <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1), data = boots_5[[i]], nclass=4, nrep=10)
  # now solve label switching issue
  ## first get the order of the classes of Y1 (assume consistence over indicators)
  order = c(which.max(LCAS[[i]]$probs$Y1[1,]),
            which.max(LCAS[[i]]$probs$Y1[2,]),
            which.max(LCAS[[i]]$probs$Y1[3,]),
            which.max(LCAS[[i]]$probs$Y1[4,]))
  
  ## store LC output under a new name
  LCAS2[[i]] = LCAS[[i]]
  
  LCAS2[[i]]$probs$Y1 = LCAS[[i]]$probs$Y1[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y2 = LCAS[[i]]$probs$Y2[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y3 = LCAS[[i]]$probs$Y3[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y4 = LCAS[[i]]$probs$Y4[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$P        = LCAS[[i]]$P[c(as.numeric(paste(order)))]
}

LCAS[[1]]$probs
LCAS[[1]]$probs$Y1
LCAS[[1]]$probs$Y1[1,]
LCAS[[2]]$probs$Y1[1,]
LCAS[[1]]$probs$Y1
LCAS[[1]]$probs$Y2
LCAS[[1]]$probs$Y3
LCAS[[1]]$probs$Y4
#we aSsume consistence over indicators. it works. now.



order = c(which.max(LCAS[[1]]$probs$Y1[1,]), 
          which.max(LCAS[[1]]$probs$Y1[2,]),
          which.max(LCAS[[1]]$probs$Y1[3,]),
          which.max(LCAS[[1]]$probs$Y1[4,]))
order
LCAS_probs <- list(NA)
for (i in 1:length(boots_5)) {  
  LCAS_probs[[i]] <- LCAS[[i]]$P 
}
LCAS_probs

LCAS2_probs <- list(NA)
for (i in 1:length(boots_5)) {  
  LCAS2_probs[[i]] <- LCAS2[[i]]$P 
  }
LCAS2_probs



order(LCAS[[1]]$P, decreasing=T)
sort(LCAS[[1]]$P, decreasing = T)

#what do we need? class 1 means the same class in each of the datasets 
#try with general probs

for (i in 1:length(boots_5)) {
  LCAS[[i]] <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1), data = boots_5[[i]], nclass=4, nrep=10)
  # now solve label switching issue
  ## first get the order of the classes of Y1 (assume consistence over indicators)
  order = c(which.max(LCAS[[i]]$probs$Y1[,1]),
            which.max(LCAS[[i]]$probs$Y1[,2]),
            which.max(LCAS[[i]]$probs$Y1[,3]),
            which.max(LCAS[[i]]$probs$Y1[,4]))
  
  ## store LC output under a new name
  LCAS2[[i]] = LCAS[[i]]
  
  LCAS2[[i]]$probs$Y1 = LCAS[[i]]$probs$Y1[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y2 = LCAS[[i]]$probs$Y2[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y3 = LCAS[[i]]$probs$Y3[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y4 = LCAS[[i]]$probs$Y4[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$P        = LCAS[[i]]$P[c(as.numeric(paste(order)))]
}







##################RE.ORDER##################
try_reorder <- list()
for (i in 1:5) {
  try_reorder[[i]] <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1), data = boots_5[[i]], nclass=4, nrep=10)

}
old.probs.start <- try_reorder[[1]]$probs.start
new.probs.start <- poLCA.reorder(old.probs.start, c(4,2,1,3))
try_reorder[[1]]$P
newmod <- poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1),probs.start = new.probs.start , data = boots_5[[1]], nclass=4, nrep=1)
newmod$P
