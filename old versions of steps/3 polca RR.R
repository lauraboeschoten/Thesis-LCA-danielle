#polca step
library(poLCA)
library(tidyverse)

load("bootstrap_dataset_RR.RData")

set.seed(123)

# Go to long format
longdat = list(NA)
LCAS    = list(NA)
LCAS2   = list(NA)

nboot = 5


for(i in 1:nboot){
  cat(i)
  # temporarily create a short dataset per bootstrap sample
  datforslice = dfboot[,c(1:4,i+5)]
  colnames(datforslice) = c("Y1","Y2","Y3","Y4","Freq")
  # use this as input for the slice function
  longdat[[i]] = datforslice %>% 
    slice(rep(1:n(), Freq)) %>%  
    select(c("Y1","Y2","Y3","Y4"))
  
  # latent class model 
  LCAS[[i]] = poLCA(formula = cbind(Y1, Y2, Y3, Y4) ~ 1, 
                    longdat[[i]],
                    nclass = 4, 
                    nrep = 10)
  
  # now solve label switching issue
  ## first get the order of the classes of Y1 (assume consistence over indicators)
#caution: unsure if this works for errors of high percentages
  order = c(which.max(LCAS[[i]]$probs$Y1[,1]), #class for which column 1 has the highest prob
            which.max(LCAS[[i]]$probs$Y1[,2]), # "    column 2  "
            which.max(LCAS[[i]]$probs$Y1[,3]),
            which.max(LCAS[[i]]$probs$Y1[,4]))
#now we have the order 
  
  ## store LC output under a new name
  LCAS2[[i]] = LCAS[[i]]
  
  LCAS2[[i]]$probs$Y1 = LCAS[[i]]$probs$Y1[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y2 = LCAS[[i]]$probs$Y2[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y3 = LCAS[[i]]$probs$Y3[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y4 = LCAS[[i]]$probs$Y4[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$P        = LCAS[[i]]$P[c(as.numeric(paste(order)))]
  
  # WARNING: other elements in the LC output are not switched!! 
}

#TEST
LCAS_probs <- list(NA)
for (i in 1:5) {
  LCAS_probs[[i]] <-   LCAS[[i]]$P
}
LCAS_probs 
LCAS_probs2 <- list(NA)
for (i in 1:5) {
  LCAS_probs2[[i]] <-   LCAS2[[i]]$P
}
LCAS_probs2 
LCAS2[[3]]$probs

#label switching worked! the classes are now consistently labelled over the boostrap samples

save.image("LCAS_RR.RData")

# stappn voor tree step (moeten in aparte scripts): 
# eerst de dataset weer terugbrengen naar de 2 selectie klassen
# vervolgens 2 klassenmodel schatten
# Vervolgens de selectie imputeren
# binnen selectie alleen de cases selecteren in klasse 2 
# daar vervolgens 3 klassen model op fitten 
