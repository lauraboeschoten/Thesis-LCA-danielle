library(poLCA)

load("bootstrap_dataset_RR.RData")

# Go to long format
longdat = list(NA)
LCAS    = list(NA)

nboot = 5


for(i in 1:nboot){
  cat(i)
  # temporarily create a short dataset per bootstrap sample
  datforslice = dfboot[,c(1:4,i+5)]
  colnames(datforslice) = c("Y1","Y2","Y3","Y4","Freq")
  # use this as imput for the slice function
  longdat[[i]] = datforslice %>% 
    slice(rep(1:n(), Freq)) %>%  # 1 moet hier nog i worden
    select(c("Y1","Y2","Y3","Y4"))
  
  # latent class model 
  LCAS[[i]] = poLCA(formula = cbind(Y1, Y2, Y3, Y4) ~ 1, 
              longdat[[i]],
              nclass = 4, 
              nrep = 10)
}


  # stappn voor tree step (moeten in aparte scripts): 
  # eerst de dataset weer terugbrengen naar de 2 selectie klassen
  # vervolgens 2 klassenmodel schatten
  # Vervolgens de selectie imputeren
  # binnen selectie alleen de cases selecteren in klasse 2 
  # daar vervolgens 3 klassen model op fitten 
  
  
  
  





