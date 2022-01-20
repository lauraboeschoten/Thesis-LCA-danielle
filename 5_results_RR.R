# 1. overall group sizes --> bias, coverage (discuss SE and ME separately)
# 2. relationship with covariate --> bias, coverage (SE and ME)
# 3. relationship with true variable 

library(caret)
confusionMatrix(data=as.factor(implist[[1]][,"imp"]), reference = as.factor(implist[[1]][,"trueclass"]))
#makes no sense because the imputations are based on bootstrap data, not on the original data (which is used for trueclass)
??confusionMatrix 
