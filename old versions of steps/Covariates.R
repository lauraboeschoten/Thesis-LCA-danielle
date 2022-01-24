
#--------------------------COVARIATES----------------------------------#

#program covariate as if it is an indicator variable

#each row is for a latent class, and the number of columns is the nr of responses 
#covariate with ... strong relation with ...
#ncol = nr of categories of covariate 
#nrow = ??
#how many rows are needed?

matrix(c(0.3, 0.7,
         0.7, 0.3), ncol=2, byrow=T) # (For selection error part)

matrix(c(0.3, 0.7, 
         0.7, 0.3,
         0.3, 0.7), ncol=2, byrow=T) # (For classification error part)

#covariate with ... equal relations
matrix(c(0.5, 0.5,
         0.5, 0.5), ncol=2, byrow=T) # (For selection error part)

matrix(c((1/3), (1/3), 
         (1/3), (1/3), 
         (1/3), (1/3)), ncol=2, byrow=T)  # (For classification error part)

#note: one covariate will be correlated with selection, and one with classification error.
#-> relations might not remain as accurate as we simulated
#alternative: specify relation t.o.v. true class in final dataset 