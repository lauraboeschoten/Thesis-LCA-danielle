#2. Imputation step (MI)
cbind(head(Dat_var_A), head(Dat_var_B), head(Dat_var_C), head(Dat_var_D))
#take m bootstrap samples from the original dataset. We use m=5 (this is sufficient, see Boeschoten (2017))
