library(poLCA)

options(scipen = 999)
set.seed(123)

select_5 <-  list(matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                  matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                  matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                  matrix(c(0.05, 0.95, 0.95, 0.05), ncol=2, byrow=T))

select_20 <- list(matrix(c(0.8, 0.2, 0.2, 0.8), ncol=2, byrow=T),
                  matrix(c(0.8, 0.2, 0.2, 0.8), ncol=2, byrow=T),
                  matrix(c(0.8, 0.2, 0.2, 0.8), ncol=2, byrow=T),
                  matrix(c(0.2, 0.8, 0.8, 0.2), ncol=2, byrow=T))

# make a list containing the different selection error matrices
select = list(select_5, 
              select_20)


meas_5 <- list(matrix(c(0.95,0.025,0.025, 0.025,0.95,0.025, 0.025,0.025,0.95 ), ncol=3, byrow=TRUE), 
               matrix(c(0.95,0.025,0.025, 0.025,0.95,0.025, 0.025,0.025,0.95 ), ncol=3, byrow=TRUE), 
               matrix(c(0.95,0.025,0.025, 0.025,0.95,0.025, 0.025,0.025,0.95 ), ncol=3, byrow=TRUE), 
               matrix(c(0.025,0.025,0.95, 0.025,0.95,0.025, 0.95,0.025,0.025 ), ncol=3, byrow=TRUE))

meas_20 <- list(matrix(c(0.8,0.1,0.1,    0.1,0.8,0.1,     0.1,0.1,0.8 ), ncol=3,   byrow=TRUE), # Y1
                matrix(c(0.8,0.1,0.1,    0.1,0.8,0.1,     0.1,0.1,0.8 ), ncol=3, byrow=TRUE), # Y2
                matrix(c(0.80,0.1,0.1,   0.1,0.8,0.1,     0.1,0.1,0.8 ), ncol=3, byrow=TRUE), #Y3
                matrix(c(0.1,0.1,0.80,   0.1,0.8,0.1,     0.80,0.1,0.1 ), ncol=3, byrow=TRUE)) #Y4

# make a list containing the different measurement error matrices
meas = list(meas_5,
            meas_20)

# simulate 10 datasets
nsim  = 10
ssize = 5000


#1. simulate 10 selection error sets
select_mods = list(NA)
dfs_select  = list(NA)
meas_mods   = list(NA)
dfs_meas    = list(NA)

for(i in 1:nsim){
  cat(i)
  select_mods[[i]] <- poLCA.simdata(N = ssize,
                                    nclass = 2,
                                    probs = select[[1]], 
                                    P = c(0.2,0.8), 
                                    missval = F)
  
  dfs_select[[i]] <- cbind(select_mods[[i]]$dat,
                           trueclass = select_mods[[i]]$trueclass)
  
  meas_mods[[i]] <- poLCA.simdata(N = ssize,
                                  nclass = 3,
                                  P = c(0.4,0.35,0.25),
                                  probs = meas[[1]])
  
  dfs_meas[[i]] <- cbind(meas_mods[[i]]$dat[,1:4]+1,
                         sectors = meas_mods[[i]]$trueclass+1) 
  
  for(j in 1:ncol(dfs_select[[i]])){
    to_replace = which(dfs_select[[i]][,j] == 2)
    dfs_select[[i]][,j][to_replace] <- dfs_meas[[i]][,j][to_replace]
    }
}



