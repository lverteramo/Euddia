
# Generate n Simulated Temperature data for a given month and m locations using copulas to capture any 
# dependance.

library(MASS)
set.seed(100)

m <- 3 # Number of locations
n <- 2000 # Number of simulated values

# Create the Cov Matrix to allow dependence across locations. Must be pos.semidefinite.
sigma <- matrix(c(1, 0.8, 0.4,  
                  0.8, 1, -0.2,
                  0.4, -0.2, 1),nrow=3)

z <- mvrnorm(n,mu=rep(0, m),Sigma=sigma,empirical=T) # Create matrix of correlated and normally distributed standard variables.
                                       
u<- pnorm(z) # get the probabilities (Marginal U~[0,1]) of the values to be <= z. They conserve the same dependence structure as simulated by mvrnorm

# Limit the value of u (prob) between .01 to .99 inclusive. Truncate values outside the range.

  for (j in 1: length(u[1,])){
  for (i in 1: length(u[,1])) 
  {
    if(u[i,j]>0.99){
      u[i,j]= 0.99
    } else if(u[i,j]<0.01){
      u[i,j]= 0.01
    }  
}
}


# Next step is to convert these marginals U, into a desired marginal PDF

loc1= qnorm(u[,1], mean= 20, sd= 3) # First location. Normal Dist(20,3)
loc2= qnorm(u[,2],mean=26,sd=3)  # Second Location. Normal(26,3) 
loc3= qt(u[,3], df= 6, ncp= 2)+22 # Thirsd Location. T-dist, right skewed, DF= 6, mean 24, sd= 1.4
