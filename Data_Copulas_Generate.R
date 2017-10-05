
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

loc11= qnorm(u[,1], mean= 20, sd= 3) # First location. Normal Dist(20,3)
loc21= qnorm(u[,2],mean=30,sd=8)  # Second Location. Normal(30,8) 
loc31= qt(u[,3], df= 4, ncp= 3)+40 # Thirsd Location. T-dist, right skewed, DF= 4, mean 40, 


# Do two more correlates time series with different correlation matrix and pdf

# Second Segment

# Create vectors with some correlation among them
a <- c(1,2,3,4,5,6)
b <- c(2,3,6,6,7,9) # Pos. corr. to vector a
c <- c(10,8,8,5,4,3) # Neg corr to vector a

x<- cbind(a,b,c)

# Create the Cov Matrix to allow dependence across locations. Must be pos.semidefinite.
sigma<- cov(x)


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

# Second Segment 

# Next step is to convert these marginals U, into a desired marginal PDF
# increase mean and sd by 20%
loc12= qnorm(u[,1], mean= 26, sd=6 ) # First location. Normal Dist(26,6)
loc22= qnorm(u[,2],mean= 35,sd=8)  # Second Location. Normal(35,8) 
loc32= qt(u[,3], df= 7, ncp= 2)+45 # Thirsd Location. T-dist, right skewed, DF= 7, mean 45

# Third Segment

# Create vectors with some correlation among them. These correlations are weaker than segment 2
a <- c(1,2,3,4,5,6)
b <- c(3,3,6,5,7,4) # Pos. corr. to vector a
c <- c(10,8,8,11,7,7) # Neg corr to vector a

x<- cbind(a,b,c)

# Create the Cov Matrix to allow dependence across locations. Must be pos.semidefinite.
sigma<- cov(x)


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

# Third segment

# Next step is to convert these marginals U, into a desired marginal PDF
# Decrease meand and sd by about 20%
loc13= qnorm(u[,1], mean= 30, sd= 6) # First location. Normal Dist(30,6)
loc23= qnorm(u[,2],mean= 20,sd=8)  # Second Location. Normal(20,8) 
loc33= qt(u[,3], df= 3, ncp= 3)+15 # Thirsd Location. T-dist, right skewed, DF= 3, mean 15, sd= 1.4


# The data are now correlated with a specific PDF each.

# Concatenate all segments to create a T x 3 Matrix. Each segment lenght 2000.

DataRaw<- matrix(c(loc11, loc21, loc31,  
                   loc12, loc22, loc32,
                   loc13, loc23, loc33),ncol=3)  


rm(loc11, loc12, loc13, loc21, loc22, loc23, loc31, loc32, loc33, a, b, c, i, j, m, n, x, z, u, sizeD, sigma)
# plot data



# add outliers




# Analyis Begin

# 

