#K-S test


#Draws random multivariate normal samples from means with
#two different distrubtions.
#Plot them in different colors.

library(MASS)
m <- 50
n <- 50

mu1 <- c(2,2)
mu2 <- c(-2,-2)


Sigma <- matrix(c(1,0,0,1),2,2) #same covariance matrix
A <- mvrnorm(m,mu1,Sigma)    # different means
B <- mvrnorm(n,mu2,Sigma)

#A <- rnorm(m, 0, 1)
#B <- rnorm(n, 0, 1)

R <- 999
z <- rbind(A,B)
#z <- c(A,B)
K <- 1:(m+n)
D <- numeric(R)
options(warn = 1)

DO <- ks.test(A,B,exact=F)$statistic

for (i in 1:R) {
  #indices sample
  k <- sample(K,m,replace=F)
  x1 <- z[k,]
  y1 <- z[-k,]
  D[i] <- ks.test(x1,y1,exact=F)$statistic
}

p <- mean(c(DO,D) >= DO)
options(warn = 0)

print(p)

if(p < .05){
  print("Rejected")
}