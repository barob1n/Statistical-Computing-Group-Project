# This contains the tests for the maximum mean discrepancy test

# input: the two samples X and Y along with m the number of points in each sample (assumed to be equal)

#    Unbiased Quadratic Time Esimator:
# uMMDDecision(X,Y,m,R) does the tests for given number of boot comparisons R
# uMMD(X,Y,m) will give you the statistic for the original sample without giving you a value to compare it to
# uMMDP gives you the statistic given the kernel matrix 

#    Unbiased Linear Time Estimator:
# lMMD(X,Y,m) will give you the statistic
# lMMDDecision  Not a very fast method (just coded in the stuff for other statistics) 

##Modified for one dim, need to make small changes (marked ###)for multivar

#compute the radial basis function kernel for two vecs
rbfk <- function(a,b, sigma = 1){
  Diff <- a - b
  abDiffNorm <- Diff %*% Diff; #the inner product of a and b
  result <- exp(-1*abDiffNorm/(2*sigma))
}

#given the two samples X and Y, define the matrix P of the kernel on pairs

kernelMatrix <- function(X,Y,m){
  Z <- c(X,Y) #combine the rows of X and Y data points  ###use rbind for multi
  P <- matrix(0,2*m,2*m)
  
  for(i in 1:(2*m)){
    for(j in 1:(2*m)){

      P[i,j] <- rbfk(Z[i],Z[j])    ### add commas fors multi
    }
  }
  
  P <- P - diag(P)*diag(2*m) #make the diagonal elements 0
  result <- P

}


uMMD <- function(X,Y,m){
  P <- kernelMatrix(X,Y,m)
  xBlock <- P[1:m,1:m]
  yBlock <- P[(m+1):(2*m),(m+1):(2*m)]
  crossBlock <- P[1:m,(m+1):(2*m)]
  
  result <- (sum(xBlock)+sum(yBlock))/(m^2-m) - 2*sum(crossBlock)/(m^2)
}

uMMDP <- function(P,m){
  xBlock <- P[1:m,1:m]
  yBlock <- P[(m+1):(2*m),(m+1):(2*m)]
  crossBlock <- P[1:m,(m+1):(2*m)]
  
  result <- (sum(xBlock)+sum(yBlock))/(m^2-m) - 2*sum(crossBlock)/(m^2)
}

#Now we shuffle the matrix to get bootstrap baseline for the value then decide acceptance

uMMDDecision <- function(X,Y,m,R){
  
  P <- kernelMatrix(X,Y,m)
  K <- 2*m
  
  S <- numeric(R)   #storage of values of statistic for different perms
  options(warn = 1)
  
  SO <- uMMDP(P,m)  #statistic on original
  
  for( i in 1:R){
  k <- sample(K,2*m,replace=F)

  Q <- P[,k] #permute the columns
  Q <- Q[k,] #permute the rows in the same way to maintain symmetry
  
  S[i] = uMMDP(Q,m)
  }

  p <- mean(c(SO,S) >= SO)
  options(warn = 0)
  
  print(p)
<<<<<<< HEAD
  
  if(p < .05){
    print("Rejected")
  }

  if(p > .95){
    print("Rejected")
  }
  pval <- p
=======

  pval <- p
#   print(p)
#   
#   if(p < .05){
#     print("Rejected")
#   }
#   
#   if(p > .95){
#     print("Rejected")
#   }
>>>>>>> myMacBranch
}



# lMMD

hk <- function(a,b,c,d){
  result <- rbfk(a,c) + rbfk(b,d) - rbfk(a,d) - rbfk(b,c)
}

lMMD <- function(X,Y,m){
  m2 <- floor(m/2)
  Z <- rbind(X,Y)   #since X and Y are rows of data, we use row bind
  stat <- 0
  for(i in 1:m2){
    stat <- stat + hk(Z[2*i-1,],Z[2*(2*i-1),],Z[2*i,],Z[4*i,])
  }
  
  stat <- stat/m2
  print(stat)
}

lMMDDecision <- function(X,Y,m,R){
  
  
  Z <- rbind(X,Y)   #combine the data
  S <- numeric(R)   #values of statistic for different perms
  options(warn = 1)
  SO <- lMMD(X,Y,m)  #statistic on original
  
  for (i in 1:R) {
    #indices sample
    k <- sample(2*m,2*m)
    x1 <- Z[k,]
    y1 <- Z[-k,]
    S[i] <- lMMD(x1,y1,m)
  }
  
  p <- 1
  for( j in 1:R){
    p <- p + as.numeric(S[i] >= SO)
  }
  p <- p/(R+1)
  #p <- mean(c(SO,S) >= SO)
  options(warn = 0)
  
  print(p)
  
  if(p < .05){
    print("Rejected")
  }
  if(p > .95){
    print("Rejected")
  }
}

# #Testing
# m <- 3
# n <- 3
# sigma <- 1;
# 
# mu1 <- c(2,2)
# mu2 <- c(-2,-2)
# 
# 
# Sigma <- matrix(c(1,0,0,1),2,2) #same covariance matrix
# A <- mvrnorm(m,mu1,Sigma)    
# B <- mvrnorm(n,mu2,Sigma)
# 
# print(A)
# print(B)
# alright <- lMMDDecision(A,B,m,99)


# alright <- lMMDDecision(A,B,m,99)
