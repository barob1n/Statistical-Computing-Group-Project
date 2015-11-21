#simple statistic

simpleStat <- function(x,y){
   sqrt(sum(mean(x-y))^2)
}

kolSmirStat <- function(x,y){
  ks.test(x,y,exact=F)$statistic
}

#m <- 3
#n <- 4
#sigma <- 1;

#mu1 <- c(2,2)
#mu2 <- c(-2,-2)


#Sigma <- matrix(c(1,0,0,1),2,2) #same covariance matrix
#A <- mvrnorm(m,mu1,Sigma)    
#B <- mvrnorm(n,mu2,Sigma)



#compute a matrix of RBF kernels
rbfk <- function(x,y,sigma=1,m,n){
  

  #the array of inner products on the x vectors
  xInner = apply(x, 1, function(x) x %*% x);

  
  #repeat the array to get an mxn matrix whose columns are xInner
  xInnerM = replicate(n,xInner);

  #the matrix of inner products on the y vectors
  yInner = apply(y, 1, function(y) y %*% y);
  
  #repeat the array to get an mxn matrix whose rows are yInner
  yInnerM = t(replicate(m,yInner));

  
  #get the mxn matrix whose (i,j) entry is <x_i, y_j>
  
  
  #there's got to be a better way this but...

  xyInnerM = matrix(, nrow=m, ncol=n);
  for(column in 1:n){
    xyInnerM[,column] <- x%*%y[column,]
  }

  
  H = xInnerM + yInnerM - 2*xyInnerM;
  
  
  #This gives us the mxn matrix whose (i,j) entry is k(x_i,y_j)
  H = exp(-H/(2*sigma));
  
}

#rbfk(A,B,sigma, m,n)

kernelStat <- function(x,y,sigma=1){
  m <- dim(x)[1];
  n <- dim(y)[1];
  K = rbfk(x,x,sigma,m,m);
  L = rbfk(y,y,sigma,n,n);
  KL = rbfk(x,y,sigma,m,n);
  

  answer = sum(K/(m*(m-1)) + L/(n*(n-1)) - KL/(m*n) - t(KL)/(m*n));
}