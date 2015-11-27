#Check for packages.  If not installed, install it.
check<-library("FNN",logical.return=T,quietly=T,verbose=F)
if(check=="FALSE"){
  install.packages("FNN")
  library("FNN")
}
library("Hotelling")
check<-library("Hotelling",logical.return=T,quietly=T,verbose=F)
if(check=="FALSE"){
  install.packages("Hotelling")
  library("Hotelling")
}
check<-library("boot",logical.return=T,quietly=T,verbose=F)
if(check=="FALSE"){
  install.packages("boot")
  library("boot")
}
check<-library("ggplot2",logical.return=T,quietly=T,verbose=F)
if(check=="FALSE"){
  install.packages("ggplot2")
  library("ggplot2")
}

################################################################################
#simpleStatr.R: simpleStat(x,y)
#  
#Description: Computes nearest neighbor statiistic 
#                
#Input: x - First sample (either matrix or vector)
#       y - Second sample (dimensions must match A)
#       
#
#Output: data frame, with the value of the statistics in $statistic
################################################################################
simpleStat <- function(x,y){
  statistic<-mean((x-y)^2)
  return(as.data.frame(statistic))
}



################################################################################
#nNeighbor.R: nNeighbor(x,y,k)
#  
#Description: Computes nearest neighbor statiistic 
#                
#Input: x - First sample (either matrix or vector)
#       y - Second sample (dimensions must match A)
#       k - how many neighbors: 1 = closes,2 = 1st and second closest, 3= ... 
#       
#Output: data frame, with the value of the statistics in $statistic
################################################################################
nNeighbor <- function(x,y,k=3) {
  n1 <- NROW(x)
  n2 <- NROW(y)
  n <- n1 + n2
  if(NCOL(x)==1){
    z<-c(x,y)
    z<-cbind(z,0)
  }else{
    z <- rbind(x, y)
  }
  NN <- get.knn(z, k=3)
  block1 <- NN$nn.index[1:n1, ]
  block2 <- NN$nn.index[(n1+1):n, ]
  i1 <- sum(block1 < n1 + .5)
  i2 <- sum(block2 > n1 + .5)
  statistic<-(i1 + i2) / (k * n)
  return(as.data.frame(statistic))
}
