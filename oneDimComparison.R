#comparison of tests
  #1D
source('permTestBoot.R')

#sample amount
m <- 50
n <- 50

#generate data
X <- rnorm(m, 0, 1)
Y <- rnorm(n, 1, 1)

#run data through tests
  #general tests

simpleStat(c(X,Y),1,c(m,n))

#graph type 1 and type 2 errors
#graph speed comparison
#graph acceptance rate