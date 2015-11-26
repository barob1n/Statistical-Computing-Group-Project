#################################################################################################################
#Main script to run test of methods

library(MASS)
source('permTestBoot.R')  #include the file which contains the function definitions for the statistics

#testing some univariate  data
m <- 10
n <- 10
thismany<-10

delta<-2
min_mu<-0
max_mu<-2
min_var<-0
max_var<-5

alpha<-.05

#steps_mu<-is.integer((max_mu-min_mu)/delta)
steps_mu<-seq(min_mu,max_mu,delta)

steps_var<-is.integer((max_var-min_var)/delta)

simp_mu_data<-numeric(thismany)
percent_simp_mu<-as.numeric(steps_mu)


#Std normal
A<-rnorm(n,0,1)

for(i in 1:length(steps_mu)){
  B<-rnorm(m,steps_mu,1)
  for(j in 1:thismany){
    
   data<-permTestBoot(A,B,999,stat="ks") 
   tb <-c(data$t, data$t0)
   mean(tb >= data$t0)
   simp_mu_data[j]<- mean(tb >= data$t0)
   
  }
  percent_simp_mu[i]<-(sum(simp_mu_data>=alpha))/length(thismany)

}
