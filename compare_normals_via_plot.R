#################################################################################################################
#Some initial plotting parameters for the abline
slope=1
int = 0

#################################################################################################################
#Same Mean Same variance
norm1<-rnorm(100,0,1)
norm2<-rnorm(100,0,1)

x<-cbind(norm1,1)
y<-cbind(norm2,2)
#rbind(x,y)

d1 <- as.data.frame(qqplot(norm1,norm2,plot.it=FALSE))
p1<-ggplot(d1) + geom_point(aes(x=x, y=y)) + geom_abline(slope = slope, intercept = int) + 
  geom_point(aes(x=x, y=y),colour="blue")+ggtitle("QQ - Norm - Same Mean and Var")

d2<-data.frame(rbind(x,y))
p2 <- ggplot(d2, aes(x=norm1, colour=as.factor(V2))) +
  geom_density() +
  ggtitle("Norm - Same Mean and Var")

#################################################################################################################
#Different Mean Same Variance

norm1<-rnorm(100,0,1)
norm2<-rnorm(100,.1,1)

x<-cbind(norm1,1)
y<-cbind(norm2,2)

d1 <- as.data.frame(qqplot(norm1,norm2,plot.it=FALSE))
p3<-ggplot(d1) + geom_point(aes(x=x, y=y)) + geom_abline(slope = slope, intercept = int) + 
  geom_point(aes(x=x, y=y),colour="blue")+ggtitle("QQ - Norm - Diffrent Mean Same Var")

d2<-data.frame(rbind(x,y))
p4 <- ggplot(d2, aes(x=norm1, colour=as.factor(V2))) +
  geom_density() +
  ggtitle("Norm - Different Mean Same Var")

#################################################################################################################
#Diffent Mean and Different Variance

norm1<-rnorm(100,0,2)
norm2<-rnorm(100,2,1)

x<-cbind(norm1,1)
y<-cbind(norm2,2)

d1 <- as.data.frame(qqplot(norm1,norm2,plot.it=FALSE))
p5<-ggplot(d1) + geom_point(aes(x=x, y=y)) + geom_abline(slope = slope, intercept = int) + 
  geom_point(aes(x=x, y=y),colour="blue")+ggtitle("QQ - Norm - Diffrent Mean Diff Var")

d2<-data.frame(rbind(x,y))
p6 <- ggplot(d2, aes(x=norm1, colour=as.factor(V2))) +
  geom_density() +
  ggtitle("Norm - Different Mean Diff Var")

#################################################################################################################

#Different Distributions
norm1<-rnorm(100,0,1)
norm2<-rexp(100,2)

x<-cbind(norm1,1)
y<-cbind(norm2,2)
#rbind(x,y)

d1 <- as.data.frame(qqplot(norm1,norm2,plot.it=FALSE))
p7<-ggplot(d1) + geom_point(aes(x=x, y=y)) + geom_abline(slope = slope, intercept = int) + 
  geom_point(aes(x=x, y=y),colour="blue")+ggtitle("Different Dist")

d2<-data.frame(rbind(x,y))
p8 <- ggplot(d2, aes(x=norm1, colour=as.factor(V2))) +
  geom_density() +
  ggtitle("Different Distributions")

#################################################################################################################
#Will need to source the multiplot.R script to plot below

multiplot(p1,p3,p5,p7,p2,p4,p6,p8,cols=2)
