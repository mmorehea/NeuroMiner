library(randomForest)

read.csv("../subsets/pyramidal_appended.csv",T)->x
nx1<-33:96
nx2<-97:161
ny<-4
dat<-data.frame(y=as.factor(x[,ny]),x[,c(nx1,nx2)])
dat<-na.omit(dat)

nxx1<-2:65
nxx2<-67:dim(dat)[2]


set.seed(100)
g1<-randomForest(y~.,data=dat[,c(1,nxx1)])##,prox=TRUE)
cols=rainbow(length(unique(dat[,1]))+2)
matplot(g1$err.rate,lwd=2,col=cols,lty=1,type="l")
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(g1)


set.seed(100)
g2<-randomForest(y~.,data=dat[,c(1,nxx2)])##,prox=TRUE)
cols=rainbow(length(unique(dat[,1]))+2)
matplot(g2$err.rate,lwd=2,col=cols,lty=1,type="l")
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(g2)


set.seed(100)
g3<-randomForest(y~.,data=dat[,-66])##,prox=TRUE)
cols=rainbow(length(unique(dat[,1]))+2)
matplot(g3$err.rate,lwd=2,col=cols,lty=1,type="l")
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(g3)







