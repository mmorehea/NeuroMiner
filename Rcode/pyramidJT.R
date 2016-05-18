library(randomForest)

#read.csv("../first_subsets/pyramidal_appended.csv",T)->dump
read.csv("../NeuronDataMaster.csv",T)->x

# kilb data showed erro in the Sholl values, these rows are deleted
x<-x[x$Archive.Name!="Kilb",]
x<-x[x$Archive.Name!="McQuiston",]


nx1<-33:96
nx2<-97:110
nx3<-111:159
ny<-4

dat<-data.frame(y=as.factor(x[,ny]),x[,c(nx1,nx2,nx3)])
dat<-na.omit(dat)
dat$Soma.Surface<-as.numeric(dat$Soma.Surface)


save(dat, file= "NeuronDataMaster.Rdata")
nxx1<-2:65
nxx2<-66:79
nxx3<-81:dim(dat)[2] #80 is Sholl.1 should always be 1

#which(dat[,80:128]<1,arr.ind=T)
#dat[dat[,80:128]<1]


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
