library(randomForest)

#read.csv("../first_subsets/pyramidal_appended.csv",T)->dump
read.csv("../NeuronDataMaster.csv",T)->x

# kilb data showed erro in the Sholl values, these rows are deleted
#x<-x[x$Archive.Name!="Kilb",]
x<-x[x$Archive.Name!="McQuiston",]


nx1<-33:96
nx2<-97:110
nx3<-111:159
ny<-4

dat<-data.frame(y=as.factor(x[,ny]),x[,c(nx1,nx2,nx3)])
dat<-na.omit(dat)
dat$Soma.Surface<-as.numeric(dat$Soma.Surface)
dat$Gstat.total.cable.length<-as.numeric(dat$Gstat.total.cable.length)
dat$Fractal_Dim<-as.numeric(dat$Fractal_Dim)

save(dat, file= "NeuronDataMaster.Rdata")
nxx1<-2:65
nxx2<-66:79
nxx3<-81:dim(dat)[2] #80 is Sholl.1 should always be 1

#which(dat[,80:128]<1,arr.ind=T)
#dat[dat[,80:128]<1]
print.err<-function(rfor,subj)
{
  last<-end(rfor$err.rate)[1]
  mains<-paste(subj, ", OOB=",round(mean(rfor$err.rate[last,1]),3))
  matplot(rfor$err.rate,lwd=2,col=cols,lty=1,type="l",main=mains)
  legend("bottomright",inset=.05,legend=colnames(rfor$err.rate),
         cex=.7,col=cols,lty=1,lwd=3)
  ##MAP cols to dimnames(g$err.rate)[[2]]
  varImpPlot(rfor, main=subj)
}
cols=rainbow(length(unique(dat[,1]))+2)


set.seed(100)
g1<-randomForest(y~.,data=dat[,c(1,nxx1)])##,prox=TRUE)
print.err(g1,"L-measure")


set.seed(100)
g2<-randomForest(y~.,data=dat[,c(1,nxx2)])##,prox=TRUE)
print.err(g2,"Gstat")

set.seed(100)
g3<-randomForest(y~.,data=dat[,c(1,nxx3)])##,prox=TRUE)
print.err(g3,"Sholl")

set.seed(100)
gtot<-randomForest(y~.,data=dat)##,prox=TRUE)
print.err(gtot,"All")


## -rat
nrow(dat[dat$y=="rat",])
dat2<-dat[dat$y!="rat",]
dat2[,1]<-droplevels(dat2[,1])

set.seed(100)
g2rat<-randomForest(y~.,data=dat2[,c(1,nxx2)])##,prox=TRUE)
cols=rainbow(length(unique(dat2[,1]))+2)
last<-end(g2rat$err.rate)[1]
mains<-paste("Gstat Minus rat , OOB=",round(mean(g2rat$err.rate[last,1]),3))
matplot(g2rat$err.rate[,-26],lwd=2,col=cols,lty=1, type="l", main=mains)
legend("bottomright",inset=.05,legend=colnames(g2rat$err.rate),
       cex=.7,col=cols,lty=1,lwd=3)
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(g2rat,main=mains)








####removing species with less than 100 samples
#nrow(dat[dat$y=="rat",])
#dat2<-dat[dat$y!="rat",]
#dat2[,1]<-droplevels(dat2[,1])
