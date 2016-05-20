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


set.seed(100)
g1<-randomForest(y~.,data=dat[,c(1,nxx1)])##,prox=TRUE)
cols=rainbow(length(unique(dat[,1]))+2)
last<-end(g1$err.rate)[1]
mains<-paste("L-measure, OOB=",round(mean(g1$err.rate[last,1]),3))
matplot(g1$err.rate,lwd=2,col=cols,lty=1,type="l",main=mains)
legend("bottomright",inset=.05,legend=colnames(g1$err.rate),
       cex=.7,col=cols,lty=1,lwd=3)
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(g1, main=mains)


set.seed(100)
g2<-randomForest(y~.,data=dat[,c(1,nxx2)])##,prox=TRUE)
cols=rainbow(length(unique(dat[,1]))+2)
last<-end(g2$err.rate)[1]
mains<-paste("Gstat, OOB=",round(mean(g2$err.rate[last,1]),3))
matplot(g2$err.rate,lwd=2,col=cols,lty=1,type="l",main=mains)
legend("bottomright",inset=.05,legend=colnames(g2$err.rate),
       cex=.7,col=cols,lty=1,lwd=3)
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(g2,main=mains)

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


set.seed(100)
g3<-randomForest(y~.,data=dat[,c(1,nxx3)])##,prox=TRUE)
cols=rainbow(length(unique(dat[,1]))+2)
last<-end(g3$err.rate)[1]
mains<-paste("Sholl , OOB=",round(mean(g3$err.rate[last,1]),3))
matplot(g3$err.rate,lwd=2,col=cols,lty=1,type="l",main=mains)
legend("bottomright",inset=.05,legend=colnames(g3$err.rate),
       cex=.7,col=cols,lty=1,lwd=3)
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(g3,main=mains)


set.seed(100)
gtot<-randomForest(y~.,data=dat)##,prox=TRUE)
cols=rainbow(length(unique(dat[,1]))+2)
last<-end(gtot$err.rate)[1]
mains<-paste("All , OOB=",round(mean(gtot$err.rate[last,1]),3))
matplot(gtot$err.rate,lwd=2,col=cols,lty=1,type="l",main=mains)
leg<-legend("bottomright",inset=.05,legend=colnames(gtot$err.rate),
       cex=.7,col=cols,lty=1,lwd =3)

##MAP cols to dimnames(g$err.rate)[[2]]
trash<-varImpPlot(gtot)
