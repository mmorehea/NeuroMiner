############################################################################
##  Purpose: Variable Importance
############################################################################


library(spa)
library(randomForest)

####
## Function Definition
####
na.mean<-function(x,...){
  handle.na<-function(x){ #plug the mean into missing values
    x[is.na(x)]<-mean(x,na.rm=TRUE)
    return(x)
  }
  p<-dim(x)
  if(is.null(p)){ #special case: `x' is a vector
    return(handle.na(x))
  }
  for(i in 1:p[2]){
    if(!is.factor(x[,i]))# if `x' is a factor ignore it
      x[,i]<-handle.na(x[,i])
  }
  return(x)
}

nipal<-function(x,y,k){
  x<-scale(x,,scale=FALSE)
  y<-scale(y,,scale=FALSE)
  b<-p<-a<-u<-NULL
  for(i in 1:k){
    wt<-t(x)%*%y/as.numeric(t(y)%*%x%*%t(x)%*%y)
    tt<-x%*%wt
    pt<- t(x)%*%tt/as.numeric(t(tt)%*%tt)
    
    npt<-as.numeric(sqrt(t(pt)%*%pt))
    wn<-npt*wt
    tn<-npt*tt
    pn<-pt/npt
    bn<-as.numeric(t(tn)%*%y)/as.numeric(t(tn)%*%tn)
    x<-x-tn%*%t(pn)
    y<-y-bn*tn
    
    b<-c(b,bn)
    p<-cbind(p,pn)
    a<-cbind(a,wn)
    u<-cbind(u,tn)
  }
  dim(x)[2]->ab
  return(structure(list(b=b,p=p,a=a,u=u,d=ab,q=k),class="upls"))
}

vip<-function(obj,y,nm=NULL){
  if(class(obj)!="upls"){
    stop("Object is not of type upls")
  }
  a1<-as.vector(obj$a^2%*%cor(obj$u,y)^2 )
  if(is.null(nm)){
    a2<-1:length(a1)
    names(a1)<-paste(a2)
  }else{
    names(a1)<-nm
  }
  return(a1)##sort(a1,decreasing=TRUE))
}

###########
## Read in Data
# setwd("~/NeuroMiner/data_sets/Neuron_subsets")
# temp =list.files(pattern="../*.csv")
# myfiles<-lapply(temp, function(x) read.csv(x,header=TRUE))
# names(myfiles)<-temp


read.csv("~/NeuroMiner/data_sets/Neuron_subsets/1to18days.csv",T)->x

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

#save(dat, file= "NeuronDataMaster.Rdata")
nxx1<-2:65
nxx2<-66:79
nxx3<-81:dim(dat)[2] #80 is Sholl.1 should always be 1

cols=rainbow(length(unique(dat[,1]))+2)

set.seed(100)
g1<-randomForest(y~.,data=dat[,c(1,nxx1)])##,prox=TRUE)

#print.err(g1,"L-measure","NeuronDataMaster")


######
### build the following into a batch style... 
### add vip and nipal to pyramidJT

set.seed(020) ##so RF gets same result as in paper

gpls<-nipal(as.matrix(dat[,c(nxx1)]),as.numeric(dat$y),20)
x<-as.matrix(dat[,c(nxx1)])
y<-as.numeric(dat$y)

gpls<-nipal(x,y,40)
grf<-randomForest(y~.,data=data.frame(y=as.factor(y),x))
#grf<-randomForest(y~.,data=dat[,c(1,nxx1)])


vrf<-varImpPlot(grf)
 vpls<-vip(gpls,y,names(x))
#vpls<-vip(gpls,as.numeric(dat$y),names(dat[,c(nxx1)]))

vrf<-scale(vrf)
vpls<-scale(vpls)
plot(vpls,vrf,sub="L-measure",ylab="RF (Variable Score)",xlab="PLS (Variable Score)",pch=16,cex.sub=0.8)
indpls<-order(vpls)[dim(x)[2]:(dim(x)[2]-4)]
indrf<-order(vrf)[dim(x)[2]:(dim(x)[2]-4)]
points(vpls[indrf],vrf[indrf],pch=16,cex=2,col="white")
text(vpls[indrf],vrf[indrf],row.names(vrf)[indrf],col=gray(0.6))
points(vpls[indpls],vrf[indpls],pch=16,cex=2,col="white")
text(vpls[indpls],vrf[indpls],row.names(vpls)[indpls],col=gray(0.3))
abline(0,1)

#X11()

gpls<-nipal(z,y,20)
grf<-randomForest(y~.,data=data.frame(y=as.factor(y),z))
vrf<-varImpPlot(grf)
vpls<-vip(gpls,y,names(z))
vrf<-scale(vrf)
vpls<-scale(vpls)

plot(vpls,vrf,sub="Chemistry",ylab="RF (Variable Score)",xlab="PLS (Variable Score)",pch=16,cex.sub=0.8)
indpls<-order(vpls)[dim(z)[2]:(dim(z)[2]-4)]
indrf<-order(vrf)[dim(z)[2]:(dim(z)[2]-4)]
points(vpls[indrf],vrf[indrf],pch=16,cex=2,col="white")
text(vpls[indrf],vrf[indrf],row.names(vrf)[indrf],col=gray(0.6))
points(vpls[indpls],vrf[indpls],pch=16,cex=2,col="white")
text(vpls[indpls],vrf[indpls],row.names(vpls)[indpls],col=gray(0.3))


###
## Fit learner without view distinction and make plots
###
set.seed(020) ##so RF gets same result as in paper

dat=cbind(x,z)

gpls<-nipal(dat,y,20)
grf<-randomForest(y~.,data=data.frame(y=as.factor(y),dat))
X11()

vrf<-varImpPlot(grf)
vpls<-vip(gpls,y,names(dat))
vrf<-scale(vrf)
vpls<-scale(vpls)
plot(vpls,vrf,sub="Biology and Chemistry Data",ylab="RF (Variable Score)",xlab="PLS (Variable Score)",pch=16,cex.sub=0.8)
indpls<-order(vpls)[dim(dat)[2]:(dim(dat)[2]-4)]
indrf<-order(vrf)[dim(dat)[2]:(dim(dat)[2]-4)]
points(vpls[indrf],vrf[indrf],pch=16,cex=2,col="white")
text(vpls[indrf],vrf[indrf],row.names(vrf)[indrf],col=gray(0.6))
points(vpls[indpls],vrf[indpls],pch=16,cex=2,col="white")
text(vpls[indpls],vrf[indpls],row.names(vpls)[indpls],col=gray(0.3))
